# R-script 02-sul-micro-analysis.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)

# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(st)){install.packages("st")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(janitor)){install.packages("janitor")}
if(!require(spdep)){install.packages("spdep")}
if(!require(vroom)){install.packages("vroom")}
if(!require(jtools)){install.packages("jtools")}


# Functions ---------------------------------------------------------------

# Function to get the centroid of a polygon
source("code/functions/fct_cent_as_col.R")

# Function normalize a vector
source("code/functions/fct_normalize.R")

# Correlation Matrix Function
source("code/functions/fct_cormatrix.R")

# Data --------------------------------------------------------------------

# Tabela t1554
source("code/functions/data_df_t1554.R")
df_t1554 <- data_df_t1554(c(41, 42, 43))

# Exportações
# source("code/functions/data_exp_mun.R")
# exp_comex <- data_exp_comex(sh2s = "", ufs = c('SC', 'PR', 'RS'))
exp_comex <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::filter(co_ano>=2010) %>%
  # dplyr::filter(sg_uf_mun%in%c(ufs)) %>%
  dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
  # dplyr::filter(sh2==sh2s) %>%
  dplyr::group_by(co_mun, sh2) %>%
  dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
  dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(cd_mun, sh2, exp_fob)



# Localizações
source("code/functions/data_loc.R")
# loc <- data_loc(c('SC', 'PR', 'RS'))
sc <- data_loc(c('SC')); pr <- data_loc(c('PR')); rs <- data_loc(c('RS'))
loc <- bind_rows(list(sc, pr, rs))
dim(loc)

# Poligonos do Sul
source("code/functions/data_sul_shp.R")
sf <- get_sul_sf()
plot(sf['sigla_uf'])


# Join --------------------------------------------------------------------

suppressWarnings(
  suppressMessages(
    micro_shp <- dplyr::left_join(sf, loc, by = 'cd_mun') %>% 
      stats::na.omit() %>% 
      dplyr::group_by(cd_micro) %>%
      dplyr::summarise() %>% 
      cent_as_cols(.) %>% 
      sf::st_set_crs(4326)
  )
)

data <- dplyr::left_join(exp_comex, df_t1554, by='cd_mun') %>%
  dplyr::left_join(., loc, by = "cd_mun") %>% 
  dplyr::group_by(cd_micro) %>% 
  dplyr::summarise(
    pop_sup_comp = sum(pop_sup_comp, na.rm = T),
    log_pop_sup_comp = log(sum(pop_sup_comp, na.rm = T)),
    exp = sum(exp_fob, na.rm = T),
    log_exp = log(sum(exp_fob, na.rm = T)),
    .groups = 'drop'
  ) %>% 
  dplyr::left_join(., micro_shp, by = "cd_micro") %>% 
  sf::st_as_sf(.)

# Matrix de correlação
matrixCorrelationPlot(
  data %>% as.data.frame() %>% 
    dplyr::select('pop_sup_comp', 'log_pop_sup_comp', 'exp', 'log_exp')
)


# Model -------------------------------------------------------------------
# data_bck <- data
# data <- data_bck
data <- data %>% na.omit()
# Aspatial linear model
reg <- lm(log_exp ~ log_pop_sup_comp, data = data)
summary(reg)
jtools::summ(reg, digits = 5)

# Análise dos resíduos
data$reg_res <- reg$residuals
data$reg_res_norm <- normalize(reg$residuals)
hist(data$reg_res_norm, 30)
ggplot()+
  geom_density(aes(data$reg_res))+
  labs(title = "Densidade dos resíduos do modelo", x = "Resíduos", y = "Densidade")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
summary(data$reg_res)


# Definição de visinhança entre as microrregiões
nb <- spdep::poly2nb(data, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Autocorrelação Espacial MC1
spdep::moran.test(data$reg_res_norm, lw)
spdep::moran.test(data$reg_res, lw)
hist(data$reg_res, 30)
plot(data['reg_res'])
ggplot(data["reg_res"])+geom_sf(aes(fill = reg_res))
plot(data['reg_res_norm'])
shapiro.test(data$reg_res) # If p-value is small, data is not normally distributed


# Spatial Model -----------------------------------------------------------
# install.packages("spgwr")
# library(spgwr)

# spatial df
dd <- sp::SpatialPointsDataFrame(
  data=data.frame(
    log_exp = data$log_exp,
    log_pop_sup_comp = data$log_pop_sup_comp
  ), 
  coords=cbind(data$centlng, data$centlat)
)

# banda adaptativa (kernel)
GWRbandwidth <- spgwr::gwr.sel(log_exp ~ log_pop_sup_comp, data=dd, adapt=T) 

# GWR model
gwr_model <- spgwr::gwr(
  log_exp ~ log_pop_sup_comp, data = dd, 
  adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE
)
gwr_model

# resultados
results <- as.data.frame(gwr_model$SDF)
results$cd_micro <- data$cd_micro
results$nm_micro <- data$nm_micro

results %>% # 'nm_micro', 
  dplyr::select('cd_micro', 'log_pop_sup_comp', 'log_pop_sup_comp_se', 'gwr.e', 'pred', 'pred.se', 'localR2', 'coords.x1', 'coords.x2') %>% 
  knitr::kable(.)

# Spatial df with the results from GWR attached to the polygons
df_gwr_sc <- dplyr::left_join(micro_shp, results, by = 'cd_micro')


plot(df_gwr_sc['log_pop_sup_comp'])



