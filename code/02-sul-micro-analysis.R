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
source("code/functions/data_exp_mun.R")
exp_comex <- exp_comex(c('SC', 'PR', 'RS'))

# Localizações
source("code/functions/data_loc.R")
loc <- data_loc(c('SC', 'PR', 'RS'))

# Poligonos de SC
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

# Aspatial linear model
reg <- lm(log_exp ~ log_pop_sup_comp, data = data)
summary(reg)
jtools::summ(reg, digits = 5)

# Análise dos resíduos
data$reg_res <- reg$residuals
data$reg_res_norm <- normalize(reg$residuals)
hist(data$reg_res_norm, 30)

# Definição de visinhança entre as microrregiões
nb <- spdep::poly2nb(data, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Autocorrelação Espacial MC1
spdep::moran.test(data$reg_res_norm, lw)
hist(data$reg_res, 30)
plot(data['reg_res'])
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

results %>% 
  dplyr::select('cd_micro', 'nm_micro', 'log_pop_sup_comp', 'log_pop_sup_comp_se', 'gwr.e', 'pred', 'pred.se', 'localR2', 'coords.x1', 'coords.x2') %>% 
  knitr::kable(.)

# Spatial df with the results from GWR attached to the polygons
df_gwr_sc <- dplyr::left_join(micro_shp, results, by = 'cd_micro')


plot(df_gwr_sc['log_pop_sup_comp'])



