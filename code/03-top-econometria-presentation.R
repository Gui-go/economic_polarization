# R-script 03-top-econometria-presentation.R

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
if(!require(GGally)){install.packages("GGally")}
if(!require(ggfortify)){install.packages("ggfortify")}


# Functions ---------------------------------------------------------------

# Function normalize a vector
source("code/functions/fct_normalize.R")

# Correlation Matrix Function
source("code/functions/fct_cormatrix.R")

# Model functions
source("code/functions/fct_model.R")

# Data --------------------------------------------------------------------

# Tabela t1554
source("code/functions/data_df_t1554.R")
df_t1554 <- data_df_t1554(c(41, 42, 43))

# Exportações
source("code/functions/data_exp_mun.R")
exp_sh0 <- data_exp_comex(c('SC', 'PR', 'RS'), sh2s = FALSE)
exp_sh2 <- data_exp_comex(c('SC', 'PR', 'RS'), sh2s = TRUE)

# Section-sh2 identifiers
source(file = "code/functions/data_hs_sections.R")
hs_sections <- data_hs_sections()
unique(hs_sections$section)

# Localizações
source("code/functions/data_loc.R")
loc <- data_loc(c('SC', 'PR', 'RS'))


# Total exp ---------------------------------------------------------------

# joins
data_sh0 <- dplyr::left_join(exp_sh0, df_t1554, by='cd_mun') %>%
  dplyr::left_join(., loc, by = "cd_mun") %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_meso, nm_meso, sg_uf) %>% 
  dplyr::summarise(
    pop_sup_comp = sum(pop_sup_comp, na.rm = T),
    log_pop_sup_comp = log(sum(pop_sup_comp, na.rm = T)),
    exp = sum(exp_fob, na.rm = T),
    log_exp = log(sum(exp_fob, na.rm = T)),
    .groups = 'drop'
  )

# Matrix de correlação completa
matrixCorrelationPlot( 
  data_sh0 %>% 
    dplyr::select('pop_sup_comp', 'log_pop_sup_comp', 'exp', 'log_exp')
)

# Matrix de correlação focada
matrixCorrelationPlot( 
  data_sh0 %>%
    dplyr::select('log_pop_sup_comp', 'log_exp')
)

# Gráfico de correlação
cor_plot <- ggplot2::ggplot(data_sh0) +
  ggplot2::geom_point(ggplot2::aes(x = log_pop_sup_comp, y = log_exp, label = nm_micro, colour=factor(sg_uf, levels = c('PR', 'SC', 'RS'), labels = c('PR', 'SC', 'RS'))), size = 4, alpha = .76, label="") +
  ggplot2::geom_line(aes(x=log_pop_sup_comp, y=predict(lm(log_exp ~ log_pop_sup_comp)))) +
  ggplot2::labs(
    title = 'Gráfico de correlação',
    y = 'Log exportações fob',
    x = 'Log População com curso superior completo',
    colour = 'UF'
  ); cor_plot

# Gráfico de correlação dinâmico
plotly::ggplotly(cor_plot, tooltip='label')

# Modelo para Total exp
m0 <- fct_model(data_sh0)

# df do resultado
res <- data.frame(
  cd_exp='S0',
  section="Total Exportações",
  b1_est=m0$lm_summary$coefficients[2,1],
  b1_stde=m0$lm_summary$coefficients[2,2],
  b1_tvalue=m0$lm_summary$coefficients[2,3],
  b1_pvalue=m0$lm_summary$coefficients[2,4],
  b0_est=m0$lm_summary$coefficients[1,1],
  b0_std_e=m0$lm_summary$coefficients[1,2],
  b0_tvalue=m0$lm_summary$coefficients[1,3],
  b0_pvalue=m0$lm_summary$coefficients[1,4],
  fstat = m0$lm_summary$fstatistic[['value']],
  fstat_pvalue = pf(m0$lm_summary$fstatistic[['value']], m0$lm_summary$fstatistic[['numdf']], m0$lm_summary$fstatistic[['dendf']], lower.tail = F),
  r2 = m0$lm_summary$r.squared,
  r2_adj = m0$lm_summary$adj.r.squared,
  bj_pvalue = m0$jb_test$p.value,
  sw_pvalue = m0$sw_test_res$p.value,
  n = length(m0$lm_summary$residuals), 
  row.names = 'S0'
)
res


# section -----------------------------------------------------------------

# join
exp_sec <- exp_sh2 %>% 
  dplyr::mutate(cd_hs2 = paste0("SH", exp_sh2$sh2)) %>% 
  dplyr::left_join(., hs_sections, by="cd_hs2") %>% 
  dplyr::group_by(cd_mun, cd_section, section) %>% 
  dplyr::summarise(
    exp_fob = sum(exp_fob),
    .groups = 'drop'
  )

# Sperando em seções de produtos
lista = list()
for(i in unique(exp_sec$cd_section[!is.na(exp_sec$cd_section)])){
  dd <- exp_sec %>% 
    dplyr::filter(cd_section==i) %>% 
    dplyr::left_join(., df_t1554, by='cd_mun') %>%
    dplyr::left_join(., loc, by = "cd_mun") %>% 
    dplyr::group_by(cd_section, section, cd_micro, nm_micro, cd_meso, nm_meso, sg_uf) %>% 
    dplyr::summarise(
      pop_sup_comp = sum(pop_sup_comp, na.rm = T),
      log_pop_sup_comp = log(sum(pop_sup_comp, na.rm = T)),
      exp = sum(exp_fob, na.rm = T),
      log_exp = log(sum(exp_fob, na.rm = T)),
      .groups = 'drop'
    ) %>%
    dplyr::filter(pop_sup_comp>0 & exp>0)
  lista[[i]] = dd
}
# lista

# Modelagem > df dos resultados
for(i in names(lista)){
  mm <- fct_model(lista[[i]])
  res <- res %>% tibble::add_row(
    cd_exp=as.character(unique(lista[[i]]$cd_section)),
    section = as.character(unique(lista[[i]]$section)),
    b1_est=mm$lm_summary$coefficients[2,1],
    b1_stde=mm$lm_summary$coefficients[2,2],
    b1_tvalue=mm$lm_summary$coefficients[2,3],
    b1_pvalue=mm$lm_summary$coefficients[2,4],
    b0_est=mm$lm_summary$coefficients[1,1],
    b0_std_e=mm$lm_summary$coefficients[1,2],
    b0_tvalue=mm$lm_summary$coefficients[1,3],
    b0_pvalue=mm$lm_summary$coefficients[1,4],
    fstat = mm$lm_summary$fstatistic[['value']],
    fstat_pvalue = pf(mm$lm_summary$fstatistic[['value']], mm$lm_summary$fstatistic[['numdf']], mm$lm_summary$fstatistic[['dendf']], lower.tail = F),
    r2 = mm$lm_summary$r.squared,
    r2_adj = mm$lm_summary$adj.r.squared,
    bj_pvalue = mm$jb_test$p.value,
    sw_pvalue = mm$sw_test_res$p.value,
    n = length(mm$lm_summary$residuals)
  )
}

# Tratamento do resultado
res <- res %>%
  tibble::remove_rownames(.) %>% 
  dplyr::mutate(elast_rank = dplyr::dense_rank(desc(b1_est))) %>% 
  dplyr::arrange(elast_rank) %>% 
  dplyr::select(elast_rank, cd_exp, b1_est, dplyr::everything())
# res %>% View()

# Gráfico de Intervalos de confinça
gg2 <- ggplot(res)+
  geom_errorbar(aes(x=rev(factor(cd_exp, levels = cd_exp, labels = rev(cd_exp))), ymin=b1_est-b1_stde, ymax=b1_est+b1_stde))+
  geom_point(aes(x = rev(elast_rank), y = b1_est))+
  coord_flip()+
  labs(
    title = "Escada de capacitação capitalista",
    subtitle = "Dados do sul Brasileiro",
    y = "Elasticidade educação superior sobre exportações",
    x = "Seções de produtos ",
    caption = "Guigo"
  ); gg2




# Educated people are better at living than uneducated people (Becker, Guigo)