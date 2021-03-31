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

source(file = 'code/functions/data_loc.R')
source(file = 'code/functions/data_br_shp.R')

# Data --------------------------------------------------------------------

br_shp <- data_br_shp()
br_loc <- data_loc('SC')

# Treatment ---------------------------------------------------------------


# Maps --------------------------------------------------------------------

# ggplot(br_shp[['br_uf']]) +
#   geom_sf(aes(fill = as.numeric(cd_uf))) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   labs(title = 'Estados do Brasil', fill = 'Legenda')

# ggplot(br_shp[['br_meso']]) +
#   geom_sf(aes(fill = as.numeric(cd_meso))) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   labs(title = 'Mesorregiões do Brasil', fill = 'Legenda')

# ggplot(br_shp[['br_micro']]) +
#   geom_sf(aes(fill = as.numeric(cd_micro))) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   labs(title = 'Microrregiões do Brasil', fill = 'Legenda')

# ggplot(br_shp[['br_rgint']]) +
#   geom_sf(aes(fill = as.numeric(cd_rgint))) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   labs(title = 'Regiões intermediárias do Brasil', fill = 'Legenda')

# ggplot(br_shp[['br_rgi']]) +
#   geom_sf(aes(fill = as.numeric(cd_rgi))) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   labs(title = 'Regiões imediatas do Brasil', fill = 'Legenda')

plot(br_shp[['br_uf']]['cd_uf'])
plot(br_shp[['br_meso']]['cd_meso'])
plot(br_shp[['br_micro']]['cd_micro'])
plot(br_shp[['br_rgi']]['cd_rgi'])
plot(br_shp[['br_rgint']]['cd_rgint'])


br_shp_f <- list()
for (i in 1:length(br_shp)) {
  br_shp_f[[i]] <- br_shp[[i]] %>% dplyr::filter(sigla_uf=='SC')
  plot(br_shp_f[[i]][1])
}


plot(br_shp_f[[5]][1])
