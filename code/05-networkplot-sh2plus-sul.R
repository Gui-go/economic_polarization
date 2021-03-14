# R-script 05-networkplot-sh2plus-sul.R

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



# Localizações
source("code/functions/data_loc.R")
loc <- data_loc(c('SC', 'PR', 'RS'))


ss <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv")

exp6 <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::mutate("sh2" = substr(SH4, 1, 2)) %>%
  dplyr::group_by(sh2, CO_MUN) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>%
  dplyr::rename('cd_mun'='CO_MUN') %>% 
  # dplyr::filter(exp >= 100000000) %>%              
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  dplyr::left_join(., loc, by = 'cd_mun') %>% 
  dplyr::group_by(sh2, sg_uf) %>% 
  dplyr::summarise(
    exp = sum(exp)
  ) %>% 
  as.data.frame() %>% 
  dplyr::mutate(
    exp = ifelse(sg_uf=='SC', exp/7252502, ifelse(sg_uf=='PR', exp/11516840, ifelse(sg_uf=='RS', exp/11422973, 0)))
  ) %>% 
  dplyr::filter(exp>=100) %>% 
  dplyr::select(sg_uf, sh2, exp)

exp6
scncm <- exp6[, c('sg_uf', 'sh2')] %>% dplyr::filter(sg_uf=='SC') %>% dplyr::select(sh2) %>% pull()

ncm <- read_excel("data/clean/tabela_ncm.xls", skip = 3) 
ncmf <- ncm %>% 
  janitor::clean_names() %>% 
  dplyr::select(codigo_ncm_sh2, descricao) %>% 
  na.omit() %>% 
  dplyr::filter(codigo_ncm_sh2 %in% unique(exp6$sh2)) #%>%
  # dplyr::filter(codigo_ncm_sh2 %in% as.character(scncm))
  # View()

ncmf[which(ncmf$codigo_ncm_sh2=="72"), "descricao"]
ncmf[c(20:30), ]

ig <- igraph::graph_from_data_frame(exp6, directed = FALSE)
ig

library(ggraph)
ggraph(ig, layout = "manual", x = V(ig)$x, y = V(ig)$y) + 
  geom_edge_link0(aes(alpha = exp, width = exp, colour = exp)) + 
  scale_edge_colour_gradient(low = "#87CEFF", 
                             high = "#27408B") + 
  scale_edge_width(range = c(0.5, 3)) + 
  scale_edge_alpha(range = c(0.4, 1)) + 
  geom_node_point(aes(fill = cluster_louvain, size = degree), colour = "#000000", 
                  shape = 21, stroke = 0.3) + 
  scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
  scale_size(range = c(3, 8)) + 
  geom_node_text(aes(label = name), colour = "#000000", 
                 size = 8.5, family = "serif") + 
  theme_graph() + 
  theme(legend.position = "none")
