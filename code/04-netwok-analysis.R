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


# Functions ---------------------------------------------------------------

# Function to get the centroid of a polygon
source("code/functions/fct_cent_as_col.R")

# Function normalize a vector
source("code/functions/fct_normalize.R")

# Correlation Matrix Function
source("code/functions/fct_cormatrix.R")

# Data --------------------------------------------------------------------

# Tabela t1554
# source("code/functions/data_df_t1554.R")
# df_t1554 <- data_df_t1554(c(41, 42, 43))

# Exportações
# source("code/functions/data_exp_mun.R")
# exp_comex <- data_exp_comex(c('SC', 'PR', 'RS'), sh2s = FALSE)
ss <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv")
exp <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  # dplyr::filter(CO_PAIS%in%c('160', '249')) %>% 
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::group_by(CO_MUN, CO_PAIS) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>% 
  dplyr::rename('cd_mun'='CO_MUN', 'co_pais'='CO_PAIS') %>% 
  dplyr::filter(exp >= 100000000) %>% 
  dplyr::mutate(cd_mun = as.character(cd_mun))
exp



# Localizações
source("code/functions/data_loc.R")
loc <- data_loc(c('SC', 'PR', 'RS'))

library(readr)
pp <- read_csv("data/raw/PAIS.csv") %>%
  dplyr::rename("name_country"="NO_PAIS_ING", "co_pais"="CO_PAIS") %>% 
  dplyr::mutate(co_pais=as.character(co_pais)) %>% 
  dplyr::select(co_pais, name_country)


df <- dplyr::left_join(exp, loc, by = 'cd_mun') %>% 
  dplyr::group_by(cd_meso, nm_meso, pais) %>% 
  dplyr::summarise(
    sg_uf = dplyr::first(sg_uf),
    exp = sum(exp)
  ) %>% 
  dplyr::mutate(
    cd_meso = as.character(cd_meso)
  ) %>% 
  as.data.frame() %>% 
  dplyr::select(nm_meso, pais, exp)# %>% 
  graph_from_data_frame(., directed = FALSE)
df


df



library(ggraph)
library(igraph)
expsh <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::mutate("sh2" = substr(SH4, 1, 2)) %>%
  # dplyr::filter(sh2==sh2s) %>%
  dplyr::group_by(sh2, CO_MUN) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>% 
  dplyr::rename('cd_mun'='CO_MUN') %>% 
  dplyr::filter(exp >= 100000000) %>%
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  igraph::graph_from_data_frame(., directed = FALSE)

exp2 <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  # dplyr::filter(CO_PAIS%in%c('160', '249')) %>% 
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::group_by(CO_MUN, CO_PAIS) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>% 
  dplyr::rename('cd_mun'='CO_MUN', 'co_pais'='CO_PAIS') %>% 
  dplyr::filter(exp >= 100000000) %>% 
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  dplyr::left_join(., loc, by = 'cd_mun') %>% 
  dplyr::group_by(nm_meso, co_pais) %>% 
  dplyr::summarise(
    sg_uf = dplyr::first(sg_uf),
    exp = sum(exp)
  ) %>% 
  as.data.frame() %>% 
  dplyr::left_join(., pp, by="co_pais") %>% 
  dplyr::select(nm_meso, name_country, exp, sg_uf) %>% 
  na.omit()

exp3 <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::group_by(CO_MUN, CO_PAIS) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>% 
  dplyr::rename('cd_mun'='CO_MUN', 'co_pais'='CO_PAIS') %>% 
  dplyr::filter(exp >= 100000000) %>% 
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  dplyr::left_join(., loc, by = 'cd_mun') %>% 
  dplyr::group_by(sg_uf, co_pais) %>% 
  dplyr::summarise(
    exp = sum(exp)
  ) %>% 
  as.data.frame() %>% 
  dplyr::left_join(., pp, by="co_pais") %>% 
  dplyr::mutate(sg_uf2=sg_uf) %>% 
  dplyr::select(sg_uf, name_country, exp, sg_uf2) %>% 
  na.omit() 

exp4 <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::mutate("sh2" = substr(SH4, 1, 2)) %>%
  dplyr::group_by(sh2, CO_MUN) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>%
  dplyr::rename('cd_mun'='CO_MUN') %>% 
  dplyr::filter(exp >= 100000000) %>%              
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  dplyr::left_join(., loc, by = 'cd_mun') %>% 
  dplyr::group_by(sh2, sg_uf) %>% 
  dplyr::summarise(
    exp = sum(exp)
  ) %>% 
  as.data.frame()

exp5 <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::mutate("sh2" = substr(SH4, 1, 2)) %>%
  dplyr::group_by(sh2, CO_MUN) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>%
  dplyr::rename('cd_mun'='CO_MUN') %>% 
  dplyr::filter(exp >= 100000000) %>%              
  dplyr::mutate(cd_mun = as.character(cd_mun)) %>% 
  dplyr::left_join(., loc, by = 'cd_mun') %>% 
  dplyr::group_by(sh2, nm_meso) %>% 
  dplyr::summarise(
    exp = sum(exp)
  ) %>% 
  as.data.frame()

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
  dplyr::filter(exp>=100)
exp6
ig <- igraph::graph_from_data_frame(exp6, directed = FALSE)
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


ig <- igraph::graph_from_data_frame(exp4, directed = FALSE)
ggraph(ig, layout = "manual", x = V(ig)$x, y = V(ig)$y) + 
	 geom_edge_link0(aes(alpha = exp, width = exp, colour = exp)) + 
	 scale_edge_colour_gradient(low = "#87CEFF", 
    high = "#27408B") + 
	 scale_edge_width(range = c(0.7, 2.5)) + 
	 scale_edge_alpha(range = c(0.5, 1)) + 
	 geom_node_point(aes(fill = cluster_louvain, size = closeness), 
    colour = "#000000", shape = 21, stroke = 0) + 
	 scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
	 scale_size(range = c(3, 8)) + 
	 geom_node_text(aes(label = name), 
    colour = "#000000", size = 8, family = "serif") + 
	 theme_graph() + 
	 theme(legend.position = "none")

expsh
exp
expi <- igraph::graph_from_data_frame(exp, directed = FALSE)
expi
# Rascunho ----------------------------------------------------------------


library(igraph)
# load raw network data ----
A <- utils::read.table(file = '/home/shiny/economic_polarization/data/clean/ss_sc.csv',
                       header = TRUE, sep = ',', quote = '', stringsAsFactors = FALSE)

# create network ----
library(ggraph)
ggraph::ggraph(g, layout = "manual", x = V(g)$x, y = V(g)$y) + 
	 geom_edge_link0(aes(alpha = backbone, 
    width = exp, colour = exp)) + 
	 scale_edge_colour_gradient(low = "#87CEFF", high = "#27408B") + 
    scale_edge_width(range = c(0.3, 1.2)) + 
	 scale_edge_alpha(range = c(0.1, 1)) + 
    geom_node_point(aes(fill = degree, size = degree), colour = "#000000", shape = 21, 
        stroke = 0.3) + 
	 scale_fill_gradient(low = "#87CEFF", high = "#27408B") + 
    scale_size(range = c(1, 10)) + 
	 geom_node_text(aes(label = name), colour = "#000000", 
    size = 3.5, family = "serif") + 
	 theme_graph() + 
	 theme(legend.position = "none")

<- graph_from_data_frame(df,directed = FALSE)
class(g)

class(df)



sources <- df %>%
  dplyr::ungroup() %>% 
  dplyr::distinct(cd_meso) %>%
  dplyr::rename(label = cd_meso) %>% 
  dplyr::select(label)

destinations <- df %>%
  dplyr::ungroup() %>% 
  dplyr::distinct(pais) %>%
  dplyr::rename(label = pais) %>% 
  dplyr::select(label)

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column("id")
# nodes

per_route <- df %>%  
  dplyr::group_by(cd_meso, pais) %>%
  dplyr::summarise(weight = exp) %>% 
  dplyr::ungroup()
per_route

edges <- per_route %>% 
  dplyr::left_join(nodes, by = c("cd_meso" = "label")) %>% 
  dplyr::rename(from = id)

edges <- edges %>% 
  dplyr::left_join(nodes, by = c("pais" = "label")) %>% 
  dplyr::rename(to = id)

edges <- select(edges, from, to, weight)
edges

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)

plot(routes_network, vertex.cex = 2)

plot(routes_network, vertex.cex = 3, mode = "circle")

library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(routes_igraph, edge.arrow.size = 0.2)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

library(tidygraph)
library(ggraph)
# install.packages("ggraph")

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()


#####################################################
# rm(list = ls())

library(tibble)
library(dplyr)


library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

edge_list
node_list

# _------------------------------

'42'%in%unique(ss$CO_PAIS)

ss <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv")
ss_sc <- ss %>% 
  dplyr::filter(SG_UF_MUN%in%c('SC', 'PR', 'RS')) %>%
  # dplyr::filter(CO_PAIS%in%c('160', '249')) %>% 
  dplyr::filter(CO_ANO>=2010) %>% 
  dplyr::group_by(SG_UF_MUN, CO_PAIS) %>% 
  dplyr::summarise(exp = sum(VL_FOB)) %>% 
  dplyr::rename('uf'='SG_UF_MUN', 'pais'='CO_PAIS') %>% 
  dplyr::filter(exp >= 800000000)
ss_sc

#230 alemanha; 1600 china; 639 argentina; 2496 eua; 6076 pt


sources <- ss_sc %>%
  ungroup() %>% 
  distinct(uf) %>%
  rename(label = uf) %>% 
  select(label)

destinations <- ss_sc %>%
  ungroup() %>% 
  distinct(pais) %>%
  rename(label = pais) %>% 
  select(label)

nodes <- full_join(sources, destinations, by = "label")
nodes

library(dplyr)
library(tidyverse)
library(network)
library(tibble)
# install.packages('tidyverse')
nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- ss_sc %>%  
  group_by(uf, pais) %>%
  summarise(weight = exp) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("uf" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("pais" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)

plot(routes_network, vertex.cex = 2)

plot(routes_network, vertex.cex = 3, mode = "circle")


library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(routes_igraph, edge.arrow.size = 0.2)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

library(tidygraph)
library(ggraph)
# install.packages("ggraph")

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

library(visNetwork)
library(networkD3)


visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/5 + 1)
edges <- mutate(edges, width = log(weight)/6)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")


# kateto ------------------------------------------------------------------

# install.packages("snahelper")
library(snahelper)

ss_sc
# rio::export(x = ss_sc, file = "ss_sc2.csv")


library(igraph)
# load raw network data ----
A <- utils::read.table(file = '/home/shiny/economic_polarization/data/clean/ss_sc2.csv',
                       header = TRUE, sep = ',', quote = '', stringsAsFactors = FALSE)
class(A)
# create network ----
ggraph(g, layout = "manual", x = V(g)$x, y = V(g)$y) + 
	 geom_edge_link0(edge_colour = "#A8A8A8", edge_width = 0.8, edge_alpha = 1) + 
	 geom_node_point(aes(fill = cluster_louvain), 
    colour = "#000000", size = 5, shape = 21, stroke = 0.3) + 
	 scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
	 theme_graph() + 
	 theme(legend.position = "none") <- graph_from_data_frame(A,directed = FALSE)
class(g)

install.packages("oaqc")
library(oaqc)
