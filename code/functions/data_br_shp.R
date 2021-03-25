#! R-script
# .shp Shapefile das divisões territoriais do Brasil (IBGE-Geociencias-divisao_do_territorio)
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html
# organizacao_do_territorio > malhas_territoriais > malhas_municipais > municipio_2020 > Brasil > BR

data_br_shp <- function(){
  
  br_uf <- sf::st_read("data/raw/BR_UF_2020/") %>%
    janitor::clean_names() %>% 
    sf::st_set_crs(4326)
  # plot(br_uf['cd_uf'])
  
  br_meso <- sf::st_read("data/raw/BR_Mesorregioes_2020/") %>%
    janitor::clean_names() %>% 
    sf::st_set_crs(4326)
  # plot(br_meso['cd_meso'])
  
  br_micro <- sf::st_read("data/raw/BR_Microrregioes_2020/") %>%
    janitor::clean_names() %>% 
    sf::st_set_crs(4326)
  # plot(br_micro['cd_micro'])
  
  br_rgi <- sf::st_read("data/raw/BR_RG_Imediatas_2020/") %>%
    janitor::clean_names() %>% 
    sf::st_set_crs(4326)
  # plot(br_rgi['cd_rgi'])
  
  br_rgint <- sf::st_read("data/raw/BR_RG_Intermediarias_2020/") %>%
    janitor::clean_names() %>% 
    sf::st_set_crs(4326)
  # plot(br_rgint['cd_rgint'])
  
  br_list <- list(
    'br_uf' = br_uf,
    'br_meso' = br_meso,
    'br_micro' = br_micro,
    'br_rgi' = br_rgi,
    'br_rgint' = br_rgint
  )
  
  return(br_list)
  
}

# data_br_shp()
