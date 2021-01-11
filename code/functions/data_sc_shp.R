# .shp Shapefile dos municípios de SC (IBGE-Geociencias-divisao_do_territorio)

suppressWarnings(
  suppressMessages(
    sc_shp <- sf::st_read("data/raw/sc_municipios/") %>%
      janitor::clean_names() %>% 
      sf::st_set_crs(4326)
  )
)