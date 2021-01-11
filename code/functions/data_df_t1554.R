suppressMessages(
  df <- readr::read_csv("data/clean/tabela1554.csv") %>% 
    janitor::clean_names() %>%
    dplyr::mutate(
      uf = substr(cod, 1, 2),
      cd_mun = as.character(cod)
    ) %>%
    dplyr::rename('pop_sup_comp'='superior_completo') %>% 
    dplyr::filter(uf%in%c('42')) %>% 
    dplyr::group_by(cd_mun) %>% 
    dplyr::summarise(
      pop_sup_comp = pop_sup_comp,
      municipio = municipio
    ) %>% 
    dplyr::select(cd_mun, municipio, pop_sup_comp)
)