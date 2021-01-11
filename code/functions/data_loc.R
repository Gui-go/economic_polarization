# Dados espaciais das divisões regionais em SC (IBGE-API-Serviços)
suppressMessages(
  loc <- readr::read_csv("data/clean/localizacoes_ibge_sc.csv") %>% 
    dplyr::mutate(
      cd_mun = as.character(cd_mun),
      cd_micro = as.character(cd_micro)
    )
)