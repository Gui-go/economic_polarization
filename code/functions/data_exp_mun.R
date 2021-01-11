# Exportações
# http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download
suppressMessages(
  exp_comex <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
    suppressMessages() %>% 
    janitor::clean_names() %>% 
    dplyr::filter(co_ano>=2010) %>%
    dplyr::filter(sg_uf_mun%in%c('SC')) %>%
    dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
    # dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
    # dplyr::filter(sh2=='69') %>% 
    dplyr::group_by(co_mun) %>%
    dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
    dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
    dplyr::select(cd_mun, exp_fob)
)