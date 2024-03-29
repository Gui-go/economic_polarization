# Exportações
# http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download
data_exp_comex <- function(ufs, sh2s){
  if(sh2s){
    suppressMessages(
      vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
        suppressMessages() %>% 
        janitor::clean_names() %>% 
        dplyr::filter(co_ano>=2010) %>%
        # dplyr::filter(sg_uf_mun%in%c(ufs)) %>%
        dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
        dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
        # dplyr::filter(sh2==sh2s) %>%
        dplyr::group_by(co_mun, sh2) %>%
        dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
        dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(cd_mun, sh2, exp_fob)
    )
  }else{
    suppressMessages(
      vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
        suppressMessages() %>% 
        janitor::clean_names() %>% 
        dplyr::filter(co_ano>=2010) %>%
        dplyr::filter(sg_uf_mun%in%c(ufs)) %>%
        dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
        dplyr::group_by(co_mun) %>%
        dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
        dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
        dplyr::select(cd_mun, exp_fob)
    )
  }
}