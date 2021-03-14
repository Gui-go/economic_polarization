# R-script

# setup -------------------------------------------------------------------
rm(list = ls())
gc()

# libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)

# Model functions
source("code/functions/fct_model.R")


# load --------------------------------------------------------------------
rais1 <- readr::read_delim(
  'data/raw/RAIS_VINC_PUB_SUL.txt',
  delim = ";", escape_double = FALSE,
  locale = readr::locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>% 
  janitor::clean_names()

rais2 <- rais1 %>% 
  dplyr::mutate(
    esc = as.numeric(escolaridade_apos_2005),
    vl_remun = as.numeric(gsub(",", '.', stringr::str_remove(vl_remun_media_nom, "^0+"))),
    vl_remun_ph = vl_remun/(qtd_hora_contr*4),
    xp = idade -18
  ) %>% 
  dplyr::filter(
    nacionalidade %in% c(10, 20),
    ind_portador_defic == 0,
    vinculo_ativo_31_12 == 1,
    idade >= 18,
    qtd_hora_contr > 12,
    vl_remun > 1000,
    vl_remun < 40000
  ) %>% 
  select(esc, xp, idade, raca_cor, sexo_trabalhador, ibge_subsetor, vl_remun, vl_remun_ph)
rm(rais1)

# ggplot(rais2)+
#   geom_boxplot(aes(x = ibge_subsetor, y = vl_remun_ph, fill = ibge_subsetor))+
#   theme_minimal()

rais3 <- rais2 %>% 
  # dplyr::filter(ibge_subsetor%in%c(11, 18, 24)) %>%
  dplyr::filter(ibge_subsetor%in%c(24)) %>%
  dplyr::mutate(log_vl_remun_ph = log(vl_remun_ph)) %>% 
  dplyr::select(esc, vl_remun_ph, log_vl_remun_ph)
# table(rais3$ibge_subsetor)
# unique(rais3$esc)

# ggplot(rais3)+
#   geom_boxplot(aes(x = ibge_subsetor, y = vl_remun, fill = ibge_subsetor))+
#   scale_fill_manual(values = c("#999999", "#E69F00"))+
#   theme_minimal()

# ggplot(rais3)+
#   geom_density(aes(x=vl_remun_ph, fill=ibge_subsetor), alpha=.5)+
#   theme_minimal()


source("code/functions/fct_cormatrix.R")
matrixCorrelationPlot(rais3 %>% dplyr::select(esc, vl_remun_ph, log_vl_remun_ph))

(lm <- lm(log(vl_remun_ph) ~ esc, data = rais3))
(slm <- summary(lm))
hist(lm$residuals)

fct_model <- function(data){
  lm = lm(log(vl_remun_ph) ~ esc, data = rais3)
  jb_test = tseries::jarque.bera.test(lm$residuals)
  # sw_test_res = shapiro.test(lm$residuals)
  lm_summary = summary(lm)
  pp_lm_summary = jtools::summ(lm, digits = 5)
  
  return(
    list(
      lm=lm,
      jb_test=jb_test,
      # sw_test_res=sw_test_res,
      lm_summary=lm_summary,
      pp_lm_summary=pp_lm_summary
    )
  )
}

# Modelo para Total exp
m0 <- fct_model(rais3)

# df do resultado
res <- data.frame(
  # cd_exp='S0',
  # section="Total Exportações",
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
  # sw_pvalue = m0$sw_test_res$p.value,
  n = length(m0$lm_summary$residuals)#, 
  # row.names = 'S0'
)
res



table(rais3$esc)




# 11, 13, 15, 16, 17, 22, 23
# 04, 11, 15, 18, 21, 23, 24, 25
#_---------------------------------------

# Extrativa mineral	01
# Indústria de produtos minerais nao metálicos	02
# Indústria metalúrgica	03
# Indústria mecânica	04
# Indústria do material elétrico e de comunicaçoes	05
# Indústria do material de transporte	06
# Indústria da madeira e do mobiliário	07
# Indústria do papel, papelao, editorial e gráfica	08
# Ind. da borracha, fumo, couros, peles, similares, ind. diversas	09
# Ind. química de produtos farmacêuticos, veterinários, perfumaria	10
# Indústria têxtil do vestuário e artefatos de tecidos	11
# Indústria de calçados	12
# Indústria de produtos alimentícios, bebidas e álcool etílico	13
# Serviços industriais de utilidade pública	14
# Construçao civil	15
# Comércio varejista	16
# Comércio atacadista	17
# Instituiçoes de crédito, seguros e capitalizaçao	18
# Com. e administraçao de imóveis, valores mobiliários, serv. Técnico	19
# Transportes e comunicaçoes	20
# Serv. de alojamento, alimentaçao, reparaçao, manutençao, redaçao	21
# Serviços médicos, odontológicos e veterinários	22
# Ensino	23
# Administraçao pública direta e autárquica	24
# Agricultura, silvicultura, criaçao de animais, extrativismo vegetal	25
