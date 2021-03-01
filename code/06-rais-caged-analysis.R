rm(list = ls())




# RAIS - Anual - Mercado de trabalho formal 31/12

# CAGED Fuxo das movimentações trabalhistas - CLT - Mercado formal de trabalho

# Google - Acesso rais caged online
# https://bi.mte.gov.br/bgcaged/
# Sistema DARDO
# user: basico
# passwd: 12345678

# RAIS Vínculos


library(readr)
library(dplyr)
rais_uf_mincer <- readr::read_csv("data/raw/rais_uf_mincer.csv", col_names = F)

rais_uf_mincer[2, array(is.na(rais_uf_mincer[2, ]))] <- ''

for(i in 1:ncol(rais_uf_mincer)) {
  rais_uf_mincer[1, i] <- paste(rais_uf_mincer[1, i], rais_uf_mincer[2, i])
}

names(rais_uf_mincer) <- array(rais_uf_mincer[1, ])

rais_uf_mincer <- rais_uf_mincer[3:nrow(rais_uf_mincer), ]
df1 <- rais_uf_mincer %>% 
  janitor::clean_names() %>%
  dplyr::mutate(
    uf = zoo::na.locf(uf)
  ) %>% 
  select(uf, escolaridade_apos_2005, contains("financeira")) %>%     # financeira, civil, ensino
  tidyr::gather('inst', 'n', 3:16) %>% 
  dplyr::filter(stringr::str_detect(uf, 'atarina')) %>% 
  dplyr::mutate(
    inst=replace(inst, inst=='instituicao_financeira_ate_0_50', 0.5),
    inst=replace(inst, inst=='instituicao_financeira_0_51_a_1_00', 0.75),
    inst=replace(inst, inst=='instituicao_financeira_1_01_a_1_50', 1.25),
    inst=replace(inst, inst=='instituicao_financeira_1_51_a_2_00', 1.75),
    inst=replace(inst, inst=='instituicao_financeira_2_01_a_3_00', 2.50),
    inst=replace(inst, inst=='instituicao_financeira_3_01_a_4_00', 3.50),
    inst=replace(inst, inst=='instituicao_financeira_4_01_a_5_00', 4.50),
    inst=replace(inst, inst=='instituicao_financeira_5_01_a_7_00', 6.00),
    inst=replace(inst, inst=='instituicao_financeira_7_01_a_10_00', 8.50),
    inst=replace(inst, inst=='instituicao_financeira_10_01_a_15_00', 12.50),
    inst=replace(inst, inst=='instituicao_financeira_15_01_a_20_00', 17.50),
    inst=replace(inst, inst=='instituicao_financeira_mais_de_20_00', 22),
    inst=replace(inst, inst=='instituicao_financeira_total', "outro"),
    inst=replace(inst, inst=='instituicao_financeira_n_class', "outro")
  ) %>% 
  dplyr::filter(inst!="outro") %>% 
  dplyr::mutate(inst=as.numeric(inst)) %>% 
  dplyr::mutate(esc=escolaridade_apos_2005) %>% 
  dplyr::filter(esc!="Total") %>% 
  dplyr::mutate(esc=factor(esc, levels = c("Analfabeto", "Até 5ª Incompleto", "5ª Completo Fundamental", "6ª a 9ª Fundamental", "Fundamental Completo", "Médio Incompleto", "Médio Completo", "Superior Incompleto", "Superior Completo", "Mestrado", "Doutorado"))) %>% 
  dplyr::select(inst, esc, n)


lm(inst ~ as.numeric(esc), data = df1)
summary(lm(inst ~ as.numeric(esc) + as.numeric(n), data = df1))
as.numeric(df1$esc)

levels(df1$esc)
tail(df1)
unique(df1$inst)

library(ggplot2)

ggplot(df1)+
  geom_point(aes(x = esc, y = inst))


diamonds
m2 <- lm(price ~ cut, diamonds)
