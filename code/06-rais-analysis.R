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

colnames(rais1)

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
    vl_remun < 40000#,
    # tipo_salaraio=='01'
  ) %>% 
  select(esc, cbo_ocupacao_2002, xp, idade, raca_cor, sexo_trabalhador, ibge_subsetor, vl_remun, vl_remun_ph)
rm(rais1)

# rio::export(rais2, "data/clean/rais2.csv")
# rais2 <- read_csv("data/clean/rais2.csv")

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


# CBO 251205 Economista ---------------------------------------------------

rais_cbo_econ <- rais2 %>% 
  dplyr::filter(cbo_ocupacao_2002==251205) %>% 
  dplyr::mutate(log_vl_remun_ph = log(vl_remun_ph))

# rais_cbo_econ <- rais1 %>% 
#   dplyr::filter(cbo_ocupacao_2002==251205)
# rio::export(rais_cbo_econ, "rais_cbo_econ.csv")

# unique(rais_cbo_econ$esc)

source("code/functions/fct_cormatrix.R")
matrixCorrelationPlot(rais_cbo_econ %>% dplyr::select(esc, vl_remun_ph, log_vl_remun_ph))

(lm <- lm(vl_remun_ph ~ esc, data = rais_cbo_econ))
(slm <- summary(lm))
hist(lm$residuals)

(lm <- lm(log(vl_remun_ph) ~ esc, data = rais_cbo_econ))
(slm <- summary(lm))
hist(lm$residuals)


# Descriptive analysis ----------------------------------------------------

rais_cbo_econ <- readr::read_csv("data/clean/rais_cbo_econ.csv")
rais_cbo_econ2 <- rais_cbo_econ %>% 
  dplyr::mutate(
    esc = as.numeric(escolaridade_apos_2005),
    vl_remun = as.numeric(gsub(",", '.', stringr::str_remove(vl_remun_media_nom, "^0+"))),
    vl_remun_ph = vl_remun/(qtd_hora_contr*4),
    log_vl_remun_ph = log(vl_remun_ph),
    xp = idade -18
  ) %>% 
  dplyr::filter(
    nacionalidade %in% c(10, 20),
    ind_portador_defic == 0,
    vinculo_ativo_31_12 == 1,
    idade >= 18,
    qtd_hora_contr > 12,
    vl_remun > 1000,
    vl_remun < 40000#,
    # tipo_salaraio=='01'
  ) %>% 
  select(esc, cbo_ocupacao_2002, xp, idade, raca_cor, sexo_trabalhador, ibge_subsetor, vl_remun, vl_remun_ph, cnae_2_0_classe, cnae_95_classe, mes_desligamento)

sapply(rais_cbo_econ2, unique)

source("code/functions/fct_cormatrix.R")
matrixCorrelationPlot(rais_cbo_econ2 %>% dplyr::select(vl_remun_ph, log_vl_remun_ph, esc, idade))

ggplot(rais_cbo_econ2)+
  geom_boxplot(aes(x = factor(esc, levels = c(9, 10, 11), labels = c("Graduação", "Mestrado", "Doutorado")), y = vl_remun_ph, fill = factor(esc, levels = c(9, 10, 11), labels = c("Graduação", "Mestrado", "Doutorado"))))+
  scale_fill_manual(values = c("#999999", "#E69F00", "#E12F00"))+
  labs(
    title = "Boxplots da remuneração por hora", 
    x = "Escolaridade",
    y = "Valor da remuneração por hora", 
    fill = "Escolaridade"
  )+
  theme_minimal()

ggplot(rais_cbo_econ2)+
  geom_density(aes(x = vl_remun_ph, fill = factor(esc, levels = c(9, 10, 11), labels = c("Graduação", "Mestrado", "Doutorado"))), alpha=.5)+
  scale_fill_manual(values = c("#999999", "#E69F00", "#E12F00"))+
  labs(
    title = "Densidade da remuneração por hora", 
    x = "Valor da remuneração por hora",
    y = "Densidade", 
    fill = "Escolaridade"
  )+
  theme_minimal()

df <- rais_cbo_econ2 %>% 
  dplyr::group_by(esc) %>% 
  dplyr::summarise(mean_vl_remun_ph = mean(vl_remun_ph))

ggplot2::ggplot(
  df, aes(x = factor(esc, levels = c(9, 10, 11), labels = c("Graduação", "Mestrado", "Doutorado")),
          y = mean_vl_remun_ph, 
          fill = factor(esc, levels = c(9, 10, 11), labels = c("Graduação", "Mestrado", "Doutorado")))
)+
  ggplot2::geom_col()+
  scale_fill_manual(values = c("#999999", "#E69F00", "#E12F00"))+
  labs(
    title = "Média da remuneração por hora",
    x = "Escolaridade",
    y = "Valor da remuneração por hora", 
    fill = "Escolaridade"
  )+
  theme_minimal()


rais_cbo_econ2 %>% 
  group_by(cnae_2_0_classe) %>% 
  summarise(nn = n()) %>% 
  arrange(desc(nn)) %>% 
  filter(nn >= 10)


# CNAE: 
# 84116 -> Administração Pública em Geral; 
# 82113 -> Serviços combinados de escritório e apoio administrativo




# Quem ganha mais? Homens ou Mulheres? ------------------------------------
# -------------------------------
# Comparação entre homens e mulheres economistas
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

rais_cbo_econ <- readr::read_csv("data/clean/rais_cbo_econ.csv")
rais_cbo_econ_f <- rais_cbo_econ %>% 
  dplyr::mutate(
    esc = as.numeric(escolaridade_apos_2005),
    vl_remun = as.numeric(gsub(",", '.', stringr::str_remove(vl_remun_media_nom, "^0+"))),
    vl_remun_ph = vl_remun/(qtd_hora_contr*4)
  ) %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(vl_remun>0, vl_remun_ph<500) %>% 
  dplyr::select("escolaridade_apos_2005", "qtd_hora_contr", "idade", "raca_cor", "sexo_trabalhador", 'vl_remun', 'vl_remun_ph')

ggplot(rais_cbo_econ_f)+
  geom_boxplot(aes(x = factor(sexo_trabalhador, levels = c("01", "02"), labels = c("Homens", "Mulheres")), y = vl_remun_ph, fill = factor(sexo_trabalhador, levels = c("01", "02"), labels = c("Homens", "Mulheres"))))+
  scale_fill_manual(values = c("#62aec5", "#e64072"))+
  labs(
    title = "Boxplots da remuneração por hora",
    x = "Sexo",
    y = "Valor da remuneração por hora", 
    fill = "Sexo"
  )+
  theme_minimal()

ggplot(rais_cbo_econ_f)+
  geom_density(aes(x=vl_remun_ph, fill=factor(sexo_trabalhador, levels = c("01", "02"), labels = c("Homens", "Mulheres"))), alpha=.5)+
  scale_fill_manual(values = c("#62aec5", "#e64072"))+
  labs(
    title = "Densidade da remuneração por hora",
    y = "Densidade",
    x = "Valor da remuneração por hora", 
    fill = "Sexo"
  )+
  theme_minimal()

# rais_cbo_econ_f$vl_remun_ph
rais_cbo_econ_fm<-rais_cbo_econ_f[which(rais_cbo_econ_f$sexo_trabalhador=='01'), ]
rais_cbo_econ_ff<-rais_cbo_econ_f[which(rais_cbo_econ_f$sexo_trabalhador=='02'), ]
tt <- t.test(rais_cbo_econ_fm$vl_remun_ph, rais_cbo_econ_ff$vl_remun_ph, alternative = "two.sided", var.equal = FALSE, conf.level = .70)
tt$estimate
tt$conf.int[1]
mean(rais_cbo_econ_fm$vl_remun_ph)-tt$conf.int[1]
mean(rais_cbo_econ_ff$vl_remun_ph)+tt$conf.int[2]



# Spatial Analysis --------------------------------------------------------

rais_cbo_econ <- readr::read_csv("data/clean/rais_cbo_econ.csv")

rais_cbo_econ_fs <- rais_cbo_econ %>%
  dplyr::mutate(
    esc = as.numeric(escolaridade_apos_2005),
    vl_remun = as.numeric(gsub(",", '.', stringr::str_remove(vl_remun_media_nom, "^0+"))),
    vl_remun_ph = vl_remun/(qtd_hora_contr*4),
    cd_mun = as.character(municipio)
  ) %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(vl_remun>0, vl_remun_ph<500) %>% 
  dplyr::select("escolaridade_apos_2005", "qtd_hora_contr", "idade", "raca_cor", "sexo_trabalhador", 'vl_remun', 'vl_remun_ph', 'mun_trab', 'cd_mun')

rais_cbo_econ_fsc <- rais_cbo_econ_fs %>% 
  group_by(cd_mun) %>% 
  summarise(count=n())

# Localizações
source("code/functions/data_loc.R")
loc <- data_loc(c('SC', 'PR', 'RS'))
410690%in%loc$cd_micro
# Poligonos de SC
source("code/functions/data_sul_shp.R")
sf <- get_sul_sf() %>% 
  dplyr::mutate(cd_mun=as.character(cd_mun))
# plot(sf['sigla_uf'])

rais_cbo_econ_fsc$cd_mun
rais_cbo_econ_fsc2 <- left_join(sf, rais_cbo_econ_fsc, by = "cd_mun")
# sapply(rais_cbo_econ_fsc2$count, unique)

sf$cd_mun
rais_cbo_econ_fsc$cd_mun

sapply(rais_cbo_econ_fs, unique)

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



source("code/functions/fct_cormatrix.R")
matrixCorrelationPlot(rais_cbo_econ2 %>% dplyr::select(esc, vl_remun_ph, log_vl_remun_ph))

rais_cbo_econ2$log_vl_remun_ph = log(rais_cbo_econ2$vl_remun_ph)
rais_cbo_econ2$log_esc = log(rais_cbo_econ2$esc)
rais_cbo_econ2$log_idade = log(rais_cbo_econ2$idade)

ggplotRegression <- function (fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue", se=F) +
    labs(title = paste("Adj R2 = ",round(signif(summary(fit)$adj.r.squared, 5), 3),
                       "Intercept =",round(signif(fit$coef[[1]],5 ), 3),
                       " Slope =",round(signif(fit$coef[[2]], 5), 3),
                       " P =",round(signif(summary(fit)$coef[2,4], 5), 3)))+
    theme_minimal()+
    theme(axis.title.y = element_text(size=13, face = "bold"),
          axis.title.x = element_text(size=13, face = "bold"))
}

gg3 <- function(fit){
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1], color = names(fit$model)[3])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue", se=F) +
    labs(title = paste("Adj R2 = ",round(signif(summary(fit)$adj.r.squared, 5), 3),
                       "Intercept =",round(signif(fit$coef[[1]],5 ), 3),
                       " Slope =",round(signif(fit$coef[[2]], 5), 3),
                       " P =",round(signif(summary(fit)$coef[2,4], 5), 3)))+
    theme_minimal()+
    theme(axis.title.y = element_text(size=13, face = "bold"),
          axis.title.x = element_text(size=13, face = "bold"),
          legend.title = element_text(size=13, face = "bold"))+
    scale_color_gradientn(colours = rainbow(4))
}

(lm <- lm(vl_remun_ph ~ esc, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
matrixCorrelationPlot(rais_cbo_econ2 %>% dplyr::select(esc, vl_remun_ph))

(lm <- lm(vl_remun_ph ~ idade, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
matrixCorrelationPlot(rais_cbo_econ2 %>% dplyr::select(idade, vl_remun_ph))

(lm <- lm(log_vl_remun_ph ~ log_idade, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
matrixCorrelationPlot(rais_cbo_econ2 %>% dplyr::select(idade, vl_remun_ph))

(lm <- lm(log_vl_remun_ph ~ esc, data = rais_cbo_econ2)) # ***
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
cor(as.numeric(lm$residuals), rais_cbo_econ2$idade)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)

(lm <- lm(vl_remun_ph ~ log_esc, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
cor(as.numeric(lm$residuals), rais_cbo_econ2$idade)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)


(lm <- lm(log_vl_remun_ph ~ log_esc, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)

# multivariated

(lm <- lm(log_vl_remun_ph ~ log_esc + idade, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplotRegression(lm)
gg3(lm)
cor(as.numeric(lm$residuals), rais_cbo_econ2$idade)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$esc)
cor(as.numeric(lm$residuals), rais_cbo_econ2$log_esc)

(lm <- lm(log_vl_remun_ph ~ log_esc + log_idade, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplotRegression(lm)
gg3(lm)

(lm <- lm(log_vl_remun_ph ~ log_idade + log_esc, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplotRegression(lm)
gg3(lm)

(lm <- lm(log_vl_remun_ph ~ log_idade + esc, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
gg3(lm)

(lm <- lm(log_vl_remun_ph ~ esc + log_idade, data = rais_cbo_econ2))
(slm <- summary(lm))
hist(lm$residuals)
ggplot(lm, aes(lm$residuals))+geom_density()+theme_minimal()+labs(x="Resíduos", y="Densidade")+theme(axis.title.y = element_text(size=18, face = "bold"), axis.title.x = element_text(size=18, face = "bold"))
ggplotRegression(lm)
gg3(lm)
min(rais_cbo_econ2$idade); max(rais_cbo_econ2$idade)
log(72)
log(55)
log(43)
log(33)
log(26)
log(19)


