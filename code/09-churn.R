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


# Análise -----------------------------------------------------------------


TESTE_BI <- read_csv("data/TESTE_BI.csv")

install.packages("esquisse")
library(esquisse)
esquisser()
