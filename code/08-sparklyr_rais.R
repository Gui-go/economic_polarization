





library(sparklyr)
# install.packages("sparklyr")
library(dplyr)
library(nycflights13)
# install.packages("nycflights13")
library(ggplot2)

# To install Spark
# spark_install()

sc <- spark_connect(master="local")
flights <- copy_to(sc, flights, "flights")
airlines <- copy_to(sc, airlines, "airlines")
rais <- sparklyr::spark_read_text(sc, "rais", "data/raw/RAIS_VINC_PUB_SUL.txt")

filter(flights, dep_delay > 1000)
