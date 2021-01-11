if(!require("GGally")){install.packages("GGally")}
if(!require("ggfortify")){install.packages("ggfortify")}
if(!require("ggplot2")){install.packages("ggplot2")}

matrixCorrelationPlot <- function(df){
  GGally::ggpairs(data = df, upper = list(continuous = GGally::wrap("cor", size = 6)))+
    ggplot2::labs(title = "Matriz de correlação")+
    ggplot2::theme_light()+
    ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
                   strip.text = ggplot2::element_text(size = 10, face = "bold"))
}