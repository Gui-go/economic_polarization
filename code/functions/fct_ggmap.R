
# Em progresso

ggplot2::ggplot() +
  # ggplot2::geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
  ggplot2::geom_sf(aes(fill=log_pop_sup_comp), data = df_gwr_sc, color = "black", alpha=.9) +
  ggplot2::geom_sf(data = df_gwr_sc, color = "black", fill = NA, size = .8)+
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#000000")) +
  ggplot2::theme_minimal()
# c("#e3e3e3", "#5c5c5c", "#303030", "#000000") # Cinza claro, Cinza médio, Cinza escuro, Preto
# c("#a10000", "#a19600", "#33a100") # Vermelho, Amarelo, Verde