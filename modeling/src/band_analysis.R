make_band_comp_plot <- function(user_band, ee_band, data, data_name) {
  ggplot(data, aes(x = !!sym(ee_band), y = !!sym(user_band))) +
    geom_point() +
    labs(title = paste0("Label-EE comparison: ", 
                        ee_band, 
                        " ",
                        data_name)) +
    theme_bw()
}


make_class_comp_plot <- function(data, data_name, band) {
  ggplot(data, aes(x = class, y = !!sym(band))) +
    geom_boxplot() +
    labs(title = paste0("Class comparison for ", 
                        band,
                        " ",
                        data_name)) +
    theme_bw()
}
