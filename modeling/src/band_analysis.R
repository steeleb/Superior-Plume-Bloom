# store some common lists
LS57_user <- c("B1", "B2", "B3", "B4", "B5", "B7")
LS57_ee <- c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7")


make_band_comp_plot <- function(user_band, ee_band, data, mission) {
  data %>% 
    filter(mission == mission) %>% 
    ggplot(., aes(x = !!sym(ee_band), y = !!sym(user_band))) +
    geom_point() +
    labs(title = paste0("Label-EE comparison: ", 
                        ee_band, 
                        " ",
                        mission)) +
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
