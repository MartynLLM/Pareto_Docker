content = function(file) {
  shinyjs::show("spinner_download_play")
  
  # Get your spatial data (same as in single_meas_fun2)
  cols = objectives()
  values = sel_tay()
  mv <- fit1() %>% filter(across(all_of(cols), ~ . %in% values))
  hru_one = plt_sel(shp = cmf(), opti_sel = mv$optimum)
  mes = read.csv("../data/measure_location.csv")
  
  # Create static ggplot version
  p <- ggplot(hru_one) +
    geom_sf(aes(fill = factor(get(names(hru_one)[grep("Optim", names(hru_one))[1]])))) +
    scale_fill_manual(values = c("#66C2A5", "#4db818", "#663e13", "#F7A600", 
                                "#03597F", "#83D0F5", "#FFEF2C", "#a84632", 
                                "#b82aa5", "#246643")[1:length(unique(mes$nswrm))]) +
    theme_void() +
    labs(fill = "Measures")
  
  # Save directly to PNG
  ggsave(file, plot = p, width = 10, height = 10, dpi = 300, device = "png")
  
  shinyjs::hide("spinner_download_play")
}