med_ioi_plot <- function(data = iso_features, remove_outlier = TRUE){
  age_group_labels <- c("5" = "5-yo", "6" = "6-yo", "7" = "7-yo", "8" = "8-yo", "e" = "Adults")
  if(!("tempo_abs_dev2" %in% names(data))){
    data <- data %>% 
      mutate(tempo_num = c("fa" = .4, "sl" = .6)[tempo]) %>% 
      mutate(tempo_dev = (med_ioi - tempo_num),
             tempo_abs_dev = abs(tempo_dev),
             tempo_dev_bin = factor(ntile(tempo_abs_dev, 3)),
             log10_tempo_abs_dev = log10(abs(tempo_dev)),
             age_group = age_group_labels[age_group], 
             combined_setting = sprintf("%s / %s", tempo, setting) %>% str_replace("fa", "Fast")) 
  } 
  if(remove_outlier){
    data <- data  %>%   
      filter(tempo_abs_dev < .15)
  }
  q <- data %>% ggplot(aes(x = age_group, y = med_ioi, fill = combined_setting))
  q <- q + geom_hline(yintercept = .4, linetype = "dotted")
  q <- q + geom_hline(yintercept = .6, linetype = "dotted")
  q <- q + geom_boxplot(outlier.color = NA, alpha = .6)
  q <- q + geom_jitter(alpha = .2, 
                       #size = 1.5,
                       position = position_dodge(width = .75), 
                       aes(group = combined_setting))
  q <- q + guides(fill = guide_legend(ncol = 2))
  q <- q + theme_bw() 
  q <- q + theme(panel.grid =  element_blank(),
                 axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x =  element_text(size = 14),
                 axis.title.y =  element_text(size = 14),
                 legend.position = c(.8, .14),
                 legend.title =  element_text(size = 10),
                 legend.text =  element_text(size = 8),
                 legend.background  = element_rect(color = "black"))
  q <- q + scale_fill_manual(values = c("dodgerblue4",  "steelblue1", "hotpink4", "violetred1"), 
                             labels = c("Acoustic / Fast", "Person / Fast", "Acoustic / Slow", "Person / Slow"))
  q <- q + labs(y = "Median IOI (s)", x = "Age Group", fill = "Conditions") 
  #  q <- q + scale_color_viridis_b()
  q
}

med_ioi_plot()
ggsave("med_ioi_plot.png", dpi = 300)
