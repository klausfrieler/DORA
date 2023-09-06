norm_dist_plot <- function(data = rhythm_features, remove_outlier = TRUE){
  age_group_labels <- c("5" = "5-yo", "6" = "6-yo", "7" = "7-yo", "8" = "8-yo", "e" = "Adults")
  setting_labels <- c("ta" = "tapping", "vo" = "voice","cl" = "clapping", "ac" = "acoustic", "so" = "social") 
  if(!("norm_dist" %in% names(data))){
    data <- data %>% 
      mutate(log_sd_ioi=log(norm_dist)) %>%
    #(tempo_num = c("fa" = .4, "sl" = .6)[tempo]) %>% 
     # mutate(tempo_dev = (med_ioi - tempo_num),
            # tempo_abs_dev = abs(tempo_dev),
             #tempo_dev_bin = factor(ntile(tempo_abs_dev, 3)),
            # log10_tempo_abs_dev = log10(abs(tempo_dev)),
             age_group = age_group_labels[age_group] 
             setting = setting_labels[setting]
             #setting = sprintf("%s", setting) %>% str_replace("fa", "Fast")) 
  } 
  if(remove_outlier){
    data <- data  %>%   
      filter(norm_dist < 1.5)
  }
  q <- data %>% ggplot(aes(x = age_group, y = log(norm_dist), fill = setting))
  #q <- q + geom_hline(yintercept = .4, linetype = "dotted")
  #q <- q + geom_hline(yintercept = .6, linetype = "dotted")
  q <- q + geom_boxplot(outlier.color = NA, alpha = .6)
  q <- q + geom_jitter(alpha = .2, 
                       #size = 1.5,
                       position = position_dodge(width = .75), 
                       aes(group = setting))
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
  q <- q + scale_fill_manual(values = c("coral3",   "darkred", "hotpink4", "darkgreen", "aquamarine"), 
                             labels = c("tapping", "voice", "clapping", "acoustic", "social")) 
  q <- q + labs(y = "Logarithmized Normed Distance", x = "Age Group", fill = "Conditions") 
  #  q <- q + scale_color_viridis_b()
  q
}

norm_dist_plot_pure <- function(data = rhythm_features, remove_outlier = TRUE){
  age_group_labels <- c("5" = "5-yo", "6" = "6-yo", "7" = "7-yo", "8" = "8-yo", "e" = "Adults")
  setting_labels <- c("ta" = "tapping","vo" = "voice", "cl" = "clapping", "ac" = "acoustic", "so" = "social") 
  if(!("norm_dist" %in% names(data))){
    data <- data %>% 
      mutate(log_sd_ioi= norm_dist) %>%
      #(tempo_num = c("fa" = .4, "sl" = .6)[tempo]) %>% 
      # mutate(tempo_dev = (med_ioi - tempo_num),
      # tempo_abs_dev = abs(tempo_dev),
      #tempo_dev_bin = factor(ntile(tempo_abs_dev, 3)),
      # log10_tempo_abs_dev = log10(abs(tempo_dev)),
      age_group = age_group_labels[age_group] 
      setting = setting_labels[setting]
      #setting = sprintf("%s", setting) %>% str_replace("fa", "Fast")) 
  } 
  if(remove_outlier){
    data <- data  
    #%>%   filter(norm_dist < 1.5)
  }
  q <- data %>% ggplot(aes(x = age_group, y = norm_dist, fill = setting))
  #q <- q + geom_hline(yintercept = .4, linetype = "dotted")
  #q <- q + geom_hline(yintercept = .6, linetype = "dotted")
  q <- q + geom_boxplot(outlier.color = NA, alpha = .6)
  q <- q + geom_jitter(alpha = .2, 
                       #size = 1.5,
                       position = position_dodge(width = .75), 
                       aes(group = setting))
  q <- q + guides(fill = guide_legend(ncol = 2))
  q <- q + theme_bw() 
  q <- q + theme(panel.grid =  element_blank(),
                 axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x =  element_text(size = 14),
                 axis.title.y =  element_text(size = 14),
                 legend.position = c(2.0, .14), #legende noch passender formatieren 
                 legend.title =  element_text(size = 10),
                 legend.text =  element_text(size = 8),
                 legend.background  = element_rect(color = "black"))
  q <- q + scale_fill_manual(values = c("coral3", "darkred",  "hotpink4", "darkgreen", "aquamarine"), 
                             labels = c("tapping", "voice", "clapping", "acoustic", "social")) 
  q <- q + labs(y = "Normed Distance", x = "Age Group", fill = "Conditions") 
  #  q <- q + scale_color_viridis_b()
  q
}

##IMPRECISION_PLOT
imprecision_plot_rhythm <- function(data = rhythm_features, 
                                    var_name = "log_norm_dist",
                                    lmer_model = NULL, 
                                    from_prediction = T, 
                                    alpha = .4){
  
  
  if(from_prediction && !is.null(lmer_model)){
    pred <- lmer_model %>% predict()
  }
  else{
    pred <- data[[var_name]]
  }
  data <- data %>% 
    mutate(pred =  pred, 
           age_group_n = factor(age_group) %>% fct_relevel("e", after = 4) %>% as.numeric() 
           ) 
  browser()
  sum_stats <- data %>%  
    group_by(age_group_n, setting) %>% 
    summarise(se = sd(pred)/sqrt(length(pred)), 
              se_data = sd(!!sym(var_name))/sqrt(length(p_id)), 
              pred = mean(pred), 
              dg = sprintf("%s / %s", 
                           c("ta" = "tapping", "vo" = "voice", "cl" = "clapping", "ac" = "acoustic", "so" = "social")[unique(setting)],  
                           .groups = "drop", n = n()))
  
  q <- sum_stats %>% ggplot(aes(x = age_group_n, y = pred, color = dg)) 
  q <- q + geom_line(aes(group = dg)) 
  q <- q + theme_bw() 
  if(from_prediction){
    q <- q + geom_ribbon(aes(ymin = pred - 1.96* se, 
                             ymax = pred + 1.96*se, 
                             fill = dg), 
                         alpha = alpha)
  }
  else{
    q <- q + geom_ribbon(aes(ymin = pred - 1.96 * se_data, 
                             ymax = pred + 1.96 * se_data,
                             fill = dg),
                         alpha = alpha)
    
  }
  q <- q + scale_x_continuous(labels = c("5 years", "6 years", "7 years", "8 years", "Adults"), 
                              breaks = 1:5)
  #palette <-  c("darkseagreen4",  "palegreen1", "hotpink4", "violetred1") #(RColorBrewer::brewer.pal(3, "Dark2")[1:2], RColorBrewer::brewer.pal(3, "Dark2")[1:2])
  q <- q + scale_fill_manual(values = c("coral3", "darkred",  "hotpink4", "darkgreen", "aquamarine")) 
  q <- q + scale_color_manual(values = c("coral3", "darkred",  "hotpink4", "darkgreen", "aquamarine")) 
  q <- q + labs(x = "Age Group", y = if(!is.null(names(var_name))) names(var_name)[1] else "Imprecision", fill = "Setting", color = "Setting")
  q <- q + theme_bw() 
  q <- q + theme(#panel.grid =  element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x =  element_text(size = 14),
    axis.title.y =  element_text(size = 14),
    legend.title =  element_text(size = 10),
    legend.text =  element_text(size = 8),
    legend.background  = element_rect(color = "black"))
  q
}




#raincloudplot tempo_abs_dev
# rhythm_features %>% 
#   filter(tempo_abs_dev < .15, tempo_abs_dev > -.5) %>%      
#   ggplot(aes(x = age_group, y = norm_dist, fill = setting)) + 
#   
#   ## add half-violin from {ggdist} package
#   ggdist::stat_halfeye(
#     adjust = .8, ## custom bandwidth
#     width = .6,  ## adjust height
#     justification = -.2, ## move geom to the right
#     point_colour = NA
#   ) + 
#   geom_boxplot(
#     width = .15, 
#     outlier.color = NA ## remove outliers
#   ) +
#   ## add justified jitter from the {gghalves} package
#   gghalves::geom_half_point(
#     side = "l", ## draw jitter on the left
#     range_scale = .4, ## control range of jitter
#     alpha = .3 ## add some transparency
#   ) + 
#   coord_cartesian(xlim = c(1.2, NA), clip = "off")   ## remove white space on the left
# +
#   facet_wrap(~tempo)