#raincloudplot tempo_abs_dev
iso_features %>% 
  mutate(tempo_num = c("fa" = .4, "sl" = .6)[tempo]) %>% 
  mutate(tempo_dev = (med_ioi - tempo_num),
         tempo_abs_dev = abs(tempo_dev),
         log_tempo_abs_dev = log(abs(tempo_dev))) %>%   
  filter(tempo_abs_dev < .15, tempo_abs_dev > -.5) %>%      
  ggplot(aes(x = age_group, y = tempo_abs_dev, fill = setting)) +   ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    adjust = .8, ## custom bandwidth
    width = 1.6,  ## adjust height
    justification = -.2, ## move geom to the right
    point_colour = "black"
  ) +
  geom_boxplot(
    width = .5,
    outlier.color = NA ## remove outliers
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    side = "l", ## draw jitter on the left
    range_scale = .4, ## control range of jitter
    alpha = .3 ## add some transparency
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off")   + ## remove white space on the left
  facet_wrap(~tempo)