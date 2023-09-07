library(tidyverse)
library(dtw)
library(circular)

tempos <- c("fa" = .4, "sl" = .6)
max_times <- tempos*c(50, 33)

get_timeline_parameters <- function(timeline, time_base){
  t0 <- timeline[1]
  t_max <- ceiling(diff(range(timeline))/time_base) * time_base
  c("t0" = t0, "t_max" = t_max, "time_base" = time_base, n = length(timeline))
}

get_isochronous_envelope <- function(query, time_base){
  offset <- min(query) %% time_base
  seq(min(query - offset), ceiling(max(query)/time_base) * time_base, time_base) 
}

#' get_best_alignment: Calc best DTW alignment between to timelines and optional features derived from this 
#'
#' @param query <dbl> List of onsets
#' @param target <dbl> List of onsets 
#' @param summary <lgl> Returned summary?
#'
#' @return Either a tibble object with a query -> target mapping or summary features
#' @export
#'
#' @examples
get_best_alignment <- function(query, target){
  #get the DTW alignment
  align <- dtw(query, target) 
  ret <- c()
  
  #find the best alignment by selecting only the closest time points in case of multiple mappings
  ba <- 
    map_dfr(1:length(query), function(i){
      target_idz <- align$index2[which(align$index1 == i)]
      candidates <- target[target_idz]
      candidate_pos <- paste(which(align$index1 == i), collapse = ";")
      
      abs_diff <- abs(candidates - query[i])
      best_idx <- which.min(abs_diff)
      best <- min(abs_diff)
      target_best <- target_idz[best_idx ]
      
      tibble(query_pos = i, 
             target_pos = target_best, 
             candidate_pos = candidate_pos, 
             query = query[i], 
             target = target[target_best], 
             d = best[1])
  })
  
  # MAE mean absolute error; mean of absolute dIntermediate statewm-ifferences between query time points and best target timepoints
  # MAS mean absolute error stddev; Sd of absolute differences between query time points and best target timepoints
  # norm_dist: normaliized DTW distance
  # d_n: difference in length between target and query, if positive, query contains *more* events than target
  summary <- tibble(MAE = mean(ba$d),
               MAS = sd(ba$d), 
               MAX = max(ba$d),
               norm_dist = align$normalizedDistance, 
               d_n = length(query) - length(target))
  list(raw = ba, summary = summary)  
}

get_circular_features <- function(onsets, time_base){
  #transform onsets to cicrular variables, based on time_base as periodictiy.
  # onsets which are separated by multiples of the time_base ar mapped to the same point on
  # the 2d circle
  circ_onsets <- circular::circular(2 * pi * onsets / time_base)
  
  #circular mean is the angle of the resultant, which is a complex number (2d vector)
  #measure for mean phase of onsets in regard to a oscillation of
  #circular standard devation is square root of the logarithm of the inverse square length of the resultant vector, 
  #values between 0 and Inf. Uniform distributions on the circle whill have the highest Circular Sd, as the resultant
  # is close to the center of the circular, so it has a very short length, i.e. the inverse square length is large
  #asynchrony is simple the sign of the circular mean
  tibble(circ_mean = as.numeric(circular::mean.circular(circ_onsets)), 
         circ_sd = as.numeric(circular::sd.circular(circ_onsets)),
         #the other two options correlate highly circular sd
         #circ_var = as.numeric(circular::var.circular(circ_onsets)),
        #circ_disp = circ_var/(2* (1 - circ_var))^2,
         asynchrony = sign(circ_mean),
        phase_sec = circ_mean/2/pi * time_base)
}



signed_mod <- Vectorize(function(x, p){
  ret <- x %% p
  if(ret > p/2) ret <- ret - p 
  ret
})

get_iso_features <- function(onset_data, cut_extra_beats = T){
  base_data <- onset_data %>% 
    distinct(experimenter, 
             age_group, 
             condition, 
             serial,
             source, 
             tempo, 
             setting, 
             p_id, 
             trial_id, 
             set_id,
             mean_ioi, 
             med_ioi,
             max_ioi,
             sd_ioi,
             log_sd_ioi,
             cv_ioi,
             valid_phase) 
  
  tids <- unique(onset_data$trial_id)
  
  #iterate over all trials
  map_dfr(tids, function(tid){
    
    #get the data and the onsts
    tmp <- onset_data %>% filter(trial_id == tid) 
    query <- tmp %>% pull(onset)
    offset <- query[1]
    
    #time_base is just the (inverse) tempo
    time_base <- tempos[tmp$tempo[1]]
    
    # if(tmp$valid_phase[1]){
    #   browser()
    # }
    
    #if requested, disregard beats beyond the maximum time of the trials, as participant sometime simple played on
    if(cut_extra_beats){
      n_before <- length(query)
      mt_before <- max(query)
      query <- query[query <= max_times[tmp$tempo[1]]]
      if(length(query) == 0){
        messagef("[%s] WARNING: Empty query after cutting", tid)
        return(NULL)
      }
      messagef("[%s] Cut query from %d to %d (d = %d, t_before = %.3f, tempo = %s)", 
               tid,
               n_before, length(query), 
               n_before - length(query),
               mt_before, tmp$tempo[1])
    }
    #browser()
    #we generate a isochronous timeline for comparison (isochronous envelope)
    iso_env <- get_isochronous_envelope(query, time_base)
    #iso_env <- iso_env[iso_env <= max(query)]
    #first circular features
    circ_features <- get_circular_features(query, time_base)
    #then alignment base features
    
    #shift
    # query_shifted <-  query - (offset - offset %% time_base)
    # iso_env_shifted <- iso_env[iso_env <= max(query_shifted)]
    if(tid == "l_e_08_02_pa_sl_so"){
      browser()
    }
    best_aligment <- get_best_alignment(query, iso_env)
    alignment_features <-  best_aligment$summary %>% 
      mutate(trial_id = tid, 
             offset = offset,
             mod_offset = signed_mod(offset, time_base),
             time_base = time_base)
    
    #combined the features
    bind_cols(alignment_features, circ_features) 
  }) %>% 
    select(trial_id, time_base, offset, MAE, MAS, d_n, norm_dist, everything()) %>% 
    left_join(base_data, by = "trial_id")
  
}

get_ref_rhythm_for_trial <- function(trial_id, stimulus_data){
  tid <- parse_filename(trial_id, type = "rhythm_prod")
  code <- stimulus_data$design %>% 
    filter(p_id == tid$condition, rhythm == tid$rhythm, setting == tid$setting) %>% 
    pull(code)
  ref_rhythm <- stimulus_data$rhythms %>% 
    filter(code == !!code, rhythm_id == tid$rhythm)
  ref_rhythm    
  
}

check_rhythm <- function(trial_id, onset_data = rhythm_data, remove_offset = T, plot = T){
  query <- onset_data %>% filter(trial_id == !!trial_id) %>% pull(onset)
  if(remove_offset){
    query <- query - query[1]  
  }
  ref_rhythm <- get_ref_rhythm_for_trial(trial_id, stimulus_data) %>%
    mutate(running_beat = (bar - 1) * period + (beat - 1)  )
  ref_ibi <- 60/69
  #print(ref_rhythm)
  target <- ref_rhythm %>% pull(onset)
  target <- target/.5 * ref_ibi 
  if(remove_offset){
    target <- target - target[1]
  }
  query_norm <- query/ref_ibi * .5
  #print(query_norm)
  ba <- get_best_alignment(query, target)
  #print(ba)
  tempo_est <- tibble(trial_id = trial_id, mean_ibi = NA, sd_ibi = NA, ibi_diff = NA, ibi_diff_rel = NA)
  #browser()
  if(nrow(ba$raw) == length(query)){
    on_beats <- which(ref_rhythm$tatum == 1)
    if(length(on_beats) != 0){
      #browser()
      beat_dur <- diff(ref_rhythm[on_beats,]$running_beat)
      ibis <- diff(query[on_beats])/beat_dur 
      tempo_est <- tibble(trial_id  = trial_id, 
                          mean_ibi = mean(ibis), 
                          sd_ibi = sd(ibis),
                          ibi_diff = mean_ibi - ref_ibi,
                          ibi_diff_perc = round(100*(mean_ibi - ref_ibi)/ref_ibi, 1))
      #print(tempo_est)
    }
  }
  if(plot){
    q <- plot_dtw_alignment(query, target) + scale_x_continuous(breaks = round(seq(1:3) * ref_ibi, 3), labels = 1:3)
    print(q)
    #browser()
  }
  #browser()
  tempo_est 
}

get_social_features <- function(onset_data = rhythm_data){
  #browser()
  set_ids <- onset_data %>% filter(setting == "so") %>% distinct(set_id) %>% pull(set_id)
  map_dfr(set_ids, function(sid){
    #browser()
    tmp <- onset_data %>% filter(set_id == sid, setting == "so")
    query <- tmp %>% filter(source == "pa") %>% pull(onset) 
    target <- tmp %>% filter(source == "ex") %>% pull(onset)
    best_alignment <-  get_best_alignment(query - query[1], target- target[1]) 
    best_alignment$summary %>% 
      mutate(set_id = sid, 
             trial_id = tmp %>% filter(source == "pa") %>% pull(trial_id) %>% unique())  
  })
}

add_social_features <- function(rhythm_features, social_features, setting_name = "so2"){
  tmp <- rhythm_features %>% filter(trial_id %in% social_features$trial_id)  
  missing_variables <- c("trial_id", setdiff(names(tmp), names(social_features)))
  social_features <- social_features %>% 
    left_join(
      tmp %>% 
        select(all_of(missing_variables)), 
      by = "trial_id") %>% 
    mutate(setting = setting_name)
 bind_rows(rhythm_features, social_features)
}

get_rhythm_features <- function(onset_data, stimulus_data){
  base_data <- onset_data %>%
    distinct(experimenter,
             age_group,
             condition,
             source,
             rhythm,
             setting,
             p_id,
             trial_id,
             set_id,
             n_onsets) %>% 
    left_join(stimulus_data$design %>% select(condition = p_id, setting,  rhythm, code),
              by = c("condition", "setting", "rhythm"))
  #browser()
  tids <- unique(base_data$trial_id)
  
  #tempo was the same in all trials
  tempo <- 60/69
  
  map_dfr(tids, function(tid){
    #get query data
    messagef("[%s] Processing...", tid)
    
    tmp <- onset_data %>% filter(trial_id == tid) 
    query <- tmp %>% pull(onset)
    offset <- query[1]
    
    #as production task was serial, absolute phase is of no interest, so remove offset 
    query <- query - offset
    #browser()
    #find rhythm (class of variants) and code of target rhythm (actual variant of rhythm)
    rhythm <- base_data %>% filter(trial_id == tid) %>% pull(rhythm)
    code <- base_data %>% filter(trial_id == tid) %>%  pull(code)
    #find target rhythm and extract max subdivision for 
    #calculating time_base = tatum duration 
    ref_rhythm <- stimulus_data$rhythms %>% filter(code == !!code)
    time_base <- tempo/max(ref_rhythm$division) 
    
    #rescale  tempo of reference rhythm (which has 120 bpm)
    target <- ref_rhythm$onset / .5 * tempo
    target <- target - target[1]
    # if(tid == "W_6_01_02_ro_vii-cl_onsets"){
    #   browser()
    # }
    
    #messagef("[%s] Calculating features with time base = %.3f, division = %d, rhythm code = %s", tid, time_base, max(ref_rhythm$division), code)
    # if(substr(code, 1,1) == "c"){
    #   q0 <- plot_dtw_alignment(query, target)
    #   q1 <- plot_dtw_alignment(query - query[1], target- target[1])
    #   browser()
    # }
    #Calc cicrcular features beased on tatums, not bsaed on beat an in the isochronous case! 
    #Attention! Might have non-linear numerical ramifications for different tatums, save tatum and time_base 
    
    circ_features <- get_circular_features(query, time_base) %>% 
      mutate(division = max(ref_rhythm$division), time_base = time_base) 
    
    #get alignment features as in the isochronuous case.
    best_alignment <-  get_best_alignment(query, target) 
                                              
    alignment_features <- best_alignment$summary %>%
      mutate(trial_id = tid,
             offset = offset,
             code = code,
             rhythm = rhythm)
    #get tempo estimation
    #browser()
    tempo_est <- tibble(mean_ibi = NA, sd_ibi = NA, ibi_diff = NA, ibi_diff_perc = NA)
    if(nrow(best_alignment$raw) == length(query)){
      on_beats <- which(ref_rhythm$tatum == 1)
      if(length(on_beats) != 0){
        #browser()
        beat_dur <- diff(ref_rhythm[on_beats,]$running_beat)
        ibis <- diff(query[on_beats])/beat_dur 
        tempo_est <- tibble(mean_ibi = mean(ibis), 
                            sd_ibi = sd(ibis),
                            ibi_diff = mean_ibi - tempo,
                            ibi_diff_perc = round(100*(mean_ibi - tempo)/tempo, 1))
      }
    }
    
    #binding features together
    bind_cols(alignment_features, circ_features, tempo_est)
  }) %>% 
    select(trial_id,  offset, MAE, MAS, d_n, norm_dist, everything()) %>% 
    left_join(base_data, by = c("trial_id", "rhythm", "code"))
  
}

broom_watson_two_test <- function(wtt, alpha = 0){
  u2 <- wtt$statistic
  if (u2 > 0.385){
    p_low <- 0
    p_hi <- 0.001
  } 
  else if (u2 > 0.268) {
    p_low <- .001
    p_hi <- 0.01
  }
  else if (u2 > 0.187) {
    p_low <- .01
    p_hi <- 0.05
  }
  else if (u2 > 0.152) {
    p_low <- .05
    p_hi <- 0.1
  }
  else{
    p_low <- .1
    p_hi <- 1.
  }
  tibble(statistic = u2, p_low = p_low, p_hi = p_hi, n1 = wtt$nx, n2 = wtt$ny, 
         circ_mean1 = wtt$mean1, circ_mean2 = wtt$mean2)
}

watson_two_test_by_list <- function(vl, alpha = 0){
  stopifnot(is.list(vl), length(vl) == 2)
  wtt <- circular::watson.two.test(circular(vl[[1]]), circular(vl[[2]]), alpha = alpha)
  wtt$mean1 <- as.numeric(median.circular(circular(vl[[1]])))
  wtt$mean2 <- as.numeric(median.circular(circular(vl[[2]])))
  wtt
}

watson_two_test_by_split <- function(data, split_var, alpha = 0){
  data %>% 
    group_split(!!sym(split_var)) %>% 
    map(function(x){
      x %>%  pull(circ_mean)
    }) %>% 
    watson_two_test_by_list(alpha = alpha) %>% 
    broom_watson_two_test()
} 

convert_phase_to_abs_time <- function(circ_mean, tempo, comp = 1){
  if(any(is.na(tempo))){
    return(NA)
  }
  tempo <- map_chr(str_split(tempo, "-"), ~{if(length(.x) >= comp) .x[[comp]] else .x[[1]]})
  circ_mean/2/pi * tempos[tempo]
}

lot_of_watsons <- function(data = iso_features %>% filter(source != "ex"), alpha = 0){
  age_groups <- unique(data$age_group)
  tempos <- unique(data$tempo)
  settings <- unique(data$setting)
  data <- data %>% 
    group_by(age_group, setting, tempo, p_id) %>% 
    summarise(circ_mean = mean.circular(circular(circ_mean)), .groups = "drop")
  data <- data %>% mutate(is_adult = age_group == "e" )  
  browser()
  omnibus <- bind_rows(
    w_setting <- watson_two_test_by_split(data, "setting") %>% 
      mutate(comp = "setting", age_group = NA, tempo = NA, setting = "ac-so"),
    w_age_group <- watson_two_test_by_split(data, "is_adult")%>% 
      mutate(comp = "is_adult", setting = NA, tempo = NA, age_group = "k-e"),
    w_tempo <- watson_two_test_by_split(data, "tempo") %>% 
      mutate(comp = "tempo", age_group = NA, setting = NA, tempo = "fa-sl")
  )
  map_dfr(unique(data$is_adult), function(a){
    map_dfr(tempos, function(t){
      map_dfr(settings, function(s){
        w_setting <- watson_two_test_by_split(data %>% filter(is_adult == a, tempo == t), "setting") %>% 
          mutate(comp = "setting", is_adult = a, tempo = t, setting = "ac-so")
        w_age_group <- watson_two_test_by_split(data %>% filter(setting == s, tempo == t), "is_adult")%>% 
          mutate(comp = "is_adult", setting = s, tempo = t, age_group = "k-e")
        w_tempo <- watson_two_test_by_split(data %>% filter(is_adult == a, setting == s), "tempo") %>% 
          mutate(comp = "tempo", is_adult = a, setting = s, tempo = "fa-sl")
        bind_rows(w_setting, w_age_group, w_tempo) 
      })
    })
  }) %>% 
    arrange(comp) %>% 
    distinct(is_adult, tempo, setting, .keep_all = T) %>% 
    mutate(async1_ms = round(1000 * convert_phase_to_abs_time(circ_mean1, tempo, 1), 0),
           async2_ms = round(1000 * convert_phase_to_abs_time(circ_mean2, tempo, 2), 0)) %>% 
    bind_rows(omnibus) 
}

comp_ages_watson <- function(data, setting = NULL, tempo = NULL){
  if(!is.null(setting)){
    data <- data %>% filter(setting == !!setting)
  }
  else{
    setting <- NA
  }
  if(!is.null(tempo)){
    data <- data %>% filter(tempo == !!tempo)
  }
  else{
    tempo <- NA
  }
  ag <- unique(data$age_group) %>% sort()
  map_dfr(ag, function(x){
    map_dfr(ag, function(y){
      if(y >= x){
        return(NULL)
      } 
      messagef("Comparing %s <-> %s", x, y)
      w_age_group <- watson_two_test_by_split(data %>% filter(age_group %in% c(x, y)), "age_group")%>% 
        mutate(comp = "age_group", setting = setting, tempo = tempo, age_group = sprintf("%s-%s", y, x))
      
    })
  })  
}
more_watsons <- function(data = iso_features %>% filter(source != "ex"), alpha = 0){
  age_groups <- unique(data$age_group) %>% sort()
  tempos <- unique(data$tempo) %>% sort()
  settings <- unique(data$setting) %>% sort()
  data <- data %>% filter(source != "ex")
  browser()
  # data <- data %>% 
  #   group_by(age_group, setting, tempo, p_id) %>% 
  #   summarise(circ_mean = mean.circular(circular(circ_mean)), .groups = "drop")
  omnibus <- bind_rows(
    w_setting <- watson_two_test_by_split(data, "setting") %>% 
      mutate(comp = "setting", age_group = NA, tempo = NA, setting = "ac-so"),
    w_age_group <- comp_ages_watson(data)%>% 
      mutate(comp = "age_group"),
    w_tempo <- watson_two_test_by_split(data, "tempo") %>% 
      mutate(comp = "tempo", age_group = NA, setting = NA, tempo = "fa-sl")
  ) %>% 
    mutate(type = "omnibus")
  
  browser()
  map_dfr(tempos, function(t){
    map_dfr(settings, function(s){
      w_s_t <- 
        map_dfr(age_groups, function(a){
          w_setting <- watson_two_test_by_split(data %>% filter(age_group == a, tempo == t), "setting") %>% 
            mutate(comp = "setting", age_group = a, tempo = t, setting = "ac-so")
          w_tempo <- watson_two_test_by_split(data %>% filter(age_group == a, setting == s), "tempo") %>% 
            mutate(comp = "tempo", age_group = a, setting = s, tempo = "fa-sl")
        bind_rows(w_setting, w_tempo) 
      })
      w_age_group <- comp_ages_watson(data, setting = s, tempo = t)
      bind_rows(w_s_t, w_age_group)
    })
  }) %>% 
    arrange(comp, age_group, setting, tempo) %>% 
    distinct(age_group, tempo, setting, .keep_all = T) %>% 
    mutate(type = "single", 
           async1_ms = round(1000 * convert_phase_to_abs_time(circ_mean1, tempo, 1), 0),
           async2_ms = round(1000 * convert_phase_to_abs_time(circ_mean2, tempo, 2), 0)) %>% 
    bind_rows(omnibus) 
}

multi_polar_hist <- function(data){
  data <- data %>% 
    mutate(setting = c("ac" = "Drum King", so = "Social")[setting], 
           age_group = c("5" = "5-year olds", 
                         "6" = "6-year olds", 
                         "7" = "7-year olds", 
                         "8" = "8-year olds", 
                         "e" = "Adults")[age_group], 
           tempo = c("fa" = "Fast (400 ms)", "sl" = "Slow (600 ms)")[tempo])
  sum_data <- data %>% 
    group_by(age_group, tempo, setting) %>% 
    summarise(m_circ_mean = mean.circular(circular(circ_mean)), 
              r_circ_mean = rho.circular(circular(circ_mean)), 
              .groups = "drop")
  
  q <- data %>% ggplot(aes(x = circ_mean, y = ..density.., fill = age_group)) 
  q <- q + geom_histogram(alpha = .25, color = "grey50") 
  q <- q + geom_segment(data = sum_data, 
                        aes(x = m_circ_mean, 
                            xend = m_circ_mean, 
                            y = 0, 
                            yend = 3*r_circ_mean, 
                            colour = age_group),
                        linetype = "solid",
                        arrow = arrow(length = unit(0.25, "cm")), size = 1)
  q <- q + coord_polar(direction = 1, start = -pi/2 - .1) 
  q <- q + facet_grid(tempo ~ setting) 
  q <- q + scale_x_continuous(breaks = seq(-pi, pi, pi/2), labels = expression(-pi, -pi/2, 0, pi/2, pi)) 
  q <- q + theme_bw() 
  q <- q + theme(strip.background  = element_rect(fill = "white"), 
                 legend.title =  element_blank(),
                 legend.position = c(.5, .5),
                 legend.background  = element_rect(colour = "black", fill = "#ffffff"),
                 panel.grid.major.y  = element_blank())
  q <- q + labs(x = "Mean Phase (rad)", y = "Density") 
  q <- q + scale_fill_manual(values = c("darkorange", "darkred", "darkblue", "darkgreen", "darkviolet"))
  q <- q + scale_color_manual(values = c("darkorange", "darkred", "darkblue", "darkgreen", "darkviolet"))
  
  
  q
} 

# readxl::read_xlsx("data/drumking_final/meta/cut_drum_king_final_pre.xlsx", col_names = F) %>% 
#   select(1:2) %>% 
#   set_names(c("file", "cut_time")) %>%  
#   mutate(cut_time = cut_time %>% str_replace("[m]", ",") %>% 
#            map_dbl(~{as.numeric(.x[[1]]) * 60 + as.numeric(.x[[2]]) + as.numeric(.x[[3]])/1000})) %>% 
#   writexl::write_xlsx("data/drumking_final/meta/cuttimes_audio_iso.xlsx")

# Usage:
# Can be used to plot predictions from models or means values from data over setting/tempo
# If  lmer_model is not NULL or from_prediction = TRUE, then model predictions are used. data must fit the model.
# Otherwise values of the variable var_name from data are plotted. If var_name is named vector, name will be used as label for y-axis.
#
# Example:
#  lsi_mod <- lmerTest::lmer(log_sd_ioi ~ age_group * setting + tempo + (1|p_id), data = iso_features)
#  prediction_interaction_plot(iso_features, lsi_mod, var_name = "c("Imprecision" = "log_sd_ioi"), from_prediction = T)

prediction_interaction_plot <- function(data = iso_features, 
                                        var_name = "log_sd_ioi", 
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
    mutate(pred =  pred, age_group_n = factor(age_group) %>% fct_relevel("e", after = 4) %>% as.numeric()) 
  sum_stats <- data %>%  
    group_by(age_group_n, setting, tempo) %>% 
    summarise(se = sd(pred)/sqrt(length(pred)), 
              se_data = sd(!!sym(var_name))/sqrt(length(p_id)), 
              pred = mean(pred), 
              dg = sprintf("%s / %s", 
                           c("so" = "Person", "ac" = "Metronome")[setting], 
                           c("fa" = "400 ms", "sl" = "600 ms")[tempo]), 
              .groups = "drop", n = n()) 
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
  q <- q + scale_x_continuous(labels = c("5yo", "6yo", "7yo", "8yo", "Adults"), 
                              breaks = 1:5)
  palette <- c(RColorBrewer::brewer.pal(3, "Blues")[1:2], RColorBrewer::brewer.pal(3, "Reds")[1:2])
  q <- q + scale_fill_manual(values = palette) 
  q <- q + scale_color_manual(values = palette)  
  q <- q + labs(x = "Age Group", y = if(!is.null(names(var_name))) names(var_name)[1] else "Imprecision", fill = "Setting/Tempo", color = "Setting/Tempo")
  q
}

tempo_abs_dev_plot_ridge <- function(data = iso_features, remove_outlier = TRUE){
  if(!("tempo_abs_dev" %in% names(data))){
    data <- data %>% 
      mutate(tempo_num = c("fa" = .4, "sl" = .6)[tempo]) %>% 
      mutate(tempo_dev = (med_ioi - tempo_num),
             tempo_abs_dev = abs(tempo_dev),
             log10_tempo_abs_dev = log10(abs(tempo_dev))) 
  } 
  if(remove_outlier){
    data <- data  %>%   
      filter(tempo_abs_dev < .15, tempo_abs_dev > -.5)
  }
  q <- data %>% ggplot(aes(x = tempo_dev, y = tempo, fill = setting))
  q <- q + ggridges::geom_density_ridges(alpha = .4, bandwidth = .001, stat = "binline")
  q <- q + theme_bw() 
  q <- q + scale_fill_viridis_d()
  q
}

tempo_abs_dev_plot <- function(data = iso_features, remove_outlier = TRUE){
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
