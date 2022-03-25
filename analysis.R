library(tidyverse)
library(dtw)

get_timeline_parameters <- function(timeline, time_base){
  t0 <- timeline[1]
  t_max <- ceiling(diff(range(timeline))/time_base) * time_base
  c("t0" = t0, "t_max" = t_max, "time_base" = time_base, n = length(timeline))
}

get_isochronous_envelope <- function(query, time_base, remove_offset = F){
  seq(floor(min(query)), ceiling(max(query)), time_base)
}

get_best_alignment <- function(query, target, summary = F){
  align <- dtw(query, target) 
  ret <- c()
  ba <- 
    map_dfr(1:length(query), function(i){
    target_idz <- align$index2[which(align$index1 == i)]
    candidates <- target[target_idz]
    candidate_pos <- paste(which(align$index1 == i), collapse = ";")
    
    abs_diff <- abs(candidates - query[i])
    best_idx <- which.min(abs_diff)
    best <- min(abs_diff)
    target_best <- target_idz[best_idx ]
    
    tibble(query_pos = i, target_pos = target_best, 
           candidate_pos = candidate_pos, 
           query = query[i], target = target[target_best], d = best[1])
  })
  #browser()
  if(summary){
    ba <- tibble(MAE = mean(ba$d), MAS = sd(ba$d), 
                 norm_dist = align$normalizedDistance, 
                 d_n = length(target) - length(query))
  }
  ba  
}

get_circular_features <- function(onsets, time_base, remove_offset = F){
  require(circular)
  #browser()
  if(remove_offset){
    onsets <- onsets - onsets[1]
  }
  circ_onsets <- circular::circular(2 * pi * onsets / time_base)
  
  tibble(circ_mean = as.numeric(circular::mean.circular(circ_onsets)), 
         circ_sd = as.numeric(circular::sd.circular(circ_onsets)),
         neg_async = sign(circ_mean))
}

get_iso_features <- function(onset_data){
  base_data <- onset_data %>% 
    distinct(experimenter, 
             age_group, condition, 
             serial, source, 
             tempo, 
             setting, 
             p_id, 
             trial_id, 
             mean_ioi, 
             med_ioi, 
             sd_ioi,
             cv_ioi) 
  tids <- unique(onset_data$trial_id)
  tempos <- c("fa" = .4, "sl" = .6)
  map_dfr(tids, function(tid){
    tmp <- onset_data %>% filter(trial_id == tid) 
    query <- tmp %>% pull(onset)
    offset <- query[1]
    query <-  query - offset
    time_base <- tempos[tmp$tempo[1]] 
    iso_env <- get_isochronous_envelope(query, time_base)
    circ_features <- get_circular_features(query, time_base, remove_offset = T)
    
    alignment_features <- get_best_alignment(query, iso_env, summary = T) %>% 
      mutate(trial_id = tid, 
             offset = offset,
             mod_offset = offset %% time_base,
             time_base = time_base)
    bind_cols(alignment_features, circ_features)
  }) %>% 
    select(trial_id, time_base, offset, mod_offset, MAE, MAS, d_n, norm_dist, everything()) %>% 
    left_join(base_data, by = "trial_id")
  
}

get_productions_features <- function(onsets, target_rhythm, isochronous = F){
  ret <- NULL
  if(isochronous){
    #ret <- make cool circular stuff
  }
  ret
}
get_all_production_features <- function(data_dir, target_data){
  #read all files
  #read_all_rhythm_data(data_dir)
  #for all rhythm get features
  #put it all togeher
  
  #ertun data
}