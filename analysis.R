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
  # MAE mean absolute error; mean of absolute differences between query time points and best target timepoints
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
         asynchrony = sign(circ_mean))
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
      #browser()
      n_before <- length(query)
      mt_before <- max(query)
      query <- query[query <= max_times[tmp$tempo[1]]]
      if(length(query) == 0){
        browser()
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

check_rhythm <- function(trial_id, onset_data, stimulus_data, remove_offset = T, plot = F){
  #browser()
  query <- onset_data %>% filter(trial_id == !!trial_id) %>% pull(onset)
  if(remove_offset){
    query <- query - query[1]  
  }
  ref_rhythm <- get_ref_rhythm_for_trial(trial_id, stimulus_data) %>% 
    mutate(running_beat = (bar -1) * period + (beat -1)  )
  ref_ibi <- 60/69
  #print(ref_rhythm)
  target <- ref_rhythm %>% pull(onset)
  target <- target/.5 * ref_ibi 
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
                          ibi_diff_rel = round(100*(mean_ibi - ref_ibi)/ref_ibi, 1))
      #print(tempo_est)
    }
  }
  if(plot){
    q <- plot_dtw_alignment(query, target) + scale_x_continuous(breaks = round(seq(1:3) * ref_ibi, 3), labels = 1:3)
    print(q)
  }
  browser()
  tempo_est 
}

# get_social_features <- function(onset_data, stimulus_data, type = "rhythm_prod"){
#   base_data <- onset_data %>%
#     distinct(experimenter,
#              age_group,
#              condition,
#              source,
#              rhythm,
#              setting,
#              p_id,
#              trial_id,
#              n_onsets) %>% 
#     left_join(stimulus_data$design %>% select(condition = p_id, setting,  rhythm, code),
#               by = c("condition", "setting", "rhythm"))
#   
#   type <- match.arg(type)
#   data <- onset_data %>% filter(setting == "so")
#   p_trials <- onset_data %>% filter(source == "pa") %>% pull(trial_id) %>% unique()
#   map_dfr(p_trials, function(ptid){
#     x_trial <- onset_data %>% filter(trial_id == str_replace(ptid, "pa", "ex"))
#     if(nrow(x_trial) == 0){
#       messagef("No match found for %s", ptid)
#       return(NULL)
#     }
#     ref_rhythm <- stimulus_data$rhythms %>% filter(code == 
#             base_data %>% filter(trial_id == ptid) %>% pull(code))
#     query <- onset_data %>% filter(trial_id == ptid)%>% pull(onset)
#     target <-  x_trial %>% pull(onset)
#     q <- plot_dtw_alignment(query - query[1], target - target[1])
#     print(q)
#     browser()
#     alignment_features <- get_best_alignment(query - query[1], target - target[1], summary = T) %>%
#       mutate(trial_id = ptid)
#     print(alignment_features)
#     alignment_features
#   })
# }
  

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
  
  tids <- unique(base_data$trial_id)
  
  #tempo was the same in all trials
  tempo <- 60/69
  
  map_dfr(tids, function(tid){
    #get query data
    tmp <- onset_data %>% filter(trial_id == tid) 
    query <- tmp %>% pull(onset)
    offset <- query[1]
    
    #as production task was serial, absolute phase is of no interest, so remove offset 
    query <- query - offset
    
    #find rhythm (class of variants) and code of target rhythm (actual variant of rhythm)
    rhythm <- base_data %>% filter(trial_id == tid) %>% pull(rhythm)
    code <- base_data %>% filter(trial_id == tid) %>%  pull(code)
    
    #find target rhythm and extract max subdivision for 
    #calculating time_base = tatum duration 
    ref_rhythm <- stimulus_data$rhythms %>% filter(code == !!code)
    time_base <- tempo/max(ref_rhythm$division) 
    
    #rescale  tempo of reference rhythm (which has  120 bpm)
    target <- ref_rhythm$onset / .5 * tempo
    
    messagef("Calculating features for trial %s, time base (raw) = %.1f, time base = %.3f, divison = %d, code = %s", tid,  time_base/max(ref_rhythm$division), time_base, max(ref_rhythm$division), code)
    
    #Calc cicrcular features beased on tatums, not bsaed on beat an in the isochronous case! 
    #Attention! Might have non-linear numerical ramifications for different tatums, save tatum and time_base 
    
    circ_features <- get_circular_features(query, time_base) %>% 
      mutate(division = max(ref_rhythm$division), time_base = time_base)
    
    #get alignmenat features as in the isochronous case.
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
    select(trial_id, set_id, offset, MAE, MAS, d_n, norm_dist, everything()) %>% 
    left_join(base_data, by = c("trial_id", "rhythm", "code"))
  
}
