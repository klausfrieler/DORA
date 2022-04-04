library(tidyverse)
source("analysis.R")
source("dtw_util.R")

messagef <- function(...) message(sprintf(...))

parse_filename <- function(fname, type = c("iso", "rhythm_prod", "rhythm_def")){
  type <- match.arg(type)
  
  fname <- fname %>% basename() %>% str_replace(".csv", "") 
  elements <- str_split(fname, "_")
  if(type == "iso"){
    map_dfr(elements, function(x){
      tibble(experimenter = x[1], 
             age_group = x[2],
             condition = x[3],
             serial = x[4],
             source = x[5],
             tempo = x[6],
             setting = x[7],
             p_id = paste(x[1:3], collapse = "_"),
             trial_id = paste(x, collapse = "_")
             )
    })
  } 
  else if(type == "rhythm_prod"){
    map_dfr(elements, function(x){
      tibble(experimenter = x[1], 
             age_group = x[2],
             condition = x[3],
             serial = x[4],
             source = x[5],
             rhythm = x[6],
             setting = x[7],
             p_id = paste(x[1:3], collapse = "_"),
             trial_id = paste(x, collapse = "_"))
      })
  }
  else if(type == "rhythm_def"){
    map_dfr(elements, function(x){
      tibble(rhythm_id = x[1], 
             code = x[2])
    })
  }
}

get_basic_stats <- function(onset, type = c("iso", "rhythm_prod")){
  iois <- diff(onset)
  type <- match.arg(type)
  if(type == "iso"){
    tibble(mean_ioi = mean(iois, na.rm = T),
           med_ioi = median(iois, na.rm = T),
           sd_ioi = sd(iois, na.rm = T),
           log_sd_ioi = log(sd(iois, na.rm = T)),
           cv_ioi = sd_ioi/mean_ioi, 
           t0 = onset[1]) 
    
  }
  else{
    tibble(t0 = onset[1]) 
  }
}

read_all_files <- function(data_dir, type = c("iso", "rhythm_prod")){
  type <- match.arg(type)
  files <- list.files(data_dir, pattern = "*.csv", full.names = T)
  map_dfr(files, 
          function(fn){
          #messagef("Reading %s", fn)
          #browser()
          tmp <- data.table::fread(fn) %>% 
            as_tibble() %>% 
            bind_cols(parse_filename(fn, type))
          tmp$V2 <- NULL
          tmp
          }) %>% 
    rename(onset =  V1) %>% 
    group_by(trial_id) %>%
    mutate(n_onsets = n()) %>% 
    mutate(get_basic_stats(onset, type = type)) %>% 
    ungroup()
}

setup_rhythm_data <- function(rhythm_stim_dir = "rhythms_simple2", 
                              fname_design = "rhythms_simple2/design/experimental_design_rhythm_simple.csv"){
  rhythm_design = read.csv(fname_design, sep = ";", stringsAsFactors = F) %>% 
    as_tibble() 
  
  patch_rhythms <- rhythm_design %>% 
    group_by(rhythm) %>% 
    mutate(has_na = any(is.na(code))) %>% 
    ungroup() %>% 
    filter(has_na,!is.na(code)) %>% 
    distinct(rhythm, patch_code = code) 

  rhythm_design <- rhythm_design %>% left_join(patch_rhythms, by = "rhythm")
  rhythm_design[is.na(rhythm_design$code),]$code <- rhythm_design[is.na(rhythm_design$code),]$patch_code
  rhythm_design <- rhythm_design %>% 
    filter(modality %in% c(2,5)) %>% 
    mutate(p_id = sprintf("%02d", p_id),
           setting = factor(modality, levels = c(2, 5), labels = c("ac", "so")),
           rhythm = factor(rhythm,
                           levels = 1:10,
                           labels = c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x'))) %>%
  select(-modality, -midi_file, -variant, -patch_code)
    
  rf <- list.files(rhythm_stim_dir, pattern = "*.csv", full.names = T)
  rhythm_data <- 
    map_dfr(rf, function(fn){
      data.table::fread(fn) %>% 
        as_tibble() %>% 
        select(onset, bar, beat, tatum, division, period) %>% 
        mutate(running_beat = (bar - 1) * period + (beat - 1)) %>% 
        bind_cols(parse_filename(fn, type = "rhythm_def"))
    }) %>% 
    mutate(rhythm_id = factor(rhythm_id, 
                              labels = c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')))
  list(design = rhythm_design, rhythms = rhythm_data)
}

post_process <- function(data, data_type = c("onsets", "features"), type = c("iso", "rhythm_prod")){
  data_type <- match.arg(data_type)
  type <- match.arg(type)
  if(type == "iso"){
    exclude_trials <- data %>% filter(source == "ex", sd_ioi > .05) %>% distinct(trial_id)   
  }
  data <- data %>% filter(source != "ex")
}

setup_workspace <- function(iso_data_dir = "data/drumking", rhythm_data_dir = "data/rhythm_prod", reread_data = FALSE){
  messagef("Reading stimulus and design data")
  stimulus_data <- setup_rhythm_data()
  assign("stimulus_data", stimulus_data, globalenv())
  
  if(reread_data){
    messagef("Importing all iso data from %s", iso_data_dir)
    iso_data <- read_all_files(iso_data_dir, type = "iso") %>% filter(source != "ro")
    saveRDS(iso_data, file.path(iso_data_dir, "iso_data.rds"))
    
    messagef("Calculating iso features...")
    iso_features <-  get_iso_features(iso_data) 
    assign("iso_features", iso_features, globalenv())
    saveRDS(iso_features, file.path(iso_data_dir, "iso_features.rds"))
    messagef("Done.")

    messagef("Importing all rhythm data from %s", iso_data_dir)
    messagef <- function(...) message(sprintf(...))
    
    rhythm_data <- read_all_files(rhythm_data_dir, type = "rhythm_prod")
    saveRDS(rhythm_data, file.path(rhythm_data_dir, "rhythm_data.rds"))
    
    messagef("Calculating iso features...")
    rhythm_features <-  get_rhythm_features(rhythm_data, stimulus_data) 
    assign("rhythm_features", rhythm_features, globalenv())
    saveRDS(rhythm_features, file.path(rhythm_data_dir, "rhythm_features.rds"))
    messagef("Done.")
    
  }
  else{
    messagef("Reading iso data")
    iso_data <- readRDS(file.path(iso_data_dir, "iso_data.rds"))
    iso_features <- readRDS(file.path(iso_data_dir, "iso_features.rds"))
    
    messagef("Reading rhythm data")
    rhythm_data <- readRDS(file.path(rhythm_data_dir, "rhythm_data.rds"))
    rhythm_features <- readRDS(file.path(rhythm_data_dir, "rhythm_features.rds"))
    
  }
  assign("iso_data", iso_data, globalenv())
  assign("rhythm_data", rhythm_data, globalenv())
  assign("iso_features", iso_features, globalenv())
  assign("rhythm_features", rhythm_features, globalenv())
  invisible(list(iso_data, iso_features, rhythm_data, rhythm_features))
}