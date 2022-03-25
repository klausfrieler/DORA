library(tidyverse)
source("analysis.R")

parse_filename <- function(fname){
  fname <- fname %>% basename() %>% str_replace(".csv", "") 
  elements <- str_split(fname, "_")
  map_dfr(elements, function(x){
    tibble(experimenter = x[1], 
           age_group = x[2],
           condition = x[3],
           serial = x[4],
           source = x[5],
           tempo = x[6],
           setting = x[7],
           p_id = paste(x[1:4], collapse = "_"),
           trial_id = paste(x, collapse = "_")
           )
  })
}

get_basic_stats <- function(onset){
  iois <- diff(onset)
  tibble(mean_ioi = mean(iois, na.rm = T),
         med_ioi = median(iois, na.rm = T),
         sd_ioi = sd(iois, na.rm = T),
         cv_ioi = sd_ioi/mean_ioi, 
         t0 = onset[1]) 
}

read_all_files <- function(data_dir){
  files <- list.files(data_dir, pattern = "*.csv", full.names = T)
  map_dfr(files, 
          function(fn){
            messagef("Reading %s", fn)
            browser()
            tmp <- read.csv(fn, header = F, stringsAsFactors = F) %>% 
              as_tibble() %>% 
              bind_cols(parse_filename(fn))
            }) %>% 
    select(-V2) %>% 
    rename(onset =  V1) %>% 
    group_by(trial_id) %>% 
    mutate(n_onsets = n()) %>% 
    mutate(get_basic_stats(onset)) %>% 
    ungroup()
}

setup_workspace <- function(iso_data_dir, rhythm_data_dir = NULL, reread_data = FALSE){
  if(reread_data){
    messagef("Importing all data fro %s", iso_dat_dir)
    all_data_iso <- read_all_files(iso_data_dir)
    #all_data_rhythm <- read_all_files("rhythm")
    saveRDS(all_data_iso, file.path(iso_data_dir, "all_data_ioi.rds"))
  }
  else{
    messagef("Reading all_data_iso.rds")
    all_data_iso <- readRDS(file.path(iso_data_dir, "all_data_ioi.rds"))
  }
  assign("all_data_iso", all_data_iso, globalenv())
  messagef("Calculating iso features...")
  iso_features <-  get_iso_features(all_data_iso) 
  assign("iso_features", iso_features, globalenv())
  messagef("Done.")
  
  invisible(all_data_iso)
}