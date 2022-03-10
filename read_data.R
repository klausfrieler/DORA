library(tidyverse)

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
  tibble(m_IOI = mean(iois, na.rm = T),
         med_IOI = median(iois, na.rm = T),
         sd_IOI = sd(iois, na.rm = T),
         t0 = onset[1])
}

read_all_files <- function(data_dir){
  files <- list.files(data_dir, full.names = T)
  map_dfr(files, 
          function(fn){
            read.csv(fn, header = F) %>% 
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