library(tidyverse)
library(combinat)

messagef <- function(...) message(sprintf(...))

rhythms <- tribble(~rhythm, ~code, ~period,
  1, "abaa",    4,
  2, "bcaa",    4,
  3, "ddba",    4,
  4, "aeaa",    4,
  5, "aabaa",   3,
  6, "bbbaaa",  3,
  7, "aeadda",  3,
  8, "aabbaba", 4,
  9, "bccxfga",  4,
  10, "bbcaada", 4
)


invalid_ends <- c("d", "e", "f", "g")
invalid_ends <- c("c", "d", "f")
triplets <- c("f", "g")

all_cycles <- function(x){
  r <- length(x)
  y <- c(x, x)  
  map(1:r, ~{y[.x:(r + .x - 1)]})
  
}

generate_rhythms <- function(rhythm, type = "cycles"){
  if(type == "cycles"){
    permutations <- all_cycles(str_split(rhythm[[1]], "")[[1]])
  }
  else{
    permutations <- permn(str_split(rhythm[[1]], "")[[1]])
    
  }
  codes <- 
    permutations %>% 
    map_chr(~{paste(.x, collapse = "")}) 
  invalid_end <- permutations %>% 
    map_lgl(function(x){
      invalid <- x[[length(x)]] %in% invalid_ends
      #messagef("%s -> %d", x[[length(x)]], invalid)
      invalid
      })
  invalid_triplets <- permutations %>% 
    map_lgl(function(x){
      where1 <- which(triplets[1] == x)
      where2 <- which(triplets[2] == x)
      if(length(where1) != 0){
        if(length(where2) == 0){
          stop()
          ret <- TRUE
        }
        if((where2 - where1) != 1 || (where1 - 1) %%2 != 0){
          #messagef("Invalid: %s, %s", where1, where2)
          return(TRUE)
        }
        return(FALSE)
      }
      return(FALSE)
    })
  ret <- tibble(code = codes) %>% 
    mutate(original = code == rhythm[[1]], invalid_end = invalid_end, invalid_triplets = invalid_triplets) %>% 
    distinct(code, .keep_all = T) %>% 
    #arrange(code) %>% 
    mutate(variant = sprintf("%s%d", substr(type, 1, 1), 1:nrow(.))) %>% 
    select(variant, everything())
  ret  
}

generate_all_rhythms <- function(type = "cycles"){
  map_dfr(1:nrow(rhythms), function(r){
    messagef("Generating rhythm %02d: %s (%s)", r, rhythms$code[r], type)
    ret <- 
      generate_rhythms(rhythms$code[r], type = type) %>% 
      filter(!invalid_end, !invalid_triplets) %>% 
      mutate(rhythm = r, period = rhythms$period[r]) %>% 
      select(rhythm, variant, code, original, period) 
    ret
  })
}


realize_rhythm_atom <-function(rhythm_code, start_mpos = 0, period = 4, pitch = 72){
  if(rhythm_code == "a"){
    ret <- tibble(pitch = pitch, mpos = start_mpos, iois = 12, division = 1)
  }
  if(rhythm_code == "b"){
    ret <- tibble(pitch = c(pitch, pitch), mpos = c(start_mpos, start_mpos + 6), iois = c(6,6), division = 2)
  }
  if(rhythm_code == "c"){
    ret <- tibble(pitch = pitch, mpos = start_mpos + 6, iois = 6, division = 2)
  }
  if(rhythm_code == "d"){
    ret <- tibble(pitch = c(pitch, pitch), mpos = c(start_mpos, start_mpos + 9), iois = c(9, 3), division = 4)
  }
  if(rhythm_code == "e"){
    ret <- tibble(pitch = c(pitch, pitch, pitch), mpos = c(start_mpos, start_mpos + 4, start_mpos + 8), iois = c(4, 4, 4), division = 3)
  }
  if(rhythm_code == "f"){
    ret <- tibble(pitch = c(pitch, pitch), mpos = c(start_mpos, start_mpos + 8), iois = c(8, 4), division = 3)
  }
  if(rhythm_code == "g"){
    ret <- tibble(pitch = c(pitch), mpos = c(start_mpos + 4), iois = c(8), division = 3)
  }
  if(rhythm_code == "h"){
    ret <- tibble(pitch = c(pitch), mpos = c(start_mpos + 6), iois = c(18), division = 2)
  }
  if(rhythm_code == "x"){
    ret <- tibble(pitch = -1, mpos = start_mpos, iois = c(12), division = 1)
  }
  ret %>% mutate(period = period)
}

realize_rhythm_code <- function(rhythm_code, period = 4){
  rc <- str_split(rhythm_code[[1]], "")[[1]]
  ret <- NULL
  mpos <- 0
  for(i in 1:length(rc)){
    tmp <- realize_rhythm_atom(rc[i], mpos, period)    
    mpos <- mpos + 12    
    ret <- bind_rows(ret, tmp)
  }
  ret %>% filter(pitch >= 0)
}

rhythm_to_mcsv2 <- function(rhythm_tbl, tempo = 120, phrase_id = 1, chorus_id = 1){
  #print(phrase_tbl)
  period <- rhythm_tbl$period[1]
  bar_duration <- 12 * period
  #browser()
  final <- tibble(
    bar = as.integer(floor(rhythm_tbl$mpos / bar_duration)) + 1,
    beat = as.integer(floor(rhythm_tbl$mpos - (bar - 1) * bar_duration) / 12)  + 1,
    tatum = as.integer(rhythm_tbl$mpos %% 12) + 1
  )
  
  T <- 60/tempo
  final$beat_duration <- T
  final$division <- rhythm_tbl$division
  final$period <- period
  final$signature <- sprintf("%s/4", period) 
  final$phrase_id <- phrase_id
  final$phrase_begin <- 0
  final$phrase_begin[1] <- 1
  final$chorus_id <- chorus_id
  final$onset <- rhythm_tbl$mpos / 12 * T
  final$duration <- rhythm_tbl$iois*T/12
  final$pitch <- rhythm_tbl$pitch
  final$tatum <- (final$tatum - 1) / (12/final$division)  + 1
  return(final[, c("onset", "duration", "period", "division", "bar", "beat", "tatum", "beat_duration", "signature", "pitch", "phrase_id", "phrase_begin",  "chorus_id")])
}

all_rhythms_to_csv2 <- function(type = "combined", save = F){
  if(type == "combined"){
    ar <- generate_combined_rhythms()
    ar[is.na(ar$cycle), ]$cycle <- FALSE
  }
  else{
    ar <- generate_all_rhythms(type = type)  
  }
  if(save){
    map(1:nrow(ar), function(x){
    realize_rhythm_code(ar[x,]$code, ar[x,]$period) %>% 
      rhythm_to_mcsv2() %>% 
      write.table(file = sprintf("rhythms/%02d_%s.csv", ar[x,]$rhythm, ar[x,]$code), sep = ";", row.names = F, quote = F)
    })
  }
  messagef("Generated %d rhythms", nrow(ar))
  ar
}

generate_combined_rhythms <- function(){
  set.seed(888)
  ar_perm <- generate_all_rhythms(type = "perm") 
  ar_cycle <- generate_all_rhythms(type = "cycles")
  ar_comb <- ar_perm %>% left_join(ar_cycle %>% select(code) %>% mutate(cycle = TRUE), by = "code")
  ar_comb[ar_comb$original,]$cycle <- TRUE
  ar_comb[is.na(ar_comb$cycle),]$cycle <- FALSE
  ar_comb <- ar_comb %>% 
    group_by(rhythm) %>% 
    mutate(n_cycles = sum(cycle), missing = (n_cycles < 4 ) * (4 - n_cycles)) %>% 
    ungroup()
  extra <- NULL
  for(i in unique(ar_comb$rhythm)){
    missing <- ar_comb[ar_comb$rhythm == i,]$missing %>% unique()
    if(missing > 0){
      extra <- bind_rows(extra, ar_comb %>% filter(!cycle, rhythm == i) %>% sample_n(missing))        
    }  
  }
  ar <- bind_rows(ar_cycle %>% mutate(cycle = TRUE), extra %>% select(-n_cycles, -missing))  
  map_dfr(unique(ar_comb$rhythm), function(x){
    ar %>% filter(rhythm == x) %>% sample_n(4)
  })  %>% 
    group_by(rhythm) %>% 
    mutate(variant = as.integer(factor(variant))) %>%
    ungroup()
}

generate_experimental_design <- function(n_modalities = 5, n_rhythms = 10, n_participants = 120, rhythm_tbl){
  perm4 <- permn(1:(n_modalities -1))
  num_perm <- length(perm4)
  rep_pairs <- combn(1:n_modalities, 2) %>% t()
  num_pairs <- nrow(rep_pairs)
  counter <- 0
  browser()
  ret <- NULL
  for(i in 1:n_participants){
    for(r in 1:n_rhythms){
      messagef("Generating for part %d, rhythm %d (Counter = %d)", i, r, (counter %% num_pairs) + 1)
      perm <- perm4[[sample(1:num_perm, size = 1)]]    
      rep <- rep_pairs[(counter %% num_pairs) + 1,]
      rep_code <- paste(rep, collapse = ", ")
      variants <- rep(0, n_modalities)
      variants[rep] <- perm[1]
      variants[variants == 0] <- perm[perm != perm[1]]
      tmp <- tibble(p_id = i, rhythm = r, modality = 1:n_modalities, variant = variants)
      #print(tmp)
      ret <- bind_rows(ret, tmp)
      counter <- counter + 1
    }
  }  
  ret %>% left_join(ar %>% select(rhythm, variant, code), by = c("rhythm", "variant"))
}