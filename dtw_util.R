library(tidyverse)
library(dtw)

simulate_beats <- function(T = .5, n_beats, offset = 0, sd = .1 * T){
  iois <- rep(T, n_beats - 1) + rnorm(n_beats - 1, 0, sd)
  t <- cumsum(c(0, iois)) + offset
  t
}

jitter_onsets <- function(t, sd = .02){
  t0 <- t[1]
  iois <- diff(t) + rnorm(length(t) - 1, 0, sd)
  while(any(iois < 0)){
    iois <- diff(t) + rnorm(length(t) - 1, 0, sd)
  }
  t <- cumsum(c(t0, iois))
  t
}

drop_elements <- function(x, n = 0, type = c("random", "boot", "first", "last")){
  l <- length(x)
  type <- match.arg(type)
  if(type == "random"){
    sz = max(1, min(l, l - n))
    x <- sample(x = x, size = sz) %>% sort()
  }
  else if(type == "boot"){
    sz = max(1, min(l, l - n))
    x <- sample(x = x, size = sz, replace = T) %>% sort()
  }
  else if(type == "first"){
    if(n >= l || n < 0){
      x <- vector(class(x), 0)
    }
    else{
      x <- x[(n+1):l]
    }
  }
  else if(type == "last"){
    if(n >= l || n < 0){
      x <- vector(class(x), 0)
    }
    else{
      n = max(1, min(l, n))
      x <- x[1:(l-n)]
    }
  }
  x
}

test_dtw <- function(T = .5, 
                     n_beats = 10, 
                     n_simul = 100, 
                     sd_range = seq(0, T, .05*T), 
                     offset_range = seq(0, T, .1*T), 
                     drop_range = seq(0, floor(n_beats/2))){
  map_dfr(offset_range,  function(off){
    map_dfr(sd_range, function(s){
      messagef("Testing sd = %.2f, offset = %.2f", s, off)
      map_dfr(drop_range, function(dr){
        map_dfr(1:n_simul, function(k){
          reference <- seq(0, n_beats) * T
          beats <- simulate_beats(T, n_beats, off) %>% sample(x = . , size = n_beats - dr, replace = F)
          d <- dtw(beats, reference)
          tibble(T = T, sd = s, offset = off, dist = d$distance, norm_dist = d$normalizedDist, iter = k, drop = dr)
        })
        
      })
    })
  })
}

plot_dtw_alignment <- function(x, y = NULL){
  if(class(x) == "dtw"){
    d <- x
    x <- d$query %>% as.vector()
    y <- d$reference %>% as.vector()
  }
  else{
    if(is.null(y)){
      stop("y must have value")
    }
    d <- dtw(x, y, keep.internals = T)
  }
  plot_df <- bind_rows(tibble(x = x, type = "query"), tibble(x = y, type = "reference"))
  plot_df2 <- tibble(x = x[d$index1], y = y[d$index2]) %>% mutate(d = x - y)
  
  #browser()
  q <- plot_df %>% ggplot(aes(x = x, y  = type, colour = type)) + geom_point(size = 5)
  q <- q + geom_segment(data = plot_df2, 
                        aes(x = x, y = "query", xend = y, yend = "reference"),
                        colour = "black", 
                        arrow = arrow(length = unit(0.30, "cm"), 
                                      ends = "last", 
                                      type = "closed"))
  q <- q + theme_minimal()
  q <- q + labs(x = "Time (s)", title = sprintf("DTW: dist = %.2f, norm = %.2f, d = %.2f, abs(d) = %.2f", 
                                                d$distance, d$normalizedDistance, 
                                                mean(plot_df2$d), mean(abs(plot_df2$d))))
  q <- q + theme(legend.title = element_blank())
  q
  
}