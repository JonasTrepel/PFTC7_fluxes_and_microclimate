require(cpop)
require(ggplot2)
source("R/functions/Join_PAR_LICOR.R")
source("R/functions/neet_calc_wav.R")
require(dplyr)
require(lubridate)
require(tidyverse)
require(reshape2)
require(cowplot)
require(sarima)

flux_calc_seg <- function(fluxfiles, PAR_files=NULL,
                          dat_sel="weighted avg",
                          param = "nee",
                          licor_skip=3,
                          par_skip=7,
                          vol = 1.2^3,
                          area = 1.2^2){

  PAR_dat <- list()

  if(!is.null(PAR_files)){
    PAR_dat[[1]] <- read_delim(PAR_files[1], delim="\t", skip = par_skip)
    if(length(PAR_files) > 1){
      for(f in c(2:length(PAR_files))){
        PAR_dat[[f]] <-  read_delim(PAR_files[f], delim="\t", skip = par_skip)
      }
      PAR_comb <- bind_rows(PAR_dat)
    }
    else{
      PAR_comb <- PAR_dat[[1]]
    }
  }
  else{
    PAR_comb = NULL
  }

  if(dat_sel == "manual"){

      # segs <- readline("Choose segments for response curve (segment numbers separated by comma)")
      #
      # segs <- lapply(str_split(segs, pattern = ","), as.numeric)
    print("WIP")
  }
  else if(dat_sel == "weighted avg"){
    stats.df <- purrr::map_df(c(sort(fluxfiles$photo_names),
                                sort(fluxfiles$resp_names)),
                                ~neet_wav(., PAR=PAR_comb, fluxfiles=fluxfiles ,
                                          param = param, vol = vol, area = area))
  }
  else if(dat_sel == "best fit"){
        # segs <- c(1:nrow(cprime_seg))
        #
        # for(s in segs){
        #   end <- length(par)
        #   rsqd_max <- 0
        #
        #   s1 <- cprime_seg$x0[s]
        #   if(s == nrow(cprime_seg)){
        #     s2 <- end
        #   }
        #   else{
        #     s2 <- cprime_seg$x0[s+1]
        #   }
        #
        #   MSS <- mean(signal_strength[s1:s2])
        #   Mpar <- mean(par[s1:s2])
        #
        #   if ("nee" == param) {
        #     cw_prime <- cprime
        #   } else if ("et" == param) {
        #     cw_prime <- wprime
        #   }
        #   if ("nee" == param) {
        #     tag <- "c_prime"
        #   } else if ("et" == param) {
        #     tag <- "w_prime"
        #   }
        #
        #   if(MSS > SS_thresh && Mpar > par_thresh){
        #     linear.fit <- stats::lm(cprime[s1:s2] ~ (time[s1:s2]))
        #     rsqd <- summary(linear.fit)$r.sq
        #
        #     if(rsqd > rsqd_max){
        #       rsqd_max <- rsqd
        #       linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time[s1:s2]))
        #       aic.lm <- stats::AIC(linear.fit)
        #       inter <- as.numeric(linear.fit$coeff[1])
        #       dcw_dt <- as.numeric(linear.fit$coeff[2])
        #       rsqd <- summary(linear.fit)$r.sq
        #
        #       if ("nee" == param) {
        #         param_lm <- -(vol * pav * (1000) * dcw_dt)/(R * area *
        #                                                       (tav + 273.15))
        #       }
        #       else if ("et" == param) {
        #         param_lm <- (vol * pav * (1000) * dcw_dt)/(R * area *
        #                                                      (tav + 273.15))
        #       }
        #     }
        #   }
        # }
      print(WIP)
  }


  if("nee" == param) {
      names.vec <- c("filename", "camb",
                     "tav", "pav", "nee_lm", "rsqd",
                     "aic_lm",
                     "c_prime_min", "c_prime_max")
  }
  else if ("et" == param) {
      names.vec <- c("filename", "wamb",
                     "tav", "pav", "flux_lm", "rsqd",
                     "aic_lm",
                     "w_prime_min", "w_prime_max")
  }
    names(stats.df) <- names.vec
    return(stats.df)


}


