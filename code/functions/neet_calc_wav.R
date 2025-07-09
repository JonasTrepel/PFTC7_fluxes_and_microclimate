neet_wav <- function(filename, PAR=NULL, fluxfiles,
                     param = "nee",
                     licor_skip=3,
                     par_skip=7,
                     vol = 2.197,
                     area = 1.69,
                     SS_thresh = 90.0,
                     par_thresh = 650){
  
  ambient_file <- "N/A"
  
  
  suppressMessages(suppressWarnings(
    LI_dat <- read_delim(filename, delim="\t", skip = licor_skip)))
  
  #So we dont get a lot of warnings
  
  if(length(fluxfiles$ambient_names) < 1){
    ambient <- LI_dat[1:5, ]
  } 
  else {
    if(length(fluxfiles$ambient_names) >= 1){
      if(length(grep("resp.txt", filename, ignore.case = TRUE, value = FALSE)) == 1){
        ambient_file <- paste(strsplit(filename, "resp.txt"),
                              "a.txt", sep = "")
      } 
      else {
        if(length(grep("photo.txt", filename, ignore.case = TRUE, value = FALSE)) == 1) {
          ambient_file <- paste(strsplit(filename, "photo.txt"),
                                "a.txt", sep = "")
        }
      }
    }
    
    if(ambient_file %in% fluxfiles$ambient_names) {
      suppressMessages(suppressWarnings(ambient <- read_delim(ambient_file, skip = licor_skip, delim = "\t")))
    } 
    else {
      ambient <- LI_dat[1:5, ]
    }
    
  }
  
  d <- join_PAR_LICOR(PAR, LI_dat, PAR_tz = "America/Mexico_City", 
                      LICOR_tz = "Africa/Johannesburg")
  if(nrow(d) < 20){
    PAR <- NULL
    d <- join_PAR_LICOR(PAR, LI_dat, PAR_tz = "America/Mexico_City", 
                   LICOR_tz = "Africa/Johannesburg")
    d$INPUT1 <- rep(0, nrow(d))
  }
  
  if(is.null(PAR)){
    d$INPUT1 <- rep(0, nrow(d))
  }
  
  check_night_resp <- FALSE
  
  if(grepl("resp", filename, fixed = TRUE)){
    check_night_resp <- TRUE
  }
  
  R <- 8.314472
  
  time <- as.numeric(d$POSIXct)-min(as.numeric(d$POSIXct))
  co2 <- as.numeric(d$`CO2 (umol/mol)`)
  h2o <- as.numeric(d$`H2O (mmol/mol)`)
  
  if(check_night_resp == TRUE | is.null(PAR)){
    par <- rep(700, nrow(d))
  }
  else{
    par <- as.numeric(d$INPUT1)
  }
  
  press <- as.numeric(d$`Pressure (kPa)`)
  temp <- as.numeric(d$`Temperature (C)`)
  signal_strength <- as.numeric(d$`CO2 Signal Strength`)
  
  cprime <- co2/(1 - (h2o/1000))
  wprime <- h2o/(1 - (h2o/1000))
  wav_dil <- mean(h2o/(1 - (h2o/1000)))
  camb <- mean(as.numeric(as.character(ambient$`CO2 (umol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
  wamb <- mean(as.numeric(as.character(ambient$`H2O (mmol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
  
  res <- cpop(cprime, minseglen = 30)
  changepoints(res)
  cprime_seg <- fitted(res)
  
  p2 <- ggplot(aes(y=`CO2 (umol/mol)`, x=as.numeric(d$POSIXct)-min(as.numeric(d$POSIXct)),
                   color=(as.numeric(`CO2 Signal Strength`))),
               data=d) + 
    geom_point()  + scale_color_gradient(high="blue", low="red", limits=c(85, 105)) +
    geom_line() + theme(legend.position="none") +
    ylim(min(d$`CO2 (umol/mol)`), max(d$`CO2 (umol/mol)`)) + 
    ylab("CO2") + xlab("Time") + geom_vline(xintercept = cprime_seg$x0)
  
  p1 <- ggplot(aes(y=INPUT1, x=as.numeric(d$POSIXct)-min(as.numeric(d$POSIXct))), 
               data=d) + 
    geom_point(color="red") + geom_line(color="red") + 
    ylim(min(d$INPUT1), max(d$INPUT1)) +
    ylab("PAR") + xlab("Time") + geom_vline(xintercept = cprime_seg$x0 )
  p_c <- plot_grid(p1, p2, ncol=1, align="v", axis=1)
  
  plot(p_c)
  
  segs <- c(1:nrow(cprime_seg))
  aic.lm <- c()
  inter <- c()
  dcw_dt <- c()
  rsqd <- c()
  param_lm <- c()
  tav <- c()
  pav <- c()
  cav <- c()
  wav <- c()
  i = 1
  eff_sample <- 0
  
  for(s in segs){
    end <- length(par)
    
    s1 <- cprime_seg$x0[s]
    if(s == nrow(cprime_seg)){
      s2 <- end
    }
    else{
      s2 <- cprime_seg$x0[s+1]
    }
    
    MSS <- mean(signal_strength[s1:s2])
    Mpar <- mean(par[s1:s2])
    
    if ("nee" == param) {
      cw_prime <- cprime
    } else if ("et" == param) {
      cw_prime <- wprime
    }
    if ("nee" == param) {
      tag <- "c_prime"
    } else if ("et" == param) {
      tag <- "w_prime"
    }
    
    
    if((MSS > SS_thresh) && (Mpar > par_thresh)){
      eff_sample <- eff_sample + (s2-s1)
      frac_sample <- (s2-s1)
      linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time[s1:s2]))
      aic.lm[i] <- as.numeric(stats::AIC(linear.fit))*frac_sample
      inter[i] <- as.numeric(linear.fit$coeff[1])
      dcw_dt[i] <- as.numeric(linear.fit$coeff[2])
      rsqd[i] <- as.numeric(summary(linear.fit)$r.sq)*frac_sample
      t_av <- mean(temp[s1:s2])
      p_av <- mean(press[s1:s2])
      tav[i] <- mean(temp[s1:s2])*frac_sample
      pav[i] <- mean(press[s1:s2])*frac_sample
      cav[i] <- mean(co2[s1:s2])*frac_sample
      wav[i] <- mean(h2o[s1:s2])*frac_sample
      
      if ("nee" == param) {
        param_lm[i] <- -(vol * p_av * (1000) * dcw_dt[i])/(R * area *
                                                            (t_av + 273.15))*frac_sample
      } 
      else if ("et" == param) {
        param_lm[i] <- (vol * p_av * (1000) * dcw_dt[i])/(R * area *
                                                           (t_av + 273.15))*frac_sample
      }
      i <- i+1
    }
  }
  aic_avg <- sum(aic.lm)/eff_sample
  inter_avg <- sum(inter)/eff_sample
  dcw_dt_avg <- sum(dcw_dt)/eff_sample
  rsqd_avg <- sum(rsqd)/eff_sample
  param_lm_avg <- sum(param_lm)/eff_sample
  tav_avg <- sum(tav)/eff_sample
  pav_avg <- sum(pav)/eff_sample
  cav_avg <- sum(cav)/eff_sample
  wav_avg <- sum(wav)/eff_sample
  
  if ("nee" == param) {
    print(tibble::tibble(filename = filename, tav = tav_avg, pav = pav_avg,
                         nee_lm = param_lm_avg,
                         rsqd = rsqd_avg, aic.lm = aic_avg,
                         c_prime_min = min(cw_prime, na.rm = T),
                         c_prime_max = max(cw_prime, na.rm = T)))
    result <- tibble::tibble(filename = filename, camb, tav = tav_avg, pav = pav_avg,
                             nee_lm = param_lm_avg,
                             rsqd = rsqd_avg, aic.lm = aic_avg,
                             c_prime_min = min(cw_prime, na.rm = T),
                             c_prime_max = max(cw_prime, na.rm = T))
  }
  else if ("et" == param) {
    print(tibble::tibble(filename = filename, tav = tav_avg, pav = pav_avg,
                         flux_lm = param_lm_avg,
                         rsqd = rsqd_avg, aic.lm = aic_avg,
                         w_prime_min = min(cw_prime, na.rm = T),
                         w_prime_max = max(cw_prime, na.rm = T)))
    result <- tibble::tibble(filename = filename, wamb, tav = tav_avg, pav = pav_avg,
                             flux_lm = param_lm_avg,
                             rsqd = rsqd_avg, aic.lm = aic_avg,
                             w_prime_min = min(cw_prime, na.rm = T),
                             w_prime_max = max(cw_prime, na.rm = T))
  }
  return(result)
}
