# filename <- "raw_data/test/1_2000_east_3_day_photo.txt"
# filename <- "raw_data/Site 2/2_2200_east_5_day_photo.txt"
# fluxfiles <- licor_files
fluxCalc <- function (fluxfiles,
                           param = "nee",
                           skip = 3,
                           vol = 2.197,
                           area = 1.69,
                           tstart = NULL,
                           tfinish = NULL,
                           signal_threshold = 95,
                           ask_flag = TRUE) {
  
  #Ask flag: a parameter to, if true, will ask me if i want to flag this file or not, it is problematic or no. It will also fill the dots in the part within the tstart and t finish, In the output there will be a new column, telling you if its flaged or not. 
  
  #The function, has a function inside, this is neeet.fit. 
  
  neeet.fit <- function(filename, tstart = tstart, tfinish = tstart, signal_threshold = signal_threshold) {
    suppressMessages(suppressWarnings(input <- read_delim(filename, skip = skip, delim = "\t"))) #So we dont get a lot of warnings
    
    # if (nrow(input) > 90) {
    #   input <- input[1:90, ]
    # }
    if(length(fluxfiles$ambient_names) < 1){
      ambient <- input[1:5, ]
    } else {
      if(length(fluxfiles$ambient_names) >= 1){
        if(length(grep("resp.txt", filename, ignore.case = TRUE, value = FALSE)) == 1){
          ambient_file <- paste(strsplit(filename, "resp.txt"),
                                "a.txt", sep = "")
        } else {
          if(length(grep("photo.txt", filename, ignore.case = TRUE, value = FALSE)) == 1) {
            ambient_file <- paste(strsplit(filename, "photo.txt"),
                                  "a.txt", sep = "")
          }
        }
      }
      
      if(ambient_file %in% fluxfiles$ambient_names) {
        suppressMessages(suppressWarnings(ambient <- read_delim(ambient_file, skip = skip, delim = "\t")))
      } else {
        ambient <- input[1:5, ]
      }
      
    }
    
    R <- 8.314472
    
    time <- as.numeric(as.character(rownames(input)))
    co2 <- as.numeric(as.character(input$`CO2 (umol/mol)`))
    h2o <- as.numeric(as.character(input$`H2O (mmol/mol)`))
    par <- NA
    press <- as.numeric(as.character(input$`Pressure (kPa)`))
    temp <- as.numeric(as.character(input$`Temperature (C)`))
    signal_strength <- as.numeric(as.character(input$`CO2 Signal Strength`))
    
    if(is.null(tstart) | is.null(tfinish)){
      pch <- 1
    } else {
      pch <- ifelse(time %in% seq(tstart, tfinish), 16, 1)
    }
    
    tav <- mean(temp)
    pav <- mean(press)
    cav <- mean(co2)
    wav <- mean(h2o)
    cprime <- co2/(1 - (h2o/1000))
    wprime <- h2o/(1 - (h2o/1000))
    wav_dil <- mean(h2o/(1 - (h2o/1000)))
    camb <- mean(as.numeric(as.character(ambient$`CO2 (umol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
    wamb <- mean(as.numeric(as.character(ambient$`H2O (mmol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
    
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
    
    plot(cw_prime ~ (time), col = c("black","red")[as.numeric((signal_strength < signal_threshold))+1],
         main = filename, ylab = tag, pch = pch)
    
    if(is.null(tstart)){
      tstart <- readline("Enter preferred start time for fitting. Do not include units. \n Round to nearest integer second. \n Do not use 0. \n  If default of 10s is preferred, press 'return':")
      if (!grepl("^[0-9]+$", tstart)) {
        tstart <- 10
      }
      tstart <- as.integer(tstart)
    }
    
    if(is.null(tfinish)){
      tfinish <- readline("Enter preferred finish time for fitting. Do not include units. \n Round to nearest integer second. \n  If default of 120s is preferred, press 'return':")
      if (!grepl("^[0-9]+$", tfinish)) {
        tfinish <- 120
      }
      tfinish <- as.integer(tfinish)
    }
    
    linear.fit <- stats::lm(cw_prime[tstart:tfinish] ~ (time[tstart:tfinish]))
    aic.lm <- stats::AIC(linear.fit)
    inter <- as.numeric(linear.fit$coeff[1])
    dcw_dt <- as.numeric(linear.fit$coeff[2])
    rsqd <- summary(linear.fit)$r.sq
    if ("nee" == param) {
      param_lm <- -(vol * pav * (1000) * dcw_dt)/(R * area *
                                                    (tav + 273.15))
    } else if ("et" == param) {
      param_lm <- (vol * pav * (1000) * dcw_dt)/(R * area *
                                                   (tav + 273.15))
    }
    abline(inter, dcw_dt, col = 6)
    cw_not <- cw_prime[tstart]
    df <- data.frame(cw_prime, time)
    df <- subset(x = df, subset = (time > tstart & time <
                                     tfinish))
    strt <- data.frame(A = c(150, 850), B = c(0, 1000))
    
    e <- try({
      optimize.start <- nls2::nls2(cw_prime ~ (cw_not - A) *
                                     exp(-time/B) + A, data = df, start = strt, algorithm = "default",
                                   control = stats::nls.control(warnOnly = TRUE), trace = FALSE)
      uptake.fm <- stats::nls(cw_prime ~ (cw_not - A) * exp(-time/B) +
                                A, data = df, start = stats::coef(optimize.start),
                              control = stats::nls.control(warnOnly = TRUE), trace = FALSE)
      sigma <- summary(uptake.fm)$sigma
    }, silent = T)
    
    if(class(e)[[1]] == "try-error"){
      print("nls2 model failed. No NLM output")
      nee_exp <- NA
      sigma <- NA
      aic.nlm <- NA
      flux_exp <- NA
    } else {
      sigma <- summary(uptake.fm)$sigma
      aic.nlm <- stats::AIC(uptake.fm)
      cw_ss <- summary(uptake.fm)$param[1]
      tau <- summary(uptake.fm)$param[2]
      if ("nee" == param) {
        nee_exp <- ((camb - cw_ss)/(area * tau)) * (vol *
                                                      pav * (1000)/(R * (tav + 273.15)))
      } else if ("et" == param) {
        flux_exp <- -((wamb - cw_ss)/(area * tau)) * (vol *
                                                        pav * (1000 - wav)/(R * (tav + 273.15)))
      }
      curve((cw_not - cw_ss) * exp(-(x)/tau) + cw_ss, col = 4,
            add = TRUE)
    }
    
    if ("nee" == param) {
      print(tibble::tibble(filename = filename, tstart = tstart,
                           tfinish = tfinish, nee_lm = param_lm, nee_exp = nee_exp,
                           rsqd = rsqd, sigma = sigma, aic.lm = aic.lm,
                           aic.nlm = aic.nlm,
                           c_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                           c_prime_max = max(cw_prime[tstart:tfinish], na.rm = T)))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if (replicate == "y") {
        neeet.fit(filename, tstart = NULL, tfinish = NULL, signal_threshold = signal_threshold)
      } else {
        if(ask_flag == TRUE){
          flag <- readline("Would you like to flag the flux measurements? (y/n)")
          if(flag == "y"){
            flagged <- TRUE
          } else {
            flagged <- FALSE
          }
        } else {
          flagged <- FALSE
        }
        
        return(tibble::tibble(filename, tstart, tfinish,
                              camb, tav, pav, param_lm, nee_exp, rsqd, sigma,
                              aic.lm, aic.nlm,
                              c_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                              c_prime_max = max(cw_prime[tstart:tfinish], na.rm = T),
                              flagged = flagged))
      }
    } else if ("et" == param) {
      print(tibble::tibble(filename = filename, tstart = tstart,
                           tfinish = tfinish, flux_lm = param_lm, flux_nlm = flux_exp,
                           rsqd = rsqd, nlm_sigma = sigma, aic.lm = aic.lm,
                           aic.nlm = aic.nlm,
                           w_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                           w_prime_max = max(cw_prime[tstart:tfinish], na.rm = T)))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if (replicate == "y") {
        neeet.fit(filename, tstart = NULL, tfinish = NULL, signal_threshold = signal_threshold)
      } else {
        if(ask_flag == TRUE){
          flag <- readline("Would you like to flag the flux measurements? (y/n)")
          if(flag == "y"){
            flagged <- TRUE
          } else {
            flagged <- FALSE
          }
        } else {
          flagged <- FALSE
        }
        return(tibble::tibble(filename, tstart, tfinish,
                              wamb, tav, pav, cav, param_lm, flux_exp, rsqd,
                              sigma, aic.lm, aic.nlm,
                              w_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                              w_prime_max = max(cw_prime[tstart:tfinish], na.rm = T),
                              flagged = flagged))
      }
    }
  }
  
  if (length(fluxfiles$photo_names) >= 1 & length(fluxfiles$resp_names) >= 1) {
    stats.df <- purrr::map_df(c(sort(fluxfiles$photo_names), sort(fluxfiles$resp_names)),
                              ~neeet.fit(., tstart = tstart, tfinish = tfinish, signal_threshold = signal_threshold))
  } else {
    
    if (length(fluxfiles$resp_names) >= 1) {
      stats.df <- purrr::map_df(sort(fluxfiles$resp_names), ~neeet.fit(., tstart = tstart, tfinish = tfinish, signal_threshold = signal_threshold))
    }
    if (length(fluxfiles$photo_names) >= 1) {
      stats.df <- purrr::map_df(sort(fluxfiles$photo_names), ~neeet.fit(., tstart = tstart, tfinish = tfinish, signal_threshold = signal_threshold))
    }
    
  }
  
  if ("nee" == param) {
    names.vec <- c("filename", "tstart", "tfinish", "camb",
                   "tav", "pav", "nee_lm", "nee_exp", "lm_rsqd", "non_linear_sigma",
                   "aic_lm", "aic_nlm",
                   "c_prime_min", "c_prime_max","flagged")
  } else if ("et" == param) {
    names.vec <- c("filename", "tstart", "tfinish", "wamb",
                   "tav", "pav", "cav", "flux_lm", "flux_nlm", "lm_rsqd",
                   "non_linear_sigma", "aic_lm", "aic_nlm",
                   "w_prime_min", "w_prime_max","flagged")
  }
  names(stats.df) <- names.vec
  return(stats.df)
}


test_flux_files <- function(fluxfiles, skip = 3, min_rows = 20){
  
  fluxfiles$photo_names <- unlist(lapply(fluxfiles$photo_names, function(filename){
    e <- try(suppressMessages(suppressWarnings(input <- read_delim(filename, skip = skip, delim = "\t"))), silent = TRUE)
    
    if(class(e)[[1]] == "try-error"){
      print(paste(filename, "error in reading the file, likely not a LI-7500 data file"))
      return(NULL)
    } else {
      if(!"CO2 (mmol/m^3)" %in% names(input)){
        print(paste(filename, "is likely not a LI-7500 data file"))
        return(NULL)
      } else {
        if(nrow(input) < min_rows){
          print(paste(filename, "has too few observations"))
          return(NULL)
        } else {
          return(filename)
        }
      }
    }
  }))
  
  fluxfiles$ambient_names <- unlist(lapply(fluxfiles$ambient_names, function(filename){
    e <- try(suppressMessages(suppressWarnings(input <- read_delim(filename, skip = skip, delim = "\t"))), silent = TRUE)
    
    if(class(e)[[1]] == "try-error"){
      print(paste(filename, "error in reading the file, likely not a LI-7500 data file"))
      return(NULL)
    } else {
      if(!"CO2 (mmol/m^3)" %in% names(input)){
        print(paste(filename, "is likely not a LI-7500 data file"))
        return(NULL)
      } else {
        if(nrow(input) < min_rows){
          print(paste(filename, "has too few observations"))
          return(NULL)
        } else {
          return(filename)
        }
      }
    }
  }))
  
  fluxfiles$resp_names <- unlist(lapply(fluxfiles$resp_names, function(filename){
    e <- try(suppressMessages(suppressWarnings(input <- read_delim(filename, skip = skip, delim = "\t"))), silent = TRUE)
    
    if(class(e)[[1]] == "try-error"){
      print(paste(filename, "error in reading the file, likely not a LI-7500 data file"))
      return(NULL)
    } else {
      if(!"CO2 (mmol/m^3)" %in% names(input)){
        print(paste(filename, "is likely not a LI-7500 data file"))
        return(NULL)
      } else {
        if(nrow(input) < min_rows){
          print(paste(filename, "has too few observations"))
          return(NULL)
        } else {
          return(filename)
        }
      }
    }
  }))
  
  return(fluxfiles)
}

filter_flagged <- function(fluxfiles, flux_df){
  x1 <- list(photo_names = fluxfiles$photo_names, resp_names = fluxfiles$resp_names) %>%
    lapply(., function(x){ x[x %in% licor_nee$filename[licor_nee$flagged]]})
  
  return(c(x1, fluxfiles["ambient_names"]))
  
}