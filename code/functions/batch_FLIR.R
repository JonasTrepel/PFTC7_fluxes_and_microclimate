## FUNCTIONS FOR PROCESSING RAW FLIR DATA


# This function takes a column which contains the numbers of pictures of a flir camera and transform them to proper file names
## --> attention - needs tweaking if supposed to be used universal 
## --> requires a dataframe with a column called file_name that contains the number if the files 

getFLIRnames <- function(data = dt){
  
  
  library(tidyverse)
  
  # Function to create a sequence of numbers as a string
  create_sequence <- function(start, end) {
    seq(start, end, by = 1) %>% toString()
  }
  
  # Create a new data frame 'dt.new' by performing a series of data manipulations on the original data frame 'data'
  dt.new <- data %>%
    
    # Separate the 'file_name' column into multiple columns using ';' as the separator
    separate(file_name, c("name1", "name2", "name3", "name4", "name5"), sep = ";" ) %>% 
    
    # Reshape the data from wide to long format, keeping 'file_name_new' as a new column
    pivot_longer(cols = name1:name5, names_to = "delete", values_to = "file_name_new") %>% 
    
    # Drop rows with missing values in 'file_name_new'
    drop_na(file_name_new) %>% 
    
    # Drop the 'delete' column
    select(-delete) %>% 
    
    # Remove leading and trailing whitespaces from 'file_name_new'
    mutate(file_name_new = trimws(file_name_new)) %>% 
    
    # Separate 'file_name_new' into 'start' and 'end' columns using '_' as the separator
    separate(file_name_new, c("start", "end"), sep = "_") %>% 
    
    # Convert 'start' and 'end' columns to numeric
    mutate(start = as.numeric(start),
           end = ifelse(is.na(end), as.numeric(start), as.numeric(end))) %>% 
    drop_na(start) %>%
    drop_na(end) %>%
    
    # Perform row-wise operations to create a new column 'sequence' using the 'create_sequence' function
    rowwise() %>%
    mutate(sequence = create_sequence(start, end)) %>% 
    
    # Separate the 'sequence' column into multiple columns using ',' as the separator
    separate(sequence, c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8", "name9", "name10", "name11", "name12"), sep = "," )  %>% 
    
    # Reshape the data again from wide to long format, keeping 'file_number' as a new column
    pivot_longer(cols = name1:name12, names_to = "delete", values_to = "file_number") %>% 
    
    # Drop rows with missing values in 'file_number'
    drop_na(file_number) %>% 
    
    # Drop the 'delete' column and the 'start' and 'end' columns
    select(-delete, start, end) %>% 
    
    # Remove leading and trailing whitespaces from 'file_number' and convert to numeric
    mutate(file_number = as.numeric(trimws(file_number))) %>% 
    
    # Filter out rows where 'file_number' is not even
    filter(file_number %% 2 != 0) %>% 
    
    # Create a new column 'file_name' by concatenating "FLIR", 'file_number', and ".jpg"
    mutate(file_name = paste0("FLIR", file_number, ".jpg"))
  
  
  return(dt.new)
  
}


# Functions for batch processing 
# taken from github.com/rasenior/ThermStats/R/batch_convert.R & batch_extract.R

#  Batch extraction of raw data from FLIR thermal images.
batch_extract <- function(in_dir,
                          file_name,
                          write_results = TRUE,
                          out_dir = NULL,
                          out_name = NULL,
                          inc = NULL,
                          exc = NULL,
                          exiftoolpath = "installed"){
  
  # # ## variables for testing
  # in_dir = dir_FLIRimages
  # file_name = flir_df$file_name[1:10]
  # # write_results = F
  # write_results = T
  # out_dir = out_dir
  # exiftoolpath = dir_exiftool
  # inc = NULL
  # exc = NULL
  # 
  
  # File names --------------------------------------------------------------
  
  # Get file names
  # file.names <- list.files(in_dir, full.names = TRUE)
  file.names <- paste0(in_dir, file_name)

  # Subset files
  if(!(is.null(inc))) {
    file.names <- file.names[file.names %in% file.path(in_dir, inc)]
  }
  if(!(is.null(exc))) {
    file.names <- file.names[!(file.names %in% file.path(in_dir, exc))]
  }
  
  # Remove path & file extension to get photo number
  photo_no <- basename(file.names)
  photo_no <- gsub("FLIR","",photo_no)
  photo_no <- gsub(".jpg","", photo_no)
  
  # Create empty list to populate with temperature matrices
  raw_dat <- vector("list", length(photo_no))
  names(raw_dat) <- photo_no
  
  # create empty list to populate with metadata
  camera_params <- vector("list", length(photo_no))
  names(camera_params) <- photo_no
  
  # Extract FLIR data -------------------------------------------------------
  
  for (i in 1:length(file.names)) {
    cat("Processing file", i, "of", length(file.names),"\n")
    cat("Reading file...","\n")
    
    # Try and read in each FLIR file
    photo_i <- tryCatch(
      {
        Thermimage::readflirJPG(imagefile = file.names[i], 
                                exiftoolpath = exiftoolpath)
      },
      error = function(x){
        message(paste("Couldn't process file:",file.names[i]))
        return(NA)
      })
    
    # Flip the matrix (makes plotting later on easier)
    photo_i <- Thermimage::mirror.matrix(Thermimage::rotate180.matrix(photo_i))
    colnames(photo_i) <- NULL
    
    # Write the matrix to the appropriate index in the empty list
    raw_dat[[i]] <- photo_i
    
    #-------------------------------
    # Get camera parameters (constant for each camera)
    cat("Extracting camera parameters...","\n")
    camera_params_i <-
      Thermimage::flirsettings(imagefile = file.names[i],
                               exiftoolpath = exiftoolpath)
    camera_params[[i]] <- camera_params_i[["Info"]]
  }
  

  # Write -------------------------------------------------------------------
  
  results <- list(raw_dat = raw_dat, camera_params = camera_params)
  
  
  if (write_results) {
    
    if (is.null(out_dir)) out_dir <- getwd()
    if (is.null(out_name)) out_name <- paste("flir_raw_", Sys.Date(),
                                               ".Rdata",sep = "")
    
    out_path <- file.path(out_dir, paste(out_name, ".Rdata", sep = ""))
    save(results,file = out_path)
  }
  
  return(results)
}

# Batch convert list of raw FLIR matrices to a list of temperature matrices (°C).


batch_convert <- function(raw_dat,
                          camera_params,
                          # E = 1,
                          # OD = 1,
                          # RTemp = 20,
                          # ATemp = RTemp,
                          # IRWTemp = RTemp,
                          # IRT = 1,
                          # RH = 50,
                          # PR1 = 21106.77,
                          # PB = 1501,
                          # PF = 1,
                          # PO = -7340,
                          # PR2 = 0.012545258,
                          write_results = TRUE,
                          out_dir = NULL,
                          file_name = NULL){
  
  
  # ## variables for testing
  # raw_dat = raw_dat
  # # Emissivity = mean of range in Scheffers et al. 2017
  # E = camera_params[, "Emissivity"]
  # # Object distance = hypotenuse of right triangle where 
  # # vertical side is 1.3 m (breast height) & angle down is 45°
  # OD = camera_params[, "ObjectDistance"]
  # # atmospheric temperature measured in the field
  # RTemp = camera_params[, "ReflectedApparentTemperature"] # Apparent reflected temperature & atmospheric temperature
  # ATemp = camera_params[, "AtmosphericTemperature"]     # atmospheric temperature measured in the field
  # RH = camera_params[, "RelativeHumidity"]      # Relative humidity = relative humidity measured in the field
  # # Calibration constants from 'batch_extract'
  # PR1 = camera_params[,"PlanckR1"]
  # PB = camera_params[,"PlanckB"]
  # PF = camera_params[,"PlanckF"]
  # PO = camera_params[,"PlanckO"]
  # PR2 = camera_params[,"PlanckR2"]
  
  
  # Apply to every element of the raw data list, converting raw data into
  # temperature using parameters from the metadata
  
  # temp_dat <- mapply(FUN = Thermimage::raw2temp,
  #                    raw = raw_dat,
  #                    E,
  #                    OD,
  #                    RTemp,
  #                    ATemp,
  #                    IRWTemp,
  #                    IRT,
  #                    RH,
  #                    PR1,
  #                    PB,
  #                    PF,
  #                    PO,
  #                    PR2,
  #                    SIMPLIFY = FALSE)
  
  photo_no <- names(raw_dat)
  # Create empty list to populate with temperature matrices
  temp_dat <- vector("list", length(photo_no))
  names(temp_dat) <- photo_no
  
  for (i in 1: length(raw_dat)){
    temp_dat_i <- Thermimage::raw2temp(raw = raw_dat[[i]], 
                                       E = camera_params[[i]][["Emissivity"]], 
                                       RTemp = camera_params[[i]][["ReflectedApparentTemperature"]],
                                       ATemp = camera_params[[i]][["AtmosphericTemperature"]],     # atmospheric temperature measured in the field
                                       # Calibration constants from 'batch_extract'
                                       PR1 = camera_params[[i]][["PlanckR1"]],
                                       PB = camera_params[[i]][["PlanckB"]],
                                       PF = camera_params[[i]][["PlanckF"]],
                                       PO = camera_params[[i]][["PlanckO"]],
                                       PR2 = camera_params[[i]][["PlanckR2"]], 
                                       ATA1 = camera_params[[i]][["AtmosphericTransAlpha1"]],         # Atmospheric Transmittance Alpha 1
                                       ATA2 = camera_params[[i]][["AtmosphericTransAlpha2"]],       # Atmospheric Transmittance Alpha 2
                                       ATB1 = camera_params[[i]][["AtmosphericTransBeta1"]],          # Atmospheric Transmittance Beta 1
                                       ATB2 = camera_params[[i]][["AtmosphericTransBeta2"]],         # Atmospheric Transmittance Beta 2
                                       ATX = camera_params[[i]][["AtmosphericTransX"]],             # Atmospheric Transmittance X
                                       OD =  camera_params[[i]][["ObjectDistance"]],                # object distance in metres
                                       # FD = camera_params[[i]][["FocusDistance"]],                 # focus distance in metres
                                       # ReflT = camera_params[[i]][["ReflectedApparentTemperature"]],  # Reflected apparent temperature
                                       # AtmosT = camera_params[[i]][["AtmosphericTemperature"]],        # Atmospheric temperature
                                       IRWTemp = camera_params[[i]][["IRWindowTemperature"]],           # IR Window Temperature
                                       IRT = camera_params[[i]][["IRWindowTransmission"]],          # IR Window transparency
                                       RH = camera_params[[i]][["RelativeHumidity"]]              # Relative Humidity
                                       # h = camera_params[[i]][["RawThermalImageHeight"]],         # sensor height (i.e. image height)
                                       # w = camera_params[[i]][["RawThermalImageWidth"]]          # sensor width (i.e. image width)
                                       )
    temp_dat[[i]] <- temp_dat_i
  }
  
  # Ensure correct element names
  names(temp_dat) <- names(raw_dat)
  
  # Write -------------------------------------------------------------------
  if(write_results){
    
    if(is.null(out_dir)) out_dir <- getwd()
    if(is.null(file_name)) file_name <- paste("flir_temp_", Sys.Date(),sep="")
    
    out_path <- file.path(out_dir, paste(file_name, ".Rds", sep = ""))
    saveRDS(temp_dat,file = out_path)
  }
  return(temp_dat)
  
}


## batch processing for cropping the temp images
batch_crop <- function(temp_dat, out_dir, row_start, row_end, col_start, col_end) {
  
  # Create empty list to populate with temperature matrices
  temp_cropped <- vector("list", length(temp_dat))
  names(temp_cropped) <- names(temp_dat)
  
  # batch crop
  for(i in 1:length(temp_dat)){
    if(length(temp_dat[[i]]) <= 0 ){
      temp_cropped_i <- NULL
    } else { 
      temp_cropped_i <- temp_dat[[i]][row_start:row_end, col_start:col_end]
    }
    temp_cropped[[i]] <- temp_cropped_i
  }  
  return(temp_cropped)
  
  # save cropped temp data
  out_path <- file.path(out_dir, "FLIR_cropped.Rds")
  saveRDS(temp_cropped,file = out_path)
}

calc_stats_flir <- function(temp_dat){
  
}