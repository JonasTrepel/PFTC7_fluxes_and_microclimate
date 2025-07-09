
require(dplyr)
require(lubridate)

join_PAR_LICOR <- function(PAR = NULL, LICOR, PAR_tz = "UTC", 
                           LICOR_tz = "UTC"){
  
    
  if(!is.null(PAR)){
    LICOR$POSIXct <- as.POSIXct(paste(LICOR$Date, LICOR$Time), 
                                format="%Y-%m-%d %H:%M:%S", 
                                tz = LICOR_tz)
    PAR$POSIXct_uc <- as.POSIXct(paste(PAR$Date, PAR$Time),
                                  format="%Y-%m-%d %H:%M:%S", 
                                  tz = PAR_tz)
    common_time_zone = LICOR_tz
    
    PAR$POSIXct <- with_tz(PAR$POSIXct_uc, tzone = common_time_zone)
    PAR_LICOR <- inner_join(PAR, LICOR, by="POSIXct")
  }
  else{
    LICOR$POSIXct <- as.POSIXct(paste(LICOR$Date, LICOR$Time), 
                                format="%Y-%m-%d %H:%M:%S", 
                                tz = LICOR_tz)
    PAR_LICOR <-LICOR
  }
  
  
  return(PAR_LICOR)
}
