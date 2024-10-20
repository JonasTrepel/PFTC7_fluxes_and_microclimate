## Function to fix corrupted file names 
## adapted from Pekka Niittynen

fixFileNames <- function(path){
  files <- list.files(path, full.names = T, recursive = T)
  
  invisible(lapply(files, function(x){
    if(grepl("-",basename(x))){
      file.rename(x,
                  paste0(strsplit(x,"-")[[1]][1], ".txt"))
    }
    
  }))
  
  invisible(lapply(files, function(x){
    if(grepl(" ",basename(x))){
      file.rename(x,
                  paste0(strsplit(x," ")[[1]][1], ".txt"))
    }
    
  }))
  
  return(files)
  
}
