#This function takes a column which contains the numbers of pictures of a flir camera and transform them to proper file names
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
    separate(file_code, c("name1", "name2", "name3", "name4", "name5"), sep = ";" ) %>%
    
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
           end = as.numeric(end)) %>%
    
    # Perform row-wise operations to create a new column 'sequence' using the 'create_sequence' function
    drop_na(start) %>%
    drop_na(end) %>%
    filter(end > start) %>%
    rowwise() %>%
    mutate(sequence = create_sequence(start, end)) %>%
    
    # Separate the 'sequence' column into multiple columns using ',' as the separator
    separate(sequence, c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8"), sep = "," )  %>%
    
    # Reshape the data again from wide to long format, keeping 'file_number' as a new column
    pivot_longer(cols = name1:name8, names_to = "delete", values_to = "file_number") %>%
    
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
