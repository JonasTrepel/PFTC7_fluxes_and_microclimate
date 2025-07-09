# Make clean FLIR data file for OSF

source("getFLIRnames.R")

# Flatten Rds to data frame ----
FLIRraw = readRDS("outputs/FLIR_cropped.Rds") |>
  lapply(FUN = as.numeric)

# Code by Brian Maitner
flir_long <- NULL
for(i in 1:length(FLIRraw)){
  if(length(as.numeric(FLIRraw[[i]]))==0){next}
  data.frame(file_number=names(FLIRraw)[i],
             value = as.numeric(FLIRraw[[i]]))%>%
    bind_rows(flir_long) -> flir_long
}

# Add in meta data
FLIRmeta = read.csv("PFTC7_SA_FLIR_2023.csv")
FLIRmeta = getFLIRnames(data = FLIRmeta) |>
  mutate(file_number = as.character(file_number))

FLIRflat = flir_long |>
  left_join(FLIRmeta) |>
  # Filter out obs without file number
  filter(file_number != "") |>
  # Rename temp col
  rename(temp_C = value) |>
  # Replace missing values
  mutate(time = case_when(
    time == "" ~ NA,
    TRUE ~ time
  ),
  day..NOT.DATE.... = case_when(
    is.na(day..NOT.DATE....) & siteID == 1 ~ 14,
    TRUE ~ day..NOT.DATE....
  )
  )

saveRDS(FLIRflat, "outputs/flir_values.Rds")
write.csv(FLIRflat, "outputs/flir_values.csv", row.names = FALSE)

# make trial plot with basic FLIR
ggplot(FLIRflat |> filter(temp_C > 0) |> drop_na(siteID),
       aes(x = temp_C, y = as.factor(id), fill = id)) +
  # geom_density_ridges(alpha = 0.5) +
  # theme_ridges() +
  facet_grid(~day.night, scales = "free_x")
  # theme(legend.position = "none")

