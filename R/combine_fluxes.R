##### Combine different flux files to final data set 

library(data.table)
library(tidyverse)

## load datetime

dt_date <- fread("data/dataFragments/licor7500_datetimes.csv")



### load carbon fluxes  
dt_c_raw <- fread("data/dataFragments/licor_nee_flagged.csv") %>% 
  mutate(file = gsub("raw_data/LI7500/All_sites/", "", filename)) %>% 
  left_join(dt_date)

dt_c <- dt_c_raw %>% 
  filter(flag %in% c("okay", "decreasing_NEE", "increasing_ER", "manual_flux_time_selection")) %>% 
  mutate(flux_type = case_when(
    day.night == "day" & flux == "ER" ~ "resp_day", 
    day.night == "night" & flux == "ER"~ "resp_night", 
    flux == "NEE" ~ "nee"), 
    unique_location_id = paste0(elevation, "_", aspect, "_", plot), 
    flux_value = flux_value*-1) %>% 
  group_by(unique_location_id, flux_type) %>% 
  slice_max(rsqd) %>% 
  ungroup() %>% 
  dplyr::select(site_id = site,
                elevation_m_asl = elevation, 
                aspect, 
                plot_id = plot, 
                r_squared = rsqd, 
                flag,
                flux_type,
                flux_value, 
                unique_location_id, 
                date_time, 
                date, 
                day_night = day.night)
  
### load water fluxes 
dt_w_raw <- fread("data/dataFragments/licor_et_flagged.csv") %>% 
  mutate(file = gsub("raw_data/LI7500/All_sites/", "", filename)) %>% 
  left_join(dt_date)

dt_w <- dt_w_raw %>% 
  filter(flag %in% c("okay", "manual_flux_time_selection")) %>% 
  mutate(flux_type = case_when(
    day.night == "day" & flux == "EVAP" ~ "evap_day", 
    day.night == "night" & flux == "EVAP"~ "evap_night", 
    flux == "ET" ~ "evapotrans"), 
    unique_location_id = paste0(elevation, "_", aspect, "_", plot)) %>% 
  group_by(unique_location_id, flux_type) %>% 
  slice_max(lm_rsqd) %>% 
  ungroup() %>% 
  dplyr::select(site_id = site,
                elevation_m_asl = elevation, 
                aspect, 
                plot_id = plot, 
                r_squared = lm_rsqd, 
                flag,
                flux_type,
                flux_value, 
                unique_location_id, 
                date_time, 
                date, 
                day_night = day.night)

### load soil resp 

dt_sr_raw <- fread("data/dataFragments/soil_respiration_pftc7.csv")

dt_sr <- dt_sr_raw %>% 
  mutate(flux_type = case_when(
    fluxType == "SoilEvap" ~ "soil_evap", 
    fluxType == "SoilResp" ~ "soil_resp"), 
    unique_location_id = paste0(elevation, "_", aspect, "_", plot), 
    flag = NA) %>% 
  dplyr::select(site_id = site,
                elevation_m_asl = elevation, 
                aspect, 
                plot_id = plot, 
                r_squared = Rsq, 
                flag,
                flux_type,
                flux_value = fluxValue, 
                unique_location_id, 
                date, 
                date_time, 
                day_night)

### calculate additional metrics  

dt_calc <- rbind(dt_c, dt_w, dt_sr) %>% 
  dplyr::select(unique_location_id, flux_type, flux_value,
                aspect, elevation_m_asl, plot_id, site_id,
                date, date_time, day_night) %>% 
  pivot_wider(names_from = flux_type, values_from = flux_value) %>% 
  mutate(gpp = resp_day - nee, 
         npp = gpp - (resp_day - soil_resp), 
         cue = npp/gpp, 
         transpiration = evapotrans - evap_day, 
         wue = transpiration/npp, 
         r_squared = NA, 
         flag = NA) %>% 
  dplyr::select(unique_location_id, aspect, elevation_m_asl,
                plot_id, site_id, r_squared, flag, 
                date, date_time, day_night,
                gpp, npp, cue, transpiration, wue) %>% 
  pivot_longer(cols = c("gpp", "npp", "cue", "transpiration", "wue"), 
               names_to = "flux_type", values_to = "flux_value")
  
### combine all 

dt_comb <- rbind(dt_c, dt_w, dt_sr, dt_calc) %>% filter(!is.na(flux_value)) %>% 
  mutate(clean_flux_type = case_when(
    flux_type == "nee" ~ "Net Ecosystem Exchange\n(µmol/m²/s)", 
    flux_type == "resp_day" ~ "Ecosystem Respiration\n(µmol/m²/s; Day)", 
    flux_type == "resp_night" ~ "Ecosystem Respiration\n(µmol/m²/s; Night)", 
    flux_type == "evap_day" ~ "Evaporation\n(mmol/m²/s; Day)", 
    flux_type == "evap_night" ~ "Evaporation\n(mmol/m²/s; Night)", 
    flux_type == "evapotrans" ~ "Evapotranspiration\n(mmol/m²/s)", 
    flux_type == "soil_evap" ~ "Soil Evaporation\n(mmol/m²/s)", 
    flux_type == "soil_resp" ~ "Soil Respiration\n(µmol/m²/s)", 
    flux_type == "gpp" ~ "Gross Primary Productivity\n(µmol/m²/s)", 
    flux_type == "npp" ~ "Net Primary Productivity\n(µmol/m²/s)", 
    flux_type == "transpiration" ~ "Transpiration\n(mmol/m²/s)", 
    flux_type == "wue" ~ "Water Use Efficiency\n(Transpiration/NPP)", 
    flux_type == "cue" ~ "Carbon Use Efficiency\n(GPP/NPP)"), 
    flux_category = case_when(
      flux_type %in% c("nee", "resp_day", "resp_night", "soil_resp", "gpp", "npp", "cue") ~ "Carbon",
      flux_type %in% c("evap_day", "evap_night", "evapotrans", "soil_evap", "transpiration", "wue") ~ "Water"
    ), 
    device = case_when(
      .default = "calculated",
      flux_type %in% c("soil_resp", "soil_evap") ~ "LI-8100", 
      flux_type %in% c("nee", "resp_day", "resp_night", "evap_day", "evap_night", "evapotrans") ~ "LI-7500")) %>%
  relocate(date_time, date, unique_location_id,
           aspect, site_id, elevation_m_asl, plot_id, day_night,
           flux_type, clean_flux_type, flux_value, 
           flux_category, r_squared, device, flag)

summary(dt_comb)

fwrite(dt_comb, "data/cleanData/x_PFTC7_clean_ecosystem_fluxes_2023.csv")
