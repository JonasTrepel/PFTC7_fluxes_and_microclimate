Readme
================
PFTC7 Group 3
2025-02-19

# Data dictionnaries

## CO<sub>2</sub> fluxes

| Variable.name | Description | Variable type | Variable range or levels | Units | How.measured |
|:---|:---|:---|:---|:---|:---|
| date_time | Date and time of observation (if known) | categorical | 2023-12-03T11:08:28Z - 2023-12-16T21:00:27Z | yyyy-mm-dd hh:mm:ss | defined |
| date | Date of observation | categorical | 2023-12-03 - 2023-12-16 | yyyy-mm-dd | defined |
| unique_location_id | Concatenated SiteID, elevation, aspect, and PlotID | categorical | 2000_east_1 - 2800_west_5 |  | defined |
| site_id | Site number | numeric | 1 - 5 |  | defined |
| elevation_m_asl | Site elevation | numeric | 2000 - 2800 | m asl | defined |
| aspect | Transect aspect | categorical | east - west |  | defined |
| plot_id | Number of plot on transect | numeric | 1 - 5 |  | defined |
| day_night | Whether the measurement was performed as part of a daytime or nighttime flux campaign | categorical | day - night |  | defined |
| device | What equipment was used to measure the microclimate value | categorical | calculated - LI-8100 |  | defined |
| flux_type | Which flux was measured or calculated, including cue (carbon-use efficiency), evap_day (daytime evaporation), evap_night (nighttime evaporation), evapotrans (evapotranspiration), gpp (gross primary productivity), nee (net ecosystem exchange), npp (net primary productivity), resp_day (daytime ecosystem respiration), resp_night (nighttime ecosystem respiration), soil_evap (soil evaporation), soil_resp (soil respiration), transpiration (ecosystem transpiration), wue (water-use efficiency) | categorical | cue - wue |  | defined |
| clean_flux_type | Full name and units of the flux | categorical | Carbon Use Efficiency |  |  |

(GPP/NPP) - Water Use Efficiency (Transpiration/NPP) \| \|defined \|
\|flux_value \|Flux readings \|numeric \|-17.879 - 28.546
\|micromol/m\_/s or GPP/NPP (CUE) or Transpiration/NPP (WUE) \|recorded
\| \|flux_category \|Carbon or water fluxes \|categorical \|Carbon -
Water \| \|defined \| \|r_squared \|Model fit \|numeric \|0 - 0.999 \|
\|recorded \| \|flag \|Quality flag \|categorical
\|manual_flux_time_selection - okay \| \|defined \|

## Microclimate

| Variable.name | Description | Variable type | Variable range or levels | Units | How.measured |
|:---|:---|:---|:---|:---|:---|
| elevation_m_asl | Site elevation | numeric | 2000 - 3000 | m asl | defined |
| treat_1 | ambient = ambient conditions, warm = plot experiencing warmed by open top chamber (OTC) | categorical | ambient - warm |  | defined |
| treat_2 | Plots contain intact vegetation. | categorical | vegetation - vegetation |  | defined |
| aspect | Transect aspect | categorical | east - west |  | defined |
| device | What equipment was used to measure the microclimate value | categorical | FLIR - Tomst |  | defined |
| climate_variable | Microclimate variable including moisture_soil, temperature_air, temperature_ground, temperature_leaf, temperature_near_surface, and temperature_soil | categorical | moisture_soil - temperature_soil |  | defined |
| value | Temperature or moisture reading discarding values later flagged as suspect | numeric | 0.029 - 72.944 | degrees C, (m3 water \_ m_3 soil) \_ 100 | recorded |

# Data quality flags

| Flag | Replaced flux | Explanation | Recommended action | \# of CO2/H2O fluxes |
|----|----|----|----|----|
| decreasing_NEE | \- | NEE decreased during the flux measurement (often followed by a redo) | keep | 4/0 |
| discard_this_keep_other_reading | NA | The less reliable of a pair of readings (usually with a redo) | discard | 6/22 |
| high_aic_discard_this_keep_other_reading | NA | The higher AIC value of a flux that was split into multiple pieces by the break points function. | discard | 16/4 |
| increasing_ER | NA | ER increased during the flux measurement (often followed by a redo | discard | 2/0 |
| manual_flux_time_selection | – | The break points function failed to select the most biologically useful portion of the flux. A new time was manually chosen. | keep | 6/34 |
| okay | – | – | keep | 140/114 |
| suspicious | NA | The changes over time do not have a reasonable explanation. | discard | 1/4 |
