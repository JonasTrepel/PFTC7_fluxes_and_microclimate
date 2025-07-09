### Vizualize fluxes
library(tidyverse)
library(data.table)
library(ggridges)
library(RColorBrewer)
library(gridExtra)
library(GGally)
library(viridis)

# Download clean data
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "pftc7_ecosystem_fluxes_south_africa_2023.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

dt <- fread("clean_data/pftc7_ecosystem_fluxes_south_africa_2023.csv") |>
  # Mutate factor for elevation
  mutate(elevation_m_asl = factor(elevation_m_asl,
                                  levels = c("2000", "2200", "2400", "2600", "2800")),
         # Separate fluxes into measured and calculated
         flux_method = case_when(
           flux_type %in% c("cue",   "gpp", "npp",
                            "wue", "transpiration") ~ "Calculated",
           flux_type %in% c("evapotrans", "nee", "resp_day", "resp_night",
                            "evap_day", "evap_night",
                            "soil_resp", "soil_evap") ~ "Measured"
         )
         )

# Summarise flux measurements
dt |>
  group_by(flux_category, flux_method) |>
  summarize(n = length(flux_value))

1# Visualise
palette_elevation = colorRampPalette(colors = c("#FDCB26", "#EF7F4F", "#DD5E66",
                                                 "#A72197", "#6F00A8"))(5)

#Fluxes  along elevation
c_ele <- dt %>%
  filter(flux_category == "Carbon") %>%
  mutate(Elevation = as.factor(elevation_m_asl)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_density_ridges(aes(x = flux_value, y = Elevation, fill = Elevation), alpha = .7) +
  scale_fill_manual(values = palette_elevation) +
  facet_wrap(~clean_flux_type, scales = "free_x", ncol = 4) +
  theme_bw() +
  labs(x= "Flux Value", title = "a)") +
  theme(legend.position = "none",
        panel.grid = element_blank())
c_ele

w_ele <- dt %>%
  filter(flux_category == "Water") %>%
  mutate(Elevation = as.factor(elevation_m_asl)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_density_ridges(aes(x = flux_value, y = Elevation, fill = Elevation), alpha = .7) +
  scale_fill_manual(values = palette_elevation) +
  facet_wrap(~clean_flux_type, scales = "free_x", ncol = 4) +
  theme_bw() +
  labs(x= "Flux Value", title = "b)") +
  theme(legend.position = "none",
        panel.grid = element_blank())
w_ele

#Fluxes  aspects
c_asp <- dt %>%
  filter(flux_category == "Carbon") %>%
  mutate(Elevation = as.factor(elevation_m_asl)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_density_ridges(aes(x = flux_value, y = aspect, fill = aspect), alpha = .7) +
  scale_fill_manual(values = c("#FF9000", "#0000f8")) +
  facet_wrap(~clean_flux_type, scales = "free_x", ncol = 4) +
  theme_bw() +
  labs(x= "Flux Value", y = "Aspect", title = "a)") +
  theme(legend.position = "none",
        panel.grid = element_blank())
c_asp

w_asp <- dt %>%
  filter(flux_category == "Water") %>%
  mutate(Elevation = as.factor(elevation_m_asl)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_density_ridges(aes(x = flux_value, y = aspect, fill = aspect), alpha = .7) +
  scale_fill_manual(values = c("#FF9000", "#0000f8")) +
  facet_wrap(~clean_flux_type, scales = "free_x", ncol = 4) +
  theme_bw() +
  labs(x= "Flux Value", y = "Aspect", title = "b)") +
  theme(legend.position = "none",
        panel.grid = element_blank())
w_asp

### combine plots
p_ele <- grid.arrange(c_ele, w_ele, heights = c(1, 1))

p_asp <- grid.arrange(c_asp, w_asp, heights = c(1, 1))


ggsave(plot = p_ele, "builds/plots/elevation_fluxes.png", dpi = 600, height = 9, width = 9)
ggsave(plot = p_asp, "builds/plots/aspect_fluxes.png", dpi = 600, height = 9, width = 9)

# Alternative using patchwork
library(patchwork)
c_asp / w_asp +
  plot_layout(axes = "collect")

ggsave("outputs/aspect_fluxes.png", dpi = 600, height = 9, width = 9)

table(dt$flux_type)

dt_corr <- dt %>%
  dplyr::select(-c("date", "date_time", "day_night", "site_id", "flux_type", "flux_category", "aspect", "plot_id", "r_squared", "flag", "device")) %>%
  pivot_wider(names_from = clean_flux_type, values_from = flux_value) %>%
  rename(Elevation = elevation_m_asl) %>%
  dplyr::select(-unique_location_id) %>%
  filter(complete.cases(.))

names(dt_corr)

library(ggcorrplot)

corr <- round(cor(dt_corr), 2)
p_corr <- ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
p_corr
ggsave(plot = p_corr, "builds/plots/flux_correlations.png", dpi = 600, height = 10, width = 10)


dt %>%
  dplyr::select(-c("site_id", "clean_flux_type", "flux_category", "plot_id", "r_squared", "flag", "device")) %>%
  pivot_wider(names_from = flux_type, values_from = flux_value) %>%
  filter(aspect == "east") %>%
  dplyr::select(evap_day) %>%
  filter(complete.cases(.)) %>%
  pull() %>%
  sd()

unique(dt$flag)

