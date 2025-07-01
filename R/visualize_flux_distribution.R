### Vizualize fluxes 


library(tidyverse)
library(data.table)
library(ggridges)
library(RColorBrewer)
library(gridExtra)
library(GGally)


dt <- fread("data/cleanData/pftc7_ecosystem_fluxes_south_africa_2023.csv") 

#Fluxes  along elevation 
c_ele <- dt %>% 
  filter(flux_category == "Carbon") %>%
  mutate(Elevation = as.factor(elevation_m_asl)) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_density_ridges(aes(x = flux_value, y = Elevation, fill = Elevation), alpha = .9) +
  scale_fill_brewer(palette = "Greens") +
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
  geom_density_ridges(aes(x = flux_value, y = Elevation, fill = Elevation), alpha = .9) +
  scale_fill_brewer(palette = "Greens") +
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
  geom_density_ridges(aes(x = flux_value, y = aspect, fill = aspect), alpha = .9) +
  scale_fill_manual(values = c("gold1", "darkblue")) +
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
  geom_density_ridges(aes(x = flux_value, y = aspect, fill = aspect), alpha = .9) +
  scale_fill_manual(values = c("gold1", "darkblue")) +
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

table(dt$flux_type)

dt_corr <- dt %>% 
  dplyr::select(-c("site_id", "flux_type", "flux_category", "aspect", "plot_id", "r_squared", "flag", "device")) %>% 
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
