library(ggh4x)
library(viridis)

microclimate = read.csv("clean_data/PFTC7_microclimate_south_africa_2023.csv") |>
  mutate(elevation_m_asl = factor(elevation_m_asl, levels = c("3000", "2800", "2600", "2400", "2200", "2000")),
         climate_variable = factor(climate_variable,
                                   levels = c("temperature_air", "temperature_leaf", "temperature_near_surface",
                                              "temperature_ground", "temperature_soil", "moisture_soil"),
                                   labels = c("Air TºC", "Leaf TºC", "Near surface TºC", "Ground TºC", "Soil TºC", "Soil moisture %")))
  ggplot(microclimate |> drop_na(value),
         aes(x = value, y = aspect, fill = aspect)) +
  geom_density_ridges(alpha = .7) +
  scale_fill_manual(values = c("#FF9000", "#0000f8")) +
    facet_nested(elevation_m_asl ~ device + climate_variable, scales = "free", independent = "y",
                 nest_line = element_line(linetype = 2)) +
  theme_bw() +
  labs(x= "Microclimate Value", y = "Aspect") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        text=element_text(size=15))

ggsave(paste0("outputs/", Sys.Date(), "_dataPaper_microclimate.png"),
       width = 14, height = 10, units = "in")
