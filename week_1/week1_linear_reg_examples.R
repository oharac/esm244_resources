library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)

### run this on Mazu...
here_anx <- function(f = '', ...) { 
  ### create file path to git-annex dir for project
  f <- paste(f, ..., sep = '/')
  f <- stringr::str_replace_all(f, '\\/+', '/')
  f_anx <- sprintf('/home/shares/ohi/spp_vuln/aquamaps_2021/%s', f)
  return(f_anx)
}

spp_cells <- data.table::fread(here_anx('hcaf_species_native_clean.csv'))
hcaf <- data.table::fread(here_anx('hcaf_v7.csv')) %>%
  janitor::clean_names()

n_hemi <- hcaf %>% 
  filter(!is.na(eez)) %>%
  filter(depth_min <= 200) %>%
  filter(center_lat >= 15 & center_lat <= 45) %>%
  filter(center_long <=  -60 & center_long >= -130) %>%
  mutate(ocean = ifelse(ocean_basin == '22000','W Atlantic', 'E Pacific')) %>%
  select(center_lat, center_long, ocean, loiczid)

set.seed(42)
n_hemi_sample <- n_hemi %>% 
  sample_n(200)

spp_cells_basins <- n_hemi_sample %>%
  oharac::dt_join(spp_cells, by = 'loiczid', type = 'left')

### summarize species richness
spp_rich_basins <- spp_cells_basins %>%
  filter(prob > 0.50) %>%
  group_by(loiczid, center_long, center_lat, ocean) %>%
  summarize(n_spp = n_distinct(am_sid)) %>%
  ungroup()

spp_rich_r <- spp_rich_basins %>%
  select(x = center_long, y = center_lat, z = n_spp) %>%
  rast()

### plot spp richness map
land <- ne_countries(returnclass = 'sf')

ggplot() +
  geom_sf(data = land) +
  scale_x_continuous(limits = c(-130, -60)) +
  scale_y_continuous(limits = c(15, 45)) +
  geom_raster(data = spp_rich_basins, aes(x = center_long, y = center_lat, fill = n_spp)) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = 'Species\nRichness')

ggplot(spp_rich_basins, aes(x = center_lat, y = n_spp)) +
  geom_point() +
  theme_minimal() +
  labs(x = 'Latitude (deg N)', y = 'Species richness')

ggplot(spp_rich_basins, aes(x = center_lat, y = n_spp)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkgreen') +
  theme_minimal() +
  labs(x = 'Latitude (deg N)', y = 'Species richness')

naive_lm <- lm(n_spp ~ center_lat, data = spp_rich_basins)

p_lm <- lm(n_spp ~ center_lat, data = spp_rich_basins %>% filter(ocean == 'E Pacific'))
a_lm <- lm(n_spp ~ center_lat, data = spp_rich_basins %>% filter(ocean == 'W Atlantic'))

both_lm <- lm(n_spp ~ center_lat + ocean, data = spp_rich_basins)

summary(both_lm)

vir_pal <- hcl.colors(5)[c(1, 4)]
ggplot() +
  geom_point(data = spp_rich_basins, aes(x = center_lat, y = n_spp, color = ocean)) +
  geom_abline(intercept = both_lm$coefficients[1], 
              slope = both_lm$coefficients[2],
              color = vir_pal[1]) +
  geom_abline(intercept = both_lm$coefficients[1] + both_lm$coefficients[3], 
              slope = both_lm$coefficients[2],
              color = vir_pal[2]) +
  theme_minimal() +
  scale_color_manual(values = vir_pal) +
  labs(x = 'Latitude (deg N)', y = 'Species richness', color = 'Region')


