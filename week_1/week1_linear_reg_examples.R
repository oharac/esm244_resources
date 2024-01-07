library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)

### run this on Mazu...
here_anx <- function(f = '', ...) { 
  ### create file path to git-annex dir for project
  f <- paste(f, ..., sep = '/')
  f <- stringr::str_replace_all(f, '\\/+', '/')
  f_anx <- sprintf('/home/shares/ohi/spp_vuln/spp_vuln_mapping/%s', f)
  return(f_anx)
}

spp_cells <- data.table::fread(here_anx('../aquamaps_2021/hcaf_species_native_clean.csv'))
hcaf <- data.table::fread(here_anx('../aquamaps_2021/hcaf_v7.csv')) %>%
  janitor::clean_names()

n_hemi <- hcaf %>% 
  filter(center_lat > 0) %>% 
  filter(!is.na(eez)) %>%
  filter(depth_min <= 200) %>%
  filter(center_lat >= 15 & center_lat <= 46) %>%
  filter(center_long <=  -60) %>%
  filter(ocean_basin %in% c('22000')) %>%
  mutate(ocean = ifelse(center_lat < 31, 'Caribbean', 'W Atlantic')) %>%
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
  ungroup() %>%
  mutate(northsouth = ifelse(ocean == 'Caribbean', center_lat - 15, center_lat - 31))

spp_rich_r <- spp_rich_basins %>%
  select(x = center_long, y = center_lat, z = n_spp) %>%
  rast()

### plot spp richness map
land <- ne_countries(returnclass = 'sf')

ggplot() +
  geom_sf(data = land) +
  scale_x_continuous(limits = c(-100, -60)) +
  scale_y_continuous(limits = c(15, 45)) +
  geom_raster(data = spp_rich_basins, aes(x = center_long, y = center_lat, fill = n_spp)) +
  geom_hline(yintercept = 31, color = 'blue') +
  annotate('text', label = 'Caribbean', y = 30, x = -75, vjust = 1, hjust = 0) +
  annotate('text', label = 'W. Atlantic', y = 32, x = -75, vjust = 0, hjust = 0) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = 'Species\nRichness')

ggplot(spp_rich_basins, aes(x = center_lat, y = n_spp)) +
  geom_point() +
  geom_vline(xintercept = 31, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkgreen') +
  theme_minimal() +
  labs(x = 'Latitude (deg N)', y = 'Species richness')

naive_lm <- lm(n_spp ~ center_lat, data = spp_rich_basins)

c_lm <- lm(n_spp ~ northsouth, data = spp_rich_basins %>% filter(ocean == 'Caribbean'))
a_lm <- lm(n_spp ~ northsouth, data = spp_rich_basins %>% filter(ocean == 'W Atlantic'))

both_lm <- lm(n_spp ~ northsouth + ocean, data = spp_rich_basins)

summary(both_lm)

vir_pal <- hcl.colors(5)[c(1, 4)]
ggplot() +
  geom_point(data = spp_rich_basins, aes(x = northsouth, y = n_spp, color = ocean)) +
  geom_abline(intercept = both_lm$coefficients[1], 
              slope = both_lm$coefficients[2],
              color = vir_pal[1]) +
  geom_abline(intercept = both_lm$coefficients[1] + both_lm$coefficients[3], 
              slope = both_lm$coefficients[2],
              color = vir_pal[2], linetype = 'dashed') +
  theme_minimal() +
  scale_color_manual(values = vir_pal) +
  labs(x = 'Degrees north from southern edge', y = 'Species richness', color = 'Region')


