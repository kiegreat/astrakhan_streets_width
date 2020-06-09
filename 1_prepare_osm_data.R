
library(tidyverse)
library(sf)

# 1. Load osm layers

places <- read_sf('data/osm/gis_osm_places_a_free_1.shp')
roads <- read_sf('data/osm/gis_osm_roads_free_1.shp')

# 2. Clip data

astra <- places %>% filter(name == 'Астрахань')
astra_roads <- roads %>% st_intersection(astra) %>% filter(!(fclass == 'footwat'), !(is.na(name)))

# 3. Check data

ggplot() +
  geom_sf(data = astra, alpha = 0.3, inherit.aes = FALSE) +
  geom_sf(data = astra_roads, inherit.aes = FALSE)

# 4. Save for further use

saveRDS(astra_roads, 'data/astra_roads.rds')
