library(tidyverse)
library(sf)
library(leaflet)

states = USAboundaries::us_states() %>%
  st_transform(5070)

state.of.interest = "Tennessee"

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches )

closest = st_make_grid(soi, n = 70, square = TRUE) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)
# Check on this
vor = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  st_sf()

#CHeck
leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = st_transform(vor, 4326))

radius = 1, color = ~colorFactor("YlOrRd", state_name)(state_name))




tmp = states %>%
  filter(grepl("n", state_name))

plot(tmp$geometry, col = "red")
