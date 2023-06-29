## code to prepare `DATASET` dataset goes here


###### 0. Read in shape file data  #####

library(sf)
library(tidyverse)
brasil_sf <- geobr::read_state() %>%
  rename(state_code = code_state,
         state_abb = abbrev_state,
         state = name_state,
         region_code = code_region,
         region = name_region,
         geometry = geom)# year 2010

### save as is if desired #####
usethis::use_data(brasil_sf, overwrite = TRUE)


#### 1, create polygon reference dataframe w xmin, ymin, xmax and ymax and save
reference_full <- brasil_sf  %>%
  ggnc::create_geometries_reference(
                            id_cols = c(state_code, state_abb,
                                        state, region_code, region))

usethis::use_data(reference_full, overwrite = TRUE)


####### 2. create and save flat file for examples, if desired ####

brasil_sf %>%
  sf::st_drop_geometry() ->
brasil_flat

usethis::use_data(brasil_flat, overwrite = TRUE)

############### 3. create polygon centers and labels reference data frame

# county centers for labeling polygons
library(sf)

brasil_state_centers <- brasil_sf |>
  ggnc::prepare_polygon_labeling_data(id_cols = c(state_code, state_abb,
                                                  state, region_code, region))


usethis::use_data(brasil_state_centers, overwrite = TRUE)
