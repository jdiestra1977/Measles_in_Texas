library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(tidyverse)

setwd("~/Documents/GitHub/Measles_in_Texas/")

### Getting dictionaries for data: cbgs to zcta, and zcta to county-----

# Get ACS ZCTA-level population for Texas
tx_zcta_pop <- get_acs(
  geography = "zcta",
  variables = "B01003_001",  # Total population
  state = "TX",
  year = 2019,
  survey = "acs5",
  geometry = TRUE             # Get spatial data
)

tx_county_pop <- get_acs(
  geography = "zcta",
  variables = "B01003_001",  # Total population
  state = "TX",
  year = 2019,
  survey = "acs5",
  geometry = TRUE             # Get spatial data
)

# Get Texas county boundaries
tx_counties <- counties(state = "TX", year = 2019, cb = TRUE)
zctas(state = "TX", year = 2010, cb = FALSE)
# Perform spatial join: Assign each ZCTA to a county
zcta_county_join <- st_join(tx_zcta_pop, tx_counties, join = st_intersects)

zcta_county_final <- zcta_county_join %>%
  group_by(GEOID.x) %>%
  slice_max(estimate)  # Keep the county with the largest population share

ggplot(data = zcta_county_join) + geom_sf()

ggplot(data = tx_zcta_pop) + geom_sf()

ggplot() + theme_void() +
  geom_sf(data = tx_counties,color="blue",fill=NA) + 
  geom_sf(data = tx_zcta_pop,color="red",fill=NA)

##
tx_zctas<-tx_zcta_pop
# Ensure both datasets use the same CRS
tx_zctas <- st_transform(tx_zctas, st_crs(tx_counties))

# Perform spatial intersection: Clip ZCTAs to county boundaries
zcta_county_intersection <- st_intersection(tx_zctas, tx_counties)

# Compute area of each ZCTA portion inside a county (in square kilometers)
zcta_county_intersection <- zcta_county_intersection %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1e6)  # Convert m² to km²

# Create a dataframe with ZCTA and county relationships
zcta_county_list <- zcta_county_intersection %>% as_tibble() %>%
  select(ZCTA = GEOID, County_FIPS = GEOID.1, County_Name = NAME.1,area_sqkm) %>%
  distinct()  # Remove duplicates

#This final list contains ZCTAs that are assigned to Counties according to area
#whithin the county
zcta_county_list_final<-zcta_county_list %>% arrange(ZCTA) %>%
  group_by(ZCTA) %>% slice_max(area_sqkm)

# And for data from safegraph at the cbg level
# get cbgs in each ZCTA

# Get Texas Census Block Groups (CBGs) geometries
tx_cbgs <- block_groups(state = "TX", year = 2019, cb = TRUE, class = "sf")

# Ensure both datasets use the same CRS
tx_zctas <- st_transform(tx_zctas, st_crs(tx_cbgs))

# Perform spatial intersection: Assign CBGs to ZCTAs
cbg_zcta_intersection <- st_intersection(tx_cbgs, tx_zctas)

# Ensure geometries are valid
cbg_zcta_intersection <- cbg_zcta_intersection %>%
  mutate(geometry = st_make_valid(geometry))  # Repair any invalid geometries

# Compute area of each CBG portion inside a ZCTA (in square kilometers)
cbg_zcta_intersection <- cbg_zcta_intersection %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1e6)  # Convert m² to km²

# Create a dataframe with ZCTA and county relationships
cbg_zcta_list <- cbg_zcta_intersection %>% as_tibble() %>%
  select(cbg = GEOID, ZCTA = GEOID.1,area_sqkm) %>%
  distinct()  # Remove duplicates

#This final list contains ZCTAs that are assigned to Counties according to area
#whithin the county
cbg_zcta_list_final<-cbg_zcta_list %>% arrange(cbg) %>%
  group_by(cbg) %>% slice_max(area_sqkm)

save(zcta_county_list_final,file="zcta_county_list_final.RData")
save(cbg_zcta_list_final,file="cbg_zcta_list_final.RData")

### filtering and cleaning mobility data -------

load("zcta_county_list_final.RData")
load("cbg_zcta_list_final.RData")

zcta_county_list_final

from_cbg_to_zcta_to_county<-cbg_zcta_list_final %>% select(-area_sqkm) %>%
  left_join(zcta_county_list_final %>% select(ZCTA,FIPS=County_FIPS,County=County_Name))

test_data<-read_csv("Data/Mobility/visits_cbg_to_cbg_TX_2018-01-01.csv")

test_data %>% select(Week,home_cbg,dest_cbg,totalVisits) %>%
  group_by(Week,home_cbg,dest_cbg) %>% summarise_each(sum) %>%
  mutate(home_cbg=as.factor(home_cbg),dest_cbg=as.factor(dest_cbg)) %>%
  left_join(cbg_zcta_list_final %>% select(home_cbg=cbg,zcta_origin=ZCTA)) %>%
  left_join(cbg_zcta_list_final %>% select(dest_cbg=cbg,zcta_destin=ZCTA)) %>%
  ungroup() %>% select(Week,zcta_origin,zcta_destin,totalVisits) %>%
  group_by(Week,zcta_origin,zcta_destin) %>% summarise_each(sum)

