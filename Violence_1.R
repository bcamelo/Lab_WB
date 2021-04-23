library(tidyverse)
library(sf)
library(lubridate)
library(raster)


path <- "~/Dropbox/Harris/Spring 2021/LAB WB"

acled <- read.csv(paste0(path,"/Africa_1997-2021_Apr16.csv")) %>%
  filter(COUNTRY == "Ivory Coast" |COUNTRY == "Mali" |COUNTRY == "Burkina Faso") %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

write_csv(acled, "acled.csv")

ci_regions <-  raster::getData('GADM', country = "CIV", level = 2) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

ml_regions <-  raster::getData('GADM', country = "MLI", level = 2) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

bf_regions <-  raster::getData('GADM', country = "BFA", level = 1) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

acled_ml <- acled %>%
  filter(COUNTRY == "Mali" & FATALITIES != 0)

acled_bf <- acled %>%
  filter(COUNTRY == "Burkina Faso" & FATALITIES != 0)


  ggplot() +
  geom_sf(data = ml_regions) +
  geom_sf(data = acled_ml, aes(alpha = 0.2, color = "red")) +
  labs(title = "Fatalities in Mali", color = "events", alpha = "") + 
  theme_minimal()

  
  ggplot() +
    geom_sf(data = bf_regions) +
    geom_sf(data = acled_bf, aes(alpha = 0.2, color = "red")) +
    labs(title = "Fatalities in BF", color = "events", alpha = "") + 
    theme_minimal()
  
acled$EVENT_DATE <- as.Date(acled$EVENT_DATE, "%d-%B-%Y")

class(acled$EVENT_DATE)
