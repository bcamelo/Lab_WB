library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(plm)
library(stargazer)


path <- "~/Dropbox/Harris/Spring 2021/LAB WB"

acled <- read.csv(paste0(path,"/Africa_1997-2021_Apr16.csv")) %>%
  filter(COUNTRY == "Ivory Coast" |COUNTRY == "Mali" |COUNTRY == "Burkina Faso")

write_csv(acled_outborders, "acled_out.csv")

ci_regions <-  raster::getData('GADM', country = "CIV", level = 2) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

ml_regions <-  raster::getData('GADM', country = "MLI", level = 1) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

bf_regions <-  raster::getData('GADM', country = "BFA", level = 1) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

acled$EVENT_DATE <- as.Date(acled$EVENT_DATE, "%d-%B-%Y")         

acled$EVENT_DATE <- format(as.Date(acled$EVENT_DATE), "%Y-%m")

acled_clean <- acled %>%
  filter(EVENT_TYPE == "Violence against civilians" | EVENT_TYPE == "Battles") %>%
  filter(ACTOR1 != "Police Forces of the Ivory Coast (1993-1999)" & ACTOR1 != "Military Forces of the Ivory Coast (1993-1999)" &
           ACTOR1 != "Police Forces of the Ivory Coast (1999-2000)" & ACTOR1 != "Military Forces of the Ivory Coast (2000-2011)"& 
           ACTOR1 != "Police Forces of the Ivory Coast (2000-2011)" & ACTOR1 != "Military Forces of Mali (2013-2020)" &
           ACTOR1 != "MINUCI: United Nations Mission in Cote d'Ivoire (2003-2004)" & ACTOR1 != "Police Forces of the Ivory Coast (2011-)" &
           ACTOR1 != "Military Forces of the Ivory Coast (2011-)" & ACTOR1 != "Former Military Forces of the Ivory Coast (2000-2011)" &
           ACTOR1 != "Military Forces of the Ivory Coast (2011-) Gendarmerie" & ACTOR1 != "Military Forces of the Ivory Coast (2011-) Presidential Security Unit" &
           ACTOR1 != "Police Forces of Ivory Coast (2011-) Gendarmerie" & ACTOR1 != "Police Forces of Ivory Coast (2011-) Customs Office" &
           ACTOR1 != "Police Forces of the Ivory Coast (2011-) Prison Guards" & ACTOR1 != "Military Forces of the Ivory Coast (2011-) Special Forces" &
           ACTOR1 != "Police Forces of the Ivory Coast (2011-) Forest Guards" & ACTOR1 != "Police Forces of Mali (1992-2002)" &
           ACTOR1 != "Military Forces of Mali (2002-2012)" & ACTOR1 != "Police Forces of Mali (2002-2012)" &
           ACTOR1 != "Military Forces of Mali (2012-2013)" & ACTOR1 != "Police Forces of Mali (2012-2013)" &
           ACTOR1 != "MINUSMA: United Nations Multidimensional Integrated Stabilization Mission in Mali (2013-)" &
           ACTOR1 != "Police Forces of Mali (2013-2020) Prison Guards" & ACTOR1 != "Military Forces of Burkina Faso (2015-)" &
           ACTOR1 != "Police Forces of Mali (2013-2020) " & ACTOR1 != "Police Forces of Mali (2013-2020) Gendarmerie" &
           ACTOR1 != "Military Forces of Mali (2013-2020) Operational Coordination Mechanism" & ACTOR1 != "Military Forces of Burkina Faso (2015-) " &
           ACTOR1 != "Military Forces of Mali (2020-)" & ACTOR1 != "Police Forces of Mali (2020-) Gendarmerie" &
           ACTOR1 != "Military Forces of Mali (2020-) Special Forces" & ACTOR1 != "Military Forces of Burkina Faso (1987-2014)" &
           ACTOR1 != "Police Forces of Burkina Faso (1987-2014)" & ACTOR1 != "Military Forces of Burkina Faso (2014-2015) Presidential Security Unit" &
           ACTOR1 != "Military Forces of Burkina Faso (2014-2015)" & ACTOR1 != "Former Military Forces of Burkina Faso (1987-2014)" &
           ACTOR1 != "Police Forces of Burkina Faso (2015-) Gendarmerie" & ACTOR1 != "Police Forces of Burkina Faso (2015-)" &
           ACTOR1 != "Military Forces of Niger (2011-2021)" & ACTOR1 != "Military Forces of France" & ACTOR1 != "Military Forces of Guinea (2010-)" &
           ACTOR1 != "Military Forces of France (2017-)" & ACTOR1 != "Military Forces of Chad (1990-)" &
           ACTOR1 != "Military Forces of Algeria (1999-)" & ACTOR1 != "Military Forces of France (2012-2017)" &
           ACTOR1 != "Military Forces of Mauritania (2009-)" & ACTOR1 != "Military Forces of the United States (2001-2009)") %>%
  subset (select = -c (ISO, EVENT_ID_CNTY, EVENT_ID_NO_CNTY, TIME_PRECISION, ASSOC_ACTOR_1, INTER1, ACTOR2,
                       ASSOC_ACTOR_2, INTER2, INTERACTION, REGION, LOCATION, GEO_PRECISION, SOURCE, SOURCE_SCALE,
                       TIMESTAMP, ADMIN3, NOTES, YEAR))
           

acled_clean$EVENT <- 1

acled_borders <- acled_clean %>%
  filter(COUNTRY == "Ivory Coast" | ADMIN1 == "Sikasso"|
           ADMIN1 == "Cascades" | ADMIN1 == "Sud-Ouest")

acled_outborders <- acled_borders %>%
  filter(COUNTRY != "Ivory Coast")                                                 

acled_civ <- acled_clean %>%
  filter(COUNTRY == "Ivory Coast") %>%
  group_by(ADMIN2, EVENT_DATE) %>%
  summarise(SUM_EVENTS = sum(EVENT))

acled_civ$EVENT <- 1


acled_outborders <- acled_outborders %>%
  select(EVENT_DATE, EVENT) %>%
  group_by(EVENT_DATE) %>%
  summarise(MONTH_CASES = sum(EVENT))
  
acled_outborders_total <- acled_clean %>%
  filter(COUNTRY != "Ivory Coast") %>%
  select(EVENT_DATE, EVENT) %>%
  group_by(EVENT_DATE) %>%
  summarise(MONTH_ALL_CASES = sum(EVENT))


## Create shapefile for CIV border with BFA or MLI

# Create full CIV border as a line
civ <- raster::getData('GADM', country = "CIV", level = 0) %>% 
  st_as_sf(crs = 4326) %>% 
  # Convert to a projection that uses meters so we can measure distance
  st_transform(crs = 2165) %>% 
  st_cast("MULTILINESTRING")
# Combine BFA and MLI polygons
bfa_mali <- st_union(
  x = raster::getData('GADM', country = "BFA", level = 0) %>%
    st_as_sf(crs = 4326) %>% 
    st_transform(crs = 2165),
  y = raster::getData('GADM', country = "MLI", level = 0) %>%
    st_as_sf(crs = 4326) %>% 
    st_transform(crs = 2165)
) 
# Intersect the two to get the relevant part of the border
border <- st_intersection(civ, bfa_mali)
# Check to see it worked
ggplot() +
  geom_sf(data = bfa_mali, size = .1) +
  geom_sf(data = civ, size = .1) +
  geom_sf(data = border, size = 1, color = "red")

# Get region level shapefile
shp <-  raster::getData('GADM', country = "CIV", level = 2) %>% 
  st_as_sf(crs = 4143) %>% 
  # Convert to meters projection
  st_transform(crs = 2165)
# Compute distance from each region to border segment 
shp <- cbind(shp, distance = st_distance(shp, border)) %>%
  mutate(distance = as.numeric(str_extract(distance, "[0-9]+"))) %>%
  # Only keep relevant variables
  select(region = NAME_2, distance)
# Plot to see if it worked

ggplot() +
  geom_sf(data = shp, aes(color = distance)) +
  geom_sf(data = civ, size = .1)

## Joint

DATES <- seq(from = as.Date("1997-05-01"), to = as.Date("2021-04-01"), by = 'month')
dates <- data.frame(DATES)
dates$DATES <- format(as.Date(dates$DATES), "%Y-%m")

plain <- read_csv("plain.csv")

dates <- plain %>%
  pivot_longer(cols = 2:34,
               names_to = "region",
               values_to = "id")

outborders_total <- dates %>%
  full_join(acled_outborders_total, by = c("DATES" = "EVENT_DATE"))

outborders_total$CASES_1 <- 
  c(NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-1)])

outborders_total$CASES_2 <- 
  c(NA,NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-2)])

outborders_total$CASES_1[is.na(outborders_total$CASES_1)] <- 0
outborders_total$CASES_2[is.na(outborders_total$CASES_2)] <- 0
outborders_total$MONTH_ALL_CASES[is.na(outborders_total$MONTH_ALL_CASES)] <- 0

outborders_total <- outborders_total %>%
  mutate(total_cases = MONTH_ALL_CASES + CASES_1 + CASES_2)

outborders_total <- outborders_total %>%
  select(DATES, region, total_cases)

# same for borders

outborders <- dates %>%
  full_join(acled_outborders, by = c("DATES" = "EVENT_DATE"))

outborders$CASES_1 <- 
  c(NA,outborders$MONTH_CASES[1:(length(outborders$MONTH_CASES)-1)])

outborders$CASES_2 <- 
  c(NA,NA,outborders$MONTH_CASES[1:(length(outborders$MONTH_CASES)-2)])

outborders$CASES_1[is.na(outborders$CASES_1)] <- 0
outborders$CASES_2[is.na(outborders$CASES_2)] <- 0
outborders$MONTH_CASES[is.na(outborders$MONTH_CASES)] <- 0

outborders <- outborders %>%
  mutate(border_cases = MONTH_CASES + CASES_1 + CASES_2)

outborders <- outborders %>%
  select(DATES, region, border_cases)

out_violence <- outborders_total %>%
  full_join(outborders, by = c("DATES","region"))

dist <- read_csv("dist.csv")


## Joint

final <- out_violence %>%
  full_join(acled_civ, by = c("DATES" = "EVENT_DATE", "region" = "ADMIN2"))
  
final <- final %>%
  left_join(dist, by = "region")

final$SUM_EVENTS[is.na(final$SUM_EVENTS)] <- 0
final$EVENT[is.na(final$EVENT)] <- 0

write_csv(final, "final.csv")

## Regressions

colnames(final)

reg1 <- lm(SUM_EVENTS ~ distance*border_cases, data = final)

reg2 <- lm(SUM_EVENTS ~ distance*total_cases, data = final)

logit_1 <- glm(EVENT ~ distance*border_cases, data = final, family = "binomial")

logit_2 <- glm(EVENT ~ distance*total_cases, data = final, family = "binomial")

stargazer(reg1, reg2, logit_1, logit_2, type = "text", title="Results", align=TRUE)


    
## Spatial Visualization

acled_clean_ml %>%
  filter(ADMIN1 == "Sikasso") %>%
  ggplot() +
  geom_sf(data = ml_regions) +
  geom_sf(aes(alpha = 0.4), color = 'red') +
  geom_sf(data = acled_clean_ml, aes(alpha = 0.2), color = 'blue') +
  labs(title = "Events in Mali 1997-2021 - Borders in red", color = "events", alpha = "") + 
  theme_minimal()

acled_clean_bf %>%
  filter(ADMIN1 == "Cascades" | ADMIN1 == "Sud-Ouest") %>%  
  ggplot() +
    geom_sf(data = bf_regions) +
    geom_sf(aes(alpha = 0.25), color = 'red') +
    geom_sf(data = acled_clean_bf, aes(alpha = 0.2), color = 'blue') +
    labs(title = "Events in BF 1997-2021 - Borders in red", color = "events", alpha = "") + 
    theme_minimal()
  
## Time Visualization
  

acled_fat %>% 
  filter(ADMIN1 == "Sikasso") %>%
  ggplot(aes(x = EVENT_DATE , y = FATALITIES)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(se = F)


acled_fat_sp %>% 
  filter(COUNTRY != "Ivory Coast") %>%
  ggplot(aes(x = EVENT_DATE , y = FATALITIES, color = COUNTRY)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(se = F) +
  labs(title = "Fatalities near North Border", color = "Countries", alpha = "")

acled_fat_sp %>% 
  ggplot(aes(x = EVENT_DATE , y = FATALITIES, color = COUNTRY)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(se = F)


# Actors Visualization


