


install.packages(c("reshape", "MASS", "psych", "Rglpk", "XML", "data.table"), dependencies=TRUE)

library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(rgeos)

library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(plm)
library(stargazer)
library(gmodels)
library(descr)
library(readr)
library(ggplot2)

# Ivory Coast is Côte d'Ivoire
# Data Source: ACLED Curated Data: https://acleddata.com/curated-data-files/
acled1 <- read.csv("C:/Users/benna/Desktop/Harris/PolicyLab - WB/WorkingDirectory/Africa_1997-2021_Apr16.csv") %>%
  filter(COUNTRY == "Ivory Coast" |COUNTRY == "Mali" |COUNTRY == "Burkina Faso")

acled <- acled1
##In case pb with as.date format 
##Sys.setlocale("LC_TIME", "C") 

write_csv(acled, "C:/Users/benna/Desktop/Harris/PolicyLab - WB/acled_out.csv")

#write_csv(acled_outborders, "C:/Users/benna/Desktop/Harris/PolicyLab - WB/acled_out.csv")

ci_regions <-  raster::getData('GADM', country = "CIV", level = 2) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

ml_regions <-  raster::getData('GADM', country = "MLI", level = 1) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

bf_regions <-  raster::getData('GADM', country = "BFA", level = 1) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

#Revoir le format date
#acled$EVENT_DATE <- dmy(acled$EVENT_DATE)
#acled$EVENT_DATE_formatted <- format(acled$EVENT_DATE, "%m/%Y")
#acled$EVENT_DATE <- format(acled$EVENT_DATE, "%m/%Y")

acled$EVENT_DATE <- dmy(acled$EVENT_DATE)

acled$EVENT_DATE <- as.Date(acled$EVENT_DATE,"%d-%M-%Y" )         

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
  subset (select = -c ( ï..ISO, EVENT_ID_CNTY, EVENT_ID_NO_CNTY, TIME_PRECISION, ASSOC_ACTOR_1, INTER1, ACTOR2,
                       ASSOC_ACTOR_2, INTER2, INTERACTION, REGION, LOCATION, GEO_PRECISION, SOURCE, SOURCE_SCALE,
                       TIMESTAMP, ADMIN3, NOTES, YEAR))

#Create a dummy for actors who operate on both sides
x<-c(actors="Bissa Ethnic Militia (Ivory Coast)" ,"Katiba Macina" ,
     "AQIM: Al Qaeda in the Islamic Maghreb",
     "Dozo Communal Militia (Ivory Coast)",  
     "Dozo Militia",
     "Lobi Ethnic Militia (Ivory Coast)" , 
     "Militia (Miners)",
     "Mossi Ethnic Militia (Burkina Faso)" ,
     "Unidentified Armed Group (Burkina Faso)", 
     "Islamic State (Greater Sahara) and/or Ansaroul Islam",
     "GMA: Mourabitounes Group of Azawad", 
     "Fula Ethnic Militia (Burkina Faso)", 
     "Fulani Ethnic Militia (Burkina Faso)",
     "Bobo Ethnic Militia (Burkina Faso)", 
     "Ansaroul Islam", 
     "Lobi Ethnic Militia (Burkina Faso)",  
     "Al Mourabitoune Battalion",
     "Fulani Ethnic Militia (Mali)",
     "Baoule Ethnic Militia (Burkina Faso)",
     "Bobo Ethnic Militia (Burkina Faso)", 
     "Bozo Ethnic Militia (Mali)" ,  
     "Unidentified Ethnic Militia (Burkina Faso)" , 
     "Ngadana Communal Militia (Ivory Coast)")


acled_clean$ActorsInOut= ifelse(acled_clean$ACTOR1 %in% x ,1,0)


acled_clean_actors <- acled_clean %>%
  filter(ACTOR1 == "Bissa Ethnic Militia (Ivory Coast)"  | 
         ACTOR1 == "Katiba Macina" |
         ACTOR1 == "AQIM: Al Qaeda in the Islamic Maghreb" |
         ACTOR1 == "Dozo Communal Militia (Ivory Coast)" | 
         ACTOR1 ==  "Lobi Ethnic Militia (Ivory Coast)"|
         ACTOR1 == "Militia (Miners)" |
         ACTOR1 == "Mossi Ethnic Militia (Burkina Faso)"  |
         ACTOR1 == "Unidentified Armed Group (Burkina Faso)" |
         ACTOR1 == "Islamic State (Greater Sahara) and/or Ansaroul Islam" |
         ACTOR1 == "GMA: Mourabitounes Group of Azawad" |
         ACTOR1 == "Fula Ethnic Militia (Burkina Faso)" |  
         ACTOR1 == "Fulani Ethnic Militia (Burkina Faso)" | 
         ACTOR1 ==  "Bissa Ethnic Militia (Burkina Faso)"|
         ACTOR1 == "Bobo Ethnic Militia (Burkina Faso)" |
         ACTOR1 == "Ansaroul Islam" |
         ACTOR1 == "Dozo Militia" | 
         ACTOR1 ==  "Katiba Macina"|
           ACTOR1 == "Lobi Ethnic Militia (Burkina Faso)" |
           ACTOR1 ==  "Al Mourabitoune Battalion" |
           ACTOR1 ==  "Fulani Ethnic Militia (Mali)"|
           ACTOR1 == "Unidentified Ethnic Militia (Burkina Faso)" | 
           ACTOR1 == "Ngadana Communal Militia (Ivory Coast)" |
         ACTOR1 == "Bozo Ethnic Militia (Mali)"|
         ACTOR1 == "Baoule Ethnic Militia (Burkina Faso)" )       

acled_clean_actors$EVENT <- 1
write_csv(acled_clean_actors, "C:/Users/benna/Desktop/Harris/PolicyLab - WB/acled_clean_actors.csv")

#Ivory coast data +Borders of Burkina Cascades and Sud ouest and Mali sikasso
acled_actors_borders <- acled_clean_actors %>%
  filter(COUNTRY == "Ivory Coast" | ADMIN1 == "Sikasso"|
           ADMIN1 == "Cascades" | ADMIN1 == "Sud-Ouest")

#Burkina and Mali data
acled_actors_outborders <- acled_actors_borders %>%
  filter(COUNTRY != "Ivory Coast")                                                 


acled_actors_civ <- acled_clean_actors %>%
  filter(COUNTRY == "Ivory Coast") %>%
  group_by(ADMIN2, EVENT_DATE) %>%
  summarise(SUM_EVENTS = sum(EVENT))

#acled$EVENT_DATE <- as.Date(acled$EVENT_DATE, "%d-%b-%y")         
#acled$EVENT_DATE <- format(as.Date(acled$EVENT_DATE), "%y-%m")


acled_actors_civ$EVENT <- 1


acled_actors_outborders <- acled_actors_outborders %>%
  dplyr::select(EVENT_DATE, EVENT) %>%
  group_by(EVENT_DATE) %>%
  summarise(MONTH_CASES = sum(EVENT))

acled_actors_outborders_total <- acled_clean_actors %>%
  filter(COUNTRY != "Ivory Coast") %>%
  group_by(EVENT_DATE) %>%
  summarise(MONTH_ALL_CASES = sum(EVENT))

#dplyr::select(EVENT_DATE_formatted, EVENT) %>%
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
  dplyr::select(region = NAME_2, distance)
# Plot to see if it worked

ggplot() +
  geom_sf(data = shp, aes(color = distance)) +
  geom_sf(data = civ, size = .1)

## Joint

DATES <- seq(from = as.Date("1997-05-01"), to = as.Date("2021-04-01"), by = 'month')
dates <- data.frame(DATES)
dates$DATES <- format(as.Date(dates$DATES), "%Y-%m")


plain <- read_csv("C:/Users/benna/Desktop/Harris/PolicyLab - WB/WorkingDirectory/plain.csv")

dates <- plain %>%
  pivot_longer(cols = 2:34,
               names_to = "region",
               values_to = "id")

outborders_actors_total <- dates %>%
  dplyr::full_join(acled_actors_outborders_total, by = c("DATES" = "EVENT_DATE"))

outborders_actors_total$CASES_1 <- 
  c(NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_actors_total$MONTH_ALL_CASES)-1)])

outborders_actors_total$CASES_2 <- 
  c(NA,NA,outborders_actors_total$MONTH_ALL_CASES[1:(length(outborders_actors_total$MONTH_ALL_CASES)-2)])

outborders_actors_total$CASES_1[is.na(outborders_actors_total$CASES_1)] <- 0
outborders_actors_total$CASES_2[is.na(outborders_actors_total$CASES_2)] <- 0
outborders_actors_total$MONTH_ALL_CASES[is.na(outborders_actors_total$MONTH_ALL_CASES)] <- 0


outborders_actors_total <- outborders_actors_total %>%
  mutate(total_cases = MONTH_ALL_CASES + CASES_1 + CASES_2)

outborders_actors_total <- outborders_actors_total %>%
  dplyr::select(DATES, region, total_cases)

# same for borders

outborders_actors <- dates %>%
  full_join(acled_actors_outborders, by = c("DATES" = "EVENT_DATE"))

outborders_actors$CASES_1 <- 
  c(NA,outborders_actors$MONTH_CASES[1:(length(outborders_actors$MONTH_CASES)-1)])

outborders_actors$CASES_2 <- 
  c(NA,NA,outborders_actors$MONTH_CASES[1:(length(outborders_actors$MONTH_CASES)-2)])

outborders_actors$CASES_1[is.na(outborders_actors$CASES_1)] <- 0
outborders_actors$CASES_2[is.na(outborders_actors$CASES_2)] <- 0
outborders_actors$MONTH_CASES[is.na(outborders_actors$MONTH_CASES)] <- 0


outborders_actors <- outborders_actors %>%
  mutate(border_cases = MONTH_CASES + CASES_1 + CASES_2)

outborders_actors <- outborders_actors %>%
  dplyr::select(DATES, region, border_cases)

out_violence_actors <- outborders_actors_total %>%
  full_join(outborders_actors, by = c("DATES","region"))

dist <- read_csv("C:/Users/benna/Desktop/Harris/PolicyLab - WB/WorkingDirectory/dist.csv")

## Joint

final_actors <- out_violence_actors %>%
  full_join(acled_actors_civ, by = c("DATES" = "EVENT_DATE", "region" = "ADMIN2"))



final_actors <- final_actors %>%
  left_join(dist, by = "region")

final_actors$SUM_EVENTS[is.na(final_actors$SUM_EVENTS)] <- 0
final_actors$EVENT[is.na(final_actors$EVENT)] <- 0


#Bradson's code
final_actors$distance <- final_actors$distance/1000

final_actors_full <- final_actors %>%
  mutate(border_ev = ifelse(border_cases != 0, 1, 0),
         border_region = ifelse(distance < 200, 1, 0))

write_csv(final_actors_full, "final_actors_full.csv")

#Andres clustered by region
library(estimatr)
library(fixest)

# Count of events, using OLS with standard errors clustered by region
summary(lm_robust(SUM_EVENTS ~ border_region*border_ev, cluster = region, data = final_actors_full))
# Indicator for 1+ events, using OLS with standard errors clustered by region
summary(lm_robust(EVENT ~ border_region*border_ev, cluster = region, data = final_actors_full))
# Indicator for 1+ events, using GLM with standard errors clustered by region
summary(feglm(EVENT ~ border_region*border_ev, family = binomial("logit"), data = final_actors_full), cluster = ~ region)







