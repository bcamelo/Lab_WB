library(sf)
library(lubridate)
library(raster)
library(plm)
library(stargazer)
library(texreg)
library(tidyverse)


setwd("~/Dropbox/Harris/Spring 2021/LAB WB/Lab_WB")

# acled <- read.csv("~/Dropbox/Harris/Spring 2021/LAB WB/Africa_1997-2021_Apr16.csv") %>%
#  filter(COUNTRY == "Ivory Coast" |COUNTRY == "Mali" |COUNTRY == "Burkina Faso")

acled <- read.csv("~/Dropbox/Harris/Spring 2021/LAB WB/Africa_1997-2021_Apr16.csv") %>%
  filter(COUNTRY == "Ivory Coast" |COUNTRY == "Mali" |COUNTRY == "Burkina Faso") %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# write_csv(acled, "acled.csv")

acled <- read_csv("acled.csv")

ci_regions <-  raster::getData('GADM', country = "CIV", level = 2) %>%
  # Convert to a sf object with the same CRS as we used above
  st_as_sf(crs = 4326)

ci_regions1 <-  raster::getData('GADM', country = "CIV", level = 1) %>%
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


acled$ActorsInOut= ifelse(acled$ACTOR1 %in% x ,1,0)

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

acled_clean <- acled_clean %>%
  filter(ActorsInOut == 1)

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

#Adding new variable with lags (1 to 6 months) # it is just copy the MONTH_ALL_CASES variable, 
#but in the first 'n' rows we add an NA and than start the MONTH_ALL_CASES column.

outborders_total$CASES_1 <- 
  c(NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-1)])

outborders_total$CASES_2 <- 
  c(NA,NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-2)])

outborders_total$CASES_3 <- 
  c(NA,NA,NA, outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-3)])

outborders_total$CASES_4 <- 
  c(NA,NA,NA,NA, outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-4)])

outborders_total$CASES_5 <- 
  c(NA,NA,NA,NA,NA,outborders_total$MONTH_ALL_CASES[1:(length(outborders_total$MONTH_ALL_CASES)-5)])

#cleaning the NA

outborders_total$CASES_1[is.na(outborders_total$CASES_1)] <- 0
outborders_total$CASES_2[is.na(outborders_total$CASES_2)] <- 0
outborders_total$CASES_3[is.na(outborders_total$CASES_3)] <- 0
outborders_total$CASES_4[is.na(outborders_total$CASES_4)] <- 0
outborders_total$CASES_5[is.na(outborders_total$CASES_5)] <- 0
outborders_total$MONTH_ALL_CASES[is.na(outborders_total$MONTH_ALL_CASES)] <- 0

#Creating this border_case_'n' variable with the acumulated months
outborders_total <- outborders_total %>%
  mutate(border_cases_6 = MONTH_ALL_CASES + CASES_1 + CASES_2 + CASES_3 + CASES_4 + CASES_5,
         border_cases_5 = MONTH_ALL_CASES + CASES_1 + CASES_2 + CASES_3 + CASES_4,
         border_cases_4 = MONTH_ALL_CASES + CASES_1 + CASES_2 + CASES_3,
         border_cases_3 = MONTH_ALL_CASES + CASES_1 + CASES_2,
         border_cases_2 = MONTH_ALL_CASES + CASES_1,
         border_cases_1 = MONTH_ALL_CASES)



outborders_total <- outborders_total %>%
  select(DATES, region, border_cases_1,border_cases_2,
         border_cases_3,border_cases_4,border_cases_5,border_cases_6)


dist <- read_csv("dist.csv")

acled_civ$ADMIN2 <- as.character(acled_civ$ADMIN2)


acled_civ$ADMIN2[acled_civ$ADMIN2 == "Me"] <- "La Me"
acled_civ$ADMIN2[acled_civ$ADMIN2 == "Sud-Comoe"] <- "Sud Comoe"

acled_civ$ADMIN2 <- as.factor(acled_civ$ADMIN2)

## Joint

final <- outborders_total %>%
  full_join(acled_civ, by = c("DATES" = "EVENT_DATE", "region" = "ADMIN2"))
  
final <- final %>%
  left_join(dist, by = "region")

final$SUM_EVENTS[is.na(final$SUM_EVENTS)] <- 0
final$EVENT[is.na(final$EVENT)] <- 0

final$distance <- final$distance/1000

#creating the Dummy variable for the lag_cases and distance

final_full <- final %>%
  mutate(border_ev_1 = ifelse(border_cases_1 != 0, 1, 0),
         border_ev_2 = ifelse(border_cases_2 != 0, 1, 0),
         border_ev_3 = ifelse(border_cases_3 != 0, 1, 0),
         border_ev_4 = ifelse(border_cases_4 != 0, 1, 0),
         border_ev_5 = ifelse(border_cases_5 != 0, 1, 0),
         border_ev_6 = ifelse(border_cases_6 != 0, 1, 0),
         border_region = ifelse(distance < 200, 1, 0))


## Regressions

reg1_1 <- lm(EVENT ~ border_region*border_ev_1, data = final_full)
reg1_2 <- lm(EVENT ~ border_region*border_ev_2, data = final_full)
reg1_3 <- lm(EVENT ~ border_region*border_ev_3, data = final_full)
reg1_4 <- lm(EVENT ~ border_region*border_ev_4, data = final_full)
reg1_5 <- lm(EVENT ~ border_region*border_ev_5, data = final_full)
reg1_6 <- lm(EVENT ~ border_region*border_ev_6, data = final_full)

reg2 <- lm(SUM_EVENTS ~ border_region*border_ev, data = final_full)

logit_1 <- glm(EVENT ~ distance*border_cases, data = final, family = "binomial")

logit_2 <- glm(EVENT ~ distance*total_cases, data = final, family = "binomial")

stargazer(reg1_1, reg1_3, reg1_6, title="Results", align=TRUE)



## New Regressions


reg3 <- lm(EVENT ~ border_region*border_ev, data = final_full)

reg4 <- plm (EVENT ~ border_region*border_ev, data = final_full, index = "region", model = "within")

logit_3 <- glm(EVENT ~ border_region*border_ev, data = final_full, family = "binomial")


stargazer(reg2, reg3,logit_3, title="Results", align=TRUE)

## New Regressions (fixed effects)
library(survival)
library(foreign)
library(plm)
library(car)
library(pROC)
library(estimatr)
library(fixest)


logit_4 <- clogit(EVENT ~ border_region*border_ev + strata(region), data = final_full)


stargazer(reg1_1, reg1_3, reg1_6, type = "text", title="Results without and with fixed effects", align=TRUE)


prob <- predict(logit_3, type="response")
summary(prob)
final_full$prob_3 <- prob
roc1 <- roc(EVENT ~ prob_3, data = final_full)
plot(roc1, col="red", main = "Area Under ROC for LOGIT_3" , ylab="Sensitivity", xlab="Specificity")
auc(roc1)
text(0.5, 0.2, "AUC = 0.6323")

## Andre's regression

final_full_2009 <- final_full %>%
  filter(DATES > 2009)

# Count of events, using OLS with standard errors clustered by region
summary(lm_robust(SUM_EVENTS ~ border_region*border_ev, cluster = region, data = final_full))
# Indicator for 1+ events, using OLS with standard errors clustered by region
reg1 <- lm_robust(EVENT ~ border_region*border_ev_1, cluster = region, data = final_full)
reg2 <- lm_robust(EVENT ~ border_region*border_ev_2, cluster = region, data = final_full)
reg3 <- lm_robust(EVENT ~ border_region*border_ev_3, cluster = region, data = final_full)
reg4 <- lm_robust(EVENT ~ border_region*border_ev_4, cluster = region, data = final_full)
reg5 <- lm_robust(EVENT ~ border_region*border_ev_5, cluster = region, data = final_full)
reg6 <- lm_robust(EVENT ~ border_region*border_ev_6, cluster = region, data = final_full)

summary(reg6)

summary(lm_robust(EVENT ~ border_region*border_ev_1, cluster = region, data = final_full_2009))
summary(lm_robust(EVENT ~ border_region*border_ev_2, cluster = region, data = final_full_2009))
summary(lm_robust(EVENT ~ border_region*border_ev_3, cluster = region, data = final_full_2009))
summary(lm_robust(EVENT ~ border_region*border_ev_4, cluster = region, data = final_full_2009))
summary(lm_robust(EVENT ~ border_region*border_ev_5, cluster = region, data = final_full_2009))
summary(lm_robust(EVENT ~ border_region*border_ev_6, cluster = region, data = final_full_2009))



# Indicator for 1+ events, using GLM with standard errors clustered by region
summary(feglm(EVENT ~ border_region*border_ev, family = binomial("logit"), data = final_full), cluster = ~ region)

stargazer(reg3, reg4,logit_3,logit_4, type = "text", title="Results", align=TRUE)
   
## Spatial Visualization

eventos <- acled_outborders %>%
  filter(ADMIN1 == "Sikasso"|ADMIN1 == "Cascades" | ADMIN1 == "Sud-Ouest")

acled_outborders$EVENT <- 1

acled_outborders %>%
  select(-c(MONTH_CASES)) %>%
  ggplot() +
  geom_sf(data = ml_regions) +
  geom_sf(data = bf_regions) +
  geom_sf(data = ci_regions1) +
  geom_sf(aes(alpha = 0.25), color = 'red') +
  geom_sf(data = acled_civ, aes(alpha = 0.25), color = 'blue') +
  labs(title = "Events in CIV 1997-2021 in Blue",
       subtitle = "Borders'cases Mali and BF in red",
       alpha = "Events") + 
  theme_minimal()


ggplot() +
  geom_point (data = final_full, aes(x = DATES, y = EVENT)) +
  geom_sf(data = bf_regions) +
  geom_sf(data = ci_regions1) +
  geom_sf(aes(alpha = 0.25), color = 'red') +
  geom_sf(data = acled_civ, aes(alpha = 0.25), color = 'blue') +
  labs(title = "Events in CIV 1997-2021 in Blue",
       subtitle = "Borders'cases Mali and BF in red",
       alpha = "Events") + 
  theme_minimal()




acled_clean %>%
  filter(ADMIN1 == "Sikasso"|ADMIN1 == "Cascades" | ADMIN1 == "Sud-Ouest") %>%  
  ggplot() +
    geom_sf(data = bf_regions) +
    geom_sf(data = ml_regions) +
    geom_sf(data = ci_regions1) +
    geom_sf(aes(alpha = 0.1), color = 'red') +
    geom_sf(data = acled_clean_ml_bf, aes(alpha = 0.1), color = 'blue') +
    labs(title = "Events in Mali & BF 1997-2021 - Borders in red", color = "events", alpha = "") + 
    theme_minimal()

acled_clean_ml_bf <- acled_clean %>%
  filter(COUNTRY != "Ivory Coast")

acled_clean_ml_bf <- acled_clean_ml_bf %>%
  filter(ADMIN1 != "Sikasso" & ADMIN1 != "Cascades" & ADMIN1 != "Sud-Ouest")



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


