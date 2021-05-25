


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
