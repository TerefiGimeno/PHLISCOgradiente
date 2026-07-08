##METEO HISTORICA SITIOS
### load libraries and sources ####
library(tidyverse)
s.err.na <- function(x) {
  return(sd(x, na.rm =T)/sqrt(length(which(!is.na(x)))))}

####1. Clean and compile meteo data for all the sites####
#####1.1 Artikutza #####
######1.1.1 Clean and process the data######
Pday_ART <- read.csv("gradienteData/meterologia sitios/Artikutza/Pday_Artikutza.csv") |> 
  mutate(date = ymd(YYYYMMDD)) |> 
  select(-c(YYYYMMDD, X1021)) |> 
  rename(Pday_mm_old = Pday_mm)
Tday_ART <- read.csv("gradienteData/meterologia sitios/Artikutza/TmeanDay_Artikutza.csv") |> 
  mutate(date = ymd(as.character(YYYYMMDD))) |> 
  left_join(Pday_ART, by = "date") |> 
  select(-c(YYYYMMDD, X1021)) |> 
  relocate(TmeanDay_C, .after = date) |> 
  rename(TmeanDay_C_old = TmeanDay_C)
meteo_art <- read.csv("gradienteData/meterologia sitios/Artikutza/meteo_art_2000_2025_daily.csv") |> 
  mutate(date = ymd(date)) |> 
  rename(Pday_mm_new = precip_mm) |> 
  rename(TmaxDay_C = temperatura_maxima) |> 
  rename(TminDay_C = temperatura_minima) |> 
  rename(TmeanDay_C_new = temp_air_celcius) |> 
  full_join(Tday_ART, by = "date") |>
  arrange(date) |> 
  mutate(Pday_mm = ifelse(is.na(Pday_mm_new), Pday_mm_old, Pday_mm_new)) |> 
  mutate(TmeanDay_C = ifelse(is.na(TmeanDay_C_new), TmeanDay_C_old, TmeanDay_C_new)) |>
  select(-c(X, Pday_mm_old, Pday_mm_new, TmeanDay_C_old, TmeanDay_C_new))
rm(Pday_ART, Tday_ART)
  
######1.1.2 Calculation of monthly and yearly average values######
meteo_art_month <- meteo_art %>%
  mutate(
    year = year(date),
    month_num = month(date),
    day_num = day(date),
    month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(Pday_mm, na.rm = TRUE),
    temp_max_month = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_month = s.err.na(TmaxDay_C),
    temp_min_month = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_month = s.err.na(TminDay_C),
    temp_mean_month = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_month = s.err.na(TmeanDay_C),
    .groups = "drop")

meteo_art_years <- meteo_art %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(Pday_mm, na.rm = TRUE),
    temp_max_year = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_year = s.err.na(TmaxDay_C),
    temp_min_year = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_year = s.err.na(TminDay_C),
    temp_mean_year = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_year = s.err.na(TmeanDay_C),
    .groups = "drop")

meteo_art_month_avg <- meteo_art_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop" )

#####1.2 Bertiz #####
######1.1.1 Clean and process the data######

meteo_ber_2000_2025_daily <- read.csv('gradienteData/meterologia sitios/meteo_ber_2000_2025_daily.csv')
meteo_ber_2000_2025_daily<-meteo_ber_2000_2025_daily %>% 
  rename(temp_max=temperatura_maxima,
         temp_min=temperatura_minima,
         temp_mean_celcius=temp_air_celcius)
meteo_ber_2000_2025_daily <- meteo_ber_2000_2025_daily %>%
  mutate(
    date = trimws(date),
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd")))
fallos <-meteo_ber_2000_2025_daily %>%
  filter(is.na(date))
meteo_ber_2000_2025_daily$date <- as.Date(meteo_ber_2000_2025_daily$date)
range(meteo_ber_2000_2025_daily$date, na.rm = TRUE)


####meteo historica BERTIZ 2000-2025 datos por meses####
meteo_ber_2000_2025_month <- meteo_ber_2000_2025_daily %>%
  mutate(
    year = year(date),
    month_num = month(date),
    day_num = day(date),
    month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_month = mean(temp_max, na.rm = TRUE),
    se_temp_max_month = s.err.na(temp_max),
    temp_min_month = mean(temp_min, na.rm = TRUE),
    se_temp_min_month = s.err.na(temp_min),
    temp_mean_month = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_month = s.err.na(temp_mean_celcius),
    .groups = "drop")

####meteo historica BERTIZ 2000-2025 datos por años####
meteo_ber_2000_2025_years <- meteo_ber_2000_2025_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_year = mean(temp_max, na.rm = TRUE),
    se_temp_max_year = s.err.na(temp_max),
    temp_min_year = mean(temp_min, na.rm = TRUE),
    se_temp_min_year = s.err.na(temp_min),
    temp_mean_year = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_year = s.err.na(temp_mean_celcius),
    .groups = "drop")

####historico por mes BERTIZ 2000-2025####
meteo_ber_month_hist <- meteo_ber_2000_2025_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop" )

####historico anual proveniente del promedio de meteo historica bertiz 2000-2025 ####
meteo_ber_hist_10years <- meteo_ber_2000_2025_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    .groups = "drop")





#METEO HISTORICA MONTE SANTIAGO
####meteo historica MS 2014-2023 datos diarios####
meteo_ms_2014_2023_daily <- read.csv('gradienteData/meterologia sitios/meteo_ms_2014_2023_daily.csv')
meteo_ms_2014_2023_daily<-meteo_ms_2014_2023_daily %>% 
  select(-c(rh_mean_day,se_rh_day,
            rh_max_day,se_rh_max,
            rh_min_day,se_rh_min,
            se_temp_day,se_temp_max,
            se_temp_min,X)) %>% 
  rename(precip_mm=precip_day_mm,
    temp_max=temp_max_day,
         temp_min=temp_min_day,
         temp_mean_celcius=temp_mean_day)
meteo_ms_2014_2023_daily <- meteo_ms_2014_2023_daily %>%
  mutate(
    date = trimws(date),
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd")))
fallos <-meteo_ms_2014_2023_daily %>%
  filter(is.na(date))
meteo_ms_2014_2023_daily$date <- as.Date(meteo_ms_2014_2023_daily$date)
range(meteo_ms_2014_2023_daily$date, na.rm = TRUE)


####meteo historica MS 2014-2023 datos de euskalmet por meses####
meteo_ms_2014_2023_month <- meteo_ms_2014_2023_daily %>%
  mutate(
    year = year(date),
    month_num = month(date),
    day_num = day(date),
    month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(precip_mm, na.rm = TRUE),
    temp_mean_month = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_month = s.err.na(temp_mean_celcius),
    temp_max_month = mean(temp_max, na.rm = TRUE),
    se_temp_max_month = s.err.na(temp_max),
    temp_min_month = mean(temp_min, na.rm = TRUE),
    se_temp_min_month = s.err.na(temp_min),
    .groups = "drop")


####meteo historica MS 2014-2023 datos de euskalmet por años####
meteo_ms_2014_2023_years <- meteo_ms_2014_2023_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(precip_mm, na.rm = TRUE),
    temp_mean_year = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_year = s.err.na(temp_mean_celcius),
    temp_max_year = mean(temp_max, na.rm = TRUE),
    se_temp_max_year = s.err.na(temp_max),
    temp_min_year = mean(temp_min, na.rm = TRUE),
    se_temp_min_year = s.err.na(temp_min),
    .groups = "drop")

####historico por mes proveniente de meteo historica MS 2014-2023####
meteo_ms_month_hist <- meteo_ms_2014_2023_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop")

####historico anual proveniente del promedio de meteo historica MS 2014-2023 ####
meteo_hist_10years_ms <- meteo_ms_2014_2023_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    .groups = "drop")






#METEO HISTORICA ITURRIETA
####meteo historica ITURRIETA 2014-2023 datos diarios####
meteo_itu_2014_2023_daily <- read.csv('gradienteData/meterologia sitios/meteo_itu_2014_2023_daily.csv')
meteo_itu_2014_2023_daily<-meteo_itu_2014_2023_daily %>% 
  select(-c(rh_mean_day,se_rh_day,
            rh_max_day,se_rh_max,
            rh_min_day,se_rh_min,
            se_temp_day,se_temp_max,
            se_temp_min,X)) %>% 
  rename(precip_mm=precip_day_mm,
         temp_max=temp_max_day,
         temp_min=temp_min_day,
         temp_mean_celcius=temp_mean_day)
meteo_itu_2014_2023_daily <- meteo_itu_2014_2023_daily %>%
  mutate(
    date = trimws(date),
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd")))
fallos <-meteo_itu_2014_2023_daily %>%
  filter(is.na(date))
meteo_itu_2014_2023_daily$date <- as.Date(meteo_itu_2014_2023_daily$date)
range(meteo_itu_2014_2023_daily$date, na.rm = TRUE)


####meteo historica ITURRIETA 2014-2023 datos de euskalmet por meses####
meteo_itu_2014_2023_month <- meteo_itu_2014_2023_daily %>%
  mutate(
    year = year(date),
    month_num = month(date),
    day_num = day(date),
    month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(precip_mm, na.rm = TRUE),
    temp_mean_month = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_month = s.err.na(temp_mean_celcius),
    temp_max_month = mean(temp_max, na.rm = TRUE),
    se_temp_max_month = s.err.na(temp_max),
    temp_min_month = mean(temp_min, na.rm = TRUE),
    se_temp_min_month = s.err.na(temp_min),
    .groups = "drop")


####meteo historica MS 2014-2023 datos de euskalmet por años####
meteo_itu_2014_2023_years <- meteo_itu_2014_2023_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(precip_mm, na.rm = TRUE),
    temp_mean_year = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_year = s.err.na(temp_mean_celcius),
    temp_max_year = mean(temp_max, na.rm = TRUE),
    se_temp_max_year = s.err.na(temp_max),
    temp_min_year = mean(temp_min, na.rm = TRUE),
    se_temp_min_year = s.err.na(temp_min),
    .groups = "drop")

####historico por mes proveniente de meteo historica ITURRIETA 2014-2023####
meteo_itu_month_hist <- meteo_itu_2014_2023_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop")

####historico anual proveniente del promedio de meteo historica ITURRIETA 2014-2023 ####
meteo_hist_10years_itu <- meteo_itu_2014_2023_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    .groups = "drop")
















###meteo historica MONTEJO DE LA SIERRA 2006-2023 datos diarios####
meteo_hm_2006_2023_daily <- read.csv('gradienteData/meterologia sitios/meteo_hm_2006_2023_daily.csv')
meteo_hm_2006_2023_daily<-meteo_hm_2006_2023_daily %>% 
  rename(precip_mm=precipitacion,
    temp_max=tempmax,
         temp_min=tempmin,
         temp_mean_celcius=temp_air_celcius)
meteo_hm_2006_2023_daily  <- meteo_hm_2006_2023_daily  %>%
  mutate(
    date = trimws(date),
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd")))
fallos <-meteo_hm_2006_2023_daily %>%
  filter(is.na(date))
meteo_hm_2006_2023_daily$date <- as.Date(meteo_hm_2006_2023_daily$date)
range(meteo_hm_2006_2023_daily$date, na.rm = TRUE)


####meteo historica MONTEJO DE LA SIERRA 2006-2023 datos por meses####
meteo_hm_2006_2023_month <- meteo_hm_2006_2023_daily %>%
  mutate(year = year(date),
         month_num = month(date),
         day_num = day(date),
         month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_month = mean(temp_max, na.rm = TRUE),
    se_temp_max_month = s.err.na(temp_max),
    temp_min_month = mean(temp_min, na.rm = TRUE),
    se_temp_min_month = s.err.na(temp_min),
    temp_mean_month = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_month = s.err.na(temp_mean_celcius),
    .groups = "drop")

####meteo historica MONTEJO DE LA SIERRA 2006-2023 datos por años####
meteo_hm_2006_2023_years <- meteo_hm_2006_2023_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_year = mean(temp_max, na.rm = TRUE),
    se_temp_max_year = s.err.na(temp_max),
    temp_min_year = mean(temp_min, na.rm = TRUE),
    se_temp_min_year = s.err.na(temp_min),
    temp_mean_year = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_year = s.err.na(temp_mean_celcius),
    .groups = "drop")

####historico por mes MONTEJO DE LA SIERRA 2006-2023####
meteo_hm_month_hist <- meteo_hm_2006_2023_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop" )

####historico anual proveniente del promedio de meteo historica MONTEJO DE LA SIERRA 2006-2023 ####
meteo_hm_hist_10years <- meteo_hm_2006_2023_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    .groups = "drop")






###meteo historica DIUSTES 1968-2023 datos diarios####
meteo_diu_1968_2023_daily <- read.csv('gradienteData/meterologia sitios/meteo_diu_1968_2023_daily.csv')
meteo_diu_1968_2023_daily<-meteo_diu_1968_2023_daily%>% 
  select(-c(X,sitio)) %>% 
  rename(precip_mm=precipitacion,
         temp_max=tempmax,
         temp_min=tempmin,
         temp_mean_celcius=temp_air_celcius)
meteo_diu_1968_2023_daily  <- meteo_diu_1968_2023_daily  %>%
  mutate(
    date = trimws(date),
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd")))
fallos <-meteo_diu_1968_2023_daily %>%
  filter(is.na(date))
meteo_diu_1968_2023_daily$date <- as.Date(meteo_diu_1968_2023_daily$date)
range(meteo_diu_1968_2023_daily$date, na.rm = TRUE)


####meteo historica DIUSTES 1968-2023 datos por meses####
meteo_diu_1968_2023_month <- meteo_diu_1968_2023_daily %>%
  mutate(year = year(date),
         month_num = month(date),
         day_num = day(date),
         month_date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month_date,month_num) %>%
  summarise(
    precip_month_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_month = mean(temp_max, na.rm = TRUE),
    se_temp_max_month = s.err.na(temp_max),
    temp_min_month = mean(temp_min, na.rm = TRUE),
    se_temp_min_month = s.err.na(temp_min),
    temp_mean_month = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_month = s.err.na(temp_mean_celcius),
    .groups = "drop")

####meteo historica DIUSTES 1968-2023 datos por años####
meteo_diu_1968_2023_years <- meteo_diu_1968_2023_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(precip_mm, na.rm = TRUE),
    temp_max_year = mean(temp_max, na.rm = TRUE),
    se_temp_max_year = s.err.na(temp_max),
    temp_min_year = mean(temp_min, na.rm = TRUE),
    se_temp_min_year = s.err.na(temp_min),
    temp_mean_year = mean(temp_mean_celcius, na.rm = TRUE),
    se_temp_year = s.err.na(temp_mean_celcius),
    .groups = "drop")

####historico por mes DIUSTES 1968-2023####
meteo_diu_month_hist <- meteo_diu_1968_2023_month %>%
  group_by(month_num) %>%
  summarise(
    precip_month_mm_hist = mean(precip_month_mm, na.rm = TRUE),
    se_precip_month_hist = s.err.na(precip_month_mm),
    temp_mean_month_hist = mean(temp_mean_month, na.rm = TRUE),
    se_temp_month_hist = s.err.na(se_temp_month),
    temp_max_month_hist = mean(temp_max_month, na.rm = TRUE),
    se_temp_max_month_hist = s.err.na(temp_max_month),
    temp_min_month_hist = mean(temp_min_month, na.rm = TRUE),
    se_temp_min_month_hist = s.err.na(temp_min_month),
    .groups = "drop" )

####historico anual proveniente del promedio de meteo historica DIUSTES 1968-2023 ####
meteo_diu_hist_10years <- meteo_diu_1968_2023_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    .groups = "drop")









