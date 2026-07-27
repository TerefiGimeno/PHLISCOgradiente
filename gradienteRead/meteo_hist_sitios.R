##METEO HISTORICA SITIOS
### load libraries and sources ####
library(tidyverse)
library(slider)
s.err.na <- function(x) {
  return(sd(x, na.rm =T)/sqrt(length(which(!is.na(x)))))}

####1. Artikutza ####
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
  select(-c(X, Pday_mm_old, Pday_mm_new, TmeanDay_C_old, TmeanDay_C_new)) |> 
  relocate(Pday_mm, .after = date)
rm(Pday_ART, Tday_ART)
  
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

meteo_art_hist <- meteo_art_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    series_start = min(year), series_end = max(year),
    .groups = "drop") |> 
  mutate(series_length = series_end - series_start + 1)

####2. Bertiz ####

Pday_ber <- read.csv("gradienteData/meterologia sitios/Bertiz/datos/precip - copia.csv") |> 
  select(-c(INDICATIVO, NOMBRE, ALTITUD, NOM_PROV)) |> 
  rename(year = ANO) |> 
  rename(month = MES) |> 
  pivot_longer(cols = starts_with("P"), names_to = "day",
               values_to = "Pday_mm", values_drop_na = TRUE) |> 
  mutate(day = as.numeric(str_remove_all(day, "P"))) |> 
  mutate(Pday_mm = (ifelse(Pday_mm == -3, 0, Pday_mm)*0.1))

Tday_ber <- read.csv("gradienteData/meterologia sitios/Bertiz/datos/temp - copia.csv") |> 
  select(-c(INDICATIVO, NOMBRE, ALTITUD, NOM_PROV)) |> 
  rename(year = ANO) |> 
  rename(month = MES) |> 
  pivot_longer(cols = starts_with("T"), names_to = "var_day",
               values_to = "T_C", values_drop_na = TRUE) |> 
  mutate(var = str_sub(var_day, 1, 4)) |> 
  mutate(day = as.numeric(str_sub(var_day, 5, nchar(var_day)))) |>
  mutate(T_C = T_C*0.1) |> 
  select(-c(var_day)) |> 
  pivot_wider(names_from = var, values_from = T_C) |>
  rename(TmaxDay_C = TMAX) |> 
  rename(TminDay_C = TMIN) |> 
  mutate(TmeanDay_C = (TmaxDay_C + TminDay_C)*0.5)

meteo_ber <- full_join(Pday_ber, Tday_ber, by = c("year", "month", "day"))
rm(Tday_ber, Pday_ber)

meteo_ber_month <- meteo_ber %>%
  group_by(year, month) %>%
  summarise(
    precip_month_mm = sum(Pday_mm, na.rm = TRUE),
    temp_max_month = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_month = s.err.na(TmaxDay_C),
    temp_min_month = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_month = s.err.na(TminDay_C),
    temp_mean_month = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_month = s.err.na(TmeanDay_C),
    .groups = "drop")

meteo_ber <- meteo_ber |> 
  mutate(date = make_date(year, month, day)) |> 
  select(-c(year, month, day)) |> 
  relocate(date, .before = Pday_mm)

meteo_ber_years <- meteo_ber %>%
  mutate(year = year(date)) |> 
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

meteo_ber_month_hist <- meteo_ber_month %>%
  group_by(month) %>%
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

meteo_ber_hist <- meteo_ber_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    series_start = min(year), series_end = max(year),
    .groups = "drop") |> 
  mutate(series_length = series_end - series_start + 1)

####3. Monte Santiago ####
Pms <- read.csv("gradienteData/meterologia sitios/Monte Satiago/Pday_Orduna.csv")[, c(1,4)]
Tms <- read.csv("gradienteData/meterologia sitios/Monte Satiago/TmeanDay_Orduna.csv")[, c(1,4)] |> 
  left_join(read.csv("gradienteData/meterologia sitios/Monte Satiago/TmaxDay_Orduna.csv")[, c(1,4)],
            by = "YYYYMMDD") |>
  left_join(read.csv("gradienteData/meterologia sitios/Monte Satiago/TminDay_Orduna.csv")[, c(1,4)],
            by = "YYYYMMDD") |>
  left_join(Pms, by = "YYYYMMDD") |> 
  mutate(date = ymd(YYYYMMDD)) |> 
  select(-c(YYYYMMDD))

meteo_msa <- read.csv('gradienteData/meterologia sitios/Monte Satiago/meteo_ms_2014_2023_daily.csv') |> 
  mutate(date = ymd(date)) |> 
  full_join(Tms, by = "date") |> 
  arrange(date) |> 
  mutate(Pday_mm = ifelse(is.na(Pday_mm), precip_day_mm, Pday_mm)) |> 
  mutate(TmaxDay_C = ifelse(is.na(TmaxDay_C), temp_max_day, TmaxDay_C)) |> 
  mutate(TminDay_C = ifelse(is.na(TminDay_C), temp_min_day, TminDay_C)) |> 
  mutate(TmeanDay_C = ifelse(is.na(TmeanDay_C), temp_mean_day, TmeanDay_C)) |>
  select(-c(rh_mean_day,se_rh_day,
            rh_max_day,se_rh_max,
            rh_min_day,se_rh_min,
            se_temp_day,se_temp_max,
            se_temp_min,X,
            precip_day_mm, temp_mean_day,
            temp_max_day, temp_min_day)) |> 
  relocate(Pday_mm, .after = date) |> 
  relocate(TmeanDay_C, .after = TminDay_C)
rm(Pms, Tms)

meteo_msa_month <- meteo_msa %>%
  mutate(
    year = year(date),
    month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    precip_month_mm = sum(Pday_mm, na.rm = TRUE),
    temp_mean_month = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_month = s.err.na(TmeanDay_C),
    temp_max_month = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_month = s.err.na(TmaxDay_C),
    temp_min_month = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_month = s.err.na(TminDay_C),
    .groups = "drop")

meteo_msa_years <- meteo_msa %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(Pday_mm, na.rm = TRUE),
    temp_mean_year = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_year = s.err.na(TmeanDay_C),
    temp_max_year = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_year = s.err.na(TmaxDay_C),
    temp_min_year = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_year = s.err.na(TminDay_C),
    .groups = "drop")

meteo_msa_month_hist <- meteo_msa_month %>%
  group_by(month) %>%
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

meteo_msa_hist <- meteo_msa_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),,
    series_start = min(year), series_end = max(year),
    .groups = "drop") |> 
  mutate(series_length = series_end - series_start + 1)

####4. Iturrieta ####
Pday_itu <- read.csv("gradienteData/meterologia sitios/Iturrieta/Pday_Iturrieta.csv") |> 
  mutate(date = ymd(YYYYMMDD)) |> 
  select(-c(YYYYMMDD, C024, G024))
Tday_itu <- read.csv("gradienteData/meterologia sitios/Iturrieta/TmeanDay_Iturrieta.csv") |> 
  mutate(date = ymd(YYYYMMDD)) |> 
  select(-c(YYYYMMDD, C024, G024)) |> 
  full_join(Pday_itu, by = "date")

meteo_itu <- read.csv('gradienteData/meterologia sitios/Iturrieta/meteo_itu_2014_2023_daily.csv') |> 
  rename(Pday_mm_new=precip_day_mm,
         TmaxDay_C=temp_max_day,
         TminDay_C=temp_min_day,
         TmeanDay_C_new=temp_mean_day) |> 
  mutate(date = ymd(date)) |> 
  full_join(Tday_itu, by = "date") |> 
  arrange(date) |> 
  mutate(TmeanDay_C = ifelse(is.na(TmeanDay_C), TmeanDay_C_new, TmeanDay_C)) |> 
  mutate(Pday_mm = ifelse(is.na(Pday_mm), Pday_mm_new, Pday_mm)) |> 
  select(-c(rh_mean_day,se_rh_day,
            rh_max_day,se_rh_max,
            rh_min_day,se_rh_min,
            se_temp_day,se_temp_max,
            se_temp_min,X,
            Pday_mm_new, TmeanDay_C_new)) |> 
  relocate(Pday_mm, .after = date)
rm(Pday_itu, Tday_itu)

meteo_itu_month <- meteo_itu %>%
  mutate(year = year(date),
    month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    precip_month_mm = sum(Pday_mm, na.rm = TRUE),
    temp_mean_month = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_month = s.err.na(TmeanDay_C),
    temp_max_month = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_month = s.err.na(TmaxDay_C),
    temp_min_month = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_month = s.err.na(TminDay_C),
    .groups = "drop")

meteo_itu_years <- meteo_itu %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    precip_year_mm = sum(Pday_mm, na.rm = TRUE),
    temp_mean_year = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_year = s.err.na(TmeanDay_C),
    temp_max_year = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_year = s.err.na(TmaxDay_C),
    temp_min_year = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_year = s.err.na(TminDay_C),
    .groups = "drop")

meteo_itu_month_hist <- meteo_itu_month %>%
  group_by(month) %>%
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

meteo_hist_itu <- meteo_itu_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    series_start = min(year), series_end = max(year),
    .groups = "drop") |> 
  mutate(series_length = series_end - series_start + 1)


####5. Montejo de la Sierra ####
# summarised monthly data from the local meteorological station
#(at the actual site). Data go from 1994 to 2021
meteo_hmo_month_hist <- read.csv("gradienteData/meterologia sitios/Montejo de la sierra/resumen_mensual_clima_HM.csv") |> 
  select(-c(Tmin.abs, Tmax.abs)) |> 
  rename(temp_mean_month_hist = Tmean,
         temp_max_month_hist = Tmax.mean,
         temp_min_month_hist = Tmin.mean,
         precip_month_mm_hist = Pp.mean)

meteo_hist_hmo <- meteo_hmo_month_hist |> 
  summarise(precip_annual_mm_hist = sum(precip_month_mm_hist),
            temp_mean_annual_hist = mean(temp_max_month_hist),
            temp_min_annual_hist = mean(temp_min_month_hist))

# daily data are not available, retrieve for a short period from hourly data:
meteo_hmo <- read.csv("gradienteData/meterologia sitios/Montejo de la sierra/detailed_climate_montejo.csv") |> 
  group_by(date) |> 
  summarise(Pday_mm = sum(precipitation_mm, na.rm = T),
            TmaxDay_C = max(air_temperature_c, na.rm = T),
            TminDay_C = min(air_temperature_c, na.rm = T),
            TmeanDay_C = mean(air_temperature_c, na.rm = T)) |> 
  mutate(date = ymd(date))

####6. Diustes ####
meteo_diu <- read.csv('gradienteData/meterologia sitios/Diustes/meteo_diu_1968_2023_daily.csv') |> 
  select(-c(X,sitio)) %>% 
  rename(Pday_mm=precipitacion,
         TmaxDay_C=tempmax,
         TminDay_C=tempmin,
         TmeanDay_C=temp_air_celcius) |> 
  mutate(date = dmy(as.character(date))) |> 
  arrange(date)

meteo_diu_month <- meteo_diu %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    precip_month_mm = sum(Pday_mm, na.rm = TRUE),
    temp_max_month = mean(TmaxDay_C, na.rm = TRUE),
    se_temp_max_month = s.err.na(TmaxDay_C),
    temp_min_month = mean(TminDay_C, na.rm = TRUE),
    se_temp_min_month = s.err.na(TminDay_C),
    temp_mean_month = mean(TmeanDay_C, na.rm = TRUE),
    se_temp_month = s.err.na(TmeanDay_C),
    .groups = "drop")

meteo_diu_years <- meteo_diu %>%
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

meteo_diu_month_hist <- meteo_diu_month %>%
  group_by(month) %>%
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

meteo_diu_hist <- meteo_diu_years %>%
  summarise(
    precip_annual_mm_hist = mean(precip_year_mm, na.rm = TRUE),
    se_precip_annual_mm_hist = s.err.na(precip_year_mm),
    temp_mean_annual_hist = mean(temp_mean_year, na.rm = TRUE),
    se_temp_annual_hist = s.err.na(se_temp_year),
    temp_max_annual_hist = mean(temp_max_year, na.rm = TRUE),
    se_temp_max_annual_hist = s.err.na(temp_max_year),
    temp_min_annual_hist = mean(temp_min_year, na.rm = TRUE),
    se_temp_min_annual_hist = s.err.na(temp_min_year),
    series_start = min(year), series_end = max(year),
    .groups = "drop") |> 
  mutate(series_length = series_end - series_start + 1)

####7. Calcualte campaign values ####

campaign_dates <- read.csv("gradienteData/sampling_dates.csv") |> 
  mutate(date = dmy(as.character(date)))

meteoList <- list()
meteoList[[1]] <- as.data.frame(meteo_art)
meteoList[[2]] <- as.data.frame(meteo_ber)
meteoList[[3]] <- as.data.frame(meteo_diu)
meteoList[[4]] <- as.data.frame(meteo_itu)
meteoList[[5]] <- as.data.frame(meteo_hmo)
meteoList[[6]] <- as.data.frame(meteo_msa)

nameVars <- c(paste0("T", c("max", "min", "mean"), "Day_C"))
sites <- c(unique(levels(as.factor(campaign_dates$site))))
results <- list()

for(i in 1:length(meteoList)){
  df <- subset(meteoList[[i]], date >= as.Date("2023-05-01") & date <= as.Date("2023-10-31"))
  df <- df %>%
    arrange(date)

  for (n in 2:10) {
    
    # Rolling means
    for (v in 1:length(nameVars)) {
      df[[paste0(nameVars[v], "_", n, "d")]] <-
        slide_index_dbl(
          .x = df[, nameVars[v]],
          .i = df$date,
          .f = ~mean(.x, na.rm = TRUE),
          .before = n - 1,
          .complete = TRUE
        )
    }
    
    # Rolling cumulative precipitation
    df[[paste0("P_", n, "d")]] <-
      slide_index_dbl(
        .x = df$Pday_mm,
        .i = df$date,
        .f = ~sum(.x, na.rm = TRUE),
        .before = n - 1,
        .complete = TRUE
      )
  }
  
  results[[i]] <- left_join(subset(campaign_dates, site == sites[i]), df, by = "date")
}

summary_meteo_campaigns <- do.call(rbind, results)
