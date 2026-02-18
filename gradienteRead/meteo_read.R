library(climaemet)
library(tidyverse)

aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0LmdpbWVub0BjcmVhZi51YWIuY2F0IiwianRpIjoiZWI4NWIwMWMtMjNkOC00Zjc0LWIyMDEtOTFjYTIwYTJlMDI1IiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3NzEyNzE5NTksInVzZXJJZCI6ImViODViMDFjLTIzZDgtNGY3NC1iMjAxLTkxY2EyMGEyZTAyNSIsInJvbGUiOiIifQ.5V0BxzZvfOX7JVJHtu_T1UiIPrcvBlQ9hFD4Ki6wUuM",
              instal = TRUE)

stations <- aemet_stations()
rows_in_fruit1 <- df[df$fruit1 %in% my_vector, ]
stations[stations$nombre %in% "BERT",]

filtered_df <- stations %>%
  filter(str_detect(nombre, "BERTI"))

url <- "/api/valores/climatologicos/inventarioestaciones/todasestaciones"
get_data_aemet(url)

ber_meteo <- get_data_aemet(
  station = "BERTIZ_CODE",
  start_date = "1993-01-01",
  end_date = Sys.Date()
)

write.csv(ber_meteo, "bertiz_daily_climate.csv", row.names = FALSE)