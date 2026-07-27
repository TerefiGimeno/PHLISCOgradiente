theTrees <- read.csv("gradienteData/alturas_individuos/dbh_height.csv") |> 
  group_by(site) |> 
  summarise(DBH = mean(dbh_cm, na.rm = T),
            seDBH = sd(dbh_cm, na.rm = T)/sqrt(length(which(!is.na(dbh_cm)))),
            H = mean(h_m, na.rm =T),
            seH =sd(h_m, na.rm = T)/sqrt(length(which(!is.na(h_m)))))