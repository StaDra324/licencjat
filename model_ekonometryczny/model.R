library("readr")
library("car")
library("lmtest")
library("foreign")
library("stargazer")
library("dplyr")
library("ggplot2")
library("sandwich")
library("tseries")
library("lubridate")

# WCZYTANIE I DOSTOSOWANIE DANYCH
uber = read_csv("uber_do_modelu.csv")
taxi = read_csv("taxi_do_modelu.csv")

# Uwzględnienie 01.01 (Nowy Rok), 19.01 i 16.02

uber <- uber %>%
  mutate(czy_01_01 = if_else(month(datetime) == 1 & day(datetime) == 1, 1, 0),
         czy_19_01 = if_else(month(datetime) == 1 & day(datetime) == 19, 1, 0),
         czy_16_02 = if_else(month(datetime) == 2 & day(datetime) == 16, 1, 0))
taxi <- taxi %>%
  mutate(czy_01_01 = if_else(month(datetime) == 1 & day(datetime) == 1, 1, 0),
         czy_19_01 = if_else(month(datetime) == 1 & day(datetime) == 19, 1, 0),
         czy_16_02 = if_else(month(datetime) == 2 & day(datetime) == 16, 1, 0))


# Dodanie grup godzin i dni (pór dnia i weekendów)

uber <- uber %>%
  mutate(
    hour_gr = case_when(
      hour %in% 7:8              ~ "7-9",
      hour %in% c(6, 9:16)       ~ "6, 9-17",
      hour %in% 17:20            ~ "17-21",
      hour %in% 21:22            ~ "21-23",      
      hour %in% c(23, 0:5)       ~ "23-6",
      TRUE                       ~ NA_character_))

taxi <- taxi %>%
  mutate(
    hour_gr = case_when(
      hour %in% 7:8              ~ "7-9",
      hour %in% c(6, 9:16)       ~ "6, 9-17",
      hour %in% 17:20            ~ "17-21",
      hour %in% 21:22            ~ "21-23",      
      hour %in% c(23, 0:5)       ~ "23-6",
      TRUE                       ~ NA_character_))

# Upewniamy się, że nie ma NA
anyNA(uber) | anyNA(taxi)

# Ustawienie zmiennych kategorycznych jako factorów i określenie baz

class(uber$czy_weekend)
class(uber$month)
class(uber$hour_gr)

uber$czy_weekend <- relevel(as.factor(uber$czy_weekend), ref = "Robocze")
taxi$czy_weekend <- relevel(as.factor(taxi$czy_weekend), ref = "Robocze")

uber$month <- relevel(as.factor(uber$month), ref = "1")
taxi$month <- relevel(as.factor(taxi$month), ref = "1")

uber$hour_gr <- relevel(as.factor(uber$hour_gr), ref = "6, 9-17")
taxi$hour_gr <- relevel(as.factor(taxi$hour_gr), ref = "6, 9-17")

class(uber$czy_weekend)
class(uber$month)
class(uber$hour_gr)

# Jeszcze raz sprawdzamy NA
anyNA(uber) | anyNA(taxi)

# REGRESJE total_amount

# Log - lin

uber_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")
# RESET nie przechodzi

taxi_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")
# RESET nie przechodzi

stargazer(uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Macierz korelacji

cor(uber[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")
cor(taxi[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")

cor.test(uber$total_amount, uber$distance, method = "spearman")
cor.test(uber$total_amount, uber$trip_time_mins, method = "spearman")
cor.test(uber$distance, uber$trip_time_mins, method = "spearman")

cor.test(taxi$total_amount, taxi$distance, method = "spearman")
cor.test(taxi$total_amount, taxi$trip_time_mins, method = "spearman")
cor.test(taxi$distance, taxi$trip_time_mins, method = "spearman")

# Współczynnik VIF

vif(uber_m, type = "predictor")
vif(taxi_m, type = "predictor")
# wychodzi ok


# DIAGNOSTYKA

# Dodanie kwadratów i interakcji distance i trip_time_mins - RESET  

uber_m2 <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
               I(distance**2) +
               I(trip_time_mins**2) +
               distance:trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m2, power = 2:3, type = "fitted")
# RESET nie przechodzi

taxi_m2 <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
               I(distance**2) +
               I(trip_time_mins**2) +
               distance:trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m2, power = 2:3, type = "fitted")
# RESET nie przechodzi

stargazer(uber_m2, taxi_m2,
          type = "text", align = TRUE, style = "default", df = FALSE)
# dodanie kwadratów i interakcji nic nie wnosi dla RESETU, ale daje 
#   ciekawe wyniki dotyczące nieliniowości, więc warto przeanalizować
#   te rozszerzone modele

# Współczynnik VIF
vif(uber_m2, type = "predictor")
vif(taxi_m2, type = "predictor")

# Modele bazowe i rozszerzone
stargazer(uber_m, uber_m2, taxi_m2, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Usuwamy niepotrzebne już modele bazowe
rm(uber_m, taxi_m)

# Reszty

# homoskedastyczność
bptest(uber_m)
bptest(taxi_m)
bptest(uber_m2)
bptest(taxi_m2)
# występuje, więc trzeba będzie użyć macierzy odpornych

# normalność
jarque.bera.test(uber_m$residuals)
jarque.bera.test(taxi_m$residuals)
jarque.bera.test(uber_m2$residuals)
jarque.bera.test(taxi_m2$residuals)
# występuje, ale próba jest duża, więc nie ma problemu


# Macierze odporne

uber_odp <- coeftest(uber_m, vcov.=vcovHC(uber_m2, type="HC0"))
taxi_odp <- coeftest(taxi_m, vcov.=vcovHC(taxi_m2, type="HC0"))
uber_odp2 <- coeftest(uber_m2, vcov.=vcovHC(uber_m2, type="HC0"))
taxi_odp2 <- coeftest(taxi_m2, vcov.=vcovHC(taxi_m2, type="HC0"))

stargazer(uber_odp, uber_odp2, taxi_odp2, taxi_odp,
          type = "text", align = TRUE, style = "default", df = FALSE)


save.image("workspace.RData")

load("workspace.RData")



