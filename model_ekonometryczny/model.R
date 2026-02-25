# - regresję: cena – determinanty,  
# - regresję: log(cena) – determinanty,  
# - regresję: log(cena) – log(determinanty) w przypadku zmiennych ciągłych,  
# - sprawdzić macierz korelacji.
# 
# Następnie proszę rozpocząć diagnostykę modelu (analiza reszt itp.).
# 
# RESET:
#   
#   - dodać dystans² oraz czas²,  
# - dodać sensowne interakcje (np. dystans × czas),  
# - dodać zmienne dodatkowe, jeżeli coś można wykorzystać z bazy.
# - jeżeli było na ekonometrii transformacja Boxa-Coxa, można zastosować 

library("car")
library("lmtest")
library("foreign")
library("stargazer")
library("dplyr")
library("ggplot2")
library("sandwich")
library("tseries")
library("MASS")

# WCZYTANIE I DOSTOSOWANIE DANYCH
lyft = read.csv("lyft_do_modelu.csv")
uber = read.csv("uber_do_modelu.csv")
taxi = read.csv("taxi_do_modelu.csv")

# Uwzględnienie 01.01 (Nowy rok)

class(uber$datetime)
# można zmienic zeby to co wyzej szybciej dzialalo

lyft <- lyft %>%
  mutate(czy_01_01 = if_else(month(datetime) == 1 & day(datetime) == 1, 1, 0),
         czy_19_01 = if_else(month(datetime) == 1 & day(datetime) == 19, 1, 0),
         czy_16_02 = if_else(month(datetime) == 2 & day(datetime) == 16, 1, 0))
uber <- uber %>%
  mutate(czy_01_01 = if_else(month(datetime) == 1 & day(datetime) == 1, 1, 0),
         czy_19_01 = if_else(month(datetime) == 1 & day(datetime) == 19, 1, 0),
         czy_16_02 = if_else(month(datetime) == 2 & day(datetime) == 16, 1, 0))
taxi <- taxi %>%
  mutate(czy_01_01 = if_else(month(datetime) == 1 & day(datetime) == 1, 1, 0),
         czy_19_01 = if_else(month(datetime) == 1 & day(datetime) == 19, 1, 0),
         czy_16_02 = if_else(month(datetime) == 2 & day(datetime) == 16, 1, 0))

# Ustawienie zmiennych kategorycznych jako factorów i określenie baz
class(uber$hour)
class(uber$czy_weekend)
class(uber$month)

lyft$hour <- relevel(as.factor(lyft$hour), ref = "12")
uber$hour <- relevel(as.factor(uber$hour), ref = "12")
taxi$hour <- relevel(as.factor(taxi$hour), ref = "12")
lyft$czy_weekend <- relevel(as.factor(lyft$czy_weekend), ref = "Robocze")
uber$czy_weekend <- relevel(as.factor(uber$czy_weekend), ref = "Robocze")
taxi$czy_weekend <- relevel(as.factor(taxi$czy_weekend), ref = "Robocze")
lyft$month <- relevel(as.factor(lyft$month), ref = "1")
uber$month <- relevel(as.factor(uber$month), ref = "1")
taxi$month <- relevel(as.factor(taxi$month), ref = "1")


# REGRESJE total_amount

# Log - lin
lyft_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      hour + 
                      czy_weekend +
                      hour:czy_weekend +
                      month +
                      czy_01_01 +
                      czy_19_01 +
                      czy_16_02,
                    data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      hour + 
                      czy_weekend +
                      hour:czy_weekend +
                      month +
                      czy_01_01 +
                      czy_19_01 +
                      czy_16_02,
                    data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      hour + 
                      czy_weekend +
                      hour:czy_weekend +
                      month +
                      czy_01_01 +
                      czy_19_01 +
                      czy_16_02,
                    data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)



# DODANIE GRUP GODZIN I DNI

lyft <- lyft %>%
  mutate(
    hour_gr = case_when(
      hour %in% 7:8              ~ "7-9",
      hour %in% c(6, 9:16)       ~ "6, 9-17",
      hour %in% 17:20            ~ "17-21",
      hour %in% 21:22            ~ "21-23",      
      hour %in% c(20:23, 0:5)    ~ "23-6",
      TRUE                       ~ NA_character_))

uber <- uber %>%
  mutate(
    hour_gr = case_when(
      hour %in% 7:8              ~ "7-9",
      hour %in% c(6, 9:16)       ~ "6, 9-17",
      hour %in% 17:20            ~ "17-21",
      hour %in% 21:22            ~ "21-23",      
      hour %in% c(20:23, 0:5)    ~ "23-6",
      TRUE                       ~ NA_character_))

taxi <- taxi %>%
  mutate(
    hour_gr = case_when(
      hour %in% 7:8              ~ "7-9",
      hour %in% c(6, 9:16)       ~ "6, 9-17",
      hour %in% 17:20            ~ "17-21",
      hour %in% 21:22            ~ "21-23",      
      hour %in% c(20:23, 0:5)    ~ "23-6",
      TRUE                       ~ NA_character_))

lyft$hour_gr <- relevel(as.factor(lyft$hour_gr), ref = "6, 9-17")
uber$hour_gr <- relevel(as.factor(uber$hour_gr), ref = "6, 9-17")
taxi$hour_gr <- relevel(as.factor(taxi$hour_gr), ref = "6, 9-17")

# Log - lin
lyft_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

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

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Log - log
lyft_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)



# MACIERZ KORELACJI

cor(lyft_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")
cor(uber_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")
cor(taxi_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")


# VIF

vif(lyft_m, type = "predictor")
vif(uber_m, type = "predictor")
vif(taxi_m, type = "predictor")
# dla log - log distance i trip_time dużo, dla log - lin ok

# Modele bez czasu i bez dystansu

# log - lin bez czasu
lyft_m <- lm(I(log(total_amount)) ~ distance +
                 hour_gr + 
                 czy_weekend +
                 hour_gr:czy_weekend +
                 month +
                 czy_01_01 +
                 czy_19_01 +
                 czy_16_02,
               data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ distance +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ distance +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# log - lin bez dystansu
lyft_m <- lm(I(log(total_amount)) ~ trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ trip_time_mins +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# log - log bez czasu
lyft_m <- lm(I(log(total_amount)) ~ I(log(distance)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ I(log(distance)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ I(log(distance)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# log - log bez dystansu
lyft_m <- lm(I(log(total_amount)) ~ I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ I(log(trip_time_mins)) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)
# ogólnie usuwanie zmiennych zaburza modele


# DIAGNOSTYKA

# Dodanie kwadratów i interakcji - RESET  
  
lyft_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
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
               data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
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
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
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
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Log - log
lyft_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               I((log(distance))**2) +
               I((log(trip_time_mins))**2) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = lyft)
resettest(lyft_m, power = 2:3, type = "fitted")

uber_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               I((log(distance))**2) +
               I((log(trip_time_mins))**2) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = uber)
resettest(uber_m, power = 2:3, type = "fitted")

taxi_m <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
               I((log(distance))**2) +
               I((log(trip_time_mins))**2) +
               hour_gr + 
               czy_weekend +
               hour_gr:czy_weekend +
               month +
               czy_01_01 +
               czy_19_01 +
               czy_16_02,
             data = taxi)
resettest(taxi_m, power = 2:3, type = "fitted")

stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)
# dodanie kwadratów i interakcji nic nie wnosi dla RESETU

# Reszty

# homoskedastyczność
bptest(lyft_m)
bptest(uber_m)
bptest(taxi_m)

# normalność
jarque.bera.test(lyft_m$residuals)
jarque.bera.test(uber_m$residuals)
jarque.bera.test(taxi_m$residuals)


save.image("workspace.RData")

load("workspace.RData")



# Macierze odporne

lyft_odp <- coeftest(lyft_m, vcov.=vcovHC(lyft_m, type="HC0"))
uber_odp <- coeftest(uber_m, vcov.=vcovHC(uber_m, type="HC0"))
taxi_odp <- coeftest(taxi_m, vcov.=vcovHC(taxi_m, type="HC0"))

# prezentujemy wyniki w tabeli publikacyjnej
stargazer(lyft_m, uber_m, taxi_m,
          type = "text", align = TRUE, style = "default", df = FALSE)








# # Lin - lin
# lyft_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
#                       as.factor(hour_gr) + 
#                       as.factor(day_gr),
#                     data = lyft_marzec)
# resettest(lyft_m_marzec, power = 2:3, type = "fitted")
# 
# uber_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
#                       as.factor(hour_gr) + 
#                       as.factor(day_gr),
#                     data = uber_marzec)
# resettest(uber_m_marzec, power = 2:3, type = "fitted")
# 
# taxi_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
#                       as.factor(hour_gr) + 
#                       as.factor(day_gr),
#                     data = taxi_marzec)
# resettest(taxi_m_marzec, power = 2:3, type = "fitted")
# 
# stargazer(lyft_m_marzec, uber_m_marzec, taxi_m_marzec,
#           type = "text", align = TRUE, style = "default", df = FALSE)







