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

# WCZYTANIE
lyft_marzec = read.csv("lyft_do_modelu_marzec.csv")
uber_marzec = read.csv("uber_do_modelu_marzec.csv")
taxi_marzec = read.csv("taxi_do_modelu_marzec.csv")

# DODANIE GRUP GODZIN I DNI

# Marzec
lyft_marzec <- lyft_marzec %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:8            ~ "6-9",
      hour %in% 9:15            ~ "9-16",
      hour %in% 16:19            ~ "16-20",
      hour %in% c(20:23, 0:5)    ~ "20-6",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

uber_marzec <- uber_marzec %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:8            ~ "6-9",
      hour %in% 9:15            ~ "9-16",
      hour %in% 16:19            ~ "16-20",
      hour %in% c(20:23, 0:5)    ~ "20-6",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

taxi_marzec <- taxi_marzec %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:8            ~ "6-9",
      hour %in% 9:15            ~ "9-16",
      hour %in% 16:19            ~ "16-20",
      hour %in% c(20:23, 0:5)    ~ "20-6",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

# MACIERZ KORELACJI

cor(lyft_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")
cor(uber_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")
cor(taxi_marzec[, c("total_amount", "distance", "trip_time_mins")],
    method = "spearman")


# REGRESJE total_amount

# Log - lin
lyft_m_marzec <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = lyft_marzec)
resettest(lyft_m_marzec, power = 2:3, type = "fitted")

uber_m_marzec <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = uber_marzec)
resettest(uber_m_marzec, power = 2:3, type = "fitted")

taxi_m_marzec <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = taxi_marzec)
resettest(taxi_m_marzec, power = 2:3, type = "fitted")

stargazer(lyft_m_marzec, uber_m_marzec, taxi_m_marzec,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Log - log
lyft_m_marzec <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = lyft_marzec)
resettest(lyft_m_marzec, power = 2:3, type = "fitted")

uber_m_marzec <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = uber_marzec)
resettest(uber_m_marzec, power = 2:3, type = "fitted")

taxi_m_marzec <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
                      as.factor(hour_gr) + 
                      as.factor(day_gr),
                    data = taxi_marzec)
resettest(taxi_m_marzec, power = 2:3, type = "fitted")

stargazer(lyft_m_marzec, uber_m_marzec, taxi_m_marzec,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Lin - lin
lyft_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(hour_gr) + 
                        as.factor(day_gr),
                      data = lyft_marzec)
resettest(lyft_m_marzec, power = 2:3, type = "fitted")

uber_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(hour_gr) + 
                        as.factor(day_gr),
                      data = uber_marzec)
resettest(uber_m_marzec, power = 2:3, type = "fitted")

taxi_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(hour_gr) + 
                        as.factor(day_gr),
                      data = taxi_marzec)
resettest(taxi_m_marzec, power = 2:3, type = "fitted")

stargazer(lyft_m_marzec, uber_m_marzec, taxi_m_marzec,
          type = "text", align = TRUE, style = "default", df = FALSE)

# DIAGNOSTYKA

# Reszty

# homoskedastyczność
bptest(lyft_m_marzec)
bptest(uber_m_marzec)
bptest(taxi_m_marzec)

# normalność
jarque.bera.test(lyft_m_marzec$residuals)
jarque.bera.test(uber_m_marzec$residuals)
jarque.bera.test(taxi_m_marzec$residuals)

# RESET
lyft_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                      I(distance**2) +
                      I(trip_time_mins**2) +
                      distance:trip_time_mins +
                      as.factor(hour_gr) +
                      as.factor(hour_gr):trip_time_mins +
                      as.factor(hour_gr):distance +
                      as.factor(day_gr),
                    data = lyft_marzec)
resettest(lyft_m_marzec, power = 2:3, type = "fitted")

uber_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                      I(distance**2) +
                      I(trip_time_mins**2) +
                      distance:trip_time_mins +
                      as.factor(hour_gr) +
                      as.factor(hour_gr):trip_time_mins +
                      as.factor(hour_gr):distance +
                      as.factor(day_gr),
                    data = uber_marzec)
resettest(uber_m_marzec, power = 2:3, type = "fitted")

taxi_m_marzec <- lm(total_amount ~ distance + trip_time_mins +
                      I(distance**2) +
                      I(trip_time_mins**2) +
                      distance:trip_time_mins +
                      as.factor(hour_gr) +
                      as.factor(hour_gr):trip_time_mins +
                      as.factor(hour_gr):distance +
                      as.factor(day_gr),
                    data = taxi_marzec)
resettest(taxi_m_marzec, power = 2:3, type = "fitted")

# Box - cox
boxcox(total_amount ~ distance + trip_time_mins +
         as.factor(hour_gr) + 
         as.factor(day_gr),
       data = lyft_marzec)

boxcox(total_amount ~ distance + trip_time_mins +
         as.factor(hour_gr) + 
         as.factor(day_gr),
       data = uber_marzec)

boxcox(total_amount ~ distance + trip_time_mins +
         as.factor(hour_gr) + 
         as.factor(day_gr),
       data = taxi_marzec)







