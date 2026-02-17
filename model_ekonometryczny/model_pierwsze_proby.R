library("car")
library("lmtest")
library("foreign")
library("stargazer")
library("dplyr")
library("ggplot2")
library("sandwich")
library("tseries")

View(lyft_luty)
# WCZYTANIE
lyft_styczen = read.csv("lyft_do_modelu_styczen.csv")
uber_styczen = read.csv("uber_do_modelu_styczen.csv")
taxi_styczen = read.csv("taxi_do_modelu_styczen.csv")
lyft_luty = read.csv("lyft_do_modelu_luty.csv")
uber_luty = read.csv("uber_do_modelu_luty.csv")
taxi_luty = read.csv("taxi_do_modelu_luty.csv")

# PIERWSZE MODELE

# Base_fare zależy od czasu i dystansu

# styczen
lyft_mb_styczen <- lm(base_fare ~ distance + trip_time_mins,
                      data = lyft_styczen)
resettest(lyft_mb_styczen, power = 2:3, type = "fitted")

uber_mb_styczen <- lm(base_fare ~ distance + trip_time_mins,
                      data = uber_styczen)
resettest(uber_mb_styczen, power = 2:3, type = "fitted")

taxi_mb_styczen <- lm(base_fare ~ distance + trip_time_mins,
                      data = taxi_styczen)
resettest(taxi_mb_styczen, power = 2:3, type = "fitted")
# niezle oszacowania

stargazer(lyft_mb_styczen, uber_mb_styczen, taxi_mb_styczen,
          type = "text", align = TRUE, style = "default", df = FALSE)

# luty
lyft_mb_luty <- lm(base_fare ~ distance + trip_time_mins,
                         data = lyft_luty)
resettest(lyft_mb_luty, power = 2:3, type = "fitted")

uber_mb_luty <- lm(base_fare ~ distance + trip_time_mins,
                         data = uber_luty)
resettest(uber_mb_luty, power = 2:3, type = "fitted")

taxi_mb_luty <- lm(base_fare ~ distance + trip_time_mins,
                         data = taxi_luty)
resettest(taxi_mb_luty, power = 2:3, type = "fitted")
# niezłe oszacowania

stargazer(lyft_mb_luty, uber_mb_luty, taxi_mb_luty,
          type = "text", align = TRUE, style = "default", df = FALSE)

# regressor
taxi_mb_luty_reg <- lm(base_fare ~ distance + trip_time_mins +
                      I(distance**2) +
                      I(distance**3) +
                      I(trip_time_mins**2) +
                      I(trip_time_mins**3) +
                      distance:trip_time_mins,
                  data = taxi_luty)
summary(taxi_mb_luty_reg)
resettest(taxi_mb_luty_reg, power = 2:3, type = "fitted")
# nic nie daje

# logarytmowanie
taxi_mb_luty_1 <- lm(I(log(base_fare)) ~ distance + trip_time_mins,
                  data = taxi_luty)
summary(taxi_mb_luty_1)
resettest(taxi_mb_luty_1, power = 2:3, type = "fitted")

taxi_mb_luty_2 <- lm(I(log(base_fare)) ~ I(log(distance)) + trip_time_mins,
                  data = taxi_luty)
summary(taxi_mb_luty_2)
resettest(taxi_mb_luty_2, power = 2:3, type = "fitted")

taxi_mb_luty_3 <- lm(I(log(base_fare)) ~ I(log(distance)) + I(log(trip_time_mins)),
                  data = taxi_luty)
summary(taxi_mb_luty_3)
resettest(taxi_mb_luty_3, power = 2:3, type = "fitted")

taxi_mb_luty_4 <- lm(base_fare ~ I(log(distance)) + I(log(trip_time_mins)),
                    data = taxi_luty)
summary(taxi_mb_luty_4)
resettest(taxi_mb_luty_4, power = 2:3, type = "fitted")

taxi_mb_luty_5 <- lm(base_fare ~ I(log(distance)) + trip_time_mins,
                    data = taxi_luty)
summary(taxi_mb_luty_5)
resettest(taxi_mb_luty_5, power = 2:3, type = "fitted")
# nic nie dają

# Total_amount zależy od czasu, dystansu, dnia tygodnia i pory dnia

# styczen
lyft_mt_styczen <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(weekday) + 
                        as.factor(hour),
                      data = lyft_styczen)
resettest(lyft_mt_styczen, power = 2:3, type = "fitted")

uber_mt_styczen <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(weekday) + 
                        as.factor(hour),
                      data = uber_styczen)
resettest(uber_mt_styczen, power = 2:3, type = "fitted")

taxi_mt_styczen <- lm(total_amount ~ distance + trip_time_mins +
                        as.factor(weekday) + 
                        as.factor(hour),
                      data = taxi_styczen)
resettest(taxi_mt_styczen, power = 2:3, type = "fitted")

stargazer(lyft_mt_styczen, uber_mt_styczen, taxi_mt_styczen,
          type = "text", align = TRUE, style = "default", df = FALSE)

# luty
lyft_mt_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(weekday) + 
                     as.factor(hour),
                  data = lyft_luty)
resettest(lyft_mt_luty, power = 2:3, type = "fitted")

uber_mt_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(weekday) + 
                     as.factor(hour),
                  data = uber_luty)
resettest(uber_mt_luty, power = 2:3, type = "fitted")

taxi_mt_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(weekday) + 
                     as.factor(hour),
                  data = taxi_luty)
resettest(taxi_mt_luty, power = 2:3, type = "fitted")

stargazer(lyft_mt_luty, uber_mt_luty, taxi_mt_luty,
          type = "text", align = TRUE, style = "default", df = FALSE)

# logarytmowanie
taxi_mt_luty_1 <- lm(I(log(total_amount)) ~ distance + trip_time_mins +
                     as.factor(weekday) + 
                     as.factor(hour),
                   data = taxi_luty)
resettest(taxi_mt_luty_1, power = 2:3, type = "fitted")

taxi_mt_luty_2 <- lm(I(log(total_amount)) ~ I(log(distance)) + I(log(trip_time_mins)) +
                     as.factor(weekday) + 
                     as.factor(hour),
                   data = taxi_luty)
resettest(taxi_mt_luty_2, power = 2:3, type = "fitted")
# nic nie daje

# Total_amount z pogrupowaniem na godziny i dni

# styczen
taxi_gr_styczen <- taxi_styczen %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)    ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

lyft_gr_styczen <- lyft_styczen %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)    ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

uber_gr_styczen <- uber_styczen %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)    ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

lyft_mt_gr_styczen <- lm(total_amount ~ distance + trip_time_mins +
                           as.factor(day_gr) + 
                           as.factor(hour_gr),
                         data = lyft_gr_styczen)
resettest(lyft_mt_gr_styczen, power = 2:3, type = "fitted")

uber_mt_gr_styczen <- lm(total_amount ~ distance + trip_time_mins +
                           as.factor(day_gr) + 
                           as.factor(hour_gr),
                         data = uber_gr_styczen)
resettest(uber_mt_gr_styczen, power = 2:3, type = "fitted")

taxi_mt_gr_styczen <- lm(total_amount ~ distance + trip_time_mins +
                           as.factor(day_gr) + 
                           as.factor(hour_gr),
                         data = taxi_gr_styczen)
resettest(taxi_mt_gr_styczen, power = 2:3, type = "fitted")

stargazer(lyft_mt_gr_styczen, uber_mt_gr_styczen, taxi_mt_gr_styczen,
          type = "text", align = TRUE, style = "default", df = FALSE)


#plot(taxi_mb_styczen, which = 1)
#plot(taxi_mt_gr_styczen, which = 1)

# luty
taxi_gr_luty <- taxi_luty %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)           ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))
lyft_gr_luty <- lyft_luty %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)           ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))
uber_gr_luty <- uber_luty %>%
  mutate(
    hour_gr = case_when(
      hour %in% 6:15             ~ "6-15",
      hour %in% 16:19            ~ "16-19",
      hour %in% c(20:23, 0:5)           ~ "20-5",
      TRUE                       ~ NA_character_),
    day_gr = case_when(
      weekday %in% 0:4         ~ "robocze",
      weekday %in% 5:6         ~ "weekend",
      TRUE                     ~ NA_character_
    ))

lyft_mt_gr_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(day_gr) + 
                     as.factor(hour_gr),
                   data = lyft_gr_luty)
resettest(lyft_mt_gr_luty, power = 2:3, type = "fitted")

uber_mt_gr_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(day_gr) + 
                     as.factor(hour_gr),
                   data = uber_gr_luty)
resettest(uber_mt_gr_luty, power = 2:3, type = "fitted")

taxi_mt_gr_luty <- lm(total_amount ~ distance + trip_time_mins +
                     as.factor(day_gr) + 
                     as.factor(hour_gr),
                   data = taxi_gr_luty)
resettest(taxi_mt_gr_luty, power = 2:3, type = "fitted")

stargazer(lyft_mt_gr_luty, uber_mt_gr_luty, taxi_mt_gr_luty,
          type = "text", align = TRUE, style = "default", df = FALSE)


#plot(taxi_mb_luty, which = 1)
#plot(taxi_mt_gr_luty, which = 1)

# ODDZIELNE MODELE DLA DNI

# Taxi 

pon <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 0))
resettest(pon, power = 2:3, type = "fitted")
wt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
sr <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 2))
resettest(sr, power = 2:3, type = "fitted")
czw <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 3))
resettest(czw, power = 2:3, type = "fitted")
pt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 4))
resettest(pt, power = 2:3, type = "fitted")
sob <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 5))
resettest(sob, power = 2:3, type = "fitted")
niedz <- lm(total_amount ~ distance + trip_time_mins +
              as.factor(hour_gr) +
              I(distance**2) +
              I(distance**3) +
              I(trip_time_mins**2) +
              I(trip_time_mins**3) +
              distance:trip_time_mins +
              as.factor(hour_gr):trip_time_mins +
              as.factor(hour_gr):distance,
            data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 6))
resettest(niedz, power = 2:3, type = "fitted")

stargazer(pon, wt, sr, czw, pt, sob, niedz,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Lyft

pon <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 0))
resettest(pon, power = 2:3, type = "fitted")
wt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
sr <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 2))
resettest(sr, power = 2:3, type = "fitted")
czw <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 3))
resettest(czw, power = 2:3, type = "fitted")
pt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 4))
resettest(pt, power = 2:3, type = "fitted")
sob <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 5))
resettest(sob, power = 2:3, type = "fitted")
niedz <- lm(total_amount ~ distance + trip_time_mins +
              as.factor(hour_gr) +
              I(distance**2) +
              I(distance**3) +
              I(trip_time_mins**2) +
              I(trip_time_mins**3) +
              distance:trip_time_mins +
              as.factor(hour_gr):trip_time_mins +
              as.factor(hour_gr):distance,
            data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 6))
resettest(niedz, power = 2:3, type = "fitted")

stargazer(pon, wt, sr, czw, pt, sob, niedz,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Uber

pon <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 0))
resettest(pon, power = 2:3, type = "fitted")
wt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
sr <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 2))
resettest(sr, power = 2:3, type = "fitted")
czw <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 3))
resettest(czw, power = 2:3, type = "fitted")
pt <- lm(total_amount ~ distance + trip_time_mins +
           as.factor(hour_gr) +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins +
           as.factor(hour_gr):trip_time_mins +
           as.factor(hour_gr):distance,
         data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 4))
resettest(pt, power = 2:3, type = "fitted")
sob <- lm(total_amount ~ distance + trip_time_mins +
            as.factor(hour_gr) +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins +
            as.factor(hour_gr):trip_time_mins +
            as.factor(hour_gr):distance,
          data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 5))
resettest(sob, power = 2:3, type = "fitted")
niedz <- lm(total_amount ~ distance + trip_time_mins +
              as.factor(hour_gr) +
              I(distance**2) +
              I(distance**3) +
              I(trip_time_mins**2) +
              I(trip_time_mins**3) +
              distance:trip_time_mins +
              as.factor(hour_gr):trip_time_mins +
              as.factor(hour_gr):distance,
            data = uber_gr_luty %>% filter(uber_gr_luty$weekday == 6))
resettest(niedz, power = 2:3, type = "fitted")

stargazer(pon, wt, sr, czw, pt, sob, niedz,
          type = "text", align = TRUE, style = "default", df = FALSE)

  # Base fare

pon <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 0))
resettest(pon, power = 2:3, type = "fitted")
wt <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
sr <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 2))
resettest(sr, power = 2:3, type = "fitted")
czw <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 3))
resettest(czw, power = 2:3, type = "fitted")
pt <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 4))
resettest(pt, power = 2:3, type = "fitted")
sob <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 5))
resettest(sob, power = 2:3, type = "fitted")
niedz <- lm(base_fare ~ distance + trip_time_mins +
              I(distance**2) +
              I(distance**3) +
              I(trip_time_mins**2) +
              I(trip_time_mins**3) +
              distance:trip_time_mins,
            data = taxi_gr_luty %>% filter(taxi_gr_luty$weekday == 6))
resettest(niedz, power = 2:3, type = "fitted")

# Lyft

pon <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 0))
resettest(pon, power = 2:3, type = "fitted")
wt <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
sr <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 2))
resettest(sr, power = 2:3, type = "fitted")
czw <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 3))
resettest(czw, power = 2:3, type = "fitted")
pt <- lm(base_fare ~ distance + trip_time_mins +
           I(distance**2) +
           I(distance**3) +
           I(trip_time_mins**2) +
           I(trip_time_mins**3) +
           distance:trip_time_mins,
         data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 4))
resettest(pt, power = 2:3, type = "fitted")
sob <- lm(base_fare ~ distance + trip_time_mins +
            I(distance**2) +
            I(distance**3) +
            I(trip_time_mins**2) +
            I(trip_time_mins**3) +
            distance:trip_time_mins,
          data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 5))
resettest(sob, power = 2:3, type = "fitted")
niedz <- lm(base_fare ~ distance + trip_time_mins +
              I(distance**2) +
              I(distance**3) +
              I(trip_time_mins**2) +
              I(trip_time_mins**3) +
              distance:trip_time_mins,
            data = lyft_gr_styczen %>% filter(lyft_gr_styczen$weekday == 6))
resettest(niedz, power = 2:3, type = "fitted")

###

tmp = read.csv("C:\\Users\\stas\\Desktop\\Moje foldery\\STUDIA\\3_rok\\licencjat\\statystyka_opisowa\\wszystkie_do_statOp_styczen.csv")
tmp <- tmp %>% filter(provider == "Lyft")

tmp_copy <- tmp
tmp <- tmp_copy
# tmp <- tmp %>%
#   mutate(
#     hour_gr = case_when(
#       hour %in% 6:15             ~ "6-15",
#       hour %in% 16:19            ~ "16-19",
#       hour %in% c(20:23, 0:5)    ~ "20-5",
#       TRUE                       ~ NA_character_),
#     day_gr = case_when(
#       weekday %in% 0:4         ~ "robocze",
#       weekday %in% 5:6         ~ "weekend",
#       TRUE                     ~ NA_character_
#     ))

tmp <- tmp %>%
  mutate(
    hour_gr = case_when(
      hour %in% 1:6             ~ "1-6",
      hour %in% 7:10            ~ "7-10",
      hour %in% 11:15           ~ "11-15",
      hour %in% 16:19           ~ "16-19",
      hour %in% c(20:23, 0)     ~ "20-0",
      TRUE                       ~ NA_character_))

wt1 <- lm(base_fare ~ distance + trip_time +
           I(distance**2) +
           I(distance**3) +
           I(trip_time**2) +
           I(trip_time**3) +
           distance:trip_time +
           as.factor(hour_gr) +
           as.factor(hour_gr):distance +
           as.factor(hour_gr):trip_time,
         data = tmp %>% filter(tmp$weekday == 1))
resettest(wt1, power = 2:3, type = "fitted")
summary(wt1)

tmp2 = read.csv("C:\\Users\\stas\\Desktop\\Moje foldery\\STUDIA\\3_rok\\ekonometria\\lyft_do_modelu.csv")

tmp2 <- tmp2 %>%
  mutate(
    hour_gr = case_when(
      hour %in% 1:6             ~ "1-6",
      hour %in% 7:10            ~ "7-10",
      hour %in% 11:15           ~ "11-15",
      hour %in% 16:19           ~ "16-19",
      hour %in% c(20:23, 0)     ~ "20-0",
      TRUE                       ~ NA_character_))

wt <- lm(base_passenger_fare ~ trip_miles +
           trip_time +
           I(trip_miles**2) + 
           I(trip_miles**3) +
           I(trip_time**2) + 
           I(trip_time**3) +
           trip_time:trip_miles +
           as.factor(hour_gr) +
           as.factor(hour_gr):trip_time +
           as.factor(hour_gr):trip_miles,
         data = tmp2 %>% filter(tmp2$weekday == 1))
resettest(wt, power = 2:3, type = "fitted")
summary(wt)

stargazer(wt, wt1,
          type = "text", align = TRUE, style = "default", df = FALSE, omit = NULL)



