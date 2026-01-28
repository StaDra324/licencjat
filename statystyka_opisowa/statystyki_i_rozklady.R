library(tidyverse)
library(psych)
library(skimr)
library(rlang)
library(patchwork)

# WCZYTUJEMY DANE
wszystkie = read.csv("wszystkie_do_modelu.csv")
#View(wszystkie)

# STATYSTYKI OPISOWE

# Funkcja do sprawdzania rozkładu do przedziałów
przedzialy <- function(df) {
  
  distance <- c(
    "< 0.15"  = sum(df$distance < 0.15),
    "0.15–3" = sum(df$distance >= 0.15 & df$distance <= 3),
    "3–15"  = sum(df$distance > 3 & df$distance <= 15),
    "> 15"   = sum(df$distance > 15)
  )
  
  trip_time_mins <- c(
    "< 1"  = sum(df$trip_time_mins < 1),
    "1–20" = sum(df$trip_time_mins >= 1 & df$trip_time_mins <= 20),
    "20–60"  = sum(df$trip_time_mins > 20 & df$trip_time_mins <= 60),
    "> 60"   = sum(df$trip_time_mins > 60)
  )
  
  base_fare <- c(
    "< 3"  = sum(df$base_fare < 3),
    "3-25" = sum(df$base_fare >= 3 & df$base_fare <= 25),
    "25–90"  = sum(df$base_fare > 25 & df$base_fare <= 90),
    "> 90"   = sum(df$base_fare > 90)
  )
  
  total_amount <- c(
    "< 6"  = sum(df$total_amount < 6),
    "6-30" = sum(df$total_amount >= 6 & df$total_amount <= 30),
    "30-105"  = sum(df$total_amount > 30 & df$total_amount <= 105),
    "> 105"   = sum(df$total_amount > 105)
  )
  
  list(
    distance = distance,
    trip_time_mins = trip_time_mins,
    base_fare = base_fare,
    total_amount = total_amount
  )
}

summary(wszystkie)
describe(wszystkie)
skim(wszystkie)
przedzialy(wszystkie)

# Wyrzucamy obserwacje poniżej kwantyla 0.001 i powyżej kwantyla 0.999
p_lo <- 0.001
p_hi <- 0.999

# progi dla zmiennych
lo_dist <- quantile(wszystkie$distance, p_lo)
hi_dist <- quantile(wszystkie$distance, p_hi)

lo_time <- quantile(wszystkie$trip_time_mins, p_lo)
hi_time <- quantile(wszystkie$trip_time_mins, p_hi)

lo_base <- quantile(wszystkie$base_fare, p_lo)
hi_base <- quantile(wszystkie$base_fare, p_hi)

lo_tot <- quantile(wszystkie$total_amount, p_lo)
hi_tot <- quantile(wszystkie$total_amount, p_hi)

wszystkie_obciete <- wszystkie %>%
  filter(
    between(distance, lo_dist, hi_dist),
    between(trip_time_mins, lo_time, hi_time),
    between(base_fare, lo_base, hi_base),
    between(total_amount, lo_tot, hi_tot)
  )

# ile zmiennych wyrzucono
tibble(
  n_przed = nrow(wszystkie),
  n_po = nrow(wszystkie_obciete),
  wyrzucone = nrow(wszystkie) - nrow(wszystkie_obciete),
  wyrzucone_proc = 
    (nrow(wszystkie) - nrow(wszystkie_obciete)) / nrow(wszystkie) * 100
)

summary(wszystkie_obciete)
describe(wszystkie_obciete)
skim(wszystkie_obciete)
przedzialy(wszystkie_obciete)

# przed <- przedzialy(wszystkie)
# po    <- przedzialy(wszystkie_obciete)
# list(przed = przed, po = po)

options(scipen = 999)
desc1 <- describeBy(wszystkie, group = wszystkie$provider, mat = TRUE)
desc1 %>% mutate(across(where(is.numeric), round, 2))
desc2 <- describeBy(wszystkie_obciete, group = wszystkie_obciete$provider, mat = TRUE)
desc2 %>% mutate(across(where(is.numeric), round, 2))

skim(filter(wszystkie, provider == "Taxi"))
skim(filter(wszystkie_obciete, provider == "Taxi"))

skim(filter(wszystkie, provider == "Uber"))
skim(filter(wszystkie_obciete, provider == "Uber"))

skim(filter(wszystkie, provider == "Lyft"))
skim(filter(wszystkie_obciete, provider == "Lyft"))
# usunięcie po percentylach pozytywnie wpłynęło na outliery, skośność i 
#   kurtozę, ale core rozkładów wydaje się że został


# WIZUALIZACJA DANYCH

par(mfrow = c(1, 2))

# Porównania zmiennych
# porównanie total_amount
boxplot(total_amount ~ provider, data = wszystkie,
        ylim = c(0, 120),
        main = "Total_amount przed obcieciem")

boxplot(total_amount ~ provider, data = wszystkie_obciete,
        ylim = c(0, 120),
        main = "Total_amount po obcieciu")

# porównanie base_fare
boxplot(base_fare ~ provider, data = wszystkie,
        ylim = c(0, 100),
        main = "Base_fare przed obcieciem")

boxplot(base_fare ~ provider, data = wszystkie_obciete,
        ylim = c(0, 100),
        main = "Base_fare po obcieciu")

# porównanie distance
boxplot(distance ~ provider, data = wszystkie,
        ylim = c(0, 15),
        main = "Distance przed obcieciem")

boxplot(distance ~ provider, data = wszystkie_obciete,
        ylim = c(0, 15),
        main = "Distance po obcieciu")

# porównanie trip_time_mins
boxplot(trip_time_mins ~ provider, data = wszystkie,
        ylim = c(0, 60),
        main = "Trip_time_mins przed obcieciem")

boxplot(trip_time_mins ~ provider, data = wszystkie_obciete,
        ylim = c(0, 60),
        main = "Trip_time_mins po obcieciu")
# ogólnie boxploty potwierdzają, że obcięcie dużo dało, rozkłady praktycznie 
#   się nie zmieniły, a prawie wszystkie wartości teraz mają sens praktyczny

# Usuwamy niepotrzebne rzeczy z pamięci
rm(desc1, desc2, wszystkie, hi_base, hi_dist, 
   hi_time, hi_tot, lo_base, lo_dist, lo_time, lo_tot, p_hi, p_lo)

# Wykresy rozproszenia (wersja 2d density)
w1 <- ggplot(wszystkie_obciete, aes(distance, total_amount)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

w2 <- ggplot(wszystkie_obciete, aes(trip_time_mins, total_amount)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

w1 / w2

ggplot(wszystkie_obciete, aes(distance, trip_time_mins)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

# Korelacje
wszystkie_obciete %>%
  group_by(provider) %>%
    summarise(
      cor_price_dist = cor(total_amount, distance, method = "spearman"),
      cor_price_time = cor(total_amount, trip_time_mins, method = "spearman"),
      cor_dist_time  = cor(distance, trip_time_mins, method = "spearman")
    )

# wszystkie_obciete %>%
#   group_by(provider) %>%
#   group_map(~ {
#     cor(.x[, c("total_amount","distance","trip_time_mins")],
#         method = "spearman")
#   })

# Rozkłady w godzinach
# lyft w godzinach
boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Lyft"),
        main = "Lyft w godzinach")
        
boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Lyft"),
        xlab = "Godzina", ylab = "Total amount",
        ylim = c(0, 40))

# uber w godzinach
boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Uber"),
        main = "Uber w godzinach")

boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Uber"),
        xlab = "Godzina", ylab = "Total amount",
        ylim = c(0, 40))

# taxi w godzinach
boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Taxi"),
        main = "Taxi w godzinach")

boxplot(total_amount ~ hour, 
        data = wszystkie_obciete %>% filter(provider == "Taxi"),
        xlab = "Godzina", ylab = "Total amount",
        ylim = c(0, 40))

# porównanie median w godzinach między providerami
hour_total <- wszystkie_obciete %>%
  group_by(provider, hour) %>%
  summarise(median_price = median(total_amount), .groups = "drop")

hour_base <- wszystkie_obciete %>%
  group_by(provider, hour) %>%
  summarise(median_price = median(base_fare), .groups = "drop")

ggplot() +
  # total_amount – linia ciagla + punkty
  geom_line(
    data = hour_total,
    aes(hour, median_price, color = provider),
    size = 1
  ) +
  geom_point(
    data = hour_total,
    aes(hour, median_price, color = provider),
    size = 2
  ) +
  # base_fare – linia przerywana + punkty (ten sam kolor!)
  geom_line(
    data = hour_base,
    aes(hour, median_price, color = provider),
    linetype = "dashed",
    size = 1
  ) +
  geom_point(
    data = hour_base,
    aes(hour, median_price, color = provider),
    shape = 1,      # puste kółko, zeby odroznic
    size = 2
  ) +
  
  labs(
    x = "Godzina",
    y = "Mediana ceny",
    color = "Provider"
  )

# Rozkłady w dniach
# lyft w dniach
boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Lyft"),
        main = "Lyft w dniach")

boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Lyft"),
        xlab = "Dzień", ylab = "Total amount",
        ylim = c(0, 40))

# uber w dniach
boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Uber"),
        main = "Uber w dniach")

boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Uber"),
        xlab = "Dzień", ylab = "Total amount",
        ylim = c(0, 40))

# taxi w dniach
boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Taxi"),
        main = "Taxi w dniach")

boxplot(total_amount ~ weekday, 
        data = wszystkie_obciete %>% filter(provider == "Taxi"),
        xlab = "Dzień", ylab = "Total amount",
        ylim = c(0, 40))

# porównanie median w dniach między providerami
weekday_total <- wszystkie_obciete %>%
  group_by(provider, weekday) %>%
  summarise(median_price = median(total_amount), .groups = "drop")

weekday_base <- wszystkie_obciete %>%
  group_by(provider, weekday) %>%
  summarise(median_price = median(base_fare), .groups = "drop")

ggplot() +
  # total_amount – linia ciagla + punkty
  geom_line(
    data = weekday_total,
    aes(weekday, median_price, color = provider),
    size = 1
  ) +
  geom_point(
    data = weekday_total,
    aes(weekday, median_price, color = provider),
    size = 2
  ) +
  # base_fare – linia przerywana + punkty (ten sam kolor!)
  geom_line(
    data = weekday_base,
    aes(weekday, median_price, color = provider),
    linetype = "dashed",
    size = 1
  ) +
  geom_point(
    data = weekday_base,
    aes(weekday, median_price, color = provider),
    shape = 1,      # puste kółko, zeby odroznic
    size = 2
  ) +
  
  labs(
    x = "Godzina",
    y = "Mediana ceny",
    color = "Provider"
  )

# Heatmapy mdediany opłat w dniach i godzinach
heat_data <- wszystkie_obciete %>%
  group_by(provider, weekday, hour) %>%
  summarise(median_total = median(total_amount), .groups = "drop")

heat_data <- heat_data %>%
  mutate(weekday = factor(weekday,
                          levels = 0:6,
                          labels = c("Pon","Wt","Sr","Czw","Pt","Sob","Nd")))

ggplot(heat_data, aes(x = factor(weekday), y = hour, fill = median_total)) +
  geom_tile() +
  facet_wrap(~ provider) +
  scale_fill_viridis_c(name = "Mediana\nTotal amount") +
  labs(
    x = "Dzien tygodnia",
    y = "Godzina",
    title = "Mediana ceny przejazdu wedlug dnia tygodnia i godziny"
  ) +
  theme_minimal()

# Eksportujemy obciete zbiory do csv
write.csv(wszystkie_obciete,
  "wszystkie_do_modelu.csv",
  row.names = FALSE)
write.csv(filter(wszystkie_obciete, provider == "Taxi"),
          "taxi_do_modelu.csv", row.names = FALSE)

write.csv(filter(wszystkie_obciete, provider == "Uber"),
          "uber_do_modelu.csv", row.names = FALSE)

write.csv(filter(wszystkie_obciete, provider == "Lyft"),
          "lyft_do_modelu.csv", row.names = FALSE)



# losujemy próbkę 
# set.seed(1) 
# s <- wszystkie_obciete %>% 
#        group_by(provider) %>% 
#          slice_sample(prop = 0.01) %>%
#            ungroup()







