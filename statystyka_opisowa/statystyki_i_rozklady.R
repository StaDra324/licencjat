library(tidyverse)
library(psych)
library(skimr)
library(patchwork)

# WCZYTANIE DANYCH
styczen = read_csv("wszystkie_do_statOp_styczen.csv")
luty = read_csv("wszystkie_do_statOp_luty.csv")
marzec = read_csv("wszystkie_do_statOp_marzec.csv")

wszystkie <- rbind(styczen, luty, marzec)

# Usunięcie niepotrzebnych zbiorów
rm(styczen, luty, marzec)

# STATYSTYKA OPISOWA

# Wyrzucamy obserwacje poniżej kwantyla 0.01 i powyżej kwantyla 0.99

p_lo <- 0.01
p_hi <- 0.99

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
    between(total_amount, lo_tot, hi_tot))

# Usuwamy niepotrzebne
rm(hi_base, hi_dist, hi_time, hi_tot, 
   lo_base, lo_dist, lo_time, lo_tot, p_hi, p_lo)

# ile zmiennych wyrzucono
tibble(
  n_przed = nrow(wszystkie),
  n_po = nrow(wszystkie_obciete),
  wyrzucone = nrow(wszystkie) - nrow(wszystkie_obciete),
  wyrzucone_proc = 
    (nrow(wszystkie) - nrow(wszystkie_obciete)) / nrow(wszystkie) * 100
)

# Statysyki opisowe

options(scipen = 999)

# oprócz kwartyli
wszystkie %>%
  select(distance, trip_time_mins, base_fare, total_amount) %>%
  describeBy(group = wszystkie$provider, mat = TRUE) %>%
  select(-c(item, vars)) %>%
  mutate(across(where(is.numeric), round, 2))

wszystkie_obciete %>%
  select(distance, trip_time_mins, base_fare, total_amount) %>% 
  describeBy(group = wszystkie_obciete$provider, mat = TRUE) %>%
  select(-c(item, vars)) %>%
  mutate(across(where(is.numeric), round, 2))

# kwartyle
#           ????????????
#             ????????????

# wszystkie %>%
#   filter(provider == "Uber") %>%
#   select(distance, trip_time_mins, base_fare, total_amount) %>%
#   skim()
# 
# wszystkie_obciete %>%
#   filter(provider == "Uber") %>%
#   select(distance, trip_time_mins, base_fare, total_amount) %>%
#   skim()
# 
# wszystkie %>%
#   filter(provider == "Taxi") %>%
#   select(distance, trip_time_mins, base_fare, total_amount) %>%
#   skim()
# 
# wszystkie_obciete %>%
#   filter(provider == "Taxi") %>%
#   select(distance, trip_time_mins, base_fare, total_amount) %>%
#   skim()

# usunięcie po kwantylach pozytywnie wpłynęło na outliery, skośność i 
#   kurtozę, a core rozkładów wydaje pozostał praktycznie nienaruszony


# Histogramy dla czasu, dystansu i ceny

# dystans
ggplot(wszystkie_obciete, aes(x = distance)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  facet_wrap(~ provider) +
  theme_minimal()

# czas
ggplot(wszystkie_obciete, aes(x = trip_time_mins)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  facet_wrap(~ provider) +
  theme_minimal()

# cena
ggplot(wszystkie_obciete, aes(x = total_amount)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  facet_wrap(~ provider) +
  theme_minimal()


# Wykresy rozproszenia (wersja 2d density)
w1 <- ggplot(wszystkie_obciete, aes(distance, total_amount)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

w2 <- ggplot(wszystkie_obciete, aes(trip_time_mins, total_amount)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

w3 <- ggplot(wszystkie_obciete, aes(distance, trip_time_mins)) +
  stat_bin2d(bins = 400) +
  facet_wrap(~provider)

w1 / w2 / w3

# Usuwamy niepotrzebne
rm(w1, w2, w3)

# Korelacje
wszystkie_obciete %>%
  group_by(provider) %>%
  summarise(
    cor_price_dist = cor(total_amount, distance, method = "spearman"),
    cor_price_time = cor(total_amount, trip_time_mins, method = "spearman"),
    cor_dist_time  = cor(distance, trip_time_mins, method = "spearman"))

# Natężenie przejazdów

# Heatmapy liczby przejazdów w dniach i godzinach

hm_wd <- wszystkie_obciete %>%
  count(provider, weekday, hour, name = "n")

hm_wd$weekday <- factor(
  hm_wd$weekday,
  levels = 0:6,
  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(hm_wd, aes(x = weekday, y = hour, fill = n)) +
  geom_tile() +
  facet_wrap(~ provider, ncol = 1) +
  scale_fill_continuous(trans = "reverse") +
  scale_y_continuous(breaks = 0:23) +
  labs(x = "weekday", y = "hour", fill = "rides") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 5))

# Histogram liczby przejazdów w godzinach w podziale na dni robocze i weekend

# dodajemy zmienną czy_weekend
wszystkie_obciete <- wszystkie_obciete %>%
  mutate(czy_weekend = if_else(weekday >= 5, "Weekend", "Robocze"))

ggplot(wszystkie_obciete, aes(x = hour)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "steelblue", color = "black") +
  facet_grid(czy_weekend ~ provider) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal()




# Eksportujemy obciete zbiory do csv

write.csv(filter(wszystkie_obciete, provider == "Taxi"),
          "taxi_do_modelu.csv", row.names = FALSE)

write.csv(filter(wszystkie_obciete, provider == "Uber"),
          "uber_do_modelu.csv", row.names = FALSE)



#
# 
# MOŻNA COŚ Z TEGO WZIĄĆ, ALE DLA UBERA NIE MA SCHEMATU...


# Heatmapy mediany cen w podziale na daty

hm_price <- wszystkie_obciete %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(provider, month, date, hour) %>%
  summarise(mean_price = mean(total_amount), .groups = "drop")

# uber
ggplot(hm_price %>% filter(provider == "Uber"), aes(x = date, y = hour, fill = mean_price)) +
  geom_tile() +
  facet_wrap(~ month, ncol = 1, scales = "free_x") +
  scale_fill_continuous(trans = "reverse") +
  scale_x_date(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  scale_y_continuous(breaks = seq(1, 23, 2)) +
  labs(fill = "Mean price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 4))

# taxi
ggplot(hm_price %>% filter(provider == "Taxi"), aes(x = date, y = hour, fill = mean_price)) +
  geom_tile() +
  facet_wrap(~ month, ncol = 1, scales = "free_x") +
  scale_fill_continuous(trans = "reverse") +
  scale_x_date(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  scale_y_continuous(breaks = 0:23) +
  labs(fill = "Mean price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 4))


# Heatmapy mediany i średniej opłat w dniach i godzinach

hm_mean <- wszystkie_obciete %>%
  group_by(provider, weekday, hour) %>%
  summarise(mean_total = mean(total_amount), .groups = "drop")

hm_mean$weekday <- factor(
  hm_mean$weekday,
  levels = 0:6,
  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(hm_mean, aes(x = weekday, y = hour, fill = mean_total)) +
  geom_tile() +
  facet_wrap(~ provider, ncol = 1) +
  scale_fill_continuous(trans = "reverse", name = "mean\ntotal_amount") +
  scale_y_continuous(breaks = 0:23) +
  labs(x = "weekday", y = "hour") +
  theme_minimal()

#
#
#


# RACZEJ DO POMINIĘCIA / POJEDYNCZE DO ANEKSU

# !!!!!!!!!!!
#   !!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!

# Funkcja do sprawdzania rozkładu do przedziałów
przedzialy <- function(df) {
  
  distance <- c(
    "< 0.3"  = sum(df$distance < 0.3),
    "0.3–3" = sum(df$distance >= 0.3 & df$distance <= 3),
    "3–15"  = sum(df$distance > 3 & df$distance <= 15),
    "> 15"   = sum(df$distance > 15)
  )
  
  trip_time_mins <- c(
    "< 3"  = sum(df$trip_time_mins < 3),
    "3–20" = sum(df$trip_time_mins >= 3 & df$trip_time_mins <= 20),
    "20–60"  = sum(df$trip_time_mins > 20 & df$trip_time_mins <= 60),
    "> 60"   = sum(df$trip_time_mins > 60)
  )
  
  base_fare <- c(
    "< 3"  = sum(df$base_fare < 3),
    "3-25" = sum(df$base_fare >= 3 & df$base_fare <= 25),
    "25–50"  = sum(df$base_fare > 25 & df$base_fare <= 50),
    "> 50"   = sum(df$base_fare > 50)
  )
  
  total_amount <- c(
    "< 6"  = sum(df$total_amount < 6),
    "6-30" = sum(df$total_amount >= 6 & df$total_amount <= 30),
    "30-70"  = sum(df$total_amount > 30 & df$total_amount <= 70),
    "> 70"   = sum(df$total_amount > 70)
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

summary(wszystkie_obciete)
describe(wszystkie_obciete)
skim(wszystkie_obciete)
przedzialy(wszystkie_obciete)


skim(filter(wszystkie, provider == "Taxi"))
skim(filter(wszystkie_obciete, provider == "Taxi"))

skim(filter(wszystkie, provider == "Uber"))
skim(filter(wszystkie_obciete, provider == "Uber"))


# Porównanie danych przed i po obcięciu

par(mfrow = c(1, 2))

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
rm(wszystkie)


# Wykresy rozproszenia (wersja 2d density)
ggplot(filter(wszystkie_obciete, provider == "Taxi"), aes(distance, base_fare)) +
  stat_bin2d(bins = 400) 

ggplot(filter(wszystkie_obciete, provider == "Lyft"), aes(distance, base_fare)) +
  stat_bin2d(bins = 400) 


# Rozkłady total_amount w godzinach

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
    linewidth = 1
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
    linewidth = 1
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
    linewidth = 1
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
    linewidth = 1
  ) +
  geom_point(
    data = weekday_base,
    aes(weekday, median_price, color = provider),
    shape = 1,      # puste kółko, zeby odroznic
    size = 2
  ) +
  
  labs(
    x = "Dzień",
    y = "Mediana ceny",
    color = "Provider"
  )



# NATĘŻENIE PRZEJAZDÓW

# Heatmapy liczby przejazdów w podziale na daty

# po providerach
hm <- wszystkie_obciete %>%
  mutate(date = as.Date(datetime)) %>%
  count(provider, month, date, hour, name = "n")

# uber
ggplot(hm %>% filter(provider == "Uber"), aes(x = date, y = hour, fill = n)) +
  geom_tile() +
  facet_wrap(~ month, ncol = 1, scales = "free_x") +
  scale_fill_continuous(trans = "reverse") +
  scale_x_date(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  scale_y_continuous(breaks = seq(1, 23, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 4))

# taxi
ggplot(hm %>% filter(provider == "Taxi"), aes(x = date, y = hour, fill = n)) +
  geom_tile() +
  facet_wrap(~ month, ncol = 1, scales = "free_x") +
  scale_fill_continuous(trans = "reverse") +
  scale_x_date(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  scale_y_continuous(breaks = 0:23) +
  #scale_y_continuous(breaks = seq(1, 23, 2))        alternatywne etykiety na y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 4))

# !!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!







