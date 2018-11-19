library(PogromcyDanych)
library(dplyr)
library(magrittr)

# Zadanie 1
table(sapply(auta2012, class))
# sapply powoduje zaaplikowanie funkcji class, która determinuje klasę obiektu.
# table zlicza i wyświetla wynik w sposób przejrzysty

# Zadanie 2
auta2012 %>% # weź dane o autach
  group_by(Marka) %>% # pogrupuj według Marki
  summarise(n = n()) %>%  # stwórz nową tabelę, w której będą zliczenia
  arrange(desc(n)) # posortuj

# Zadanie 3
auta2012 %>%
  group_by(Rodzaj.paliwa) %>% 
  summarise(percent = n()/ nrow(auta2012) * 100)

# Zadanie 4
auta2012 %>%
  filter(Cena.w.PLN < 2000) %>%
  summarise(tansze = n())

# Zadanie 5
auta2012 %>%
  filter(Pojemnosc.skokowa >= 1500) %>%
  summarise(n() / nrow(auta2012) * 100)

# Zadanie 6
auta2012 %>%
  filter(Kraj.aktualnej.rejestracji == 'Polska') %>%
  filter(Cena.w.PLN < 2000) %>%
  summarise(n())

# Zadanie 7
auta2012 %>%
  filter(Pojemnosc.skokowa > 1500) %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  summarise(n() / nrow(auta2012) * 100)

# Zadanie 8
library(MASS)
Cars93 %>%
  filter(Type == 'Small' | Type == 'Sporty') %>%
  summarise(n())

# Zadanie 9
koty_ptaki %>%
  filter(dlugosc > 1)

# Zadanie 10
koty_ptaki %>%
  arrange(predkosc)

# Zadanie 11
auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  group_by(Rodzaj.paliwa) %>% 
  summarise(n())

# Zadanie 12
auta2012 %>%
  filter(Marka=="Volkswagen") %>% 
  na.omit() %>% 
  summarise(mean(Cena.w.PLN), mean(Przebieg.w.km))

# Zadanie 13
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(mean(Cena.w.PLN))

# Zadanie 14
auta2012 %>%
  filter(Model == "Corolla") %>%
  summarise(Q1 = quantile(Cena.w.PLN, probs = 0.25), Q2 = quantile(Cena.w.PLN, probs = 0.75))

# Zadanie 15
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(mean.price = mean(Cena.w.PLN)) %>% 
  arrange(desc(mean.price))

# Zadanie 16
auta2012 %>%
  filter(Marka == "Volkswagen", Model == "Passat", Rok.produkcji == 2006) %>%
  summarise(mean.price = mean(Cena.w.PLN), percent = sum(Cena.w.PLN < 35000) / n() * 100)

# Zadanie 17
auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(count = n()) %>%
  arrange(count)
    