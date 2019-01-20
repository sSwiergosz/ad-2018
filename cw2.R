setLang(lang = 'eng')

library(PogromcyDanych)
library(magrittr)

#install.packages('gridExtra')
library(gridExtra)

# Zadanie1
ggplot(cats_birds, aes(x=length, y=speed, color=group, shape=group)) + 
  geom_point() + #rysowanie punkt??w
  geom_smooth(method = "lm") #prosta regresji

# Zadanie2
ggplot(pearson, aes(x=father, y=son)) + 
  geom_point() +
  geom_smooth(method = "lm" , level=0) #level 0 zeby nie bylo przedzialu ufnosci (ten taki szary cien)

# Zadanie3
ggplot(seriesIMDB, aes(x=note, y=name)) + 
  geom_boxplot()# duzow filmow , dlug sie laduje

# Zadanie4
ggplot(diagnosis, aes(x = eduk4_2013, y=gp29, fill = gp29)) +
  geom_bar(stat = "identity") #cos nie tak

# Zadanie5
auta2012 %>% 
  filter(Brand == "Volkswagen", Model == "Passat") %>% 
  ggplot(aes(Year, Price.in.PLN)) +  
  geom_smooth(method = "lm")
  
# Zadanie6
ggplot(cats_birds, aes(x = weight, y = speed, size = lifespan, color = lifespan)) + 
  geom_point(shape=15) +
  scale_color_gradient(low = "Green" , high = "Red") + #dodanie gradientu od zielonego do czerwo
  scale_shape_manual(labels = c('Angry birds', 'Big cats'), name = 'Animals') 

# Zadanie7
#wykres kolowy
auta2012 %>%
  filter(Brand == "Toyota") %>% #wybierz tylko toyota
  group_by(Model) %>% # pogrupuj po moedl
  summarise(n = n()) %>% # podsumuj licznosc kazdego modelu
  top_n(5,n) %>% #wybierz top 5
  ggplot(aes(x="", y = n , fill = Model )) + # nie ma x bo wykres kolowy
  geom_bar(width = 1, stat = "identity") + #daje slupki z ktorych robimy se kolowy 
  coord_polar("y", start=0) + # daje kolowocs wykresu
  theme_void() # usuwa szare t??o

auta2012 %>% 
  filter(Brand == "Toyota") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  top_n(5,n) %>% 
  ggplot(aes(x=Model, y = n , fill = Model , group = Model)) +
  geom_bar(width = 1, stat = "identity") 

# Zadanie8
f <-ggplot(pearson, aes(x=father, y=..density..)) + # density daje nieparametryczna ocene gestosci
  geom_histogram(colour="white") +
  geom_density() +
  theme_bw()

s <- ggplot(pearson, aes(x=son, y=..density..)) +
  geom_histogram(colour="white") +
  geom_density() +
  theme_bw()

grid.arrange(f, s, ncol=2) # grid arrange do podania dwoch wykresow na jednym

