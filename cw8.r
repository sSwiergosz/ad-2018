
library("MASS")
library(dplyr)
library(ggplot2)

#PONIŻEJ MOJE PIZDOWATE WYPOCINY
# 3 TYGODNIE PICIA ZROBIŁY SWOJE

painters
var(painters)
painters.pca <- prcomp(painters, scale = TRUE)

#DALEJ SPISUJE Z RZUTNICZKA HEHEHHE I CO MI ZROBIĄ

#JESZCZE MIKI SKURWESYN APPEARS

#______________$$$$$$$
# _____________$$$$$$$$$
#  ____________$$$$$$$$$$$
#  ____________$$$$$$$$$$$
#  ____________$$$$$$$$$$$
#  _____________$$$$$$$$$
#  _____$$$$$$_____$$$$$$$$$$
#  ____$$$$$$$$__$$$$$$_____$$$
#  ___$$$$$$$$$$$$$$$$_________$
#  ___$$$$$$$$$$$$$$$$______$__$
#  ___$$$$$$$$$$$$$$$$_____$$$_$
#  ___$$$$$$$$$$$__________$$$_$_____$$
#  ____$$$$$$$$$____________$$_$$$$_$$$$
#  ______$$$__$$__$$$______________$$$$
#  ___________$$____$_______________$
#  ____________$$____$______________$
#  _____________$$___$$$__________$$
#  _______________$$$_$$$$$$_$$$$$
#  ________________$$____$$_$$$$$
#  _______________$$$$$___$$$$$$$$$$
#  _______________$$$$$$$$$$$$$$$$$$$$
#  _______________$$_$$$$$$$$$$$$$$__$$
#  _______________$$__$$$$$$$$$$$___$_$
#  ______________$$$__$___$$$______$$$$
#  ______________$$$_$__________$$_$$$$
#  ______________$$$$$_________$$$$_$_$
#  _______________$$$$__________$$$__$$
#  _____$$$$_________$________________$
#  ___$$$___$$______$$$_____________$$
#  __$___$$__$$_____$__$$$_____$$__$$
#  _$$____$___$_______$$$$$$$$$$$$$
#  _$_____$____$_____$$$$$$__$$$$$$$$

#DOBRA KRUCAFUX JACA PPRACA

#zad 1

data.set <- MASS::painters
diag(var(data.set[, 1:4]))
model.pca <- prcomp(data.set[, 1:4])
plot(model.pca)
summary(model.pca)
#pierwsz dwie wyjasniaja 84% wiec mozna by sie do nich ogranbiczyc
#3 pierwsze najlepsze wyjasniaja az 93 % summary cumulative orioirtion
cbind(data.frame(model.pca$x), School = data.set$School) %>% 
  as_tibble() %>% 
  ggplot(aes(x = PC1, y = PC2, col = School, label = rownames(.))) + 
  geom_text()

#zad 2

data.cars <- MASS::Cars93[-c(19,57), c(4:8, 12:15, 17:23,25)]
#jeszcze tylko 15 min...
ds.Origin <- tibble(Origin = MASS::Cars93$Origin[-c(19,57)])
ds.Type <- tibble(Type = MASS::Cars93$Type[-c(19,57)])
ds.Model <- tibble(Model = MASS::Cars93$Model[-c(19,57)])
prcomp(data.cars, scale. = TRUE)$x %>% 
  as_tibble() %>% 
  select(PC1, PC2) %>% 
  bind_cols(ds.Origin, ds.Type, ds.Model) -> new.data

new.data %>%  #dalej nie zadążyłem na ten moment
  ggplot(aes(x = PC1, y = PC2, col = Origin, label = Model)) + 
  geom_text() -> p1

new.data %>%  #dalej nie zadążyłem na ten moment
  ggplot(aes(x = PC1, y = PC2, col = Type, label = Model)) + 
  geom_text() -> p2

library(gridExtra)
grid.arrange(p1,
             p2,
             ncol = 2)


#to ^^ napisalem ja jarzombek

#RESZTA NA ZA TYDZIEN BO GRUPA SPIERDOLENIA MOZGOWEGO , 2 zadania na godzine srednia predkosc XDDDDDD

#ZNACZY SIE ZA TYDZIEN BEDZIEMY ROBIC NIE NA ZA TYDZIEN OJ NIE NIE

#ZADANIE CZWARTE JESZCZE BYŁO

#4

data.set <-factoextra::housetasks
model.ca <-ca::ca(data.set)
model.ca$rowcoord %>% 
  as_tibble() %>% 
  ggplot(aes(x = Dim1, y= Dim2, label = rownames(data.set))) +
  geom_text(col='red') +
  geom_text( data = as_tibble(model.ca$colcoord),
             aes(label =colnames(data.set)),
             col ='blue')

