library(DAAG)
library(ggplot2)
library(nlstools)

#Zad 1

head(litters)

litters.lm <- lm(brainwt ~ bodywt + lsize, data = litters) # tworzymy modle liniowy
vif(litters.lm) #liczymy rozdęcie

#zad 2

library(carData)

pop.ss <- nls(population ~ SSlogis(year, a, b, c), data=USPop)
summary(pop.ss)
broom::tidy(pop.ss)
ggplot(USPop,aes( x =  year, y = population)) +
  geom_point() +
  stat_function(fun = function(x) coef(pop.ss)[1]/( 1 + exp((coef(pop.ss)[2] - x)/ coef(pop.ss)[3])), col = 'red')

plot(nlsResiduals(pop.ss))
test.nlsResiduals(nlsResiduals(pop.ss))

#zad 3 podobnie

library(drc)
library(ggplot2)
heartrate.lr <- nls(pressure ~ SSlogis(rate , a , b , c), data = heartrate)
broom::tidy(heartrate.lr)

ggplot(heartrate,aes( x =  rate, y = pressure)) +
  geom_point() +
  stat_function(fun = function(x) coef(heartrate.lr)[1]/( 1 + exp((coef(heartrate.lr)[2] - x)/ coef(heartrate.lr)[3])), col = 'red')

#zad 4

v <- c(0,10, 16.3, 23, 27.5, 31, 35.6, 39, 41.5, 42.9, 45, 46,
       45.5, 46, 49, 50)
t <- c(0:length(v))

v_name <- "speed"
y_name <- "time"

require(reshape2)
df <- melt(data.frame(v,t))
colnames(df) <- c(v_name,y_name)
print(df)

ggplot(data.frame(v) , aes(x = v , y = t)) +
  geom_point() + 
  geom_smooth()
