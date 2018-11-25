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

plot(nlsResiduals(heartrate.lr))
test.nlsResiduals(nlsResiduals(heartrate.lr))

#zad 4

v <- c(10, 16.3, 23, 27.5, 31, 35.6, 39, 41.5, 42.9, 45, 46,
       45.5, 46, 49, 50)
t <- c(1:(length(v)))

length(v)
t


v_name <- "speed"
y_name <- "time"

require(reshape2)
df <- data.frame(v,t)
colnames(df) <- c(v_name,y_name)
print(df)

df.md = nls(speed ~ SSmicmen(time, a, b), data = df)
#broom::tidy(df.md)

data.frame(time = 18)

predict(df.md, new_data = data.frame(time = 500))

ggplot(df, aes( x = df$time, y =speed)) + 
  geom_point() +
  stat_function(fun = function(x) (coef(df.md)[1] * x)/((coef(df.md)[2] + x)), col = 'red')
coef(df.md)[2]


