#Zad 1

# różnica między as.Date oraz strptime - http://rfunction.com/archives/1912
test1 <- as.Date("1901-01-01", tz="UTC")
test2 <- strptime("2000-12-31", format="%Y-%m-%d", tz="UTC")
difftime(as.POSIXct(test2), as.POSIXct(test1, tz="UTC"), units="days")

# 2gi sposób
d1 <- as.Date('1901-01-01')
d2 <- as.Date('2000-12-31')
round(difftime(d2, d1, units = 'days'))

#Zad 2
#install.packages('TSA')
library('TSA')
library(ggplot2)
library(dplyr)
library(stats) # Shapiro-Wilk

data(wages) # load into environment
plot(wages)

wages
wages.lm <- lm(wages ~ time(wages))

data.frame(x = as.numeric(time(wages)) ,  y = as.numeric(wages)) %>% 
  ggplot(aes(x , y ), data =  .) + 
  geom_line() + 
  geom_smooth(method = 'lm', col = 'red', se = FALSE) + 
  geom_smooth(method =  'lm', col = 'blue' , formula = y ~ I(x^2)  + x  , se = FALSE)
  
model.lm = lm( y ~ x, data = data.frame(x = as.numeric(time(wages)) ,  y = as.numeric(wages)))
model.q = lm(y ~ I(x^2)  + x  ,  data = data.frame(x = as.numeric(time(wages)) ,  y = as.numeric(wages)))

AIC(model.lm , model.q)
BIC(model.lm , model.q)
# Należy wybrać model.q, ponieważ najlepszym modelem jest ten, dla którego wartość
# kryterium informacyjnego (AIC, BIC) jest najniższa.

# Standaryzowane residua
par(mfrow = c(1,2))
model.lm.stdres = rstandard(model.lm)
model.q.stdres = rstandard(model.q)
plot(model.lm.stdres)
plot(model.q.stdres)

# Normalność reszt
shapiro.test(model.lm.stdres)
shapiro.test(model.q.stdres)
# Należy odrzucić model liniowy, ponieważ wartość pvalue < 0.05

#Zad 3

# Inaczej forecast nie chciało zainstalować RcppArmadillo
#install.packages("https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/RcppArmadillo_0.6.100.0.0.tar.gz", repos=NULL, type="source")
#install.packages("forecast")
library(forecast)
library(dplyr)
library(ggplot2)
library(auto)
par(mfrow = c(1,1))
my_data <- read.delim("female.txt", header=FALSE)
Female <- ts(data=my_data, start = c(1961, 7), frequency = 12)
ts.plot(Female)



# Średnia ruchoma rzędu 17
lines(ma(Female, order=17), col="red")

auto.model <- auto.arima(Female)
summary(auto.model)


#z �wicze� :

female <- read.table("female.txt")
female <- ts(female, frequency = 12, start = c(1961 ,7))
female.filter <- stats::filter(female, rep(1/17,17))
tibble(Time =as.vector(time(female)),
       Female = as.numeric(female),
       Filter = as.numeric(female.filter)) %>% 
  ggplot(aes(x = Time, y = Female)) +
  geom_line() + 
  geom_line(aes(y=Filter), col = 'red', size = 1.2)
auto.arima(female)


# Zad4 
unemp_data <- read.delim("unemp.txt", header=FALSE)
unemp <- ts(data=unemp_data, start = c(1975, 7), frequency = 12)
plot(unemp)
lines(ma(unemp, order=12), col="red")
lines(HoltWinters(unemp, beta=FALSE)$fitted[,1], col = "blue")

auto.model <- auto.arima(unemp)
summary(auto.model)

unemp_data2 <- read.table("unemp.txt")
unemp2 <- ts(data=unemp_data2, start = c(1975, 7), frequency = 12)
unemp2.filter <- stats::filter(unemp2, rep(1/12,12))
unemp2.hw <- c(rep(NA,12), unclass(HoltWinters(unemp2)$fitted)[,1])
tibble(Time =as.vector(time(unemp2)),
       Unemployed = as.numeric(unemp2),
       Filter = as.numeric(unemp2.filter),
       HW = unemp2.hw) %>% 
  ggplot(aes(x = Time, y = Unemployed)) +
  geom_line() + 
  geom_line(aes(y=Filter), col = 'red', size = 1.2) +
  geom_line(aes(y = HW), col = 'blue')
auto.arima(unemp2)

# zad 5

# arma 2,1 // 2 wstecz y , 1 wstecz b��d
# ar 2 // 2 wstecz y , 0 wstecza b��d
# arma 2,2 // 2 wstecz y , 2 wstecz b��d



# Zad6 Stationarity
# https://www.matematyka.pl/25578.htm - jak tworzyc równania charakterystyczne
abs(polyroot(c(1, -3/2, 1/2))) # nie jest stacjonarny - pierwiastki 1 i 2
abs(polyroot(c(1, -5/6, 1/6))) # jest stacjonarny - pierwiastki 2 i 3
abs(polyroot(c(1, -2/3, 5/3))) # nie jest stacjonarny - pierwiastki 0.7745967 i 0.7745967

# Zad7
library(TSA)
library(forecast)

data(robot)
robot.ts <- ts(data=robot)
ts.plot(robot.ts) # nie jest stacjonarny -> opczaj wyklad z tego (s22-24) dobry chlopaku <-

auto.ar <- arima(x=robot.ts, order = c(1, 0, 0))
auto.arima <- arima(x=robot.ts, order = c(0, 1, 1))
summary(auto.ar)
summary(auto.arima)

AIC(auto.ar, auto.arima)
# Należy wybrać auto.arima, ponieważ najlepszym modelem jest ten, dla którego wartość
# kryterium informacyjnego (AIC) jest najniższa

prediction <- predict(auto.arima, n.ahead=5)
prediction



# Zadanie 8
#install.packages("TSA")
library(TSA)
data(gold)

gold.ts <- ts(data=gold)
ts.plot(gold.ts)

# Differences in logs
log.diff <- diff(log(gold.ts))
ts.plot(log.diff)
