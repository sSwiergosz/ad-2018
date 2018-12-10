#Zad 1

test1 <- as.Date("1901-01-01", tz="UTC")
test2 <- strptime("2000-12-31", format="%Y-%m-%d", tz="UTC")
difftime(as.POSIXct(test2), as.POSIXct(test1, tz="UTC"), units="days")

d1 <- as.Date('1901-01-01')
d2 <- as.Date('2000-12-31')
round(difftime(d2, d1, units = 'days'))

#Zad 2

library('TSA')
library(ggplot2)
library(dplyr)

data(wages)
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

par(mfrow = c(1,2))

#zad 3
