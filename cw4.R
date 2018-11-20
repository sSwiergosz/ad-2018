library(PogromcyDanych)
setLang("eng")
library(ggplot2)
library(gridExtra)

#Zad1
cars %>% 
  ggplot(aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE, formula = y ~ x^2) + 
  geom_smooth(method = "lm", se=FALSE, formula = y ~ x + I(x^2), color = "red")

#zad2
library(MASS)
  
p1 = hills %>% 
  ggplot(aes(x = time , y = dist)) + geom_point() + geom_smooth(method = "lm",se=FALSE, formula = y ~ x^2)
p2 = hills %>% 
  ggplot(aes(x = time , y = climb)) + geom_point() + geom_smooth(method = "lm",se=FALSE, formula = y ~ x^2)

grid.arrange(p1,p2)

#zad3
install.packages("UsingR", dependencies=TRUE)
library(UsingR)

x <- homedata$y1970
y <- homedata$y2000
data.set <- data.frame(x, y)
model.lm <- lm(y ~ x, data = data.set)
summary(model.lm)
predict(model.lm, newdata = data.frame(x = 75000), se = TRUE) # Prediction
