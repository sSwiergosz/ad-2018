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

#zad4
library(data.table)
price <- c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559)
rooms <- c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5)
data.city <- data.table(x=rooms, y=price)

ggplot(data.city, aes(x=data.city$x, y=data.city$y)) + geom_point() + 
  labs(x="Rooms", y="Price") +
  geom_smooth(method = "lm", se=FALSE, formula = y ~ x^2)
model.lm <- lm(y ~ x, data = data.city)
summary(model.lm)
predict(model.lm, newdata = data.frame(x = 2), se = TRUE)$fit # Prediction

#zad5
install.packages("plotly")
library(plotly)
library(UsingR)

ggplot(florida, aes(x=BUSH, y=BUCHANAN)) + 
  geom_point()
ggplotly()

# remove outliers
florida <- florida[florida$BUSH != 152846 & florida$BUCHANAN != 3407, ]
florida <- florida[florida$BUSH != 289456 & florida$BUCHANAN != 561, ]

model.lm <- lm(BUCHANAN ~ BUSH, data = florida)
summary(model.lm)
predict(model.lm, newdata = data.frame(BUSH = 289456)) # Prediction
