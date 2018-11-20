library(PogromcyDanych)
setLang("eng")
library(ggplot2)
library(gridExtra)

#Zad1

curve(x^2)

cars %>% 
  ggplot(aes(x = speed, y = dist)) + geom_point() + geom_smooth(method = "lm",se=FALSE, formula = y ~ x^2) 

=
#zad2

library(MASS)
  
p1 = hills %>% 
  ggplot(aes(x = time , y = dist)) + geom_point() + geom_smooth(method = "lm",se=FALSE, formula = y ~ x^2)
p2 = hills %>% 
  ggplot(aes(x = time , y = climb)) + geom_point() + geom_smooth(method = "lm",se=FALSE, formula = y ~ x^2)

grid.arrange(p1,p2)
