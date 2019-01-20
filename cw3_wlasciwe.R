#Zad1

hist(rnorm(100,0,1))
hist(rnorm(100,0,1))


set.seed(123)#ustawia stalego seeda dla random generatora - random generator wygeneruje to samo 
hist(rnorm(100,0,1))

#Zad2

set.seed(123)
vec1 = rnorm(100,100,10) #N(100 , 10^2)
mn = mean(vec1) #srednia
dev = sd(vec1) #odchylenie standardowe
length(vec1[vec1<=100+2*10 & vec1 >=100-2*10])/length(vector)

#zad3
#REGULA regula 3 sigm
set.seed(123)
vec2 =  rnorm(200,0,1) # 200 obserwacji N(0,1)
length(vec2[vec2<=mean(vec2)+sd(vec2) & vec2 >= mean(vec2) - sd(vec2)]) / length(vec2) # dlugosc
length(vec2[vec2<=mean(vec2)+ 2 * sd(vec2) & vec2 >= mean(vec2) - 2 * sd(vec2)]) / length(vec2)
length(vec2[vec2<=mean(vec2)+ 3 * sd(vec2) & vec2 >= mean(vec2) - 3 * sd(vec2)]) / length(vec2)

#zad4

sym = function(x) {
  vec = c()#wektor do histo                rep=T oznacza ze ma byc pobiranie ze zwracaniem
  for(i in 1:x){ vec[i] = sum(sample(1:6,3,rep=T))} # dla x prob vec[i] =  suma oczek(od 1 do 6 1:6) dla 3 kostek
  hist(vec)} #fsst histogramik
set.seed(123)
sym(1000)

#zad5
bulbs <- function(){# 0 przepalona 1 dziala , prob przepalona 0.01 , dziala 0.99 , 500  zarowek , zwracanie
  vec <- sample(c(0, 1), prob=c(0.01, 0.99), size = 500, replace=TRUE)
  return(sum(vec))
}
#E
bulbs()/500 #wartosc oeczekiwana

#var:
bulbs()/500 -(bulbs()/500)^2 # wariancja


#E(X^2) = P(X^2=1)*1 + P(X^2=0)*1 = P(X^2=1) = P(X=1) = 0.99
#E(X) = P(X=1) = 0.99
#Var(X) =E(X^2) -  [E(x)]^2 = 0.99 - 0.99^2 = 0.099



#zad6
pnorm(4, mean=4.8, sd=0.4) + 1-pnorm(5.6, mean=4.8, sd=0.4)

(pnorm(4, mean=4.8, sd=0.4) + 1-pnorm(5.6, mean=4.8, sd=0.4))*50

t <- abs(4 - 4.8)/0.4
P <- 2 - 2*pnorm(t, mean=0, sd=1)
50*P < 0.5

t <- abs(6-4.8)/0.4
P <- 2 - 2*pnorm(t, mean=0, sd=1)
50*P < 0.5

#zad7
install.packages("outliers")
library("outliers")
pomiary <- c(12, 34, 22, 14, 22, 17, 24, 22, 18, 14, 18, 12)
m <- mean(pomiary)
s <- sd(pomiary)
t <- abs(34-m)/s
#Chauveneta
length(pomiary)*(2-2*pnorm(t, mean=0, sd=1))
#Dixona
dixon.test(pomiary)
grubbs.test(pomiary)



