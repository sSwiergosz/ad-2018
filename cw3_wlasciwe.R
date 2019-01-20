#Zad1

hist(rnorm(100,0,1))
hist(rnorm(100,0,1))


set.seed(123)
hist(rnorm(100,0,1))

#Zad2

set.seed(123)
vec1 = rnorm(100,100,10)
mn = mean(vec1)
dev = sd(vec1)
length(vec1[vec1<=100+2*10 & vec1 >=100-2*10])/length(vector)

#zad3

set.seed(123)
vec2 =  rnorm(200,0,1)
length(vec2[vec2<=mean(vec2)+sd(vec2) & vec2 >= mean(vec2) - sd(vec2)]) / length(vec2)
length(vec2[vec2<=mean(vec2)+ 2 * sd(vec2) & vec2 >= mean(vec2) - 2 * sd(vec2)]) / length(vec2)
length(vec2[vec2<=mean(vec2)+ 3 * sd(vec2) & vec2 >= mean(vec2) - 3 * sd(vec2)]) / length(vec2)

#zad4

sym = function(x) {
  vec = c()
  for(i in 1:x){ vec[i] = sum(sample(1:6,3,rep=T))}
  hist(vec)}
set.seed(123)
sym(1000)

#zad5

lights = function()
{
  sample(c(1,0),500, prob = c(0.99,0.01), replace = T)
}


