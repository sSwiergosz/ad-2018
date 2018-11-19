# Zadanie1
first <- rnorm(100, 0, 1)
second <- rnorm(100, 0, 1)

hist(first)
hist(second)

# Zadanie2
dist <- rnorm(100, 100, 10^2)
length(dist[abs(scale(dist)) <= 2]) / 100

# Zadanie3
dist200 <- rnorm(200, 0, 1)
s <- sd(dist200)

dist200 <= s & dist200 >= (-s) -> sigma1
length(sigma1[sigma1 == TRUE])/200*100

dist200 <= (s * 2) & dist200 >= (-s * 2) -> sigma2
length(sigma2[sigma2 == TRUE])/200*100

dist200 <= (s * 3) & dist200 >= (-s * 3) -> sigma3
length(sigma3[sigma3 == TRUE])/200*100

# Zadanie4
hist(rowSums(replicate(3, sample(6, 10^6, replace=T))))

# Zadanie5
