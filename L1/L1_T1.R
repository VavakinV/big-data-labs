# Задание 1
p <- c(7, 6, 5, 4)
q <- c(0, 1, 2, 3)

p+q

p*q

p/q

p^q

# Задание 2
v1 <- rep(c(FALSE, TRUE), 10)*(1:20)
v1

v2 <- 2^(0:19)
v2

v3 <- 10^(0:4)
v3

# Задание 3
sum(1 / (1:50 * (2:51)))

sum(1 / 2^(0:20))

seq3 <- (1 + 3 * (0:10)) / 3^(0:10)
sum(seq3)

sum(seq3 > 0.5)

# Задание 4
vec3 <- seq(3, 27, by=3)
vec3

vec3[-c(2, 5, 7)]

vec3[-c(1:length(vec3)-1)]

vec3[-c(6)]

vec3[100]

vec3[-c(1, length(vec3))]

vec3[vec3 > 4 & vec3 < 10]

vec3[vec3 < 4 | vec3 > 10]
                           