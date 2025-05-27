library("igraph")
library("network")
library("sna")
library("ndtv")

# 1. Построение кольцевого графа
# Генерация случайного числа вершин от 13 до 44
num_vertices <- sample(13:44, 1)

# Создание кольцевого графа
ring_graph <- make_ring(num_vertices)

# Вывод информации о графе
cat("Число вершин графа:", vcount(ring_graph), "\n")
cat("Число ребер графа:", ecount(ring_graph), "\n")

# Визуализация графа
plot(ring_graph, 
     vertex.label = NA,
     main = paste("Кольцевой граф с", num_vertices, "вершинами"))

# Вывод матрицы смежности
ring_graph[]

# 2. 
g1 <- make_empty_graph() + vertices(c(1:num_vertices), color="yellow")
g1 <- g1 + edges(sample(V(g1), 24, replace=TRUE), color="red")
plot(g1)
g1[]

g1 <- g1 + edges(sample(V(g1), 30, replace=TRUE), color="blue")
plot(g1)
g1[]
