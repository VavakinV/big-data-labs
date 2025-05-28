library("igraph")
library("network")
library("sna")
library("ndtv")

# 1. Построение кольцевого графа
num_vertices <- sample(13:44, 1)

ring_graph <- make_ring(num_vertices)

cat("Число вершин графа:", vcount(ring_graph), "\n")
cat("Число ребер графа:", ecount(ring_graph), "\n")

plot(ring_graph, 
     vertex.label = NA,
     main = paste("Кольцевой граф с", num_vertices, "вершинами"))

ring_graph[]

# 2. Построение графа из пустого
g1 <- make_empty_graph() + vertices(c(1:num_vertices), color="yellow")
g1 <- g1 + edges(sample(V(g1), 48, replace=TRUE), color="red")
plot(g1)
g1[]

g1 <- g1 + edges(sample(V(g1), 60, replace=TRUE), color="blue")
plot(g1)
g1[]

# 3. Добавление рёбер
if (all(c(29, 26) %in% V(g1))) g1 <- add_edges(g1, c(29, 26), color = "black")
if (all(c(18, 18) %in% V(g1))) g1 <- add_edges(g1, c(18, 18), color = "black")
if (all(c(5, 11) %in% V(g1))) g1 <- add_edges(g1, c(5, 11), color = "black")
if (all(c(6, 7) %in% V(g1))) g1 <- add_edges(g1, c(6, 7), color = "black")
if (all(c(10, 16) %in% V(g1))) g1 <- add_edges(g1, c(10, 16), color = "black")

plot(g1)

neighbors(g1, v = 3)
incident(g1, v = 3)
are_adjacent(g1, V(g1)[13], V(g1)[15])

g1[]

# 4. Добавление вершины, именование графа
# Присоединение вершины к вершине с наибольшим числом соседей
g1 <- add_vertices(g1, 1, color = "yellow")
new_vertex_id <- vcount(g1)

max_degree_vertex <- which.max(ego_size(g1))
g1 <- add_edges(g1, c(new_vertex_id, max_degree_vertex), color="purple")

# Именование вершин
total_vertices <- vcount(g1)
vertex_names <- if(total_vertices <= 26) {
  LETTERS[1:total_vertices]
} else {
  c(LETTERS, letters[1:(total_vertices-26)])
}

V(g1)$name <- vertex_names

plot(g1)
g1[]

degrees <- ego_size(g1)
from2to5degree <- which(degrees > 2 & degrees < 5)
from2to5degree

# 5. Размещение графа
# Разные варианты размещения
layouts <- list(
  circle = layout_in_circle(g1),
  tree = layout_as_tree(g1),
  lattice <- layout_on_grid(make_lattice(
    length = ceiling(sqrt(vcount(g1))) + 1000,
    dim = 2
  ))
)

# Построение графиков
plot(g1, layout = layouts$circle)
plot(g1, layout = layouts$tree)
plot(g1, layout = layouts$lattice)

# 6. Измерение графа
diameter(g1)

all_shortest_paths(g1, V(g1), to = V(g1))$vpaths

plot(g1, 
     vertex.size = ego_size(g1) * 4,
     vertex.label.cex = 0.8,
     main = "Граф с размерами вершин по их степеням")

# 7.
# Начальные данные
N <- 20
edges_matrix <- matrix(c(
  1,3, 2,4, 3,4, 3,5, 4,6, 5,7, 6,8, 7,9, 8,10, 13,15, 16,17
), ncol = 2, byrow = TRUE)

g <- make_empty_graph(n = N, directed = FALSE) %>%
  add_edges(as.vector(t(edges_matrix)))

V(g)$name <- as.character(1:N)

# Проверка на двудольность
is_bipartite <- tryCatch({
  bm <- bipartite.mapping(g)
  if(!bm$res) {
    cat("NO\n")
    FALSE
  } else {
    TRUE
  }
}, error = function(e) {
  cat("NO\n")
  FALSE
})

# Если граф двудольный
if(is_bipartite) {
  bm <- bipartite.mapping(g)
  
  part1 <- which(bm$type)
  part2 <- which(!bm$type)
  
  cat("YES\n")
  cat("Стол 1:", sort(part1), "\n")
  cat("Стол 2:", sort(part2), "\n")

  V(g)$color <- ifelse(V(g) %in% part1, "lightblue", "pink")
  
  layout_matrix <- matrix(NA, nrow = N, ncol = 2)
  layout_matrix[part1, 1] <- 0
  layout_matrix[part2, 1] <- 1

  layout_matrix[part1, 2] <- seq(1, 0, length.out = length(part1))
  layout_matrix[part2, 2] <- seq(1, 0, length.out = length(part2))
  
  plot(g, layout=layout_matrix)  
}


