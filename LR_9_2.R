library(igraph)
input = readLines("input.txt")
#Холмы
N = c(as.numeric(strsplit(input[1], " ")[[1]]))
matr_E = numeric(0)
for (i in 1:N) {
  arr_E <- c(as.numeric(strsplit(input[i+1], " ")[[1]]))
  matr_E = c(matr_E,arr_E)
}
matr_E = matrix(matr_E,nrow = N)
colors_E = c(as.numeric(strsplit(input[N+2], " ")[[1]]))

# Создание графа и определение цветов вершин
g = graph.adjacency(matr_E,mode = "undirected")
V(g)$color = colors_E
plot(g)
# Подсчет "плохих" мостов
bad_bridges <- c()
for(e in E(g)){
  v1 = ends(g, e)[1]
  v2 <- ends(g, e)[2]
  if (V(g)$color[v1] != V(g)$color[v2]){
    bad_bridges <- c(bad_bridges, as.numeric(e))
  }
}

# Вывод результатов
if (length(bad_bridges) > 0) {
  file_con = file("output.txt", open = "w")
  writeLines(as.character(length(bad_bridges)),file_con)
  edges = get.edges(g,bad_bridges)
  vertex_pair = t(edges)
  arr_str = as.character(vertex_pair)
  for (i in 1:length(bad_bridges) ){
    str = paste(arr_str[i*2-1],arr_str[i*2])
    writeLines(str,file_con)
  }
  close(file_con)
}



