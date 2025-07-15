library(dplyr)
library(stringr)
library(igraph)
library(tidygraph)
library(ggraph)
library(readxl)
library(visNetwork)


art_filt_microend_oaks <- read_xlsx("../data/art_filt_microend_oaks_44.xlsx")

# Convertir la columna de autores a una lista de vectores de autores
author_list <- strsplit(art_filt_microend_oaks$Authors, "; ")


# Crear un data frame de todas las combinaciones de autores para cada artículo
author_combinations <- lapply(author_list, function(x) {
  combn(x, 2, simplify = FALSE)
}) %>% unlist(recursive = FALSE)

# Convertir las combinaciones a un data frame
edges <- do.call(rbind, lapply(author_combinations, function(x) {
  data.frame(From = x[1], To = x[2])
}))


# Ver el data frame de edges
print(edges)

# Crear un grafo de igraph
author_network <- graph_from_data_frame(edges, directed = FALSE)

# Ver el resumen del grafo
summary(author_network)

# Convertir a tidygraph
author_network_tidy <- as_tbl_graph(author_network)

# Visualizar la red utilizando ggraph
ggraph(author_network_tidy, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5)) +
  geom_node_point(size = 5, color = "blue") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Red de Colaboración entre Autores")






# Asegúrate de que las columnas `Year` y `Source_title` existen y están correctamente nombradas
if (!all(c("Year", "Source_title") %in% colnames(filas_filtradas_art_B_A))) {
  stop("Las columnas `Year` y `Source_title` no están presentes en el data frame `filtered_data`.")
}


# Crear un data frame de todas las combinaciones de autores para cada artículo
author_combinations <- lapply(1:nrow(art_filt_microend_oaks), function(i) {
  combn(author_list[[i]], 2, simplify = FALSE) %>%
    lapply(function(x) data.frame(From = x[1], To = x[2], Year = art_filt_microend_oaks$Year[i], Source_title = art_filt_microend_oaks$Source_title[i]))
}) %>% unlist(recursive = FALSE)

# Convertir las combinaciones a un data frame
edges <- do.call(rbind, author_combinations)

# Ver el data frame de edges
print(edges)

# Crear un grafo de igraph
author_network <- graph_from_data_frame(edges, directed = FALSE)

# Asignar el Year y Source_title más frecuente a cada nodo
V(author_network)$Year <- sapply(V(author_network)$name, function(x) {
  years <- edges$Year[edges$From == x | edges$To == x]
  if (length(years) > 0) {
    return(names(sort(table(years), decreasing = TRUE))[1])  # Año más frecuente
  } else {
    return(NA)
  }
})

V(author_network)$Source_title <- sapply(V(author_network)$name, function(x) {
  sources <- edges$Source_title[edges$From == x | edges$To == x]
  if (length(sources) > 0) {
    return(names(sort(table(sources), decreasing = TRUE))[1])  # Fuente más frecuente
  } else {
    return(NA)
  }
})



# Ver el resumen del grafo
summary(author_network)

# Convertir a tidygraph
author_network_tidy <- as_tbl_graph(author_network)

# Visualizar la red utilizando ggraph
ggraph(author_network_tidy, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.1, edge_width = 0.1)) +
  geom_node_point(aes(color = factor(Year)), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 vjust = 1, hjust = 1, max.overlaps = 30) +
  scale_color_discrete(name = "Year") +
  theme_void() +
  labs(title = "Red de Colaboración entre Autores") +
  theme(legend.position = "right")


# Convertir el grafo a un formato de visNetwork
nodes <- data.frame(id = V(author_network_tidy)$name,
                    label = V(author_network_tidy)$name,
                    group = V(author_network_tidy)$Source_title)  # Asignar grupo basado en la fuente

# Crear un data frame de aristas para visNetwork
edges_vis <- data.frame(from = get.edgelist(author_network)[,1],
                        to = get.edgelist(author_network)[,2],
                        title = E(author_network)$Source_title)  # Asignar título basado en la fuente

## Verificar los datos en `nodes` y `edges_vis`
print(head(nodes))
print(head(edges_vis))

# Crear la red interactiva utilizando visNetwork
visNetwork(nodes, edges_vis) %>%
  visNodes(size = 10) %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend() %>%
  visLayout(randomSeed = 123)






# Convertir el grafo a un formato de visNetwork
nodes <- data.frame(id = V(author_network_tidy)$name,
                    label = V(author_network_tidy)$name,
                    group = V(author_network_tidy)$Year)  # Asignar grupo basado en el año

# Crear un data frame de aristas para visNetwork
edges_vis <- data.frame(from = get.edgelist(author_network)[,1],
                        to = get.edgelist(author_network)[,2],
                        title = E(author_network)$Year)  # Asignar título basado en el año

## Verificar los datos en `nodes` y `edges_vis`
print(head(nodes))
print(head(edges_vis))

# Crear la red interactiva utilizando visNetwork
visNetwork(nodes, edges_vis) %>%
  visNodes(size = 10) %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend() %>%
  visLayout(randomSeed = 123)


# Crear la red interactiva utilizando visNetwork
network <- visNetwork(nodes, edges_vis) %>%
  visNodes(size = 10) %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend() %>%
  visLayout(randomSeed = 123)

# Guardar la red como un archivo HTML
htmlwidgets::saveWidget(network, file = "../plots/network_visualization.html")
