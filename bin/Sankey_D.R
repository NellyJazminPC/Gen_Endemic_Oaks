
library(dplyr)
library(stringr)
library(networkD3)


# Datos de ejemplo
articles <- data.frame(
  Title = c("Paper 1", "Paper 2", "Paper 3"),
  Authors = c("Alice, Bob, Carol", "Alice, Dave", "Bob, Carol, Dave"),
  Year = c(2021, 2020, 2019),
  Source_title = c("Journal A", "Journal B", "Journal C")
)

split_data_clean
colnames(split_data_clean)

# Convertir la columna de Subgenus a una lista de vectores 

Subgenus_list <- split_data_clean$Subgenus

# Crear nodos únicos para 

Subgenus <- unique(unlist(Subgenus_list))


Sections <- unique(split_data_clean$Quercus_section)


RedList <- unique(split_data_clean$`IUCN Red List Category`)

WoS <- unique(split_data_clean$WoS_categories)

nodes <- data.frame(name = c(Subgenus, Sections, RedList, WoS))

# Crear enlaces (links) para el diagrama de Sankey
links <- data.frame(
  source = rep(0:(length(Subgenus)-1), each = length(Sections)),
  target = rep(length(Subgenus):(length(Subgenus)+length(Sections)-1), length(Subgenus)),
  value = 1
)

# Añadir enlaces entre años y fuentes
year_source_links <- data.frame(
  source = rep((length(Subgenus)):(length(Subgenus)+length(Sections)-1), each = length(RedList)),
  target = rep((length(Subgenus)+length(Sections)):(length(Subgenus)+length(Sections)+length(RedList)-1), length(Sections)),
  value = 1
)

links <- rbind(links, year_source_links)

# Verificar los nodos y enlaces
print(nodes)
print(links)



# Crear el diagrama de Sankey
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              units = "T", fontSize = 12, nodeWidth = 30)






# Convertir la columna de Subgenus a una lista de vectores 
Subgenus_list <- split_data_clean$Subgenus

# Crear nodos únicos
Subgenus <- unique(Subgenus_list)
Sections <- unique(split_data_clean$Quercus_section)
RedList <- unique(split_data_clean$`IUCN Red List Category`)
WoS <- unique(split_data_clean$WoS_categories)

# Combinar todos los nodos en un dataframe
nodes <- data.frame(name = c(Subgenus, Sections, RedList, WoS))

# Crear enlaces (links) para el diagrama de Sankey
links <- data.frame(
  source = rep(0:(length(Subgenus) - 1), each = length(Sections)),
  target = rep(length(Subgenus):(length(Subgenus) + length(Sections) - 1), length(Subgenus)),
  value = 1
)

# Añadir enlaces entre secciones y categorías de la Lista Roja
section_source_links <- data.frame(
  source = rep((length(Subgenus)):(length(Subgenus) + length(Sections) - 1), each = length(RedList)),
  target = rep((length(Subgenus) + length(Sections)):(length(Subgenus) + length(Sections) + length(RedList) - 1), length(Sections)),
  value = 1
)

# Añadir enlaces entre categorías de la Lista Roja y WoS
redlist_wos_links <- data.frame(
  source = rep((length(Subgenus) + length(Sections)):(length(Subgenus) + length(Sections) + length(RedList) - 1), each = length(WoS)),
  target = rep((length(Subgenus) + length(Sections) + length(RedList)):(length(Subgenus) + length(Sections) + length(RedList) + length(WoS) - 1), length(RedList)),
  value = 1
)

links <- rbind(links, section_source_links, redlist_wos_links)

# Verificar los nodos y enlaces
print(nodes)
print(links)

# Crear el diagrama de Sankey
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              units = "T", fontSize = 12, nodeWidth = 30)








# Combinar todos los nodos en un dataframe
nodes <- data.frame(name = c(Subgenus, Sections, RedList, WoS))

# Crear enlaces (links) para el diagrama de Sankey
links <- data.frame(
  source = rep(0:(length(Subgenus) - 1), each = length(Sections)),
  target = rep(length(Subgenus):(length(Subgenus) + length(Sections) - 1), length(Subgenus)),
  value = 1
)

# Obtener las combinaciones únicas de Sección y Categoría de la Lista Roja en los datos
section_redlist_combinations <- unique(split_data_clean[, c("Quercus_section", "IUCN Red List Category")])

# Crear enlaces solo para las combinaciones presentes en los datos
section_source_links <- data.frame(
  source = match(section_redlist_combinations$Quercus_section, nodes$name),
  target = match(section_redlist_combinations$`IUCN Red List Category`, nodes$name),
  value = 1
)

# Añadir los enlaces al dataframe de enlaces
links <- rbind(links, section_source_links)

# Crear el diagrama de Sankey
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              units = "T", fontSize = 12, nodeWidth = 30)

