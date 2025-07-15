

# Dividir las celdas en varias filas
split_data <- art_filt_microend_oaks %>%
  separate_rows(Quercus_species, sep = ", ") %>%
  separate_rows(WoS_categories, sep = "; ") %>%
  mutate(Quercus_species = trimws(Quercus_species), WoS_categories = trimws(WoS_categories))
split_data[,-c(6:12)]


# Columnas específicas que deseas agregar
columns_to_add <- c("Subgenus", "IUCN Red List Category", "Country distribution", "Quercus_section")

# Unir los dataframes por la columna Quercus_species
merged_data <- left_join(split_data, df_filtered_microend[, c("Scientific_Name", columns_to_add)],
                         by = c("Quercus_species" = "Scientific_Name"))

# Imprimir el resultado
print(merged_data)





# Aplicar la función para filtrar las filas

split_data_end <- filtrar_filas(merged_data, columnas_a_buscar, palabras_buscar)

# Mostrar las filas filtradas
print("Filas donde aparecen cualquiera de los epitetos de las 114 especies consideras microendémicas:")
print(split_data_end[,-c(6:12)])


# Eliminar filas con NA en WoS_categories
split_data_clean <- split_data_end[!is.na(split_data_end$WoS_categories), ]



library(dplyr)
library(networkD3)

# Paso 1: Prepara los datos
links <- split_data_clean %>%
  select(Subgenus, Quercus_section, `IUCN Red List Category`, WoS_categories) %>%
  drop_na() %>%
  mutate_all(as.character()) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -ID, names_to = "Source", values_to = "Target") %>%
  mutate(Source = factor(Source, levels = c("Subgenus", "Quercus_section", "IUCN Red List Category", "WoS_categories")))

# Obtener los nodos únicos
nodes <- data.frame(name = unique(c(links$Source, links$Target)))

# Crear el diagrama de Sankey
sankeyNetwork(Links = links, Nodes = nodes, Source = "Source", Target = "Target", Value = "ID",
              NodeID = nodes$name)



# Paso 2: Crea el diagrama de Sankey
sankeyNetwork(Links = links, Source = "Source", Target = "Target", Value = "ID",
              NodeID = c(levels(links$Source), levels(links$Target)))


sankeyNetwork(Links = links, Nodes = nodes, Source = "Source", Target = "Target", Value = "ID",
              NodeID = "name")


# Instala y carga la biblioteca networkD3 si aún no lo has hecho
install.packages("networkD3")
library(networkD3)

# Supongamos que tienes un dataframe llamado split_data con las columnas Quercus_species y WoS_categories

# Contar la frecuencia de cada combinación de especie de Quercus y categoría de WoS
sankey_data <- table(split_data_clean$Quercus_species, split_data_clean$WoS_categories)

# Convertir los datos a un formato de matriz
sankey_matrix <- as.matrix(sankey_data)

# Crear el diagrama de Sankey
sankey <- sankeyNetwork(Links = sankey_matrix, 
                        Source = colnames(sankey_matrix), 
                        Target = rownames(sankey_matrix), 
                        Value = sankey_matrix, 
                        NodeID = c(rownames(sankey_matrix), colnames(sankey_matrix)),
                        units = "Frequency")






library(networkD3)
library(dplyr)
library(tidyr)
# Datos de ejemplo
links <- data.frame(
  Source = c("A", "A", "B", "B"),
  Target = c("X", "Y", "X", "Z"),
  Value = c(6, 8, 10, 12)
)

nodes <- data.frame(name = unique(c(links$Source, links$Target)))

# Crear el gráfico de Sankey
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "Source",
  Target = "Target",
  Value = "Value",
  NodeID = "name"
)

# Guardar el gráfico como archivo HTML
saveNetwork(sankey, file = "../plots/sankey.html")
