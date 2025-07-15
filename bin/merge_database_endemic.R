library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(scales)
library(writexl)

# Leer el archivo Excel
file_path <- "../data/Base_datos_encinos_endémicos.xlsx"
Base_datos_encinos_end <- read_excel(file_path, sheet = "Oaks_RedList")

# ---- Código binario - Lista de Backs & Ashley (2021) ----

# Crear la nueva columna convirtiendo "N" en 1 y "Y" en 0 - Especies en preocupación
Base_datos_encinos_end$Backs_Ashley_2021_binary <- ifelse(
  Base_datos_encinos_end$Backs_Ashley_2021 == "N", 1, 0)



# Ordenar el dataframe por "Scientific_Name" de manera ascendente y por "hex_count" de manera descendente
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  arrange( Backs_Ashley_2021_binary,`Scientific Name`,)


# ---- Escala de categorías de la "IUCN Red List Category" ----

# Convertir los valores de "IUCN Red List Category" a una escala de 1 a 6 y asignar a una nueva columna
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  mutate(IUCN_Red_List_Scale = recode(`IUCN Red List Category`,
                                      "CR" = 1,
                                      "EN" = 2,
                                      "VU" = 3,
                                      "NT" = 4,
                                      "LC" = 5,
                                      "DD" = 6))

# Mostrar el dataframe con la nueva columna
print(head(Base_datos_encinos_end))


# ---- Conteo de paises ----

# Función para contar los países en cada celda
count_countries <- function(cell) {
  return(length(strsplit(cell, ",")[[1]]))
}

# Aplicar la función a la columna Country_distribution

Base_datos_encinos_end$country_count <- sapply(
  Base_datos_encinos_end$`Country distribution`, 
  count_countries)


# Mostrar el dataframe resultante
print(Base_datos_encinos_end)


# ---- Conteno de hexágonos por especie ----

# Leer el primer archivo Excel
file_path2 <- "../data/hex_summary_filtered_quercus.xlsx"
hex_summary_filtered_quercus <- read_excel(file_path2)


# Mostrar las primeras filas de ambos dataframes para verificar la carga
print(head(hex_summary_filtered_quercus))


# Verificar los nombres de las columnas en ambos dataframes
print(colnames(hex_summary_filtered_quercus))
print(colnames(Base_datos_encinos_end))

# Renombrar la columna en hex_summary_filtered_quercus si es necesario para que coincida con Base_datos_encinos_end
hex_summary_filtered_quercus <- hex_summary_filtered_quercus %>%
  rename(Scientific_Name = species)

# Renombrar la columna en Base_datos_encinos_end si es necesario para que coincida
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  rename(Scientific_Name = `Scientific Name`)



# Realizar un left join para agregar la columna hex_count al dataframe Base_datos_encinos_end
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  left_join(hex_summary_filtered_quercus, by = "Scientific_Name")

# Ordenar el dataframe resultante por la columna Scientific_Name
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  arrange(Scientific_Name)

# Mostrar las primeras filas del dataframe resultante
print(head(Base_datos_encinos_end))


# ---- Conteo de ecorregiones (WWF) por especie ----

# Leer el segundo archivo Excel
file_path3 <- "../data/species_ecoregions_count_3813.xlsx"
species_ecoregions_count <- read_excel(file_path3)

# Mostrar las primeras filas de ambos dataframes para verificar la carga
print(species_ecoregions_count, n=20)



# Verificar los nombres de las columnas en ambos dataframes
print(colnames(species_ecoregions_count))
print(colnames(Base_datos_encinos_end))

# Renombrar la columna en species_ecoregions_count si es necesario para que coincida con Base_datos_encinos_end
species_ecoregions_count <- species_ecoregions_count %>%
  rename(Scientific_Name = species)


# Agrupar por especie y contar las ecoregiones
species_ecoregion_summary <- species_ecoregions_count %>%
  group_by(Scientific_Name) %>%
  summarise(ecoregion_count = n_distinct(ecoregion))


# Realizar un left join para agregar la columna ecoregion_count al dataframe Base_datos_encinos_end
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  left_join(species_ecoregion_summary, by = "Scientific_Name")



# Ordenar el dataframe resultante por la columna Scientific_Name
Base_datos_encinos_end <- Base_datos_encinos_end %>%
  arrange(Scientific_Name)

# Mostrar las primeras filas del dataframe resultante
print(head(Base_datos_encinos_end))


#---- Eliminar filas exactamente duplicadas -----

# Identificar y contar filas duplicadas
duplicate_rows <- Base_datos_encinos_end[duplicated(Base_datos_encinos_end) | duplicated(Base_datos_encinos_end, fromLast = TRUE), ]

# Contar el número de filas duplicadas
num_duplicates <- nrow(duplicate_rows) / 2

# Mostrar las filas duplicadas y sus valores
print("Número de filas duplicadas:")
print(num_duplicates)

print("Filas duplicadas:")
print(duplicate_rows)


# Eliminar las filas exactamente duplicadas
database_quercus_endemic_final <- Base_datos_encinos_end %>% distinct()

# Mostrar las primeras filas del dataframe resultante
print(head(database_quercus_endemic_final))

colnames(database_quercus_endemic_final)


# Exportar el resultado a un archivo Excel
write_xlsx(database_quercus_endemic_final, "../data/database_hex_ecoreg_redlist.xlsx")


#-------- Heatmap con las columnas Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count ------

# Seleccionar solo las columnas de interés
df <- database_quercus_endemic_final %>%
  select(Scientific_Name, Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count)

# Escalar las columnas (normalización)
df_scaled <- df %>%
  mutate(across(c(Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count), rescale))

# Convertir las columnas de interés a numéricas si no lo son
df_scaled <- df_scaled %>%
  mutate(across(c(Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count), as.numeric))

# Convertir el dataframe a formato largo para ggplot2
df_melt <- melt(df_scaled, id.vars = c("Scientific_Name", "Backs_Ashley_2021_binary"))

# Mostrar las primeras filas del dataframe escalado y convertido
print(head(df_melt))


# Crear el heatmap usando ggplot2 y facet_grid para agrupar por Backs_Ashley_2021_binary
heatmap <- ggplot(df_melt, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("darkred", "red", "orange", "yellow", "white")) +
  labs(title = "Heatmap of Quercus Endemic Data",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(rows = vars(Backs_Ashley_2021_binary), scales = "free_y", space = "free")

# Mostrar el heatmap
print(heatmap)





# Seleccionar solo las columnas de interés
df <- database_quercus_endemic_final %>%
  select(Scientific_Name, Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count)

# Aplicar una transformación logarítmica a las columnas de interés (añadir 1 para evitar log(0))
df_log <- df %>%
  mutate(across(c(Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count), ~ log1p(.)))

# Escalar las columnas (normalización)
df_scaled <- df_log %>%
  mutate(across(c(Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count), rescale))

# Convertir las columnas de interés a numéricas si no lo son
df_scaled <- df_scaled %>%
  mutate(across(c(Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count), as.numeric))

# Convertir el dataframe a formato largo para ggplot2
df_melt <- melt(df_scaled, id.vars = c("Scientific_Name", "Backs_Ashley_2021_binary"))

# Mostrar las primeras filas del dataframe escalado y convertido
print(head(df_melt))

# Crear el heatmap usando ggplot2 y facet_grid para agrupar por Backs_Ashley_2021_binary
heatmap <- ggplot(df_melt, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "red", "darkred")) +
  labs(title = "Heatmap of Quercus Endemic Data (Log-Scaled)",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(rows = vars(Backs_Ashley_2021_binary), scales = "free_y", space = "free")

# Mostrar el heatmap
print(heatmap)


#---- Heatmap solo con las especies propuestas de Black & Ashley 2021 ----

# Filtrar los datos para incluir solo filas donde Backs_Ashley_2021_binary es 0
df_filtered <- df_scaled %>%
  filter(Backs_Ashley_2021_binary == 0)

# Convertir el dataframe filtrado a formato largo para ggplot2
df_melt_filtered <- melt(df_filtered, id.vars = c("Scientific_Name", "Backs_Ashley_2021_binary"))

# Crear el heatmap usando ggplot2
heatmap_filtered <- ggplot(df_melt_filtered, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "red", "darkred")) +
  labs(title = "Heatmap of Quercus Endemic Data (Backs_Ashley_2021_binary = 0, Log-Scaled)",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar el heatmap
print(heatmap_filtered)





# Crear el heatmap usando ggplot2
heatmap_filtered <- ggplot(df_melt_filtered, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "yellow", "white")) +
  labs(title = "Heatmap of Quercus Endemic Data (Backs_Ashley_2021_binary = 0, Log-Scaled)",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el heatmap
print(heatmap_filtered)









# Realizar la agrupación jerárquica utilizando la variable IUCN_Red_List_Scale
dist_matrix <- dist(df_filtered$`IUCN_Red_List_Scale`)
hclust_result <- hclust(dist_matrix)

# Ordenar los datos según los clústeres generados
df_filtered$cluster <- cutree(hclust_result, k = 4)  # Aquí `k` es el número de clústeres deseados
df_filtered <- df_filtered %>%
  arrange(cluster, IUCN_Red_List_Scale)

# Convertir el dataframe filtrado a formato largo para ggplot2
df_melt_filtered <- melt(df_filtered, id.vars = c("Scientific_Name", "Backs_Ashley_2021_binary", "cluster"))

# Crear el heatmap usando ggplot2
heatmap_filtered <- ggplot(df_melt_filtered, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "white", "yellow")) +
  labs(title = "Heatmap of Quercus Endemic Data (Backs_Ashley_2021_binary = 0, Log-Scaled, Clustered by IUCN)",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el heatmap
print(heatmap_filtered)






# Ordenar los datos primero por IUCN_Red_List_Scale y luego por otra variable
df_filtered <- df_filtered %>%
  arrange(IUCN_Red_List_Scale, Scientific_Name)

# Convertir el dataframe filtrado a formato largo para ggplot2
df_melt_filtered <- melt(df_filtered, id.vars = c("Scientific_Name", "Backs_Ashley_2021_binary"))

# Crear el heatmap usando ggplot2
heatmap_filtered <- ggplot(df_melt_filtered, aes(x = variable, y = Scientific_Name, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "white", "yellow")) +
  labs(title = "Heatmap of Quercus Endemic Data (Backs_Ashley_2021_binary = 0, Log-Scaled, Ordered by IUCN)",
       x = "Variables",
       y = "Scientific Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el heatmap
print(heatmap_filtered)

