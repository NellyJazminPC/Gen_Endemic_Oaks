library(rgbif)
library(dplyr)
library(leaflet)
library(sf)
library(sp)
library(tmaptools)
library(tmap)
library(readxl)
library(writexl)

##### Busqueda de las 116 especies en Backs & Ashely (2021) Gap Analysis + EN, CR y VU de la Oak RedList

# Leer el archivo Excel
file_path <- "../data/Base_datos_encinos_endémicos.xlsx"
Base_datos_encinos_end <- read_excel(file_path, sheet = "Oaks_RedList")


# Mostrar el data frame leído
print("Data frame leído:")
print(Base_datos_encinos_end)

# Filtrar las filas con los valores "Y" en la columna "Backs_Ashley_2021"
Base_Backs_Ashley <- Base_datos_encinos_end %>%
  filter(Backs_Ashley_2021 %in% "Y")

# Extraer el nombre científico

nombre_cientifico <- tolower(Base_Backs_Ashley$`Scientific Name`)

# Mostrar el vector extraído
print("Nombres científicos de las 124 especies:")#81 para esta primera prueba
print(nombre_cientifico)



# Lista de especies
species_list <- nombre_cientifico

# Función para obtener el key de la especie
get_species_key <- function(species_name) {
  backbone <- name_backbone(name = species_name)
  return(backbone$speciesKey)
}

# Función para descargar datos de presencia de una especie
download_species_data <- function(species_key) {
  # Descargar los datos de presencia
  occurrences <- occ_search(taxonKey = species_key, limit = 500)
  
  return(occurrences$data)
}

# Obtener los keys de las especies
species_keys <- sapply(species_list, get_species_key)

print(species_keys)

# Descargar datos de todas las especies en la lista
species_data <- lapply(species_keys, download_species_data)

# Encontrar todas las columnas presentes en los data frames descargados
all_columns <- unique(unlist(lapply(species_data, colnames)))

# Función para asegurar que un data frame tenga todas las columnas necesarias
standardize_columns <- function(df, all_columns) {
  for (col in all_columns) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA
    }
  }
  return(df[, all_columns])
}

# Estandarizar columnas en todos los data frames
species_data <- lapply(species_data, standardize_columns, all_columns)

# Combinar todos los datos en un solo data frame
combined_data <- do.call(rbind, species_data)

# Filtrar columnas necesarias para el mapa
map_data <- combined_data %>% select(species, decimalLongitude, decimalLatitude) %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# Convertir los datos a un objeto sf
sf_points <- st_as_sf(map_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Generar hexágonos
hex_grid <- st_make_grid(sf_points, cellsize = 1, square = FALSE) # Ajusta cellsize según sea necesario
hex_grid <- st_as_sf(hex_grid)
hex_grid$id <- 1:nrow(hex_grid)

# Intersección de puntos con hexágonos
hex_intersection <- st_intersects(hex_grid, sf_points)

# Contar el número de puntos por especie en cada hexágono
hex_species_count <- sapply(hex_intersection, function(x) {
  if (length(x) == 0) {
    return(NULL)
  } else {
    return(table(map_data$species[x]))
  }
})

# Crear un data frame con los resultados
hex_species_df <- do.call(rbind, lapply(1:length(hex_species_count), function(i) {
  if (is.null(hex_species_count[[i]]) || length(hex_species_count[[i]]) == 0) {
    return(data.frame(id = i, species = NA, count = 0))
  } else {
    return(data.frame(id = i, species = names(hex_species_count[[i]]), count = as.vector(hex_species_count[[i]])))
  }
}))

# Unir los datos de especies a los hexágonos
hex_grid <- merge(hex_grid, hex_species_df, by = "id")


# Crear el resumen del número de hexágonos por especie
hex_summary <- hex_species_df %>%
  filter(!is.na(species)) %>%
  group_by(species) %>%
  summarise(hex_count = n_distinct(id))

print(hex_summary)


# Crear el mapa
tmap_mode("view")

tm_shape(hex_summary_filtered) +
  tm_polygons("count", title = "Species Count", alpha = 0.5) +
  tm_shape(sf_points) +
  tm_dots(col = "species", palette = "Set1", title = "Species Occurrences", size = 0.5) +
  tm_layout(legend.outside = TRUE)


mapa <- tm_shape(hex_grid) +
  tm_polygons("count", title = "Species Count", alpha = 0.5) +
  tm_shape(sf_points) +
  tm_dots(col = "species", palette = "Set1", title = "Species Occurrences", size = 0.5) +
  tm_layout(legend.outside = TRUE)

# Guardar el mapa como un archivo HTML
tmap_save(mapa, "../plots/mapa_hex.html")
