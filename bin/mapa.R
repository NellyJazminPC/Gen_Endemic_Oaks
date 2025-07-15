library(rgbif)
library(dplyr)
library(leaflet)
library(sf)
library(sp)
library(tmaptools)
library(tmap)
library(readxl)

#install.packages("sf")
#install.packages("sp")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("lwgeom")
#install.packages("tmap")
#install.packages("tmaptools")

# Instalar remotes si no está instalado
#if (!require(remotes)) install.packages("remotes")

# Instalar tmap y tmaptools desde GitHub
#remotes::install_github("mtennekes/tmap")
#remotes::install_github("r-tmap/tmap")
# Lista de especies

##### Busqueda de las 112 especies EN, CR y VU de la Oak RedList

# Leer el archivo Excel
file_path <- "../data/Base_datos_encinos_endémicos.xlsx"
Base_datos_encinos_end <- read_excel(file_path, sheet = "Oaks_RedList")


# Mostrar el data frame leído
print("Data frame leído:")
print(Base_datos_encinos_end)

# Filtrar las filas con los valores "CR","EN" y "VU" en la columna "IUCN Red List Category"
Base_Backs_Ashley <- Base_datos_encinos_end %>%
  filter(`IUCN Red List Category` %in% c("CR","EN", "VU"))

# Extraer el epíteto específico
epiteto_especifico <- sapply(strsplit(Base_CR_EN_VU$`Scientific Name`, " "), `[`, 2)

# Mostrar el vector extraído
print("Epíteto específico extraído:")
print(epiteto_especifico)



# Extraer la columna 'Palabras' como un vector
palabras_buscar <- epiteto_especifico






species_list <- c("Panthera leo", "Ursus arctos", "Canis lupus")

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
  if(length(x) == 0) {
    return(NA)
  } else {
    return(table(map_data$species[x]))
  }
})

# Crear un data frame con los resultados
hex_species_df <- do.call(rbind, lapply(1:length(hex_species_count), function(i) {
  if (is.null(hex_species_count[[i]])) {
    return(data.frame(id = i, species = NA, count = 0))
  } else {
    return(data.frame(id = i, species = names(hex_species_count[[i]]), count = as.vector(hex_species_count[[i]])))
  }
}))

# Unir los datos de especies a los hexágonos
hex_grid <- merge(hex_grid, hex_species_df, by = "id")

# Crear el mapa
tmap_mode("view")

tm_shape(hex_grid) +
  tm_polygons("count", title = "Species Count", alpha = 0.5) +
  tm_shape(sf_points) +
  tm_dots(col = "species", palette = "Set1", title = "Species Occurrences", size = 0.5) +
  tm_layout(legend.outside = TRUE)
