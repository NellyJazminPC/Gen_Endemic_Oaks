# Cargar los paquetes necesarios
library(sf)
library(dplyr)
library(tmap)
library(rgbif)
library(stringr)
library(writexl)
library(readxl)


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

# Descargar datos de todas las especies en la lista
species_data <- lapply(species_keys, download_species_data)

# Encontrar todas las columnas presentes en los data frames descargados
all_columns <- unique(unlist(lapply(species_data, colnames)))

# Función para asegurar que un data frame tenga todas las columnas necesarias
standardize_columns <- function(df, all_columns) {
  if (is.data.frame(df)) {
    for (col in all_columns) {
      if (!col %in% colnames(df)) {
        df[[col]] <- NA
      }
    }
    return(df[, all_columns])
  } else {
    # Si df no es un data frame, devuelve un data frame vacío con las columnas esperadas
    return(data.frame(matrix(ncol = length(all_columns), nrow = 0, dimnames = list(NULL, all_columns))))
  }
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

# Cargar las ecorregiones de WWF
ecoregions <- st_read("../data/official/wwf_terr_ecos.shp")  

# Corregir las geometrías inválidas en las ecorregiones
ecoregions <- st_make_valid(ecoregions)

# Transformar las ecorregiones al mismo CRS que los puntos
ecoregions <- st_transform(ecoregions, crs = st_crs(sf_points))

# Intersecar los puntos de presencia con las ecorregiones
points_in_ecoregions <- st_intersects(sf_points, ecoregions, sparse = FALSE)

# Crear un data frame con las especies y las ecorregiones
species_ecoregions <- lapply(1:nrow(points_in_ecoregions), function(i) {
  if (any(points_in_ecoregions[i, ])) {
    data.frame(species = sf_points$species[i],
               ecoregion = ecoregions$ECO_NAME[points_in_ecoregions[i, ]])
  } else {
    data.frame(species = sf_points$species[i], ecoregion = NA)
  }
})

species_ecoregions_df <- do.call(rbind, species_ecoregions)

# Imprimir el data frame resultante
print(species_ecoregions_df)

# Filtrar las filas que contienen "Quercus" seguido de cualquier otra palabra
species_ecoregions_df_filtered <- species_ecoregions_df %>%
  filter(str_detect(species, "^Quercus\\s+\\w+"))

# Exportar el resultado a un archivo Excel
#write_xlsx(species_ecoregions_df_filtered, "species_ecoregions_filtered.xlsx")

# Crear un data frame con la especie, la ecorregión y el número de puntos en cada ecorregión
species_ecoregions_count <- species_ecoregions_df_filtered %>%
  group_by(species, ecoregion) %>%
  summarise(count = n()) %>%
  ungroup()

# Imprimir el data frame con los conteos
print(species_ecoregions_count)

# Exportar el resultado a un archivo Excel
write_xlsx(species_ecoregions_count, "../data/species_ecoregions_count_3813.xlsx")

# Leer el archivo Excel
file_path <- "../data/species_ecoregions_count_3813.xlsx"
species_ecoregions_count <- read_excel(file_path, sheet = "Sheet1")


# Crear el mapa con las ecorregiones y los puntos de presencia
tmap_mode("view")

tm_shape(ecoregions) +
  tm_borders() +
  tm_shape(sf_points) +
  tm_dots(col = "species", palette = "Set1", title = "Species Occurrences", size = 0.5) +
  tm_layout(legend.outside = TRUE)


