# Carga los paquetes necesarios
library(readxl)
library(dplyr)
library(writexl)
library(magrittr)
library(ggplot2)
library(stringr)
library(readxl)
# Lee los archivos
csv_data <- read.csv("../data/scopus_18-05-24.csv", stringsAsFactors = FALSE)
xls_data <- read_excel("../data/web_sciences_18-05-24.xls")


#----------- Generación de una sola base de datos (Scopus + Web of Science)  -------
# manteniendo solo los artículos

#### Base de SCOPUS ####

print(csv_data)
# Imprimir los nombres originales de las columnas
print("Nombres originales de las columnas:")
print(colnames(csv_data))

# Extraer columnas específicas (Authors, Title, Year, Source title, DOI, Author Keywords, 
# Index Keywords, Language of Original Doc, Abbreviated Source, Document type,  Publication stage )
# Convierte los valores de Source Title a mayúsculas

scopus_extraido <- csv_data %>%
  select(1,4,5,6,14,19,20,40,41,42,43) %>%
  mutate(across(c(4,7),toupper))

# Renombrar las columnas extraídas: 
print(colnames(scopus_extraido))
# Renombrar: Source title, Author Keywords, Index Keywords,
# Language of Original Doc, Abbreviated Source, Document type
scopus_renombrado <- scopus_extraido %>%
  rename(
    Source_title = Source.title,
    Author_keywords = Author.Keywords,
    Keywords_extra = Index.Keywords,
    Language = Language.of.Original.Document,
    Journal_ISO_Ab = Abbreviated.Source.Title,
    Document_type = Document.Type,
    Publication_stage = Publication.Stage
  )

# Mostrar el resultado
print("Data frame con columnas extraídas y renombradas:")
colnames(scopus_renombrado)


#### Base de WEB OF SCIENCE ####

print(xls_data)
# Imprimir los nombres originales de las columnas
print("Nombres originales de las columnas:")
print(colnames(xls_data))

# Extraer columnas específicas (Authors, Article Title, Source title,Language, Document type,
# Author Keywords, Keywords plus, Journal ISO Abbreviation, Publication Year, DOI, 
# WoS Categories, Research Areas )
wos_extraido <- xls_data %>%
  select(2,9,10,13,14,20,21,45,47,57,62,64)

# Renombrar las columnas extraídas: 
print(colnames(wos_extraido))

# Renombrar: Article Title, Source title, Document type, Author Keywords, Keywords plus,
# Journal ISO Abbreviation, Publication Year, WoS Categories, Research Areas
wos_renombrado <- wos_extraido %>%
  rename(
    Title = `Article Title`,
    Source_title = `Source Title`,
    Document_type = `Document Type`,
    Author_keywords = `Author Keywords`,
    Keywords_extra =`Keywords Plus`,
    Journal_ISO_Ab = `Journal ISO Abbreviation`,
    Year = `Publication Year`,
    WoS_categories = `WoS Categories`,
    Research_areas = `Research Areas`
  )

# Mostrar el resultado
print("Data frame con columnas extraídas y renombradas:")
colnames(wos_renombrado)



#### Juntar las dos bases de datos SCOPUS + WEB OF SCIENCE ####

# Mostrar los data frames originales
print("Base de Scopus con datos extraidos y con columnas renombradas")
print(scopus_renombrado)

print("Base de Web of Science con datos extraidos y con columnas renombradas")
print(wos_renombrado)

# Fusionar los data frames manteniendo todas las columnas, incluidas las filas con NA en 'id'
database_fusionado <- full_join(scopus_renombrado, wos_renombrado, 
                                by = c("DOI", "Title", "Authors","Year", "Language", 
                                       "Journal_ISO_Ab", "Source_title", "Author_keywords", 
                                       "Document_type", "Keywords_extra"))

# Mostrar el resultado
print("Data frame fusionado:")
print(database_fusionado)


dim(scopus_renombrado)
dim(wos_renombrado)
dim(database_fusionado)

#### Contar las ocurrencias de cada valor en la columna 'DOI' ####
conteo_repetidos <- database_fusionado %>%
  group_by(DOI) %>%
  summarise(conteo = n()) %>%
  filter(conteo > 1)

print(conteo_repetidos)

# Obtener el conteo de datos únicos en la columna 'DOI'
conteo_unicos <- database_fusionado %>%
  summarise(conteo_unicos = n_distinct(DOI))

print(conteo_unicos)



#### Combinar filas duplicadas, preservando los datos diferentes en otras columnas y excluyendo NA ####

# Función personalizada para colapsar los valores únicos excluyendo NA
collapse_unique <- function(x) {
  paste(na.omit(unique(x)), collapse = ", ")
}

# Combinar filas duplicadas, preservando los datos diferentes en otras columnas y excluyendo NA
database_sinduplicados <- database_fusionado %>%
  group_by(DOI) %>%
  summarise(across(
    c(Authors, Title, Year, Source_title, Author_keywords, Keywords_extra, Language, 
      Journal_ISO_Ab, Document_type, Publication_stage, WoS_categories, Research_areas),
    collapse_unique
  ))

dim(database_sinduplicados)

# Guarda el archivo fusionado 
write_xlsx(database_sinduplicados, "../data/database_sinduplicados.xlsx")


#### Primer filtro con el Tipo de Documento - dejar solo los artículos ####

# Contabilizar el número de filas para cada tipo de valor en la columna "Document_type"
conteo_document_type <- database_sinduplicados %>%
  count(Document_type)
print(conteo_document_type)


ggplot(conteo_document_type, aes(x = "", y = n, fill = Document_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de Document Type") +
  theme_void() +
  theme(legend.title = element_blank())


# Filtrar las filas que contienen el valor "Article" en la columna "Document_type"
articles_others <- database_sinduplicados %>%
  filter(str_detect(Document_type, "\\bArticle\\b"))


# Filtrar las filas que solo tienen el valor "Article" en la columna "Document_type"
articles_only <- database_sinduplicados %>%
  filter(Document_type == "Article")

# Exportar la base de datos con solo artículos

# Guarda el archivo fusionado 
write_xlsx(articles_only, "../data/database_articles_only.xlsx")



####### Segundo Filtro - Eliminar por Source_title ####
####### Quitar los artículos que tengan Animal en Source_title
#### Exploración extra - Columna Source title - conteo ####

# Contabilizar el número de filas para cada tipo de valor en la columna "Source_title"
conteo_source_title <- articles_only %>%
  count(Source_title)

# Mostrar el conteo
print("Conteo de filas para cada tipo de valor en la columna 'Source_title':")
print(conteo_source_title)
print(conteo_source_title[,1], n=50)

# Después de un escaneo de los títulos y los Source Title, se seleccionaron 
# palabras clave para remover aquellas revistas que muy probablemente no publiquen
# trabajos de genética o genómica de plantas.


# Filtrar y quitar las filas donde 'Source_title' contiene "ZOO"
art_filt_source_title <- articles_only %>%
  filter(!str_detect(Source_title,
                     paste0("ZOO|ANIMAL|HERPETO|ULTRASONI|",
                     "REPTIL|MEDIC|SOIL|WINE|INVASION|PREGNANCY|",
                     "ENTOMOLOG|YEAST|FOOD|PEDAGOGIA|CLINICAL|",
                     "WATER|PSYCHOLOGY|ENERGY|FUNGAL|HYDRO|INDUSTR|",
                     "INSECT|FISH|FORENSIC|HEPATOLOGY|GEOPHYSICAL|",
                     "LIPID|MAMMAL|RADIOLOGICAL|WILDLIFE|MARINE|",
                     "MICROBI|THERAPY|MYCO|IMMUNO|PALAEO|PALEO|",
                     "PARASYT|SOCIOLOGY|PLASMA|PRESLIA|ARCHAEO|",
                     "PURINERGIC|RESONANCE|VIRTUAL|ACAROLOGY|PEDOBIOLOG")))

# Con este filtro, pasamos de 623 artículos a 507.
print(art_filt_source_title)



conteo_WoS<- art_filt_source_title %>%
  count(WoS_categories)

# Filtrar los valores de "WoS_categories" que tengan un valor menor a 10 en la columna "n"
conteo_menor10_WoS <- conteo_WoS %>%
  filter(n < 10) %>%
  select(WoS_categories)

# Imprimir los valores filtrados
print("Valores de 'WoS_categories' con 'n' menor a 10:")
print(conteo_menor10_WoS$WoS_categories)

### Escaneo a partir de filtrar y seleccionar las categorías específicas con menos de 10 datos
art_rev_filt_WoS_cat <- art_filt_source_title %>%
  filter(WoS_categories %in% conteo_menor10_WoS$WoS_categories)



# Filtrar y quitar las filas donde 'Keywords_extra' contenga las siguientes palabras
art_filt_Keywords_extra <- art_filt_source_title %>%
  filter(!str_detect(Keywords_extra, 
                     paste0("MOUSE|CARCINOGENICITY|LUMBRICID|",
                            "ATRICAPILLA|ALLIGATOR|TORTOISE|",
                            "OOMYCET|GALLWASP|HYDRAULIC|MARINE|",
                            "PROTOZOA|TURKEY|ANURA|FISSION|",
                            "NANO|CARNIVOR|MONKEY|SPIDER|",
                            "ANIMAL EXPERIMENT|ARACHNID|",
                            "PHACOCYSTI|VITIS|PONDEROSA| BRYOPHYTA|",
                            "REMOVAL|GRASSLAND|COLEOPTERA|",
                            "ARTHROPOD|PARROTS")))

# Con este filtro, pasamos de 507 artículos a 448.
print(art_filt_Keywords_extra)

#### Se revisaron los títulos, resúmenes, y los nombres de la especies de encinos
#### Se removieron duplicados y aquellos trabajos que no tenían un enfoque de genética
#### Quedaron 169 artículos


#------ Busqueda de los artículos que sean de las 124 especies descritas en Black & Ashley (2021) ----

##### Busqueda de las 124 especies EN, CR y VU de la Oak RedList
library(readxl)
# Leer el archivo Excel
file_path1 <- "../data/Base_datos_encinos_endémicos.xlsx"
Base_datos_encinos_end <- read_excel(file_path1, sheet = "Oaks_RedList")

# Leer el archivo Excel
file_path2 <- "../data/database_448.xlsx"
art_169 <- read_excel(file_path2, sheet = "169_articles")

# Mostrar el data frame leído
print("Data frame leído:")
print(Base_datos_encinos_end)
print(art_169)

# Filtrar las filas con los valores  en la columna "IUCN Red List Category"
Base_B_A <- Base_datos_encinos_end %>%
  filter(`Backs_Ashley_2021` %in% "Y")

# Extraer el epíteto específico
epiteto_especifico <- sapply(strsplit(Base_B_A$`Scientific Name`, " "), `[`, 2)

# Mostrar el vector extraído
print("Epíteto específico extraído:")
print(epiteto_especifico)



# Extraer la columna 'Palabras' como un vector
palabras_buscar <- epiteto_especifico


# Lista de columnas en las que quieres buscar
colnames(art_169)

columnas_a_buscar <- c("Quercus_species", "Author_keywords", "Keywords_extra")


# Filtrar filas donde cualquier palabra aparece en al menos una de las columnas especificadas
filtrar_filas <- function(data, columnas, palabras) {
  patrones <- paste(palabras, collapse = "|") # Crear un patrón de regex uniendo las palabras con "|"
  data %>%
    filter(Reduce(`|`, lapply(columnas, function(col) str_detect(.[[col]], patrones))))
}

# Aplicar la función para filtrar las filas

filas_filtradas_art_B_A <- filtrar_filas(art_169, columnas_a_buscar, palabras_buscar)

# Mostrar las filas filtradas
print("Filas donde aparecen cualquiera de los epitetos de las 124 especies buscadas:")
print(filas_filtradas)

# Mostrar el vector extraído
print("Vector de palabras extraídas:")
print(palabras_buscar)



# Filtrar las filas donde Backs_Ashley_2021 es igual a "Y"
filtered_data_B_A <- Base_datos_encinos_end %>%
  filter(Backs_Ashley_2021 == "Y")

# Extraer el epíteto específico
epiteto_especifico <- sapply(strsplit(filtered_data_B_A$`Scientific Name`, " "), `[`, 2)

palabras_buscar <- epiteto_especifico
columnas_a_buscar <- c("Quercus_species", "Author_keywords", "Keywords_extra")

filtrar_filas <- function(data, columnas, palabras) {
  patrones <- paste(palabras, collapse = "|")
  data %>%
    filter(Reduce(`|`, lapply(columnas, function(col) str_detect(.[[col]], patrones))))
}

filas_filtradas_art_B_A <- filtrar_filas(art_169, columnas_a_buscar, palabras_buscar)

#----- cargar base de datos de los 49 artículos ------


# Guarda el archivo con los 49  artículos
write_xlsx(filas_filtradas_art_B_A, "../data/database_49.xlsx")


# Leer el archivo Excel
file_path4 <- "../data/database_49.xlsx"
art_49 <- read_excel(file_path4, sheet = "Sheet1")




#----------- Base de datos de 246 artículos y filtrado de especies microendémicas----------

library(readxl)
library(magrittr)
library(dplyr)
library(stringr)

# Leer el archivo Excel con los 246 artículos
file_path2 <- "../data/database_448.xlsx"
art_246 <- read_excel(file_path2, sheet = "246_articles")

# Mostrar el data frame leído
print("Data frame con los 246 artículos:")
print(art_246)


# Leer el archivo Excel con los datos de distribución y categorias de RedList
file_path2 <- "../data/database_hex_ecoreg_redlist.xlsx"
redlist_hex_eco <- read_excel(file_path2, sheet = "redlist_hex_eco")

# Mostrar el data frame leído
print("Data frame con datos del conteo de hex, de ecorregiones y categorías en la RedList:")
print(redlist_hex_eco)

# Filtro de las especies microendémicas:
# Conteo de hex menor a 20 y presentes en menos de 5 ecorregiones
df_filtered_microend <- redlist_hex_eco %>%
  filter(hex_count < 20, ecoregion_count < 5)

print(df_filtered_microend)
colnames(df_filtered_microend)
print(df_filtered_microend[,-c(1,3,4)])

# Extraer el epíteto específico
epiteto_especifico <- sapply(strsplit(df_filtered_microend$Scientific_Name, " "), `[`, 2)

# Agregar el elemento "sagraeana" al vector
#epiteto_especifico <- c(epiteto_especifico, "sagraeana")

# Mostrar el vector extraído
print("Epíteto específico extraído:")
print(epiteto_especifico)


# Renombrar el vector:
palabras_buscar <- epiteto_especifico


# Lista de columnas en las que quieres buscar
colnames(art_246)

columnas_a_buscar <- c("Quercus_species")


# Filtrar filas donde cualquier palabra aparece en al menos una de las columnas especificadas
filtrar_filas <- function(data, columnas, palabras) {
  patrones <- paste(palabras, collapse = "|") # Crear un patrón de regex uniendo las palabras con "|"
  data %>%
    filter(Reduce(`|`, lapply(columnas, function(col) str_detect(.[[col]], patrones))))
}

# Aplicar la función para filtrar las filas

art_filt_microend_oaks <- filtrar_filas(art_246, columnas_a_buscar, palabras_buscar)


write_xlsx(art_filt_microend_oaks, "../data/art_filt_microend_oaks_44.xlsx")


# Mostrar las filas filtradas
print("Filas donde aparecen cualquiera de los epitetos de las 114 especies consideras microendémicas:")
print(art_filt_microend_oaks)

# Mostrar el vector extraído
print("Vector de palabras extraídas:")
print(palabras_buscar)


#--------- Gráfico nube de palabras con la columna Author Keywords -------

# Carga las librerías
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)


# Creamos un vector con todas las palabras separadas por ;
all_keywords <- unlist(strsplit(art_filt_microend_oaks$Author_keywords, "; "))

# Convertimos todas las palabras a minúsculas
all_keywords <- tolower(all_keywords)


# Creamos una tabla de frecuencia de las palabras
word_freq <- table(all_keywords)


# Obtener las frecuencias de palabras y las palabras asociadas
word_freq <- unlist(word_freq)
palabras <- names(word_freq)

# Imprimir las palabras y sus frecuencias asociadas
for (i in seq_along(palabras)) {
  cat(palabras[i], ": ", word_freq[i], "\n")
}

# Definir una paleta de colores suaves y amigables
colors <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F")

# Crear la nube de palabras con la escala de colores personalizada
windows(width = 10, height = 8) # Cambia el tamaño según tus necesidades

wordcloud(words = names(word_freq), freq = word_freq, min.freq = 1,
          max.words = 50, random.order = FALSE, colors = colors)

# Exportar el gráfico como un archivo PNG
png("../plots/wordcloud.png", width = 8, height = 6, units = "in", res = 300)
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 1,
          max.words = 50, random.order = FALSE, colors = colors)
dev.off()









# Filtrar las filas donde Backs_Ashley_2021 es igual a "Y"
filtered_data_B_A <- Base_datos_encinos_end %>%
  filter(Backs_Ashley_2021 == "Y")

# Extraer el epíteto específico
epiteto_especifico <- sapply(strsplit(filtered_data_B_A$`Scientific Name`, " "), `[`, 2)

palabras_buscar <- epiteto_especifico
columnas_a_buscar <- c("Quercus_species", "Author_keywords", "Keywords_extra")

filtrar_filas <- function(data, columnas, palabras) {
  patrones <- paste(palabras, collapse = "|")
  data %>%
    filter(Reduce(`|`, lapply(columnas, function(col) str_detect(.[[col]], patrones))))
}

filas_filtradas_art_B_A <- filtrar_filas(art_169, columnas_a_buscar, palabras_buscar)

