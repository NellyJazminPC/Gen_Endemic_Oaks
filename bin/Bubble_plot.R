library(ggplot2)
library(dplyr)
library(ggrepel)
library(plotly)
library(tidyr)
library(magrittr)
library(stringr)

#---------------------- Gráficos de Burbujas ----------------------------

# Leer el archivo Excel
#file_path <- "../data/Base_datos_encinos_endémicos.xlsx"
#database_quercus_endemic_final <- read_excel(file_path, sheet = "Oaks_RedList")


# Filtrar los datos para incluir solo filas donde Backs_Ashley_2021_binary es 0
df_filtered <- database_quercus_endemic_final %>%
  filter(Backs_Ashley_2021_binary == 0)

# Crear el gráfico de burbujas
bubble_plot <- ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = country_count, size = hex_count, label = Scientific_Name)) +
  geom_point(alpha = 0.6, color = "blue") +  # Las burbujas
  geom_text_repel(size = 3, max.overlaps = Inf) +  # Etiquetas de los puntos sin superposición con max.overlaps ajustado
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  labs(title = "Bubble Plot of Quercus Endemic Data (Backs_Ashley_2021_binary = 0)",
       x = "IUCN Red List Scale",
       y = "Country Count",
       size = "Hex Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)




# Crear el gráfico de burbujas con hex_count en el eje Y y ecoregion_count como tamaño de las burbujas
bubble_plot <- ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, label = Scientific_Name)) +
  geom_point(alpha = 0.6, color = "blue") +  # Las burbujas
  geom_text_repel(size = 3, max.overlaps = Inf) +  # Etiquetas de los puntos sin superposición con max.overlaps ajustado
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  labs(title = "Bubble Plot of Quercus Endemic Data (Backs_Ashley_2021_binary = 0)",
       x = "IUCN Red List Scale",
       y = "Hex Count",
       size = "Ecoregion Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)


#-------- Plot coloreando las especies que solo están en MX y CN ------------


# Seleccionar solo las columnas de interés y combinar con Base_datos_encinos_end para obtener la columna Country distribution
df <- database_quercus_endemic_final %>%
  select(Scientific_Name, Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count) %>%
  inner_join(Base_datos_encinos_end %>% select(Scientific_Name, `Country distribution`), by = "Scientific_Name")

# Filtrar los datos para incluir solo filas donde Backs_Ashley_2021_binary es 0
df_filtered <- df %>%
  filter(Backs_Ashley_2021_binary == 0)

# Agregar una columna de color basada en la condición de los valores en Country distribution
df_filtered <- df_filtered %>%
  mutate(color = case_when(
    grepl("^MX$", `Country distribution`) ~ "red",
    grepl("^CN$", `Country distribution`) ~ "green",
    TRUE ~ "blue"
  ))

# Crear el gráfico de burbujas con hex_count en el eje Y y ecoregion_count como tamaño de las burbujas
bubble_plot <- ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, label = Scientific_Name, text = Scientific_Name)) +
  geom_point(aes(color = color), alpha = 0.6) +  # Las burbujas con color basado en la condición
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  scale_color_identity() +  # Usar los colores especificados en la columna color
  labs(title = "Bubble Plot of Quercus Endemic Data (Backs_Ashley_2021_binary = 0)",
       x = "IUCN Red List Scale",
       y = "Hex Count",
       size = "Ecoregion Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)


# Convertir el gráfico a un gráfico interactivo con plotly
interactive_bubble_plot <- ggplotly(bubble_plot, tooltip = "text")

# Mostrar el gráfico interactivo
print(interactive_bubble_plot)


#------------------ Bubble plot - Especies mexicanas ------------

# Seleccionar solo las columnas de interés y combinar con Base_datos_encinos_end para obtener la columna Country distribution
df <- database_quercus_endemic_final %>%
  select(Scientific_Name, Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count) %>%
  inner_join(Base_datos_encinos_end %>% select(Scientific_Name, `Country distribution`), by = "Scientific_Name")

# Filtrar los datos para incluir solo filas donde Backs_Ashley_2021_binary es 0 y Country distribution contiene "MX"
df_filtered <- df %>%
  filter(Backs_Ashley_2021_binary == 0 & grepl("MX", `Country distribution`))

# Crear el gráfico de burbujas con hex_count en el eje Y y ecoregion_count como tamaño de las burbujas
bubble_plot <- ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, label = Scientific_Name)) +
  geom_point(alpha = 0.6, color = "red") +  # Las burbujas en color rojo
  geom_text_repel(size = 3, max.overlaps = Inf) +  # Etiquetas de los puntos sin superposición con max.overlaps ajustado
  scale_x_continuous(breaks = 1:6, labels = c("CR", "EN", "VU", "NT", "LC", "DD")) +  # Etiquetas personalizadas para el eje X
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  labs(title = " ",
       x = "IUCN Red List Scale",
       y = "Hex Count",
       size = "Ecoregion Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)

# Convertir el gráfico a un gráfico interactivo con plotly
interactive_bubble_plot <- ggplotly(bubble_plot, tooltip = "text")

# Añadir anotaciones manualmente para evitar superposiciones
annotations <- list()
for (i in 1:nrow(df_filtered)) {
  annotations[[i]] <- list(
    x = df_filtered$IUCN_Red_List_Scale[i],
    y = df_filtered$hex_count[i],
    text = df_filtered$Scientific_Name[i],
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 2,
    ax = 20,
    ay = -30
  )
}

interactive_bubble_plot <- interactive_bubble_plot %>% layout(annotations = annotations)

# Mostrar el gráfico interactivo
print(interactive_bubble_plot)



#----------------- Bubble plot con todas las especies -------------------

# Seleccionar solo las columnas de interés y combinar con Base_datos_encinos_end para obtener la columna Country distribution
df <- database_quercus_endemic_final %>%
  select(Scientific_Name, Backs_Ashley_2021_binary, IUCN_Red_List_Scale, country_count, hex_count, ecoregion_count) %>%
  inner_join(Base_datos_encinos_end %>% select(Scientific_Name, `Country distribution`), by = "Scientific_Name")

# Agregar una columna de color y etiqueta basada en la condición de los valores en Country distribution
df <- df %>%
  mutate(color = case_when(
    grepl("^MX$", `Country distribution`) ~ "red",
    grepl("^CN$", `Country distribution`) ~ "green",
    TRUE ~ "blue"
  ),
  label = case_when(
    grepl("^MX$", `Country distribution`) ~ "only MX",
    grepl("^CN$", `Country distribution`) ~ "only CN",
    TRUE ~ "Other"
  ))

# Crear el gráfico de burbujas con hex_count en el eje Y y ecoregion_count como tamaño de las burbujas
bubble_plot <- ggplot(df, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, label = Scientific_Name, color = label)) +
  geom_point(alpha = 0.6) +  # Las burbujas con transparencia
  geom_text_repel(size = 3, max.overlaps = Inf) +  # Etiquetas de los puntos sin superposición con max.overlaps ajustado
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  scale_color_manual(values = c("only MX" = "red", "only CN" = "green", "Other" = "blue")) +  # Usar los colores especificados con etiquetas personalizadas
  labs(title = "All the Quercus species",
       x = "IUCN Red List Scale",
       y = "Hex Count",
       size = "Ecoregion Count",
       color = "Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)

# Convertir el gráfico a un gráfico interactivo con plotly
interactive_bubble_plot <- ggplotly(bubble_plot, tooltip = "text")

# Mostrar el gráfico interactivo
print(interactive_bubble_plot)



#----------------- Bubble plot - list of Back & Ashley (2021) -------------

# Filtrar los datos para incluir solo filas donde Backs_Ashley_2021_binary es 0
df_filtered <- df %>%
  filter(Backs_Ashley_2021_binary == 0)


# Agregar una columna de color y etiqueta basada en la condición de los valores en Country distribution
df_filtered <- df_filtered %>%
  mutate(color = case_when(
    grepl("^MX$", `Country distribution`) ~ "red",
    grepl("^CN$", `Country distribution`) ~ "green",
    TRUE ~ "blue"
  ),
  label = case_when(
    grepl("^MX$", `Country distribution`) ~ "only MX",
    grepl("^CN$", `Country distribution`) ~ "only CN",
    TRUE ~ "Other"
  ))

# Crear el gráfico de burbujas con hex_count en el eje Y y ecoregion_count como tamaño de las burbujas
bubble_plot <- ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, text = Scientific_Name, color = label)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.1, height = 0.1)) +  # Las burbujas con transparencia y jitter
  geom_text(aes(label = Scientific_Name), vjust = 1.5, hjust = 0.5, size = 3) + # Nombres de las especies
  scale_size_area(max_size = 20) +  # Ajustar el tamaño máximo de las burbujas
  scale_color_manual(values = c("only MX" = "red", "only CN" = "green", "Other" = "blue")) +  # Usar los colores especificados con etiquetas personalizadas
  scale_x_continuous(breaks = 1:6, labels = c("CR", "EN", "VU", "NT", "LC", "DD")) +  # Etiquetas personalizadas para el eje X
  labs(title = "",
       x = "IUCN Red List",
       y = "Hex Count",
       size = "Ecoregion Count",
       color = "Country distribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto del eje Y

# Mostrar el gráfico de burbujas
print(bubble_plot)


# Convertir el gráfico a un gráfico interactivo con plotly
interactive_bubble_plot <- ggplotly(bubble_plot, tooltip = "text")

# Mostrar el gráfico interactivo
print(interactive_bubble_plot)



#------- Hex count < 50 y Ecoregion count < 15 ------------------

df_filtered <- database_quercus_endemic_final %>%
  filter(hex_count < 20, ecoregion_count < 5)


# Agregar una columna con nombres de especies abreviados
df_filtered <- df_filtered %>%
  mutate(Species_abbrev = str_replace(Scientific_Name, "^Quercus\\s(\\S+)", "Q. \\1"))

# Verificar el resultado
head(df_filtered)

# Supongamos que `df_filtered` es tu data frame filtrado
# Agregar una columna de color y etiqueta basada en la condición de los valores en Country distribution
df_filtered <- df_filtered %>%
  mutate(color = case_when(
    grepl("^MX$", `Country distribution`) ~ "red",
    grepl("^CN$", `Country distribution`) ~ "green",
    grepl("^US$", `Country distribution`) ~ "purple",
    TRUE ~ "blue"
  ),
  label = case_when(
    grepl("^MX$", `Country distribution`) ~ "only MX",
    grepl("^CN$", `Country distribution`) ~ "only CN",
    grepl("^US$", `Country distribution`) ~ "only US",
    TRUE ~ "Other"
  ))


# Crear el gráfico de burbujas utilizando la columna Species_abbrev y una paleta manual bien definida
ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, color = label)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.3, height = 0)) +
  geom_text_repel(aes(label = Species_abbrev), size = 3, max.overlaps = 30) +
  scale_size_continuous(name = "Ecoregion Count") +
  scale_color_manual(values = c("only MX" = "#0072B2", "only CN" = "#CC79A7", "only US" = "#009E73", "Other" = "#D55E00"), 
                     name = "Country Distribution") +
  scale_x_continuous(breaks = 1:6, labels = c("CR", "EN", "VU", "NT", "LC", "DD")) +  # Etiquetas personalizadas para el eje X
  geom_vline(xintercept = 1:6, linetype = "longdash", color = "grey") +  # Agregar líneas en los puntos de break del eje x
  theme_minimal() +
  labs(title = "Filtered Bubble Chart of Oak Species",
       x = "IUCN Red List",
       y = "Hex Count") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.margin = unit(c(1, 1, 2, 1), "cm"))  # Ajustar los márgenes del gráfico


#----------- Sin los encinos de la categoría DD --------------------

# Filtrar los datos para excluir los que tienen "DD" en la columna IUCN Red List Category
df_filtered <- df_filtered %>%
  filter(`IUCN Red List Category` != "DD")


# Agregar una columna de etiqueta basada en la condición de los valores en Country distribution
df_filtered <- df_filtered %>%
  mutate(label = case_when(
    grepl("^MX$", `Country distribution`) ~ "only MX",
    grepl("^CN$", `Country distribution`) ~ "only CN",
    grepl("^US$", `Country distribution`) ~ "only US",
    TRUE ~ "Other"
  ))

# Crear el gráfico de burbujas utilizando la columna Species_abbrev y una paleta manual bien definida
ggplot(df_filtered, aes(x = IUCN_Red_List_Scale, y = hex_count, size = ecoregion_count, color = label)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.3, height = 0)) +
  geom_text_repel(aes(label = Species_abbrev), size = 3, max.overlaps = 20) +
  scale_size_continuous(name = "Ecoregion Count") +
  scale_color_manual(values = c("only MX" = "#0072B2", "only CN" = "#CC79A7", "only US" = "#009E73", "Other" = "#D55E00"), 
                     name = "Country Distribution") +
  scale_x_continuous(breaks = 1:5, labels = c("CR", "EN", "VU", "NT", "LC")) +  # Etiquetas personalizadas para el eje X sin "DD"
  geom_vline(xintercept = 1:5, linetype = "dashed", color = "grey") +  # Agregar líneas en los puntos de break del eje x
  theme_minimal() +
  labs(title = "Filtered Bubble Chart of Oak Species",
       x = "IUCN Red List",
       y = "Hex Count") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.margin = unit(c(1, 1, 2, 1), "cm"))  # Ajustar los márgenes del gráfico
