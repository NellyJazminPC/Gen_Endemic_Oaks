
library(pdftools)
library(tidyverse)
library(ggplot2)


pdf_oak_uicn <- "/Users/nelly/Downloads/Oaks2020HR-41-50.pdf"
raw_text <- map(pdf_oak_uicn, pdf_text)

#Función para Scrape data and clean
clean_table1 <- function(raw){
  
  #Split the single pages
  raw <- map(raw, ~ str_split(.x,"\\n")) %>% unlist()
  #Concatenate the split pages
  raw <- reduce(raw, c)
  
  
  #Quercus albicaulis Quercus devia
  #quercus disciformis quercus ajoensis
  #Quercus arkansana Quercus rysophylla
  #Quercus sadleriana Quercus porphyrogenita*
  
  
  # Especifica el inicio y el final
  table_start <- stringr::str_which(tolower(raw), "nombres")
  table_end <- stringr::str_which(tolower(raw), "quercus ajoensis")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #Construye la tabla y elimina los caracteres especiales
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}","|")
  text_con <-textConnection(table)
  data_table <-read.csv(text_con, sep = "|")
  
  #Crea una lista de los nombres de las columnas
  colnames(data_table) <- c("Scientific Name", 
                            "UICN Red List Category",
                            "Country distribution",
                            "Number of ex situ collections", 
                            "Threat codes","Quercus section")
  data_table
}

map_df(raw_text, clean_table1)
#results_01 <- map_df(raw_text, clean_table1)
results_02 <- map_df(raw_text, clean_table1)


head(results_02)
class(results_01)
 results <- merge(results_01, results_02)
 results <- bind_rows(results_01, results_02)

head(raw_text)



