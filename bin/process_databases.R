readxl::read_excel("../data/Appendix_2_1Classifications_Quercusspecies.xlsx", sheet = "All species")
library(readxl)
library(tidyverse)
data_oak_redlist <- read_excel("../data/Base_datos_encinos_endémicos.xlsx", 
                               sheet = "Oaks_RedList", col_names = T)
#convertir a data.frame
data_oak <- as.data.frame(data_oak_redlist)
#Nombrar las columnas
colnames(data_oak) <- c("Name", "UICN_RedList_Category", "Country", "Ex_situ_collections","Threats", "Section")
head(data_oak)
##### Manipulación de la base de datos
#Reemplazar los NA con 0
data_oak$Threats[data_oak$Threats == "-"] <- NA
data_oak$Ex_situ_collections[data_oak$Ex_situ_collections == NA ] <- "0"
data_oak  %>%
  select(UICN_RedList_Category, Ex_situ_collections) %>%
  # "replace_na"  reemplaza los NA por "0" 
  mutate(Ex_situ_collections = replace_na(Ex_situ_collections, "0")) %>%
  # no contabiliza los NA en columna, pero como se modificaron por "0"
  # igual mostrará todos los registros 
  drop_na(Ex_situ_collections) 

### Agrupar y contar cuantos individuos hay en las dos categorías que estamos considerando para definir a un encino como endémico

data_oak %>%                       
  group_by(UICN_RedList_Category)

data_oak %>% 
  filter(UICN_RedList_Category %in% c("EN", "CR")) %>%
  filter(Country == "MX") %>%
  select(Name) %>%
  tally()


library(stringr)

bins2 <- c(1:430)

db_long_countries <- str_split_fixed(data_oak$Country, ",", n=Inf) %>%
  cbind(bins2)

db_countries <- cbind(data_oak, bins2)  %>% 
  merge(db_long_countries, by= "bins2")

#Formato ancho a largo

db_long_countries <- db_countries  %>%
  pivot_longer(V1:V43, names_to = "Freq", values_to = "Distribution") %>%
  as.data.frame() 
  
### Necesitamos reemplazar los espacios vacios por NA 

db_long_countries$Distribution[db_long_countries$Distribution == ""] <- NA 

### Quitamos las filas con NA y las columnas iniciales de Country, Freq y bins2
db_long_countries <- db_long_countries[!is.na(db_long_countries$Distribution),] %>%
  select(-Country, -Freq, -bins2)


db_long_countries %>% 
  filter(UICN_RedList_Category %in% c("EN", "CR")) %>%
  filter(Distribution == "MX") %>%
  select(Name) %>%
  tally()

