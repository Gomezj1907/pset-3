## ************************ ##
##  **** PROBLEM SET 3 **** ##
## ************************ ##

# Version.string R version 4.2.1 (2022-06-23 ucrt)
# nickname       Funny-Looking Kid  

# INTEGRANTES / Autores: 
# + JORGE GOMEZ  201923526
# + FELIPE CUADROS 201820965

# Clean environment

rm(list = ls())

require(pacman)

p_load(tidyverse, data.table, rio)

##  **** PUNTO 1 **** ##

## Punto 1.1

files <- list.files("input", recursive = T, full.names = T)

## Punto 1.2

files_resto <- str_subset(string = files, pattern = "Resto - Car")

all_files <- import_list(files_resto)

extract_files <- function(files_list, paths){
  
  for (i in 1:length(files_list)) {
    files_list[[i]] <- janitor::clean_names(files_list[[i]])
    files_list[[i]]$path <-  paths[i]
    files_list[[i]]$year <- str_extract(files_list[[i]]$path,"\\d+")
    
  }
  
  return(files_list)
}

list_result <- extract_files(all_files,files_resto)

## Punto 1.3

cg = rbindlist(l = list_result , use.names = T, fill = T)

export(cg, "output/geih_cg.rds")


##  **** PUNTO 3 **** ##


#Graficas#

cg <- import("output/geih_cg.rds")
colnames(cg)

# GRAFICA 1

cg <- cg |>
  mutate(age_group = case_when(p6040 < 18 ~ "niños", 
                               p6040 >=18 & p6040 <= 28 ~ "jovenes",
                               p6040 >28 ~ "adultos"),
         sexo_text = case_when(p6020 ==1 ~ "Hombre", p6020 == 2 ~ "Mujer")) 



cg |>
  group_by(sexo_text, age_group) |>
  summarize(total = n()) |> na.omit() %>% 
  ggplot(aes(x = sexo_text, y = total, fill = sexo_text)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title ="Distribución de sexo grupos de edad",x = "", y = "Total personas") +
  facet_wrap(~age_group)+
  theme_bw() +
  theme(legend.position="none")

# GRAFICA 2

cg <- cg |> group_by(hogar) |>
  mutate(jefe_mujer = case_when(p6050 ==1 & p6020 == 2 ~ "jefe del hogar es mujer", 
                                p6050 ==1 & p6020 == 1 ~ "jefe del hogar no es mujer")) 


cg |> group_by(hogar, jefe_mujer) %>% 
  summarise(mean_age_hogar = mean(p6040, na.rm = T)) |> na.omit() |>
  ggplot(aes(x =jefe_mujer, y = mean_age_hogar,fill = jefe_mujer)) +
  geom_bar(position = "dodge", stat = "identity")   +
  labs(title = "Edad promedio hogares Madres cabeza de familia",x = "", y = "edad promedio del hogar") +
  theme_bw() +
  theme(legend.position="none")





