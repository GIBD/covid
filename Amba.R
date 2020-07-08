
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(knitr)
library(kableExtra)
library(highcharter)
library(rjson)
library(plotly)
library(gganimate)
library(stringr)
library(leaflet)
library(sf)
library(tmap)
library(googlesheets4)
library(readxl)
library(RcppRoll)



  # amba <- read_excel("data/covid19casos.xlsx")
  # names(amba) <- c("id_evento_caso", "sexo", "edad", "edad_anos_meses", "residencia_pais_nombre",
  #                       "residencia_provincia_nombre", 	"residencia_departamento_nombre",
  #                       "carga_provincia_nombre", 	"fecha_inicio_sintomas", 	"fecha_apertura",
  #                       "sepi_apertura", 	"fecha_internacion", 	"cuidado_intensivo", 	"fecha_cui_intensivo",
  #                       "fallecido", 	"fecha_fallecimiento",	"asistencia_respiratoria_mecanica",
  #                       "carga_provincia_id",	"origen_financiamiento",	"Clasificacion",
  #                       "clasificacion_resumen",	"residencia_provincia_id", 	"fecha_diagnostico",
  #                       "residencia_departamento_id",	"ultima_actualizacion")
  # amba <- amba %>% filter(residencia_provincia_nombre %in% c("Buenos Aires", "CABA"))
  

  amba <- read_excel("data/argentina.xlsx", "GBA")
  amba$fecha <- as.Date(amba$fecha,"%d/%m/%Y")
  last_date <- last(amba$fecha)
  
  amba <- amba %>% left_join(cordones, by=c("Partido"="Partido"))
  cordones <- read_excel("data/argentina.xlsx", "POBLAC_AMBA")
  
  norte <- c("Escobar", "General San Martín", "José C. Paz", "Malvinas Argentinas", "Pilar", "San Fernando", "San Isidro",
           "San Miguel", "Tigre","Vicente López")
  
  oeste <- c("General Rodríguez", "Hurlingham", "Ituzaingó", "La Matanza", "Marcos Paz",
           "Merlo", "Moreno", "Morón", "Tres de Febrero")
  
  sur <- c("Almirante Brown", "Avellaneda", "Berazategui", "Esteban Echeverría", "Ezeiza",
         "Florencio Varela", "Lanús", "Lomas de Zamora", "Presidente Perón", "Quilmes", "San Vicente")
  
  amba %>% mutate(zona = if_else(Partido %in% norte, "Norte",
                                 if_else(Partido %in% oeste, "Oeste", 
                                         if_else(Partido %in% sur, "Sur", "")))) %>% 
    select(zona, dia=FECHA, distrito=Partido, cantidad=CASOS) -> plot_data_n

  p <- plot_data_n %>% 
    ggplot(mapping=aes(x=dia, color=distrito, y=cantidad)) +
    geom_line() + geom_point() + 
    geom_smooth(method = 'loess', formula = 'y ~ x', alpha = 0.2, size = 1, span = .3, se=FALSE) + 
    labs(title = paste("COVID-19 en PBA"), 
         subtitle = paste0("Variación de los casos diarios Zona Norte Buenos Aires por día (al: ", last_date, ")") , 
         y = "Casos",  x = "Fecha") +
    theme_elegante_std(base_family = "Assistant") +
     facet_wrap(~ zona, scales = "free_y", ncol = 1) 
  ggplotly(p)
  
  
  
  
  amba %>%  filter(Partido %in% norte) %>% select(dia=FECHA, distrito=Partido, cantidad=CASOS) -> plot_data_n
  
  p <- plot_data_n %>% 
    ggplot(mapping=aes(x=dia, color=distrito, y=cantidad)) +
    geom_line() + geom_point() + 
    geom_smooth(method = 'loess', formula = 'y ~ x', alpha = 0.2, size = 1, span = .3, se=FALSE) + 
    labs(title = paste("COVID-19 en PBA"), 
         subtitle = paste0("Variación de los casos diarios Zona Norte Buenos Aires por día (al: ", last_date, ")") , 
         y = "Casos",  x = "Fecha") +
    theme_elegante_std(base_family = "Assistant") 
  
  ggplotly(p)
  