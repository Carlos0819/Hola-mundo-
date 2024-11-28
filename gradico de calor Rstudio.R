# Instalar y cargar las librerías necesarias
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

library(stringr)


library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

library(readxl)
saludcompleto <- read_excel("~/ARTICULO INVESTIGACIÓN/base de datos/saludcompleto.xlsx")
View(saludcompleto)

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar los datos desde el archivo Excel
file_path <- "~/ARTICULO INVESTIGACIÓN/base de datos/saludcompleto.xlsx"
saludcompleto <- read_excel(file_path)

# Asegurarse de que las variables están en el formato correcto
saludcompleto <- saludcompleto %>%
  mutate(municipio = as.factor(municipio),
         edad = as.factor(edad),
         Variable = as.factor(Variable))

# Calcular el total de casos por municipio
total_por_municipio <- saludcompleto %>%
  group_by(municipio) %>%
  summarise(total_casos_municipio = sum(nuevoval, na.rm = TRUE))

# Mostrar los municipios con más "calor" (más casos)
municipios_con_mas_casos <- total_por_municipio %>%
  arrange(desc(total_casos_municipio))

print(municipios_con_mas_casos)


# Calcular el total de casos por municipio y trastorno mental
casos_por_trastorno_municipio <- saludcompleto %>%
  group_by(Variable, municipio) %>%
  summarise(total_casos = sum(nuevoval, na.rm = TRUE)) %>%
  arrange(Variable, desc(total_casos))

# Mostrar los municipios con más casos por trastorno mental
print(casos_por_trastorno_municipio)


# Alternativamente, si deseas ver solo los municipios con más casos para cada trastorno mental
municipios_top_por_trastorno <- casos_por_trastorno_municipio %>%
  group_by(Variable) %>%
  top_n(5, total_casos)  # Puedes ajustar el número 5 para mostrar más o menos municipios

print(municipios_top_por_trastorno)

# Calcular el porcentaje transversal por municipio y condición
saludcompleto_pct <- saludcompleto %>%
  group_by(municipio, Variable) %>%
  summarise(total_val = sum(nuevoval, na.rm = TRUE), .groups = 'drop') %>%
  left_join(total_por_municipio, by = "municipio") %>%
  mutate(pct_val = (total_val / total_casos_municipio) * 100)  # Calcular el porcentaje

# Modificar los nombres en la variable X (Variable)
saludcompleto_pct <- saludcompleto_pct %>%
  mutate(Variable = factor(Variable)) %>%
  mutate(Variable = recode(Variable,
                           "depresion" = "E. depresivos",
                           "ansiedad" = "E. de ansiedad",
                           "esquizofrenia" = "E. esquizofrenia",
                           "epilepsia" = "Epilepsia",
                           "transcompsuspsico" = "T. sust. psicotrópicas",
                           "transconducta" = "T. de conducta"))

saludcompleto_pct <- saludcompleto_pct %>%
  mutate(municipio = str_replace_all(municipio, "_", " "))

# Crear el mapa de calor con los nombres actualizados y una estética mejorada
ggplot(saludcompleto_pct, aes(x = Variable, y = municipio, fill = pct_val)) +
  geom_tile(color = "white") +  # Bordes blancos para separar las celdas
  labs(title = "Mapa de Calor de Enfermedades Mentales por Municipio en el departamento de Santa Cruz (2018-2023)",
       x = "Condición de Salud", y = "Municipio", fill = "Frecuencia de casos") +
  scale_fill_gradient2(low = "lightyellow", mid = "lightyellow", high = "darkred", 
                       midpoint = median(saludcompleto_pct$pct_val), 
                       breaks = c(0, 25, 50, 75, 100),  # Puntos de quiebre específicos
                       labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),  # Etiquetas limpias
                       name = "Frecuencia de casos") +  # Nombre de la leyenda ajustado
  theme_minimal(base_size = 11) +  # Tamaño de la fuente base reducido
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Tamaño reducido en eje X con rotación para legibilidad
    axis.text.y = element_text(size = 8),  # Tamaño de las etiquetas en el eje Y ajustado
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Título centrado y ajustado
    legend.title = element_text(size = 12, face = "bold"),  # Ajustar tamaño y estilo del título de la leyenda
    legend.text = element_text(size = 10)  # Ajustar tamaño del texto de la leyenda
  )

# Guardar el gráfico
ggsave("visualizacion_mapcalor_actualizado.png", width = 12, height = 8)

























