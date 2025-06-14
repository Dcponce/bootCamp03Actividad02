# Instalar y cargar librerías (si no las tienes instaladas)
install.packages(c("tidyverse", "readr", "ggplot2", "skimr", "DataExplorer","dplyr"))
library(tidyverse)
library(readr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(dplyr)

# --- 2. Limpieza y Transformación de los Datos ---

# 2.1 Cargar el conjunto de datos
# Asegúrate de que el archivo CSV esté en el mismo directorio que tu R script/notebook
# O proporciona la ruta completa al archivo
df <- read_csv("C:/Users/David Cordova/Desktop/Curso python/info git/bootCamp03Actividad02/exchanges.csv")

# 2.2 Inspeccionar la estructura inicial de los datos
#Ver primeras filas
head(df)

#Ver ultimas filas
tail(df)

#Dimensiones
dim(df)

#Nombres de columnas
colnames(df)

#Ver estructura del dataset
str(df)

#Ver resumen estadistico
summary(df)

#Tipo de variables
sapply(df, class)

#Ver valores unicos
sapply(df, function(x) length(unique(x)))

#Verificar valores NA
colSums(is.na(df))

# Porcentaje de NA
mean(is.na(df)) * 100

#Ver filas completas
sum(complete.cases(df))

#Ver duplicados
sum(duplicated(df))

#Resumen detallado
skim(df)

#overview
plot_intro(df)

# 2.3 Renombrar la columna de fecha para mayor claridad (si es necesario)
# Suponiendo que la columna de fecha se llama "Date" o similar
df <- df %>%
  rename(Fecha = date)

# 2.4 Convertir la columna de fecha al formato de fecha correcto
df <- df %>%
  mutate(Fecha = ymd(Fecha))

# 2.5 Identificar y manejar valores faltantes
# Contar valores faltantes por columna
missing_values <- colSums(is.na(df))
print("Valores faltantes por columna:")
print(missing_values)

#Estrategias para valores faltantes

df_clean <- df %>%
  # Primero, ordenar por fecha para un rellenado correcto
  arrange(Fecha) %>%
  # Rellenar NA hacia adelante
  fill(starts_with("US"), .direction = "down") %>%
  # Luego, rellenar cualquier NA restante hacia atrás
  fill(starts_with("US"), .direction = "up")

# 2.6 Verificar tipos de datos después de la limpieza
print(str(df_clean))
print(summary(df_clean))

# --- 3. Análisis Exploratorio de Datos (EDA) ---

# 3.1 Resumen estadístico de las variables numéricas
# Resumen de las columnas de tipo de cambio
print(summary(df_clean %>% select(starts_with("US"))))

# 3.2 Calcular medias, medianas, desviaciones estándar específicas
df_stats <- df_clean %>%
  select(starts_with("US")) %>%
  summarise(
    across(everything(), list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ))
  )
print(df_stats)

# 3.3 Identificar patrones y tendencias (ej. a lo largo del tiempo)
# Podemos observar la tendencia de una moneda específica
# Por ejemplo, la tasa de cambio USD

df_clean %>%
  ggplot(aes(x = Fecha, y = `USD`)) +
  geom_line() +
  labs(title = "Tendencia diaria del USD", x = "Fecha", y = "Tasa de Cambio") +
  theme_minimal()

#Distribución de dos tasas de cambio específicas.
ggplot(df_clean, aes(x = USD, y = JPY)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relación entre USD y JPY",
       x = "Tasa de Cambio USD",
       y = "Tasa de Cambio JPY") +
  theme_minimal()