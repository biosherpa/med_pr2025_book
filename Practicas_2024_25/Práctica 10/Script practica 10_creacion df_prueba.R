
#Le digo el directorio en el que trabajamos
setwd("C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 10")

set.seed(123)

# Crear el vector 'sex' con valores 0 o 1, con probabilidad 0.5 para cada uno
sex <- rbinom(536, 1, 0.5)

# Definir el rango de fechas de nacimiento
edad_min <- 34
edad_max <- 92

# Definir la fecha de referencia
fecha_fin <- as.Date("2022-12-31")

# Calcular fechas de nacimiento mínima y máxima
fecha_nac_min <- fecha_fin - as.difftime(edad_max * 365.25, units = "days")
fecha_nac_max <- fecha_fin - as.difftime(edad_min * 365.25, units = "days")

# Generar un vector con todas las fechas posibles en el rango
fechas_posibles <- seq.Date(fecha_nac_min, fecha_nac_max, by = "day")

# Seleccionar 536 fechas aleatorias del vector de fechas posibles
set.seed(123)  # Para reproducibilidad
fech_nac <- sample(fechas_posibles, size = 536, replace = TRUE)

print(fech_nac[1:10])

edad<- (fecha_fin - fech_nac)/365.25
edad<- as.numeric (edad)
print (edad[1:10])

summary (edad)
mean (edad)
print (edad[11:30])

# Ajustar la probabilidad de hipertensión según la edad
# Usamos un enfoque donde la probabilidad de hipertensión aumenta con la edad
prob_hta <- ifelse(edad < 40, 0.1,           # 10% probabilidad de HTA si la edad es menor de 40
            ifelse(edad < 60, 0.3,           # 30% probabilidad de HTA si la edad está entre 40 y 59
            ifelse(edad < 75, 0.5,           # 50% probabilidad de HTA si la edad está entre 60 y 74
                          0.7)))            # 70% probabilidad de HTA si la edad es mayor de 75

# Generar la variable 'hta' como binaria (0 o 1) usando la probabilidad calculada
hta <- rbinom(536, 1, prob_hta)




# Crear el vector 'alt' con valores aleatorios dependiendo del valor de 'sex'
alt <- ifelse(sex == 0, rnorm(536, mean = 160, sd = 8), rnorm(536, mean = 175, sd = 10))

#Redondear altura
alt <- round(alt, 1)

# Ajustar valores fuera del rango deseado
alt[alt < 145] <- 145
alt[alt > 198] <- 199


# Crear el vector 'peso' como alt - 100 con una variación aleatoria entre -10 y +25
peso <- (alt - 100) + sample(-10:25, 536, replace = TRUE)
#Redondear peso
peso <- round(peso, 1)

# Ajustar valores fuera del rango deseado
peso[peso < 45] <- 45
peso[peso > 140] <- 140



# Crear el vector 'fum' con la probabilidad condicional según el valor de 'sex'
prob_fum <- ifelse(sex == 1, 0.3, 0.25)
fum <- rbinom(536, 1, prob_fum)

# Crear la variable 'colesterol' con distribución normal, media 186 y desviación estándar 20
colesterol <- rnorm(536, mean = 186, sd = 50)

# Redondear la variable 'colesterol' a 0 decimales
colesterol <- round(colesterol, 0)

# Ajustar valores fuera del rango deseado
colesterol[colesterol < 140] <- 140
colesterol[colesterol > 300] <- 300


# Calcular el índice de masa corporal (IMC) como alt/peso^2
imc <- peso/((alt/100)^2) 

# Creamos clas_soc con diferente probabilidad de distirbución de diferentes factores de riesgo
z_alta <- 1 - 0.01 * imc - 0.005 * colesterol + 0.005 * (1 - hta) - 0.2 * fum  # Fumar disminuye probabilidad de ser alta
z_media <- 0.2 + 0.01 * imc + 0.005 * colesterol + 0.1 * fum                  # Fumar aumenta probabilidad de ser media
z_baja <- 0.01 * imc + 0.005 * colesterol + 0.01 * hta + 0.3 * fum             # Fumar aumenta probabilidad de ser baja

# Aplicar la función softmax para convertir los valores a probabilidades
denominator <- exp(z_alta) + exp(z_media) + exp(z_baja)  # Denominador común

prob_alta <- exp(z_alta) / denominator
prob_media <- exp(z_media) / denominator
prob_baja <- exp(z_baja) / denominator

# Asignar clase social basada en las probabilidades
clas_soc <- mapply(function(pa, pm, pb) {
  sample(c(0, 1, 2), size = 1, prob = c(pb, pm, pa))  # 0 = baja, 1 = media, 2 = alta
}, pa = prob_alta, pm = prob_media, pb = prob_baja)

# Verificamos los primeros valores de 'clas_soc'
head(clas_soc)

# Verificar la distribución de las clases
table(clas_soc)

#Creamos DM
# Generamos la probabilidad de tener DM basada en clas_soc
prob_DM <- ifelse(clas_soc == 1, 0.10,  # 20% si clas_soc = 1 (media)
                  ifelse(clas_soc == 0, 0.20,  # 10% si clas_soc = 0 (alta)
                         ifelse(clas_soc == 2, 0.06, 0)))  # 6% si clas_soc = 2 (baja)

# Generar la variable DM de manera probabilística (0 = no, 1 = sí)
DM <- rbinom(536, size = 1, prob = prob_DM)

table(DM)



# Añadir un valor aleatorio entre -0.2 y 0.2 al log_RR_total
random_variation <- runif(536, min = -0.1, max = 0.1) 

# Calcular el logaritmo del riesgo relativo total
log_RR <- log (1.001) * edad + log(1.05) * sex + log (1.01)* hta + log(1.0038) * colesterol + log(2.5) * fum + log(1.001) * imc - log(1.8) * clas_soc  + log(1.1)* DM +random_variation - 2.6

# Convertir el logaritmo del riesgo relativo a una probabilidad mediante la función logística
prob_iam <- 1 / (1 + exp(-log_RR))

# Generar la variable 'iam1' como binaria (0 o 1) usando la probabilidad calculada
iam1 <- rbinom(536, 1, prob_iam)
table (iam1)



df_prueba <- data.frame(fech_nac,sex, alt, peso, imc,hta,fum, DM, colesterol, clas_soc, iam1)



# Etiquetamos variables

# Convertir a factor la variable clase_social
clas_soc_factor <- factor(clas_soc, levels = c(0, 1, 2), labels = c("baja", "media", "alta"))




# Convertir variables a factor con etiquetas

df_prueba$sex <- factor(df_prueba$sex, levels = c(0, 1), labels = c("Mujer", "Varón"))
df_prueba$hta <- factor(df_prueba$hta, levels = c(0, 1), labels = c("No", "Sí"))
df_prueba$fum <- factor(df_prueba$fum, levels = c(0, 1), labels = c("No", "Sí"))
df_prueba$iam1 <- factor(df_prueba$iam1, levels = c(0, 1), labels = c("No", "Sí"))
df_prueba$clas_soc <- factor(df_prueba$clas_soc, levels = c(0, 1, 2), labels = c("Baja", "Media", "Alta"))



write.csv(df_prueba, file = "df_prueba.csv", row.names = FALSE)


