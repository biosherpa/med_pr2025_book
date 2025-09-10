
#Le digo el directorio en el que trabajamos
setwd("C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 6")

set.seed(123)

# Crear el vector 'sex' con valores 0 o 1, con probabilidad 0.5 para cada uno
sex <- rbinom(936, 1, 0.5)

# Definir el rango de fechas de nacimiento
edad_min <- 34
edad_max <- 92

# Definir la fecha de referencia
fecha_fin <- as.Date("2023-12-31")

# Calcular fechas de nacimiento mínima y máxima
fecha_nac_min <- fecha_fin - as.difftime(edad_max * 365.25, units = "days")
fecha_nac_max <- fecha_fin - as.difftime(edad_min * 365.25, units = "days")

# Generar un vector con todas las fechas posibles en el rango
fechas_posibles <- seq.Date(fecha_nac_min, fecha_nac_max, by = "day")

# Seleccionar 936 fechas aleatorias del vector de fechas posibles
set.seed(123)  # Para reproducibilidad
fech_nac <- sample(fechas_posibles, size = 936, replace = TRUE)

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
hta <- rbinom(936, 1, prob_hta)




# Crear el vector 'alt' con valores aleatorios dependiendo del valor de 'sex'
alt <- ifelse(sex == 0, rnorm(936, mean = 160, sd = 8), rnorm(936, mean = 175, sd = 10))

#Redondear altura
alt <- round(alt, 1)

# Ajustar valores fuera del rango deseado
alt[alt < 145] <- 145
alt[alt > 198] <- 199


# Crear el vector 'peso' como alt - 100 con una variación aleatoria entre -10 y +25
peso <- (alt - 100) + sample(-10:25, 936, replace = TRUE)
#Redondear peso
peso <- round(peso, 1)

# Ajustar valores fuera del rango deseado
peso[peso < 45] <- 45
peso[peso > 140] <- 140



# Crear el vector 'fum' con la probabilidad condicional según el valor de 'sex'
prob_fum <- ifelse(sex == 1, 0.3, 0.25)
fum <- rbinom(936, 1, prob_fum)

# Crear la variable 'colesterol' con distribución normal, media 186 y desviación estándar 20
colesterol <- rnorm(936, mean = 186, sd = 50)

# Redondear la variable 'colesterol' a 0 decimales
colesterol <- round(colesterol, 0)

# Ajustar valores fuera del rango deseado
colesterol[colesterol < 140] <- 140
colesterol[colesterol > 300] <- 300


# Calcular el índice de masa corporal (IMC) como alt/peso^2
imc <- peso/((alt/100)^2) 

# Definir la probabilidad de clase alta (1) o baja (0) en función de IMC, colesterol y hipertensión
# Mayor probabilidad de clase alta si imc <= 25, colesterol <= 250 y no tiene hipertensión
# Menor probabilidad de clase alta si imc > 25, colesterol > 250 o tiene hipertensión

prob_clase_social <- ifelse(imc <= 25 & colesterol <= 250 & hta == 0, 
                            0.8,     # 80% probabilidad de ser clase alta si imc <= 25, colesterol <= 250, y no tiene hipertensión
                     ifelse(imc > 25 & colesterol > 250 & hta == 1, 
                            0.2,     # 20% probabilidad de ser clase alta si IMC > 25, colesterol > 250, y tiene hipertensión
                     ifelse(imc > 25 | colesterol > 250 | hta == 1, 
                            0.4,     # 40% probabilidad de ser clase alta si al menos uno de los factores de riesgo está presente
                            0.6)))   # 60% probabilidad en otros casos donde los factores de riesgo son moderados

# Generar la variable 'clase_social' como binaria (0 = baja, 1 = alta) usando la probabilidad calculada
clas_soc <- rbinom(936, 1, prob_clase_social)




# Añadir un valor aleatorio entre -0.2 y 0.2 al log_RR_total
random_variation <- runif(936, min = -0.1, max = 0.1) 

# Calcular el logaritmo del riesgo relativo total
log_RR <- log (1.001) * edad + log(1.05) * sex + log (1.01)* hta + log(1.0038) * colesterol + log(2.5) * fum + log(1.001) * imc - log(1.8) * clas_soc  + random_variation - 2.5


# Convertir el logaritmo del riesgo relativo a una probabilidad mediante la función logística
prob_iam <- 1 / (1 + exp(-log_RR))

# Generar la variable 'iam' como binaria (0 o 1) usando la probabilidad calculada
iam <- rbinom(936, 1, prob_iam)

#fech_nac viene definida como character, cambiamos a formato fecha
fech_nac <- as.Date(fech_nac)
fecha_fin <- as.Date("2023-12-31")
edad <- (fecha_fin-fech_nac)/365.25

edad <- as.numeric (round (edad,0))



# Crear el dataframe con todas las variables
df_iam2 <- data.frame(edad,sex, alt, peso, imc,hta,fum, colesterol, clas_soc, iam)


#Categorizamos iam
df_iam2$imc_r <- cut(df_iam2$imc,
                    breaks = c(-Inf, 20, 30, Inf),
                    labels = c("Bajo", "Normal", "Obesidad"))
# Convertir variables a factor con etiquetas

df_iam2$sex <- factor(df_iam2$sex, levels = c(0, 1), labels = c("Mujer", "Varón"))
df_iam2$hta <- factor(df_iam2$hta, levels = c(0, 1), labels = c("No", "Sí"))
df_iam2$fum <- factor(df_iam2$fum, levels = c(0, 1), labels = c("No", "Sí"))
df_iam2$clas_soc <- factor(df_iam2$clas_soc, levels = c(0, 1), labels = c("Baja", "Alta"))
df_iam2$iam <- factor(df_iam2$iam, levels = c(0, 1), labels = c("No", "Sí"))


table (df_iam2$iam)

summary (imc)

write.csv(df_iam2, file = "C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 6/df_iam2.csv", row.names = FALSE)


