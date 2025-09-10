
#Le digo el directorio en el que trabajamos
setwd("C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 13")

set.seed(123)

# Crear el vector 'sex' con valores 0 o 1, con probabilidad 0.5 para cada uno
sex <- rbinom(984, 1, 0.5)

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

# Seleccionar 984 fechas aleatorias del vector de fechas posibles
set.seed(123)  # Para reproducibilidad
fech_nac <- sample(fechas_posibles, size = 984, replace = TRUE)

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
hta <- rbinom(984, 1, prob_hta)




# Crear el vector 'alt' con valores aleatorios dependiendo del valor de 'sex'
alt <- ifelse(sex == 0, rnorm(984, mean = 160, sd = 8), rnorm(984, mean = 175, sd = 10))

#Redondear altura
alt <- round(alt, 1)

# Ajustar valores fuera del rango deseado
alt[alt < 145] <- 145
alt[alt > 198] <- 199


# Crear el vector 'peso' como alt - 100 con una variación aleatoria entre -10 y +25
peso <- (alt - 100) + sample(-10:25, 984, replace = TRUE)
#Redondear peso
peso <- round(peso, 1)

# Ajustar valores fuera del rango deseado
peso[peso < 45] <- 45
peso[peso > 140] <- 140



# Crear el vector 'fum' con la probabilidad condicional según el valor de 'sex'
prob_fum <- ifelse(sex == 1, 0.3, 0.25)
fum <- rbinom(984, 1, prob_fum)

# Crear la variable 'colesterol' con distribución normal, media 186 y desviación estándar 20
colesterol <- rnorm(984, mean = 186, sd = 50)

# Redondear la variable 'colesterol' a 0 decimales
colesterol <- round(colesterol, 0)

# Ajustar valores fuera del rango deseado
colesterol[colesterol < 140] <- 140
colesterol[colesterol > 300] <- 300


# Calcular el índice de masa corporal (IMC) como alt/peso^2
imc <- peso/((alt/100)^2) 

# Creamos clas_soc con diferente probabilidad de distirbución de diferentes factores de riesgo
z_alta <- 1 - 0.01 * imc - 0.010 * colesterol + 0.005 * (1 - hta) - 0.2 * fum  # Fumar disminuye probabilidad de ser alta
z_media <- 0.2 + 0.01 * imc + 0.005 * colesterol + 0.1 * fum                  # Fumar aumenta probabilidad de ser media
z_baja <- 0.01 * imc + 0.01o * colesterol + 0.01 * hta + 0.3 * fum             # Fumar aumenta probabilidad de ser baja

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
DM <- rbinom(984, size = 1, prob = prob_DM)

table(DM)



# Añadir un valor aleatorio entre -0.2 y 0.2 al log_RR_total
random_variation <- runif(984, min = -0.1, max = 0.1) 

# Calcular el logaritmo del riesgo relativo total
log_RR <- log (1.001) * edad + log(1.05) * sex + log (1.01)* hta + log(1.0038) * colesterol + log(2.5) * fum + log(1.001) * imc - log(1.8) * clas_soc  + log(1.1)* DM +random_variation - 2.6

# Convertir el logaritmo del riesgo relativo a una probabilidad mediante la función logística
prob_iam <- 1 / (1 + exp(-log_RR))

# Generar la variable 'iam1' como binaria (0 o 1) usando la probabilidad calculada
iam1 <- rbinom(984, 1, prob_iam)
table (iam1)




#Creamos fum_p
prob_fum_p <- ifelse(fum == 1, 0.8, 0)  # Si fum = 1, probabilidad base de 0.8

# Disminuimos la probabilidad si iam1 = 1 (infarto)
prob_fum_p <- ifelse(iam1 == 1, prob_fum_p * 0.5, prob_fum_p)  # Si iam = 1, reducimos la probabilidad en un 50%

# Generamos fum_p de manera probabilística
fum_p <- rbinom(984, 1, prob = prob_fum_p)
table (fum_p)

table (fum, fum_p)



# Colesterol posterior con ruido añadido
ruido <- rnorm(984, mean = 0, sd = 5)  # Ruido con desviación estándar de 5
colesterol_p <- colesterol + ruido

# Reducción probabilística del colesterol si iam = 1
# Si el individuo ha tenido un IAM, vamos a reducir el colesterol en un 10-20% con una probabilidad de 80%
colesterol_p <- ifelse(iam1 == 1, 
                       colesterol_p * sample(c(0.8, 0.9), 984, replace = TRUE, prob = c(0.5, 0.5)), 
                       colesterol_p)

colesterol_p[colesterol_p < 140] <- 140
colesterol_p[colesterol_p > 300] <- 300

# Redondear la variable 'colesterol_p' a 0 decimales
colesterol_p <- round(colesterol_p, 0)

summary(colesterol_p)

#Vector salud

generar_salud <- function(DM, fum, clas_soc) {
  n <- length(DM)  # El número de sujetos
  salud <- numeric(n)  # Inicializar el vector salud
  
  for (i in 1:n) {
    # Probabilidades ajustadas para cada nivel de salud
    p0 <- 0.05 + 0.1 * DM[i] + 0.1 * fum[i]  # P salud = 0
    p1 <- 0.15 + 0.05 *DM[i] + 0.1 * fum[i]  # P salud = 1
    p2 <- 0.25 + 0.05 *DM[i] + 0.1 * fum[i]  # P salud = 2
    p3 <- 0.40 + 0.05 * clas_soc[i]- 0.05 *DM[i] - 0.1 * fum[i] # P salud = 3
    p4 <- 0.15 + 0.05 * clas_soc[i]-  0.05 *DM[i] - 0.05 * fum[i] # P de salud = 4
    
    # Normalizar para que las probabilidades sumen 1
    total_prob <- p0 + p1 + p2 + p3 + p4
    p0 <- p0 / total_prob
    p1 <- p1 / total_prob
    p2 <- p2 / total_prob
    p3 <- p3 / total_prob
    p4 <- p4 / total_prob
    
    # Escoger un valor de salud basado en las probabilidades ajustadas
    salud[i] <- sample(0:4, size = 1, prob = c(p0, p1, p2, p3, p4))
  }
  
  return(salud)
}
salud <- generar_salud(DM, fum, clas_soc)


print(salud[1:10])  
table (salud)

# Función para generar el vector "salud_p" basado en "salud", "iam1" y "fum"
generar_salud_p <- function(salud, iam1, fum) {
  n <- length(salud)  # Número de sujetos
  salud_p <- salud  # Inicializar salud_p con los mismos valores que salud
  
  for (i in 1:n) {
    if (iam1[i] == 1) {  # Si ha habido infarto
      if (fum[i] == 1) {  # Si el sujeto es fumador
        prob_bajar <- 1.0  # Probabilidad del 100% de bajar
      } else {  # Si el sujeto no es fumador
        prob_bajar <- 0.5  # Probabilidad del 50% de bajar
      }
      
      # Disminuir salud en 1 con la probabilidad correspondiente
      if (runif(1) < prob_bajar && salud[i] > 0) {
        salud_p[i] <- salud[i] - 1
      }
    }
    # Si iam1[i] == 0, el valor de salud_p[i] no cambia, se mantiene igual a salud[i]
  }
  
  return(salud_p)
}


# Generar el vector 'salud_p' basado en 'salud', 'iam1', y 'fum'
salud_p <- generar_salud_p(salud, iam1, fum)

# Imprimir los primeros valores del vector salud_p para verificar
print(salud_p[1:10])
table (salud_p)

table (salud, salud_p)
#Creamos iam2

prob_iam2 <- ifelse(iam1 == 0, 0,  # Si iam1 = 0, probabilidad de iam2 = 0
                    0.1 + 0.5 * fum_p + 0.001 * (colesterol_p ) + 
                      ifelse(clas_soc == 0, +0.05,  # Aumenta la probabilidad si es clase baja
                             ifelse(clas_soc == 1, 0,  # Probabilidad intermedia si es clase media
                                    -0.05)))  # Disminuye la probabilidad si es clase alta

# Nos aseguramos de que la probabilidad esté entre 0 y 1
prob_iam2 <- pmin(pmax(prob_iam2, 0), 1)

# Generar la variable iam2 de manera probabilística (0 = no, 1 = sí)
iam2 <- rbinom(984, size = 1, prob = prob_iam2)

table (iam1, iam2)

# Crear el dataframe con todas las variables
df_iam3 <- data.frame(fech_nac,sex, alt, peso, imc,hta,fum, DM, colesterol, salud, clas_soc, iam1, fum_p, colesterol_p, salud_p, iam2)

write.csv(df_iam3, file = "C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 10/df_iam3.csv", row.names = FALSE)

head (df_iam3)

# Etiquetamos variables

# Convertir a factor la variable clase_social
clas_soc_factor <- factor(clas_soc, levels = c(0, 1, 2), labels = c("baja", "media", "alta"))

#fech_nac viene definida como character, cambiamos a formato fecha
df_iam3$fech_nac <- as.Date(df_iam3$fech_nac)
fecha_fin <- as.Date("2023-12-31")
df_iam3$edad <- (fecha_fin-df_iam3$fech_nac)/365.25
df_iam3$edad <- as.numeric (round (df_iam3$edad,0))

#Categorizamos imc
df_iam3$imc_r <- cut(df_iam3$imc,
                    breaks = c(-Inf, 20, 30, Inf),
                    labels = c("Bajo", "Normal", "Obesidad"))
# Convertir variables a factor con etiquetas

df_iam3$sex <- factor(df_iam3$sex, levels = c(0, 1), labels = c("Mujer", "Varón"))
df_iam3$hta <- factor(df_iam3$hta, levels = c(0, 1), labels = c("No", "Sí"))
df_iam3$fum <- factor(df_iam3$fum, levels = c(0, 1), labels = c("No", "Sí"))
df_iam3$fum_p <- factor(df_iam3$fum_p, levels = c(0, 1), labels = c("No", "Sí"))
df_iam3$iam1 <- factor(df_iam3$iam1, levels = c(0, 1), labels = c("No", "Sí"))
df_iam3$iam2 <- factor(df_iam3$iam2, levels = c(0, 1), labels = c("No", "Sí"))
df_iam3$clas_soc <- factor(df_iam3$clas_soc, levels = c(0, 1, 2), labels = c("Baja", "Media", "Alta"))
etiquetas_salud <- c("Muy Mal", "Mal", "Regular", "Bien", "Muy Bien")
# Convertir los vectores 'salud' y 'salud_p' en factores con etiquetas
df_iam3$salud <- factor(df_iam3$salud, levels = 0:4, labels = etiquetas_salud)
df_iam3$salud_p <- factor(df_iam3$salud_p, levels = 0:4, labels = etiquetas_salud)

df_iam3_r<-df_iam3


write.csv(df_iam3_r, file = "C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Práctica 10/df_iam3_r.csv", row.names = FALSE)


