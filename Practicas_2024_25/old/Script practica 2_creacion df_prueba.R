# Configurar el proxy en PC Sala 32
Sys.setenv(http_proxy = paste0("proxy.gslb.madrid.org", ":", "8080"))
install.packages ("R")

#Le digo el directorio en el que trabajamos
setwd("C:/Users/jesus.martin/OneDrive - Universidad Rey Juan Carlos/Escritorio/Accesibilidad")

set.seed(123)

# Crear el vector 'sex' con valores 0 o 1, con probabilidad 0.5 para cada uno
sex <- rbinom(100, 1, 0.5)

# Crear el vector 'edad' 
edad <- sample(42:83, 100, replace = TRUE)


# Crear el vector 'alt' con valores aleatorios dependiendo del valor de 'sex'
alt <- ifelse(sex == 0, rnorm(100, mean = 160, sd = 12.5), rnorm(100, mean = 175, sd = 12.5))
# Ajustar valores fuera del rango deseado
alt[alt < 145] <- 145
alt[alt > 198] <- 198


# Crear el vector 'peso' como alt - 100 con una variación aleatoria entre -10 y +10
peso <- (alt - 100) + sample(-10:25, 100, replace = TRUE)



# Crear el vector 'fum' con la probabilidad condicional según el valor de 'sex'
prob_fum <- ifelse(sex == 1, 0.4, 0.2)
fum <- rbinom(100, 1, prob_fum)

# Crear el dataframe con los vectores 'alt', 'peso', 'sex' y 'fum'
df_prueba <- data.frame(alt, peso, sex, fum)

# Crear la variable 'colesterol' con distribución normal, media 186 y desviación estándar 20
colesterol <- rnorm(100, mean = 186, sd = 60)

# Redondear la variable 'colesterol' a 0 decimales
colesterol <- round(colesterol, 0)

# Ajustar valores fuera del rango deseado
colesterol[colesterol < 140] <- 140
colesterol[colesterol > 300] <- 300



# Calcular el índice de masa corporal (IMC) como alt/peso^2
imc <- peso/((alt/100)^2) 

# Añadir un valor aleatorio entre -0.2 y 0.2 al log_RR_total
random_variation <- runif(100, min = -0.1, max = 0.1) 

# Calcular el logaritmo del riesgo relativo total
log_RR <- log (1.001) * edad + log(1.10) * sex + log(1.0038) * colesterol + log(2.5) * fum + log(1.001) * imc + random_variation - 2.5


# Convertir el logaritmo del riesgo relativo a una probabilidad mediante la función logística
prob_iam <- 1 / (1 + exp(-log_RR))

# Generar la variable 'iam' como binaria (0 o 1) usando la probabilidad calculada
iam <- rbinom(100, 1, prob_iam)

# Crear el dataframe con todas las variables
df_prueba <- data.frame(edad,alt, peso, sex, fum, colesterol, iam)

table (df_prueba$iam)

summary (imc)


write.csv(df_prueba, file = "C:/Users/jesus.martin/OneDrive - Universidad Rey Juan Carlos/Escritorio/Prácticas/df_prueba.csv", row.names = FALSE)