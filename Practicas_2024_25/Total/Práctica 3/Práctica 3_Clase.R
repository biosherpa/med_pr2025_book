#Recordamos




readClipboard()

setwd("C:\\Users\\jesus.martin\\OneDrive - Madrid Digital\\Docente\\Introducción a la AP\\Curso 2024_25\\Bioestadística 1\\Prácticas\\Práctica 3")

getwd ()


v1<- c(1,3,5,7,9)
v2<- 10
v3<- c("Luis", "Pedro")
v4 <- sample(c("No", "Sí"), 10, replace=TRUE)

dir.create("Práctica 3_ejemplo")

setwd ("Práctica 3_ejemplo")
save.image("Práctica 3_ejemplo/prueba1.RData")

setwd ("Práctica 3_ejemplo")
getwd ()
save.image("prueba1.RData")

rm(list=ls())

load ("prueba1.RData")



rm(list=ls())

setwd("C:\\Users\\jesus.martin\\OneDrive - Madrid Digital\\Docente\\Introducción a la AP\\Curso 2024_25\\Bioestadística 1\\Prácticas\\Práctica 3")



#Instalar y cargar paquetes

install.packages ("readxl")
library(readxl) 


install.packages("psycho")
library (psycho)


remove.packages(c("readxl", "pshycho"))




#Construir dataframe a partir de un número de vectores de igual longitud

n<-10
v1 <- sample(c("Varón", "Mujer"), size = n, replace = TRUE)
v2 <- sample(c("Sí", "No"), size = n, replace = TRUE)
v3 <- sample(102:162, size = n, replace = TRUE)
v4 <- sample(60:96, size = n, replace = TRUE)             

#Ahora construimos el dataframe


df <- data.frame(v1, v2, v3, v4)
df

#Se pueden etiquetar las columnas
df <- data.frame(Sexo = v1, Fumador = v2, TAS = v3, TAD = v4)
df


#La misma acción podría hacerse con la función `colnames`


colnames(df) <- c("Sexo", "Fumador", "TAS", "TAD")
df

str (df)

# A este dataframe se le puede añadir una columna


DM<- c("No", "Sí","No","No","Si","No","No","Si","No","No")
df1<- data.frame(df,DM)
df1


#Se conseguiría el mismo resultado con la función `cbind`

df1<-cbind (df, DM)
df1


#También se podría añadir una fila (debe tener las mismas variables) con la función `rbind`

n.file<- c("Varón", "No", 136, 78, "No")
df2<- rbind (df1,n.file)
df2

getwd()
write.csv(df2, "df2.csv", row.names = FALSE)


#Un dataframe puede obtenerse a partir de otra. Vamos a obtener nuevas datframes a partir de `df2`´. Imaginemos que queremos obtener `df3` eliminando las filas 1 a 5. Puede hacerse de varios modos

df3<- df2[-c(1,2,3,4,5), ]                                           
df3

df3<- df2[6:11, ]                                           
df3


#También puede crearse un dataframe a partir de otro eliminando columnas. Probemos a obtener `df4` desde `df3` eliminando las columnas TAD y DM


df4<- df3[ , -c(4:5)]
df4


#Importación de un dataframe

install.packages("readxl")
library(readxl) 

#Importación desde Stata (.dta) y SPSS (.sav)

#Primero debe instalarse la librería `haven` (la librería `foreign` se usaba para versiones más antiguas de Stata y SPSS

#library(haven) df \<- read_dta("ruta/del/archivo.dta")

#library(haven) df \<- read_sav("ruta/del/archivo.sav")

getwd ()
rm (list=(ls()))

df2 <- read.csv("df2.csv")


df2<-df2[order(df2$Sexo, -df2$TAS), ]   
df2
head (df2)

#Ordena df2 primero por el sexo en orden creciente y luego por la TAS en orden decreciente

#Da un error porque TAS se convirtió en `character` cuando usamos la función `rbind` . Así que debemos cambiar TAS a variable numérica `integer`


#class(df2$TAS)
#df2$TAS <- as.integer(df2$TAS)

df2_sorted <- df2[order(df2$Sexo, -df2$TAS), ]

df2_sorted



#Seleccionar un subgrupo de casos o variables


df2
df2_h <- subset(df2,  Sexo == "Varón") 

df2_h


df2_h2 <- subset(df2_h, select = c("TAS","TAD"))
df2_h2


df2_h2 <- df2_h[, c("TAS", "TAD")]
df2_h2

#Crear nuevas variables
df2$HTA[df2$TAS >= 140 | df2$TAD >=90] <- "Sí"
df2$HTA[df2$TAS < 140 & df2$TAD <90] <- "No"
df2

#Recodificación de una variable

Edad<-c(38,48,52,60,70,49,52,72,68,61,59)
class (Edad)
df2_E<-data.frame (df2, Edad)
str (df2_E)
df2_E

class (df2_E$Edad)
df2_E$Edad[df2_E$Edad > 64] <- "65 y más años"
df2_E$Edad[df2_E$Edad >= 50 & df2_E$Edad < 65] <- "Entre 50 y 64 años"
df2_E$Edad[df2_E$Edad <50] <- "Menor de 50 años"

# Se podría hacer también con la función `cut`
df2_E$Edad <- cut(df2_E$Edad, breaks = c(-Inf, 50, 65, Inf), 
                  labels = c("Menor de 50 años", "Entre 50 y 64 años", "65 y más años"))
df2_E

#Vamos a cambiar el nombre de la variable Tabaco por Fumador.

names(df2_E)[names(df2_E) == "Fumador"] <- "Tabaco"
df2_E

#
# Temperaturas diarias para 3 ciudades durante 7 días
# Cada ciudad tiene 2 temperaturas al día (mañana y tarde)
temperaturas <- c(
  # Ciudad 1: (mañana, tarde para 7 días)
  20, 25,  # Día 1
  22, 27,  # Día 2
  24, 29,  # Día 3
  26, 31,  # Día 4
  28, 33,  # Día 5
  30, 35,  # Día 6
  32, 37,  # Día 7
  
  # Ciudad 2: (mañana, tarde para 7 días)
  18, 23,  # Día 1
  19, 24,  # Día 2
  21, 26,  # Día 3
  22, 27,  # Día 4
  24, 29,  # Día 5
  25, 30,  # Día 6
  27, 32,  # Día 7
  
  # Ciudad 3: (mañana, tarde para 7 días)
  28, 33,  # Día 1
  29, 34,  # Día 2
  30, 35,  # Día 3
  32, 37,  # Día 4
  33, 38,  # Día 5
  35, 40,  # Día 6
  37, 41   # Día 7
)

temperaturas

# Crear el array
# Dimensiones: 7 días, 3 ciudades, 2 períodos del día (mañana y tarde)
temp_array <- array(temperaturas, dim = c(7, 3, 2))

# Asignar nombres a las dimensiones para mayor claridad
dimnames(temp_array) <- list(
  Días = paste("Días", 1:7),        # Nombres para los días
  Ciudad = paste("Ciudad", 1:3),     # Nombres para las ciudades
  Periodo = c("Mañana", "Tarde") )

# Mostrar el array
print(temp_array)


# 3. Lista


#Veamos un ejemplo básico de creación de una lista y a mostrar el resultado


lista <- list(
  Nombres = c("Carlos", "Ana"),           
  # Vector con nombres de los sujetos
  
  Edades = c(25, 30), 
  # Vector con edades de los sujetos
  Calificaciones = list(
    c(88, 92, 75),          # Primer vector de calificaciones
       c(90, 85, 80)        # Segundo vector de calificaciones
      )
)

# Mostrar la lista 
print(lista) 

lista
