

getwd ()
setwd ("C:\\Users\\jesus.martin\\OneDrive - Madrid Digital\\Docente\\Introducción a la AP\\Curso 2024_25\\Bioestadística 1\\Prácticas\\Tutoría") 

readClipboard()

setwd("C:\\Users\\jesus.martin\\OneDrive - Madrid Digital\\Docente\\Introducción a la AP\\Curso 2024_25\\Bioestadística 1\\Prácticas\\Tutoría")

getwd ()


v1<- c(1,3,5,7,9)
v2<- 10
v3<- c("Luis", "Pedro")
v4 <- sample(c("No", "Sí"), 10, replace=TRUE)

dir.create("Tutoría_ejemplo")

setwd ("Tutoría_ejemplo")
save.image("Tutoría_ejemplo/prueba1.RData")


setwd("C:/Users/jesus.martin/OneDrive - Madrid Digital/Docente/Introducción a la AP/Curso 2024_25/Bioestadística 1/Prácticas/Tutoría")

getwd ()
save.image("prueba1.RData")

rm(list=ls())

load ("prueba1.RData")



rm(list=ls())

setwd("C:\\Users\\jesus.martin\\OneDrive - Madrid Digital\\Docente\\Introducción a la AP\\Curso 2024_25\\Bioestadística 1\\Prácticas\\Tutoría")



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

df_t<- data.frame (v1,v2,v3,v4)
head (df_t)

write.csv(df_t, "df_t.csv", row.names = FALSE)

rm(list=ls())

#Importación de un dataframe

install.packages("readxl")
library(readxl) 

#Importación desde Stata (.dta) y SPSS (.sav)

#Primero debe instalarse la librería `haven` (la librería `foreign` se usaba para versiones más antiguas de Stata y SPSS

#library(haven) df \<- read_dta("ruta/del/archivo.dta")

#library(haven) df \<- read_sav("ruta/del/archivo.sav")

getwd ()
rm (list=(ls()))

df_t2 <- read.csv("df_t.csv")


