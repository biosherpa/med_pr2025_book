
# Un vector

x<- 5
x

# Pero un vector puede contener otro tipo de datos como cadenas de texto o datos lógicos

x<- as.vector (TRUE)
x
y<-as.vector(c("Andrés", "Luis"))
y




### Creación de vectores


x <- c(1,2,3,4,5,6,7,8,9,10)
x



x <- 1:10
x




x <- 1:10-2 
x




x <- 1:(10-2) # Genera una serie de 1 a 8
x




x<-sequence(4:5) 
x



x<- seq(1, 2, by= 0.1)
x 

x<- seq(length=9, from=1, to=5)
x


x <-rep(1, 10) 
x



x<-rnorm(10, 50, 5) 
x 



x<-gl(3, 4) 
x 


ciudades<- c("Jaen", "Córdoba", "Sevilla")
ciudades



vector_logico <- c(TRUE, FALSE, TRUE, FALSE)
print(vector_logico)


numeros <- c(1, -5, 7, -2, 8, 9, 2)

positivo <- numeros>0
positivo
print(positivo)


# Operaciones con vectores

v0 <- 10
v0

is.vector(v0) # Verificar si el objeto o variable 'v0' es un vector


#Operar con vectores
v1<-c(2,5,10)
v1


#Se pueden añadir elementos a un vector


v2<-c (v1,8)
v2



v1+v2



# Aviso en v1 + v2 : longitud de objeto mayor no es múltiplo de la longitud de uno menor. Al último valor de v2, le suma el primero de V1 ("reciclaje")\

v1*v2



#Aviso en v1\*v2 : longitud de objeto mayor no es múltiplo de la longitud de uno menor. Multiplica último valor de v2, por el primero de V1 ("reciclaje"). #No ocurriría si v1 y v2 tuviesen igual tamaño.



v1^2 


sqrt (v2) #v2(2,5,10,8)




sum (v2) #v2(2,5,10,8) 


mean (v2) #v2(2,5,10,8)



v3<-c(0,10,8,NA,6)
mean (v3)


#la forma correcta de calcular la media sería


mean(v3, na.rm = TRUE)


#Existen otras funciones que operan con los valores de un vector


v3<-c(0,10,8,NA,6)
length(v3) #número de valores de v3
max(v3,na.rm = TRUE) #Valor máximo de v3, sabiendo que hay valores NA
min(v3, na.rm = TRUE) #Valor mínimo de v3, sabiendo que hay valores NA






v2*2+10


#Cuando le indicamos a R que calcule `v2* 2 + 10`, lo que realmente está calculando es:`v2 * c(2, 2, 2, 2) + c(10, 10, 10, 10)`

#Algunas operaciones se pueden escribir de forma simplificada


v2 + c (2, 10)

print (v2)


#Esta operación suma 2 a los valores impares y 10 a los pares. Recuerde que `v2(2,5,10,8)`


#Recuperar valores

v1[2]
v2[3]
```


v1[2] +v2[3]


v1[4] <- 12
v1
v1[2] <- 12
v1



# Los elementos de un vector se pueden recuperar en función de una orden lógica. Siendo `v1(2,12,10,12)`


v1 > 7
v1 < 7
v1 == 12


which(v3 > 2) # Posiciones de los elementos mayores que 2
which(v3 < 2 | v3 >= 8) # Posiciones de los elementos <2 o >= 8.  
```

#Atención, hay un elemento NA, aunque R en esta versión lo identifica como tal, es más recomendable


which(!is.na(v3) & v3 < 2 | v3 >= 8) 

?which
help (which)


# Matrices


matrix(1:12, nrow = 4, ncol = 3)


# Cuando no se especifica nada la matriz se cumplimenta por columnas.



matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)



# La función matrix() se puede sustituir con la instrucción dim, que transforma un vector en una matriz


x<-1:15
dim(x) <- c(5, 3)
x






# Las funciones básicas explicadas se pueden emplear en una matriz


range(x) #Rango de la matriz x
length(x) #Número de elementos en la matriz x
mean(x) #Media de los valores de la matriz x
median(x) #Mediana de los valores de la matriz x
sum(x) #Suma de los valores de la matriz x
prod(x) #Producto de los valores de la matriz x
max(x) #Valor máximo de la matriz x
min(x) #Valor mínímo de la matriz x
which.max(x) # Devuelve el valor máximo de la matriz x
which.min(x) #Devuelve valor mínimo de la matriz x


#sum (z)


m1<-matrix(1:4, nrow=2, ncol=2)
m2<-matrix(5:8, nrow=2, ncol=2)
m1
m2


#Se pueden unir por filas con la orden `rbind`


rbind (m1,m2)


#Se puden unir por columnas con la orden `cbind`


cbind (m1,m2)



m1+m2
m1*m2



t (m1)
t (m2)


colnames(m1) <- c("Columna 1", "Columna 2")
rownames (m1) <- c("Fila 1", "Fila 2")
m1





m3<- rbind(m1,m2)
rownames (m3) <- c("Fila 1", "Fila 2", "Fila 3", "Fila 4")
m3



#Vamos a intentar localizar algunas posiciones en la matriz


m3[1,] # fila 1 todas las columnas.

m3[,2] # todas filas, columna 2

m3[1:2,1] #filas 1 y 2,  columna 1 

m3[-(1:2),2] #excluir filas 1 a 2, incluir columna 2.

m3[,"Columna 2"] #La columna 2, usando el nombre de la columna

m3[3,c("Columna 1","Columna 2")] # La fila 3 y todas las columnas, llamadas por su nombre


# Para pensar
x1<-1:100
dim (x1)<-c (10,10)
x1
sum (x1)

