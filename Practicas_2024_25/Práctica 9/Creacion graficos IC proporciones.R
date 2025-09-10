# Instalar el paquete binom si no está instalado

install.packages("binom")


# Cargar el paquete binom
library(binom)

# Definir proporciones de 0.1 a 0.9 con incrementos de 0.1
proporciones <- seq(0.1, 0.9, by = 0.1)

# Definir el tamaño de muestra
n <- 50

# Inicializar vectores para almacenar los resultados
wald_lower <- c()
wald_upper <- c()
wilson_lower <- c()
wilson_upper <- c()
clopper_lower <- c()
clopper_upper <- c()

# Calcular los intervalos de confianza para cada proporción
for (p in proporciones) {
  # Número de éxitos para la proporción actual
  x <- p * n
  
  # Método de Wald (sin corrección de continuidad)
  wald <- prop.test(x, n, correct = FALSE)
  wald_lower <- c(wald_lower, wald$conf.int[1])
  wald_upper <- c(wald_upper, wald$conf.int[2])
  
  # Método de Wilson
  wilson <- binom.confint(x, n, method = "wilson")
  wilson_lower <- c(wilson_lower, wilson$lower)
  wilson_upper <- c(wilson_upper, wilson$upper)
  
  # Método Clopper-Pearson (exacto)
  clopper <- binom.confint(x, n, method = "exact")
  clopper_lower <- c(clopper_lower, clopper$lower)
  clopper_upper <- c(clopper_upper, clopper$upper)
}

# Crear un solo gráfico
plot(proporciones, proporciones, type = "n", ylim = c(0, 1), xlim = c(0, 1),
     xlab = "Proporción", ylab = "Intervalos de Confianza",
     main = "Comparación de IC para 50 Sujetos")

# Método de Wald
arrows(proporciones, wald_lower, proporciones, wald_upper, 
       angle = 90, code = 3, length = 0.05, col = "blue")
points(proporciones, proporciones, pch = 16, col = "blue")

# Método de Wilson
arrows(proporciones + 0.01, wilson_lower, proporciones + 0.01, wilson_upper, 
       angle = 90, code = 3, length = 0.05, col = "red")
points(proporciones + 0.01, proporciones, pch = 16, col = "red")

# Método Clopper-Pearson
arrows(proporciones - 0.01, clopper_lower, proporciones - 0.01, clopper_upper, 
       angle = 90, code = 3, length = 0.05, col = "green")
points(proporciones - 0.01, proporciones, pch = 16, col = "green")

