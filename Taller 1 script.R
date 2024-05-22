# PRIMERA PREGUNTA ----------------------------------------------------------------
tabla <- read.csv("/home/julian/Documentos/Estadistica multivariada/tabla_0.csv",header = TRUE, row.names = 1)


# a)
#Describa el entorno multivariado de estos datos. Encuentre los vectores y
#matrices X; Sn, R con las fórmulas vistas en clase, descríbalos y corrobórelos
#con Excel y R y que significarían sus diferencias.

X1 <- as.numeric(tabla[1,])
X2 <- as.numeric(tabla[2,])
X3 <- as.numeric(tabla[3,])

X1 <- mean(X1, na.rm = TRUE)
X2 <- mean(X2, na.rm = TRUE)
X3 <- mean(X3, na.rm = TRUE)

print(X1)
print(X2)
print(X3)


Sn <-var(t(tabla)) # matriz de covarianza o matriz de disersion (Sn)
print(Sn)

R <- cor(t(tabla)) # Matriz de correlación ()
print(R)

#b)
#Haga una gráfica tipo r. en R e interpretelo.

tabla_traspuesta <- t(tabla)
tabla_traspuesta <- as.data.frame(tabla_traspuesta)

# Graficar las columnas
plot(tabla_traspuesta)

library(psych)# con una función:
scatter.hist(tabla_traspuesta$X1,tabla_traspuesta$X2,smooth=F,ab=T,density=T,ellipse=F,title="X1 vs X2", xlab="X1", ylab="X2",
             correl=TRUE,pch=20,cex.axis=1.2,cex.lab=1.2,cex.main=1.5)

scatter.hist(tabla_traspuesta$X1,tabla_traspuesta$X3,smooth=F,ab=T,density=T,ellipse=F,title="X1 vs X2", xlab="X1", ylab="X2",
             correl=TRUE,pch=20,cex.axis=1.2,cex.lab=1.2,cex.main=1.5)

scatter.hist(tabla_traspuesta$X2,tabla_traspuesta$X3,smooth=F,ab=T,density=T,ellipse=F,title="X1 vs X2", xlab="X1", ylab="X2",
             correl=TRUE,pch=20,cex.axis=1.2,cex.lab=1.2,cex.main=1.5)

#c)
#Con las observaciones, 3, 5 y 12, haga una gráfica tipo q e, interprétela y lo
#mismo con observaciones, 1, 10 y 13. Si puede relacione ambos casos.

# Graficar cada una de las tres columnas
scatter.hist(tabla$X3, tabla$X5, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X3", ylab = "X5", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
scatter.hist(tabla$X3, tabla$X12, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X3", ylab = "X12", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
scatter.hist(tabla$X5, tabla$X12, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X5", ylab = "X12", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)


scatter.hist(tabla$X1, tabla$X10, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X1", ylab = "X10", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
scatter.hist(tabla$X3, tabla$X12, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X1", ylab = "X13", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
scatter.hist(tabla$X5, tabla$X12, smooth = FALSE, ab = TRUE, density = TRUE, ellipse = FALSE,
             xlab = "X10", ylab = "X13", correl = TRUE, pch = 20, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)




#D)
# Vectores de observaciones
# Crear la matriz de observaciones

# Crear la matriz de observaciones
observaciones <- matrix(c(tabla$X1, tabla$X3, tabla$X5,   # Inserta tus observaciones aquí
                          tabla$X7, tabla$X10, tabla$X13
), 
ncol = 3, byrow = TRUE)

# Nombres de las observaciones
nombres_observaciones <- c("X1", "X3", "X5", "X7", "X10", "X13")

# Calcular la matriz de distancias euclidianas
matriz_distancias <- dist(observaciones, method = "euclidean")

# Convertir la matriz de distancias en una matriz regular
matriz_distancias <- as.matrix(matriz_distancias)

# Asignar nombres de observaciones a filas y columnas
rownames(matriz_distancias) <- nombres_observaciones
colnames(matriz_distancias) <- nombres_observaciones

# Mostrar la matriz de distancias con nombres de filas y columnas
print(matriz_distancias)



#E)
# Estandariza las variables sobre la varianza
observaciones_estandarizadas <- apply(observaciones, 2, function(x) (x - mean(x)) / sqrt(var(x)))

# Nombres de las observaciones
nombres_observaciones <- c("X1", "X3", "X5", "X7", "X10", "X13")

# Calcula la matriz de distancias euclidianas entre las observaciones estandarizadas
matriz_distancias_estadisticas <- dist(observaciones_estandarizadas, method = "euclidean")

# Convertir la matriz de distancias en una matriz regular
matriz_distancias_estadisticas <- as.matrix(matriz_distancias_estadisticas)

# Asignar nombres de observaciones a filas y columnas
rownames(matriz_distancias_estadisticas) <- nombres_observaciones
colnames(matriz_distancias_estadisticas) <- nombres_observaciones

# Muestra la matriz de distancias
print(matriz_distancias_estadisticas)


# SEGUNDA PREGUNTA ----------------------------------------------------------------------

# combinaciones lineales

combinacion1 <- 3 * tabla_traspuesta$X1 + tabla_traspuesta$X2 - 2 * tabla_traspuesta$X3
combinacion2 <- tabla_traspuesta$X1 + 2 * tabla_traspuesta$X2 + tabla_traspuesta$X3

# se crea la tabla con los resultados
estadisticas_conjuntas <- data.frame(Combinacion1 = combinacion1,
                                     Combinacion2 = combinacion2)
View(estadisticas_conjuntas)
#  estadisticas descriptivas
summary(estadisticas_conjuntas)



# TERCERA PREGUNTA --------------------------------------------------------
# Definimos la matriz A
A <- matrix(c(7, 3, 3, 3), nrow = 2)

# Calculamos los eigenvalores y eigenvectores
eig <- eigen(A)

# Eigenvalores
lambda1 <- eig$values[1]
lambda2 <- eig$values[2]

# Eigenvectores
v1 <- eig$vectors[, 1]
v2 <- eig$vectors[, 2]

# Mostramos los resultados
print("Eigenvalores:")
print(lambda1)
print(lambda2)

print("Eigenvector 1:")
print(v1)

print("Eigenvector 2:")
print(v2)

# QUINTA PREGUNTA --------------------------------------------------------------
#Sean los siguientes pares de mediciones en las variables X1  y x2 :

tabla1 <- read.csv("/home/julian/Documentos/Estadistica multivariada/tabla_1.csv",header = TRUE, row.names = 1)

#a) Grafique los datos como un diagrama de puntos y calcule s11, s22, y s12

transpuesta1 <- t(tabla1)
transpuesta1 = data.frame(transpuesta1)
plot(transpuesta1,pch=16)
plot(transpuesta1$X1,pch=16,col="blue",ylab = "X1")
plot(transpuesta1$X2,pch=16,col="red",ylab = "X2")

Sii <-var(t(tabla1)) # matriz de covarianza o matriz de disersion (Sn)
print(Sii)

#b)
theta = 30
tilde_X1 <- transpuesta1$X1 * cos(theta) + transpuesta1$X1 * sin(theta)
tilde_X2 <- -transpuesta1$X2 * sin(theta) + transpuesta1$X2 * cos(theta)
print(tilde_X1)
print(tilde_X2)
# Crear el DataFrame con los resultados

data_tildes <- data.frame(tilde_X1 = tilde_X1, tilde_X2 = tilde_X2)

tildeSii <-var(data_tildes) # matriz de covarianza o matriz de disersion (Sn)
print(tildeSii)

#C)
# Definir los valores de X1 y X2
x1 <- 4
x2 <- -2

# Definir el ángulo theta en grados
theta_grados <- 30

# Calcular la transformación
tildeX1 <- x1 * cos(theta_grados) + x2 * sin(theta_grados)
tildeX2 <- x1 * -sin(theta_grados) + x2 * cos(theta_grados)

# Crear el DataFrame con los resultados
resultados <- data.frame(tilde_X1 = tilde_X1, tilde_X2 = tilde_X2)
resultadosii <- var(resultados)
# Imprimir el DataFrame
print(resultadosii)

#D)
# Definir los valores de s11, s12 y s22
s11 <- 3.972511
s12 <- 1.428619
s22 <- 18.989655

# Definir el ángulo theta en grados
theta_grados <- 30

# Calcular los términos de la fórmula
cos_theta <- cos(theta_grados)
sin_theta <- sin(theta_grados)

# Calcular a11 según la fórmula dada
a11 <- (cos_theta^2) / ((cos_theta^2 * s11) + (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s22)) +
  (sin_theta^2) / ((cos_theta^2 * s22) - (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s11))

# Imprimir el resultado
print(a11)

# Calcular a22 según la fórmula dada
a22 <- (sin_theta^2) / ((cos_theta^2 * s11) + (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s22)) +
  (cos_theta^2) / ((cos_theta^2 * s22) - (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s11))

# Imprimir el resultado
print(a22)

# Calcular a12 según la fórmula dada
a12 <- (cos_theta * sin_theta) / ((cos_theta^2 * s11) + (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s22)) -
  (sin_theta * cos_theta) / ((cos_theta^2 * s22) - (2 * sin_theta * cos_theta * s12) + (sin_theta^2 * s11))

# Imprimir el resultado
print(a12)

# Calcular la distancia d(O,P) según la fórmula dada
distancia <- sqrt(a11 * x1^2 + 2 * a12 * x1 * x2 + a22 * x2^2)

# Imprimir el resultado
print(distancia)

#D)
#E)

# SEXTA PREGUNTA ----------------------------------------------------------
Xtranspueto <- c(2,3,1)
Ytranspuesto <- c(3,1,-3)

# a) Longitud de los vectores
longitud_Xtranspuesto <- sqrt(sum(Xtranspueto^2))
longitud_Ytranspuesto <- sqrt(sum(Ytranspuesto^2))
#b)Vectores sombra
Xnormal <- t(Xtranspueto)
Ynormal <- t(Ytranspuesto)
Vs = ((Xtranspueto*Ynormal)/(Ytranspuesto*Ynormal))*Ynormal
print(Vs)
#C) vectores de longitud unitaria
XLu = Xnormal/(longitud_Xtranspuesto)
YLu = Ynormal/(longitud_Ytranspuesto)
print(XLu)
print(YLu)

# SEPTIMA PREGUNTA --------------------------------------------------------

#a)
Xmatriz <- matrix(c(9,7,3,4,10,7,5,7,2),nrow =3)
Bmatriz <- matrix(c(0,0,0),nrow=3)
solve(Xmatriz,Bmatriz)

#b)
det(Xmatriz)

# OCTAVA PREGUNTA ---------------------------------------------------------
# Calculamos la inversa de los eigenvalores
inv_lambda1 <- 1 / lambda1
inv_lambda2 <- 1 / lambda2

# Calculamos la matriz diagonal de los inversos de los eigenvalores
inv_Lambda <- diag(c(inv_lambda1, inv_lambda2))

# Calculamos la matriz Q^-1
Q_inv <- solve(eig$vectors)

# Calculamos la inversa de A
A_inv <- Q_inv %*% inv_Lambda %*% t(Q_inv)

# Mostramos la inversa de A
print("Inversa de A:")
print(A_inv)


# NOVENA PREGUNTA ---------------------------------------------------------
#A)
num_apar_especie <- c(1,1,1,2,2,2,3,3,3,4,4,4)
num_apar_enre <- c(1,2,3,1,2,3,1,2,3,1,2,3)
frecuencias <- c(12,5,15,15,6,2,7,2,2,26,7,1)

# Creación del dataframe
df9 <- data.frame(num_apar_especie, num_apar_enre, frecuencias)

# Encontrar la matriz de covarianzas
cov_matrix9 <- cov(df9)
print(cov_matrix9)

#B)
# Encontrar la matriz de correlaciones
cor_matrix9 <- cor(df9)
print(cor_matrix9)

#C)
# Calcular los eigenvalores de la matriz de covarianzas
eigen_values <- eigen(cov_matrix9)$values

# Verificar si todos los eigenvalores son positivos
is_positive_definite <- all(eigen_values > 0)

if (is_positive_definite) {
  print("La matriz de covarianzas es definida positiva.")
} else {
  print("La matriz de covarianzas NO es definida positiva.")
}
# Calcular los valores y vectores propios de la matriz de covarianzas
eigen_info <- eigen(cov_matrix9)

#D)
# Valores propios
eigenvalues <- eigen_info$values

# Vectores propios
eigenvectors <- eigen_info$vectors

print("Valores propios:")
print(eigenvalues)

print("Vectores propios:")
print(eigenvectors)
# Graficar los eigenvalores
plot(eigen_values, type = 'b', main = "Eigenvalores de la matriz de covarianzas", 
     xlab = "Índice", ylab = "Eigenvalor")
# DECIMA PREGUNTA ---------------------------------------------------------



tabla10 <- read.csv("/home/julian/Documentos/Estadistica multivariada/tabla_3.csv")

#a)
# Cargar la biblioteca ggplot2
library(ggplot2)

# Diagrama marginal para Uradsol (Radiación solar) y CO
scatter_plot_uradsol <- ggplot(tabla10, aes(x = Uradsol, y = CO)) +
  geom_point() + 
  labs(x = "Radiación solar (Uradsol)", y = "Monóxido de carbono (CO)",
       title = "Diagrama marginal: Radiación solar vs. Monóxido de carbono")

# Diagrama marginal para Viento y Industrias
scatter_plot_viento <- ggplot(tabla10, aes(x = Velocvient, y = Industrias)) +
  geom_point() +
  labs(x = "Industrias", y = "Velocidad del viento",
       title = "Diagrama marginal: Velocidad del viento vs industrias")

# Mostrar los gráficos
scatter_plot_uradsol
scatter_plot_viento

#b)
# Calcular el vector de medias
medias <- colMeans(tabla10[-1])
# Calcular la matriz de covarianza
cov_matriz <- cov(tabla10[,-1])

# Calcular la matriz de correlación
cor_matriz <- cor(tabla10[,-1])


print(medias)
print(cov_matriz)
print(cor_matriz)

library(corrplot)
corrplot(cor_matriz, method = "square",tl.col = "black")
# D)
# Descomposición espectral de la matriz de covarianza Sn
eigen_Sn <- eigen(cov_matriz)

# Descomposición espectral de la matriz de correlación R
eigen_R <- eigen(cor_matriz)

# Calcular Sn^2 y R^3 después de la descomposición espectral
Sn_cuadrado <- eigen_Sn$vectors %*% diag(eigen_Sn$values^2) %*% t(eigen_Sn$vectors)
R_cubo <- eigen_R$vectors %*% diag(eigen_R$values^3) %*% t(eigen_R$vectors)

#E)
# Transponer la tabla
tabla_transpuesta <- t(tabla10)

# Establecer la primera fila como nombres de columna
colnames(tabla_transpuesta) <- tabla_transpuesta[1, ]

# Eliminar la primera fila
tabla_transpuesta <- tabla_transpuesta[-1, ]

# Eliminar el encabezado de las filas
rownames(tabla_transpuesta) <- NULL

# Convertir a dataframe
df <- as.data.frame(tabla_transpuesta, stringsAsFactors = FALSE)

# Convertir cada columna a numérica
for (i in 1:ncol(df)) {
  df[, i] <- as.numeric(df[, i])
}

# Crear un nuevo dataframe con solo las primeras tres filas
df_primeras_tres <- df[1:3, ]

# Calcular la distancia euclidiana entre las filas
distancias <- dist(df_primeras_tres, method = "euclidean")

# Calcular la matriz de distancias entre columnas
distancias_columnas <- dist(t(df_primeras_tres), method = "euclidean")

# Realizar agrupamiento jerárquico aglomerativo
agrupamiento <- hclust(distancias_columnas, method = "complete")

# Obtener los grupos
grupos <- cutree(agrupamiento, k = 2)  
# Mostrar la asignación de columnas a grupos
print(grupos)

# Graficar el dendrograma con parámetros ajustados
plot(agrupamiento, hang = -1, main = "Dendrograma de Agrupamiento de Columnas", xlab = "Columnas", ylab = "Altura de Fusión", cex = 0.8)

# F

promedio_variables <- colMeans(df_primeras_tres)

# Tipificando los datos
tipified_data <- scale(df_primeras_tres)

# Calcular la distancia euclidiana entre cada columna y el vector de medias tipificadas
distancias <- apply(tipified_data, 2, function(col) sqrt(sum((col - promedio_variables)^2)))

# Encontrar la columna más alejada
columna_mas_alejada <- which.max(distancias)

print(distancias)
# Mostrar el índice de la columna más alejada
print(columna_mas_alejada)

