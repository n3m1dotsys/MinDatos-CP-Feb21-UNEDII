# Author: Martin Romera Sobrado
# 
# Exercises for "Data analysis recipes: Fitting a model to data" 
#
# Fitting a straight line to data - Exercise 1

library(matlib)
# setwd(getSrcDirectory()[1])
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Para RStudio
# Cargamos los datos de la tabla y eliminamos los datos sobrantes
table <- read.csv('../Datos/tabla1.csv')
table = table[5:nrow(table),1:4]

# Recogemos los datos de table en variables diferentes
x <- table[,"x"]
y <- table[,"y"]
sigmay <- table[,"sigma_y"]

# Creamos las matrices para el calculo de la matriz X
YMat <- matrix(y, ncol = 1)
AMat <- matrix(1, nrow = length(x), ncol = 2)
AMat[,2] = x 
CMat <- diag(x = sigmay)

# Calculamos la matriz X
XMat <- inv(t(AMat)%*%inv(CMat)%*%AMat)%*%(t(AMat)%*%inv(CMat)%*%YMat)
SMat <- inv(t(AMat)%*%inv(CMat)%*%AMat)
# Mostramos datos
plot(x,y,
     ylim=range(c(0, y+sigmay)), 
     pch=19, xlab="X", ylab="Y", 
     main="Ejercicio 1",
     xlim=c(0,300)
)
abline(a = XMat[1], b = XMat[2])
arrows(x,y-sigmay,x,y+sigmay,length=0.05,angle=90,code=3)
cat("Varianza de de m:" , SMat[2,2], "\n")

