# Ejercicio correlacion de spearman
# JEGR
# 25/09/2025

# Datos
resp <- data.frame(
  Tiempo = c(12,15,17,18,20,21,22,26),
  Edad = c(12,25,20,35,45,30,60,95)
)
resp

# Crear columnas con las rampas (1 a 8)
resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

# Ver resultado
resp

# Graficos de dispersion
plot(resp$Tiempo, resp$Edad,
     col = "blue",
     xlab = "Tiempo",
     ylab = "Edad",
     pch = 20)

plot(resp$Rango_Tiempo, resp$Rango_Edad,
     col = "blue",
     pch =20)

# Procedimientos para obtener correlacion de spearman
resp$dif <- resp$Rango_Tiempo - resp$Rango_Edad
resp$dif2 <- resp$dif^2
sum(resp$dif2)

# Coeficiente de correlacion rho = 0.9047619

# Comprobar dentro de R
cor.test(resp$Rango_Tiempo, resp$Rango_Edad,
         method = "spearman")  # <0.05 (p-value = 0.004563), correlacion alta significativa

cor.test(resp$Tiempo, resp$Edad, method = "spearman")
