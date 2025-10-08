# Tarea 3, EJERCICIO 3
# JEGR
# 02/10/2025

anscombe
data("anscombe")

anscombe_data <- anscombe

# resumen estadistico
summary(anscombe_data)

cor(anscombe_data$x1, anscombe_data$y1)
cor(anscombe_data$x2, anscombe_data$y2)
cor(anscombe_data$x3, anscombe_data$y3)
cor(anscombe_data$x4, anscombe_data$y4)

# Graficas
par(mfrow=c(2,2))
plot(anscombe_data$x1, anscombe_data$y1,
     main="Conjunto1")
abline(lm(y1 ~ x1, data = anscombe_data), col = "blue")

plot(anscombe_data$x2, anscombe_data$y2,
     main = "Conjunto2")
abline(lm(y2 ~ x2, data = anscombe_data), col = "yellow")

plot(anscombe_data$x3, anscombe_data$y3,
     main = "Conjunto3")
abline(lm(y3 ~ x3, data = anscombe_data), col = "red")

plot(anscombe_data$x4, anscombe_data$y4,
     main = "Conjunto4")
abline(lm(y4 ~ x4, data = anscombe_data), col = "orange")

