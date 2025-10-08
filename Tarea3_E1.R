# Tarea 3, EJERCICIO 1
# JEGR
# 02/10/2025

library(ggplot2)

speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)

mayfly <- data.frame(speed, abundance)
View (mayfly)

plot(mayfly,
     xlab = "Variable independiente",
     ylab = "Variable dependiente")

# Tabla


ggplot(mayfly, aes(x = speed, color = abundance))+   # dist. datos en una grafica de densidad
  geom_density()

shapiro.test(mayfly$speed)      # Datos normales, p-value = 0.2572
shapiro.test(mayfly$abundance)  # Datos normales, p-value = 0.1046

# Si significativo, p-value = 0.008393
cor.test(mayfly$speed, mayfly$abundance,
         method = "pearson")

