# Tarea 3
# JEGR
# 02/10/2025

library(ggplot2)
library(corrplot)
library(gt)
library(Hmisc)

# =========================================================================
# Ejercicio 1 -------------------------------------------------------------
# =========================================================================

# Creacion de dataset
speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)

mayfly <- data.frame(speed, abundance)
View (mayfly)

print(mayfly)

# Tabla 

mayfly %>%
  gt()
  
# Grafico de dispersion
plot(mayfly,
     xlab = "Velocidad del arroyo",
     ylab = "Cantidad de individuos")

# Grafico densidad
ggplot(mayfly, aes(x = speed, color = abundance))+   # dist. datos en una grafica de densidad
  geom_density()


# Prueba de normalidad
shapiro.test(mayfly$speed)      # Datos normales, p-value = 0.2572
shapiro.test(mayfly$abundance)  # Datos normales, p-value = 0.1046

# Si significativo, p-value = 0.008393
cor.test(mayfly$speed, mayfly$abundance,
         method = "pearson")

# =========================================================================
# Ejercicio 2 -------------------------------------------------------------
# =========================================================================

# Creacion del dataset
soil <- data.frame(
  Gp   = c("T0", "T0", "T0", "T0", "T1", "T1", "T1"),
  Block= 1:7,
  pH   = c(5.40, 5.65,5.14,5.14,5.14,5.10,4.70),
  N    = c(0.188,0.165,0.260,0.169,0.164,0.094,0.100),
  Dens = c(0.92,1.04,0.95,1.10,1.12,1.22,1.52),
  P    = c(215,208,300,248,174,129,117),
  Ca   = c(16.35, 12.25,13.02,11.92,14.17,8.55,8.74),
  Mg   = c(7.65,5.15,5.68,7.88,8.12,6.92,8.16),
  K    = c(0.72, 0.71, 0.68, 1.09,0.70, 0.81, 0.39),
  Na   = c(1.14, 0.94, 0.60, 1.01,2.17,2.67,3.32),
  Conduc = c(1.09,1.35,1.41,1.64,1.85,3.18,4.16)
)

print(soil)

# Matriz de correlacion
cor_matrix <- cor(soil[,3:11],method = "pearson")

# Test de normalidad para cada par
cor_results <- rcorr(as.matrix(soil[,3:11]))

cor_matrix        # Son los coeficientes de los valores de R
cor_results$P     # Valores de p de las variables

# Variables que nos interesan analizar con correlacion
vars <- c("N", "Dens", "P", "Ca", "Mg", "K", "Na")

# Correlaciones de pH con las demas variables
resultados <- data.frame()

for (v in vars){
  test <- cor.test(soil$pH, soil[[v]], method = "pearson")
  resultados <- rbind(resultados,
                      data.frame(Conjunto = paste("pH -", v),
                                 t = round(test$estimate, 3),
                                p_value = round(test$p.value, 4)))
}

# Tabla de resultados de las correlacions y los r
print(resultados)

# Correlograma
corrplot(cor_matrix, method = "circle", type = "upper",
         tl.cex = 0.8, col = colorRampPalette(c("red3","dodgerblue4"))(200))

# =========================================================================
# Ejercicio 3 -------------------------------------------------------------
# =========================================================================

# Base de datos incorporada a R
anscombe
data("anscombe")

anscombe_data <- anscombe

# resumen estadistico
summary(anscombe_data)

# Correlacion de las distintas variables
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
par(mfrow=c(1,1))

# Tabla

print(anscombe_data)

anscombe_data %>%
  gt()
