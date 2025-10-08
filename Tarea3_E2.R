# Tarea 3, EJERCICIO 2
# JEGR
# 02/10/2025

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

# Matriz de correlacion
cor_matrix <- cor(soil[,3:11],method = "pearson")

# Test de normalidad para cada par
library(Hmisc)
cor_results <- rcorr(as.matrix(soil[,3:11]))

cor_matrix
cor_results$P

# Correlograma
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper",
         tl.cex = 0.8, col = colorRampPalette(c("red", "white","blue"))(200))


