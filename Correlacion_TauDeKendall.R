# Ejercicio correlacion de spearman
# JEGR
# 25/09/2025

# Datos
tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)

# Metodo de correlacion kendall
cor.test(tau$A, tau$B, method = "kendall") # >0.05 (p-value = 0.2722), se acepta h0
# No significativa y correlacion media