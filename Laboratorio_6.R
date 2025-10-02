# ===================================
# Correlacon Pearson
# Datos del geyser Old Faithful
# JEGR
# 24/09/2025
# ===================================

data("faithful") # Eruptions (cantidad de minutos que dura la erupcion)
# Causalidad, causa (tiempo de espera) - efecto (tiempo que dura la erupcion)

plot(faithful$waiting, faithful$eruptions,
     xlab = "Tiempo de esera (m)",
     ylab = "Erupcion (m)",
     col = "cornflowerblue",
     pch =20)                 # Correlacion positiva

# Correlacionar las dos variables 
# h0 = 0
# h1!= 0

# ========================================
# comprobar que los datos sean o no normales antes de hacer las pruebas de corr
# ========================================

# Prueba de shapiro para normalidad
shapiro.test(faithful$eruptions) # 9.036e-16<0.05, datos no normales
shapiro.test(faithful$waiting)   # 1.015e-10<0.05, datos no normales
# Los datos no son normales

# ========================================
# Pearson solo se utiliza cuando hay datos normales
# ========================================

cor.test(faithful$waiting, faithful$eruptions,
         method = "spearman") # Correlacion alta (0.7779721 )
# valor de p para comprobar, r para la correlacion si rechazar o no hipotesis nula


