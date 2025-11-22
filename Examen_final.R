# JEGR
# Examen final
# 20/11/2025  

ejercicio_3 <- read.csv("Ejercicio_3.csv")

# Pregunta de investigacion: Existe una correlacion significativa entre la densidad
# arbustiva y la cobertura en 30 individuos

# Ho = No existe una correlacion significativa entre la densidad arbustiva
# y la cobertura entre ambas variables (p = 0)
# H1 = Existe una correlacion significativa entre la densidad arbustiva y la
# cobertura entre ambas variables (p diferente de 0)

# Analisis descriptivo ----------------------------------------------------

# Graficos de dispersion
plot(ejercicio_3$Densidad_Arbustiva_indha, ejercicio_3$Cobertura_pct,
     xlab = "Cobertura",
     ylab = "Densidad",
     main = "Grafico de dispersion",
     col = "red")


# Se puede observar como los datos son relativamente lineales, con algunos datos
# (sobre la linea de tendencia), con algunos datos sobresalientes en cobertura

summary(ejercicio_3)

# Medidas de dispersion y mediana de densidad_arbustiva
sd(ejercicio_3$Densidad_Arbustiva_indha)
var(ejercicio_3$Densidad_Arbustiva_indha)
mean(ejercicio_3$Densidad_Arbustiva_indha)

# Medidas de dispersion y mediana de cobertura
sd(ejercicio_3$Cobertura_pct)
var(ejercicio_3$Cobertura_pct)
mean(ejercicio_3$Cobertura_pct)

# Se puede observar como la desviacion estandar, la media y la varianza del
# primer grupo es mucho mayor que los datos de cobertura

# Metodo estadistico ------------------------------------------------------

shapiro.test(ejercicio_3$Densidad_Arbustiva_indha) # mayor a 0.05 (0.635)

shapiro.test(ejercicio_3$Cobertura_pct)            # mayor a 0.05 (0.3588)
# Ambos son datos normales, no se rechaza hipotesis alternativa

var.test(ejercicio_3$Densidad_Arbustiva_indha, ejercicio_3$Cobertura_pct)
# Los datos no tienen homogeneidad en sus varianzas(p-value < 2.2e-16)

cor.test(ejercicio_3$Densidad_Arbustiva_indha, ejercicio_3$Cobertura_pct,
         method = "pearson")

# ====================================================
# Interpretacion de resultados
# ====================================================
# Se utilizo una correlacion de pearson debido a la normalidad de los datos,
# aunque estos no contaban con una homogeneidad de varianzas. Sin embargo, 
# comparando ambos metodos, no hay una diferencia significativa entre ambos p-values,
# siendo la diferencia más signifactiva entre ambos el valor de R (R =~ -0.90 y
# Rho = ~ -0.78).
# Segun lo obtenido en base al metodo de pearson, se obtuvo un valor de r de =
# -0.09084653 y un p-value = 0.6331, lo que demuestra una correlacion negativa
# fuerte y valores no significativos, por lo cual, se concluye que no hipotesis
# hay evidencia para aceptar la hipotesis alternativa. Por lo tanto, se acepta
# la hipotesis nula = no hay evidencia significativa que indique la existencia
# de una la relacion significativa entre ambas variables