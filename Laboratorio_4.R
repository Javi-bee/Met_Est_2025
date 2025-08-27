# Prueba de t
# Caso de muestras independientes
# JEGR
# 27/08/2025

# Importar datos de Indice de calidad

calidad <- read.csv("Calidad_plantas.csv", header = T)
View(calidad)

calidad$Tratamiento <- as.factor(calidad$Tratamiento) # Para cambiar caracteres a factores

colores <- c("cornflowerblue", "navajowhite")
boxplot(calidad$IE ~ calidad$Tratamiento,
        xlab = "Tratamientos",
        ylab = "Indice de calidad",
        col = colores,                                # Cambiar limite en el eje de las y
        ylim = c(0.4, 1.2),
        main = "Vivero Iturbide")                     

# Estadistica descriptiva

# tapply sirve para obtener un valor cuando contamos
# con varios grupos

tapply(calidad$IE, calidad$Tratamiento, mean)         # Medias para el indice de desvertes para el grupo control y el experimental
tapply(calidad$IE, calidad$Tratamiento, var)          # Varianza para el grupo de variacion y control

# En este caso, como una de las varianzas es mas grande que la otra (3 veces mas), esto podria conllevar problemas
# a la hora de realizar las pruebas de varianzas

