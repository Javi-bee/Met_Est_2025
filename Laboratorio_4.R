# Prueba de t
# Caso de muestras independientes
# JEGR
# 27/08/2025

# Importar datos de Indice de calidad

calidad <- read.csv("Calidad_plantas.csv", header = T)
View(calidad)

calidad$Tratamiento <- as.factor(calidad$Tratamiento)# Para cambiar caracteres a factores

colores <- c("cornflowerblue", "navajowhite")
boxplot(calidad$IE ~ calidad$Tratamiento,
        xlab = "Tratamientos",
        ylab = "Indice de calidad",
        col = colores,                               # Cambiar limite en el eje de las y
        ylim = c(0.4, 1.2),
        main = "Vivero Iturbide")                     

# Estadistica descriptiva -------------------------------------------------

# tapply sirve para obtener un valor cuando contamos
# con varios grupos

tapply(calidad$IE, calidad$Tratamiento, mean)        # Medias para el indice de de esvertes 
tapply(calidad$IE, calidad$Tratamiento, var)         # Varianza para ambos grupos
tapply(calidad$IE, calidad$Tratamiento, sd)          # Desviacion media para ambos grupos

# Observamos que la varianza del grupo fert es 3 veces
# mas grande que el grupo control (Ctrl)
library(ggplot2)

ggplot(calidad, aes(x = IE, color = Tratamiento))+   # dist. datos en una grafica de densidad
  geom_density()

df_ctrl <- subset(calidad, Tratamiento == "Ctrl")    # == Igual a 
df_fert <- subset(calidad, Tratamiento != "Ctrl")    # != Diferente a

# qqnorm realizar normalidad

par(mfrow = c(2,2))                                  # par(mfrow) dos columnas de graficas
qqnorm(df_ctrl$IE); qqline(df_ctrl$IE)               # Normalidad de ctrl
qqnorm(df_fert$IE); qqline(df_fert$IE)               # Normalidad de fert
par(mfrow = c(1,1))                                  # c(1,1) una columna con una sola grafica

# Prueba de normalidad

shapiro.test(df_ctrl$IE)
shapiro.test(df_fert$IE)

# Revisar homogeneidad de varianzas
var.test(df_ctrl$IE, df_fert$IE)                     # Son datos homogeneos 
var.test(calidad$IE ~ calidad$Tratamiento)           # Otra manera de hacer 
                                                     # prueba de varianzas
# Aplicar la prueba de t, varianzas iguales
# Dos colas = two.sided

# Prueba de t
t.test(calidad$IE ~ calidad$Tratamiento,             # Intervalo de confianza 
       alternative = "two.sided",                     
       var.equal = T)                                

# si la diferencia estuviese entre grupo Ctrl y fert fuese entre -0.23331192 -0.04478332
# la diferencia entre ambos no seria significativa?

# Reportar datos
# r(40) = -2.9813, p = 0.004868 
# IC (40) = 
# Medir el efecto

cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1-1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 -2))
  (mean(x) - mean(y)) / sp
}
 
d_cal <- cohens_efecto(df_ctrl$IE, df_fert$IE)
d_cal
