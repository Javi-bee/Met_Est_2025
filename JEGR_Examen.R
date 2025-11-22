# Examen parcial
# Javier Elias Gloria Rodriguez
# 22/10/2025

read.csv("https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1")

download.file("https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1",
              destfile = "C:/Repositorios_GIT/Met_Est_2025")

suelo <- read.csv("obs.csv")

suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)

View(suelo)

# Actividad 1 -------------------------------------------------------------

# P1
summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)
# se observa que el contenido promedio de arcilla conforme aumenta la profundidad
# es mayor respecto a niveles de profundidad mas superficiales

# Actividad 2 -------------------------------------------------------------

# Grafica boxplot de clay1
boxplot(suelo$Clay1,
        main = "Contenido de arcilla en profundidad 0-10 cm",
        ylab = "Contenido de arcilla",
        col = "navajowhite")

# P2
# Si

# P3
head(suelo$Clay1, 3L) # Primeros 3 valores de Clay1

# Actividad 3 -------------------------------------------------------------

# Media de la variable Clay1
mean(suelo$Clay1)

# P4
t.test(suelo$Clay1, mu=30)
# El contenido de arcilla en suelos tropicales no es significativamente diferente
# al contenido de arcilla analizado en la prueba experimental TCP

# Actividad 4 -------------------------------------------------------------

# P5
shapiro.test(suelo$Clay1)
shapiro.test(suelo$Clay5)

# Datos no normales

cor.test(suelo$Clay1, suelo$Clay5,
         method = "spearman")
# Existe una correlacion positiva fuerte entre los contenidos de arcilla 
# con respecto a los perfiles superiores e inferiores

# P7
# La relacion es estadisticamente significativa

# Actividad 5 -------------------------------------------------------------

# P6
# Si, podrian identificarse de manera visual por medio de graficos de bigotes
# (boxplot), y observando sus variaciones por medio de pruebas estadisticas
# para encontrar similitudes entre concentracion de arcilla por zonas.
# Como medias, sd, varianzas, correlacion, etc.

# P7
plot(suelo$zone, suelo$Clay5,
        main = "% de arcilla en profundidad 30-50cm en diversas zonas",
        col = "pink2",
     xlab = "Zonas",
     ylab = "Contenido de arcilla")
# Si, existen indicios. se puede observar como el contenido de arcilla es mayor
# en las zonas 1 y 2, y menor en las zonas 3 y 4
