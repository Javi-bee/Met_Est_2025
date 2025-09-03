# 31/08/2025
#JEGR
# Base de datos Iris

library("ggplot2")
library("dplyr")

# Base de datos
data("iris")
View(iris)

# Renombrar base de datos, para hacerlo mas facil de seguir
iris_df <- rename(iris,
                  petal_length = Petal.Length,
                  petal_width = Petal.Width,
                  sepal_length = Sepal.Length,
                  sepal_width = Sepal.Width,
                  species = Species)

# Datos estadisticos descriptivos simples
summary(iris_df)
head(iris_df)

# Grafico boxplot simple
color <- c("cornflowerblue", "tomato", "navajowhite")

windows(width = 4.5, height = 4) # Hacer la ventana mas ancha que alta

opar <- par(no.readonly = T)  #Guardar los parametros graficos actuales

par(mar = c(5, 5, 4, 6)) # Cambiar margenes de la grafica

boxplot(iris_df$petal_length ~ iris_df$species,
        col = color,
        main = "Distribucion del largo del petalo por especie",
        xlab = "Especie",
        ylab = "Largo del petalo (cm)")
legend("right",
       legend = c("setosa", "versicolor", "virginica"),
       inset = c(-0.57, 0),
       fill = color,
       col = color,
       xpd = T)
on.exit(par(opar))


# Estadistica descriptiva -------------------------------------------------

data_sub <- subset(iris_df, species %in% c("versicolor", "virginica"))
iris_sp <- data.frame(species = data_sub$species,
                      petal_length = data_sub$petal_length) # Dataframe con solo
                      # species (en orden, versicolor y virginica) y petal_length
View(iris_sp)
head(iris_sp)
summary(iris_sp)

# Media, desv.est y varianza

tapply(iris_sp$petal_length, iris_sp$species, mean)
tapply(iris_sp$petal_length, iris_sp$species, sd)
tapply(iris_sp$petal_length, iris_sp$species, var)

# Grafica de densidad
ggplot(iris_sp, aes(x = petal_length, color = species,))+
         geom_density()

df_versicolor <- subset(iris_sp, species  == "versicolor")
df_virginica <- subset(iris_sp, species != "versicolor")

# Hipotesis ---------------------------------------------------------------

# ¿Existe una diferencia significante entre el largo del petalo de ambas especies?
# H0 = no hay diferencia
# H1 = si hay diferencia


# Grafico de normalidad para ambas especies
qqnorm(df_versicolor$petal_length); qqline(df_versicolor$petal_length)
qqnorm(df_virginica$petal_length); qqline(df_virginica$petal_length)
# Ambos tienen datos normales

# Prueba de normalidad
shapiro.test(df_versicolor$petal_length)
shapiro.test(df_virginica$petal_length)
# Mayor a 0.05 (p-value = 0.1585), por lo que existe normalidad en variables

# Homogeneidad de varianzas
var.test(df_versicolor$petal_length, df_virginica$petal_length)
# p-value = 0.2637, varianzas relativamente similares, se puede utilizar prueba de t

# Prueba de t
t.test(df_versicolor$petal_length, df_virginica$petal_length,
       alternative = "two.sided",
       var.equal = T)
# p-value < 2.2e-16, menor a 0.05, por lo que se rechaza H0, hay una gran diferencia
# en tamaño de petalos

# Prueba de cohen´s d
cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1-1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 -2))
  (mean(x) - mean(y)) / sp
}

d_cal <- (cohens_efecto(df_versicolor$petal_length, df_virginica$petal_length))
d_cal
# Valor que representa la diferencia entre las medias de ambas variables 
# Mientras mas grande, mayor diferencia habra entre sus medias y datos

# Grafico de violin
ggplot(iris_sp, aes(x = species, y = petal_length))+
  geom_violin()
