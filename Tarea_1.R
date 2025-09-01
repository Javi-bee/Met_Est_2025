# 31/08/2025
#JEGR
# Base de datos Iris

library("ggplot2")

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


