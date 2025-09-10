# 04/09/2025
# JEGR
# Base de datos Iris

library("ggplot2")
library("dplyr")
library("hrbrthemes")
library("viridis")
library("gt")
library("gtExtras")

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


# Estadistica descriptiva -------------------------------------------------

data_sub <- subset(iris_df, species %in% c("setosa", "versicolor"))
iris_sp <- data.frame(species = data_sub$species,
                      petal_length = data_sub$petal_length) # Dataframe con solo
# species (en orden, setosa y versicolor) y petal_length
View(iris_sp)
head(iris_sp)
summary(iris_sp)

# Media, desv.est y varianza

tapply(iris_sp$petal_length, iris_sp$species, mean) # Media de vers. 3 veces mayor
tapply(iris_sp$petal_length, iris_sp$species, sd)   # Var. de vers. 3 veces mayor
tapply(iris_sp$petal_length, iris_sp$species, var)  # Sd. de vers. mucho mayor

# Grafica de densidad
ggplot(iris_sp, aes(x = petal_length, color = species,))+
  geom_density()

df_setosa <- subset(iris_sp, species  == "setosa")
df_versicolor <- subset(iris_sp, species != "setosa")

# Hipotesis ---------------------------------------------------------------

# ¿En comparacion?
# H0 = no hay una diferencia significativa entre las medias de ambas especies
# H1 = hay una gran diferencia entre las medias de setosa con respecto a versicolor


# Grafico de normalidad para ambas especies
qqnorm(df_setosa$petal_length); qqline(df_setosa$petal_length)
qqnorm(df_versicolor$petal_length); qqline(df_versicolor$petal_length)
# Setosa muestra datos una distribucion no normal, al contrario de versicolor,
# que cuenta con una distribucion normal en su grafica

# Prueba de normalidad
shapiro.test(df_setosa$petal_length)
shapiro.test(df_versicolor$petal_length)
# df_setosa presenta datos no normales (p-value = 0.05481), mientras que df_versicolor
# si presenta datos normales (p-value = 0.1585)

# Homogeneidad de varianzas
var.test(df_setosa$petal_length, df_versicolor$petal_length)
# IC(95%) = [0.07750613, 0.24068043], p -value =  1.026e-10, varianzas muy diferentes
# en ambos grupos con un p-value significativo >.005.

##########################################################################
##########################################################################
# Prueba de t ()
t.test(df_setosa$petal_length, df_versicolor$petal_length,
       alternative = "less",
       var.equal = F)
# p-value < 2.2e-16, menor a 0.05, por lo que se rechaza H0, hay una gran diferencia
# en tamaño de petalos
##########################################################################
##########################################################################

# Prueba de cohen´s d
cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1-1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 -2))
  (mean(x) - mean(y)) / sp
}

d_cal <- (cohens_efecto(df_setosa$petal_length, df_versicolor$petal_length))
d_cal
# Valor que representa la diferencia entre las medias de ambas variables 
# Mientras mas grande, mayor diferencia habra entre sus medias y datos
# Valor muy significativo, cohen´s D = -7.898544

# Grafico boxplot simple de ambas especies
boxplot(df_setosa$petal_length, df_versicolor$petal_length,
        names = c("setosa","versicolor"),
        col = color,
        main = "Distribucion del largo del petalo por especie",
        xlab = "Especie",
        ylab = "Largo del petalo (cm)")

# Grafico de violin
ggplot(iris_sp, aes(x = species, y = petal_length))+
  geom_violin()

sample_size = iris_sp %>%group_by(species) %>%summarize(num=n())

iris_sp %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(species, "", "")) %>%
  ggplot( aes(x=myaxis, y= petal_length, fill=species))+
  ylab("Longitud del petalo")+
  geom_violin(width=1.4)+
  geom_boxplot(width=0.1, color="grey", alpha=0.2)+
  scale_fill_viridis(discrete= T)+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  )+
  ggtitle("Grafico de violin sobre boxplot")+
  xlab("")

# Tablas

table_iris_sp <- data.frame(
  Especies = c("setosa", "versicolor"),
  Media = c(1.462, 4.260),
  Varianza = c(0.03015918, 0.22081633),
  Desv.Estandar = c(0.173664, 0.469911)
)
table_iris_sp %>%
  gt() %>%
  gt_theme_pff()

mean_iris_sp <- data.frame(                 # Media de ambas especies
  Especies = c("setosa", "versicolor"),
  Media = c(1.462, 4.260)
)
mean_iris_sp %>%
  gt() %>%
  gt_theme_pff()

var_iris_sp <- data.frame(                 # Varianza de ambas especies
  Especies = c("setosa", "versicolor"),
  Varianza = c(0.03015918, 0.22081633)
)
var_iris_sp %>%
  gt() %>%
  gt_theme_pff()

sd_iris_sp <- data.frame(                  # Desv.Est de ambas especies
  Especies = c("setosa", "versicolor"),
  Desv.Estandar = c(0.173664, 0.469911)
)
sd_iris_sp %>%
  gt() %>%
  gt_theme_pff()
