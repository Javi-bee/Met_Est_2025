# Laboratorio semana 3
# 20/08/2025
# Javier Elias Gloria Rodriguez

# Correr (si no se tienen descargados) los paquetes dentro de la consola y hacer
## Library() dentro de mi espacio de trabajo (script)

library(readr)

library(repmis)

# Importar datos ----------------------------------------------------------

Temp <- read.csv("temperatura.csv", header = T)
Temp <- read.csv("Data/medias_temp.csv", header = T)

# Ingresar datos de manera manual

edad <- c(18, 19, 18, 18, 25, 19, 18, 18, 18, 17, 19,
          19, 18, 17, 19, 18, 19, 19)
alumno <- seq(1,18,1)

info <- data.frame(alumno, edad)

info$Altura <- c(174, 174, 170, 160, 158, 155, 188,
                 170, 175, 170, 172, 170, 174, 180,
                 158, 161, 188, 164)


# Graficar datos ----------------------------------------------------------

boxplot(info$Altura,
        # col sirve para colorear la grafica
        col = "cornflowerblue",
        # main sirve para poner titulo a la grafica
        main = "Clase 3 semestre")  

# Este comando me da una lista entera de los colores en R colors()

colores = c("indianred", "navajowhite", "skyblue")

# Datos obtenidos de una URL de manera seguida sin comas
url <- "https://repodatos.atdt.gob.mx/api_update/senasica/actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv"

inspeccion <- read.csv(url)

head(inspeccion)

# Datos obtenidos de una URL con comas de por medio
# No afecta a la lectura de la URL, ademas de no extenderse demasiado 
# por el script
prof_url_2 <- paste0("https://repodatos.atdt.gob.mx/api_update/senasica/",
                     "actividades_inspeccion_movilizacion/",
                     "29_actividades-inspeccion-movilizacion.csv")

senasica <- read.csv(prof_url_2)

head(senasica)
# Lectura de datos.csv proveniente de la pagina dropbox
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

head(conjunto)

file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")

inventario <- read.csv(file)

head(inventario)

inventario

inventario$Diametro

# Estadisticas descriptivas -----------------------------------------------

# gl no sirve para dar diversos niveles a un conjunto de datos
# En este caso, como se tienen 30 datos, 6 niveles de 5 datos cada una
gl(6,5)
parcelas <- gl(6,5)
parcelas

# En el inventario original se tenian 50 datos, con el comando seq se
# seleccionaron datos del 1 hasta el 30
trees <- seq(1,30)

dbh<-c(16.5,25.3,22.1,17.2,16.1,8.1,34.3,5.4,5.7,11.2,24.1,
       14.5,7.7,15.6,15.9,10,17.5,20.5,7.8,27.3,
       9.7,6.5,23.4,8.2,28.5,10.4,11.5,14.3,17.2,16.8)

trees <- data.frame(trees, dbh, parcelas)

View(trees)

# Media del diametro de los arboles
mean(trees$dbh)
# Desviacion media de los arboles
sd(trees$dbh)

#Suma de los valores del diametro de los arboles los cuales son menores
# a 10 cm
sum(trees$dbh < 10)
# El comando which nos devuelve cuales fueron los arboles que tenian un diametro
# menor a 10 cm
which(trees$dbh < 10)

# Con este comando se puede excluir, en este caso, los arboles de la parcela
# numero 2
trees.13 <- trees[!(trees$parcelas=="2"),]
trees.13

# Se crea un set de datos el cual tome como consideracion 
trees.1 <-subset(trees, dbh <=10)
head(trees.1)
trees.1

hist(trees$dbh, 
     col = "tomato",
     main = "Muestra original trees")

hist(trees.1$dbh,
     col = "skyblue",
     main = "dbh < 10 cm. trees.1")

# Histogramas -------------------------------------------------------------

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")

hist(mamiferos$total_sleep, #Datos
     xlim = c(0,20), ylim = c(0,14), #Cambiar los limites de x & y
     main = "Total de horas de sueño de las 39 especies", # Cambiar el titulo
     xlab = "Horas de sueño", # Cambiar eje de las x
     ylab = "Frecuencia", # Cambiar eje de las y
     las = 1, # Cambiar orientacion de y
     col = ("navajowhite"))

data("chickwts")
head(chickwts[c(1:2,42:43, 62:64),])

feeds <- table(chickwts$feed)
feeds

row.names(feeds)

barplot(feeds,
        col =  "red")

barplot(feeds[order(feeds, decreasing = TRUE)],
        col = "orange")

barplot(feeds[order(feeds, decreasing = TRUE)],
        names.arg = c("casein",    "horsebean", "linseed",
                      "meatmeal",  "soybean", "sunflower"), horiz = TRUE,
        col = "cornflowerblue",
        xlab = substitute(paste(bold("Tipo de alimentacion de los polluelos"))),
        ylab = substitute(paste(bold("Cantidad de los polluelo"))),
)  

help("barplot")

library(tinytex)