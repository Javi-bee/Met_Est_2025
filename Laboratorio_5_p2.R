# Laboratorio 5, parte 2
# Base de datos arboles
# JEGR
# 10/09/2025

sem <- read.csv("Datos_arboles.csv", header = T)
sem$Tiempo <- as.factor(sem$Tiempo)

View(sem)

tapply(sem$Kgsem, sem$Tiempo, mean)
tapply(sem$Kgsem, sem$Tiempo, var)
tapply(sem$Kgsem, sem$Tiempo, sd)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col = "navajowhite",
        xlab = "Año",
        ylab = "Semilla (kg)")     

temp_2012 <- subset(sem, sem$Tiempo == "T2012")
temp_2013 <- subset(sem, sem$Tiempo != "T2012")

t.test(temp_2012$Kgsem, temp_2013$Kgsem, var.equal = T, paired = T)  # solo 49 valores libertad
# Porque tenemos una diferencia de años entre el mismo grupo
# No hay produccion significativa entre la produccion del 2012 y el 2013

# Prueba de t comparando dos grupos iguales en distintos años
# Aqui se utiliza alternative = less, para comparar 2012 con 2013 en Kgsem
t.test(temp_2012$Kgsem, temp_2013$Kgsem, var.equal = T, paired = T,
       alternative = "less")
