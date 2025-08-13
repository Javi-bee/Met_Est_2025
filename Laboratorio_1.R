# Práctica: gastos de estudiante universitaria

## Gastos totales por mes
300 + 240 + 1527 + 400 + 1500 + 1833
Celular <- 300
Celular
Transporte <- 240
Comestibles <- 1527
Gimnasio <- 400
Alquiler <- 1500
Otros <- 1833
Total <- Celular + Transporte + Comestibles + Alquiler + Gimnasio + Otros
Semestre <- Total*5
Aual <- Total*10

#**Funciones de R**

#**Valor absoluto**
abs(10)
abs(-4)

#**Raíz cuadrada**
sqrt(9)

#**Logaritmo natural**
log(2)

# Comentarios normales

2*9
4+5 #Tambien se puede colocar un comentario aqui

celular <- 300
Celular <- 300
CELULAR <- 300

## Buscar ayuda dentro de R

help(abs)
help(mean)
?abs
help.search("absolute")

#**Ejercicio de autoevaluación**

gastos <- c(Celular, Transporte, Comestibles,
            Gimnasio, Otros, Alquiler)
gastos

barplot(gastos)

help(sort)
gastos_ord <- sort(gastos, decreasing = TRUE)
gastos_ord
barplot(gastos_ord)
barplot(gastos_ord, main = "Gastos mensuales",
        names.arg = c("Otros", "Comestibles",
        "Alquiler", "Gimnasio",
        "Celular", "Transporte"))
help(barplot)
