# Ejercicio correlacion de spearman
# JEGR
# 25/09/2025

set.seed(123)  # Para fijar el logaritmo y siempre que corra el codigo sean los
               # mismos datos

# Numero de observaciones
n <-20

# Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = T)

# Asignar probabilidad de aprobar en funcion  de horas de estudio

# A mas horas, mas alta probabilidad

Resultado <- sapply (Horas_estudio, function(horas) {
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
})

# Crear data frame

estudio <- data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)
estudio

# Convertir puntuacion de examen en numeros asignando valores de 1 (apr) y 0(susp)
estudio$Rsultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)

cor.test(estudio$Horas_estudio, estudio$Rsultado_bin,
         method = "pearson")

# Media de horas de estudio de los aprobados
mean_aprobados <- mean(estudio$Horas_estudio[estudio$Resultado == "Aprobado"])
mean_aprobados

# Media de horas de estudio de los reprobados
mean_reprobados <- mean(estudio$Horas_estudio[estudio$Resultado == "Reprobado"])
mean_reprobados

sd(estudio$Horas_estudio)

N <- length(estudio$Estudiante)
n