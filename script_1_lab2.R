## Function custom para calcular un intervalo de confianza para la media
## También se puede utilizar la función t.test(pesos, conf.level)$conf.init
##   para hacer el cálculo del intervalo de confianza de la media
intervalo <- function(x, alfa) {
  n <- length(x)
  nu <- n -1 #grados de libertad
  cuantil <- qt( 1 - alfa/2, df = nu)
  LS <- mean(x) + cuantil*sqrt( var(x)/n )
  LI <- mean(x) - cuantil*sqrt( var(x)/n )
  
  return( c(LI, mean( x ), LS))
}

### Array con las datos de las horas sueño
horas_sueño <- c(6.9, 7.6, 6.5, 6.2, 5.3, 7.8, 7.0, 5.5, 7.6, 6.7, 7.3, 6.6, 7.1, 6.9, 6.0, 6.8, 6.5, 7.2, 5.8, 8.6, 7.6, 7.1, 6.0, 7.2, 7.7)


## Análisis descriptivo de los datos
summary(horas_sueño)
par(mfrow = c(1, 2) )
hist(horas_sueño, main = "Horas de sueño por noche", ylab ="Frecuencia", xlab = "Horas")
boxplot(horas_sueño, col = "orange", horizontal = F, xlab = "Horas de sueño por noche")

## Cálculo del intervalo de confianza de un intervalo de 78% para la población media del número de horas sueño por noche

## Método 1: Función de librería de R para proporciones
t.test(horas_sueño, conf.level = 0.75)$conf.int

## Método 2: Función custom desarrollada
intervalo(horas_sueño, 0.25)

