# Laboratorio 2 Estadística

# Leonardo López 14-10576
# Manuel Gil 14-10397

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

### Segunda Parte

x1 <- c(15, 16, 14, 15, 17, 18, 19, 15)
x2 <- c(13, 12, 11, 13, 11, 9, 10, 10)

summary(x1)
summary(x2)

boxplot(x1,x2, names=c("Metodo 1","Metodo2"))

# En los diagrama de caja se puede observar que el método 1
# tiene resultados muy superiores al método 2, teniendo su
# mínimo por encima del máximo del método 2 y su mediana
# casi 6 unidades por encima.

t.test( x1, x2, conf.level = 0.95, var.equal = TRUE)$conf.int
# t.test( x1, x2, conf.level = 0.95)$conf.int

# El intervalo de confianza del 95% para la diferencia de las medias es
# (3.286326, 6.713674)
# Como el intervalo no incluye al 0, es probable que las medias de ambos
# métodos no sean iguales. Que el intervalo sea positivo indica que la
# media del método 1 es mayor, lo cual concuerda con lo observado en el
# análisis descriptivo.

sd(x1)
sd(x2)

# La diferencia entre las medias es muy pequeña por lo cual podríamos
# considerarlas estadísticamente iguales.
# De hecho, el intervalo de confianza que se obtiene asumiendo que las
# varianzas son iguales es casi igual al que se obtiene asumiendo que
# no lo son
