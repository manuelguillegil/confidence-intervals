# Laboratorio 2 Estadística

# Leonardo López 14-10576
# Manuel Gil 14-10397

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
