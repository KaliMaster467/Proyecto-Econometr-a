library(readr)
library(tseries)
library(lmtest)
library(corrplot)
library(tidyverse)
library(car)
library(nlme)
library(stargazer)

DEMANDA_AMERICA <- read_csv("ECONOMETRIA.csv")
View(DEMANDA_AMERICA)

DEMANDA_AMERICA <- DEMANDA_AMERICA[complete.cases(DEMANDA_AMERICA), ]


regresion <- lm(Q ~ P + PRECIO_GASOLINA_USA + TIPO_CAMBIO_REAL, data = DEMANDA_AMERICA)

summary(regresion)

stargazer(type = "text",regresion)


bptest(regresion)

corrplot(cor(DEMANDA_AMERICA |> 
               select(-MES, -MXN_USD, -Q, -VENTA_INTERNA_GASOLINA, -PCANAD, -PANDEMI, -BARRIL_OPEP)), method= "number")

par(mfrow = c(2, 2))  # Configura la ventana gráfica para mostrar 4 gráficos
plot(regresion)



# Gráficos de residuos vs. variables independientes
plot(DEMANDA_AMERICA$P, resid(regresion), main = "Residuos vs X1", xlab = "X1", ylab = "Residuos")
abline(h = 0, col = "red")


plot(DEMANDA_AMERICA$PRECIO_GASOLINA_USA, resid(regresion), main = "Residuos vs X3", xlab = "X3", ylab = "Residuos")
abline(h = 0, col = "red")


vif(regresion)

dwtest(regresion)


# Ajustar el modelo con una estructura AR(1)



# Obtener los residuos del modelo
residuos <- residuals(regresion)

# Graficar el correlograma
acf(residuos, main = "Correlograma de los Residuos")


# Graficar residuos vs. tiempo
plot(residuos, type = "o", col = "blue", main = "Residuos vs. Tiempo",
     xlab = "Índice Temporal", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)


### HACER LA DEMANDA

# Calcular los valores promedio de las variables
mean_gas <- mean(DEMANDA_AMERICA$PRECIO_GASOLINA_USA, na.rm = TRUE)

# Crear un rango de precios para la gráfica
P_values <- seq(0, max(DEMANDA_AMERICA$P)+100, length.out = 100)

# Calcular los valores predichos de Q
Q_pred <- -12.926988 - 0.013795 * P_values + 27.363944 * mean_gas

# Crear la gráfica de la curva de demanda
plot(P_values, Q_pred, type = "l", col = "blue", lwd = 2,
     xlab = "Precio (P)",
     ylab = "Cantidad Demandada (Q)",
     main = "Curva de Demanda Exterior en América del Petróleo mexicano")
grid()

points(DEMANDA_AMERICA$P, DEMANDA_AMERICA$Q, pch = 19, col = "red")
legend("topright", legend = c("Curva Estimada", "Datos Observados"), col = c("blue", "red"), lty = 1, pch = 19)

####USANDO LA VARIACION DEL TIPO DE CAMBIO REAL T-1

# Excluir la primera fila
datos_sin_primera_fila <- DEMANDA_AMERICA[-1, ] 

# Ajustar el modelo de regresión lineal

inex <- datos_sin_primera_fila$P/datos_sin_primera_fila$PRECIO_GASOLINA_USA

modelo <- lm(QUSA ~ inex + PCANAD + IP_INDEX + INDEX_BRENT + VAR_TC_COR + ANUNCIO, data = datos_sin_primera_fila)
# Transformación logarítmica en las variables
modelo_log <- lm(log(QUSA) ~ log(P) + log(PRECIO_GASOLINA_USA) + log(PCANAD) + 
                   log(IP_INDEX) + log(INDEX_BRENT), 
                 data = datos_sin_primera_fila)

# Resumen del modelo ajustado
summary(modelo_log)

 
modelo2 <- lm(Q ~ P + PRECIO_GASOLINA_USA + CPIAUCSL, data = datos_sin_primera_fila)
# Resumen del modelo
summary(modelo)
summary(modelo_log)
stargazer(type = "text",modelo)
stargazer(type = "text",modelo_log)

plot(modelo)

###PRUEBAS SIGNIFICANCIA POR SIGNO

##INEX
p_P <- pt(-5.252 , df = 46, lower.tail = TRUE)
###PRECIO CANADA
p_P <- pt(1.997 , df = 46, lower.tail = FALSE)
###IP INDEX
p_P <- pt(6.109, df = 46)
###INDEX BRENT
p_P <- pt(4.334, df = 46, lower.tail = FALSE)
###VARIACIÓN TIPO DE CAMBIO REAL
p_P <- pt(-1.654, df = 46, lower.tail = TRUE)
###ANUNCIO
p_P <- pt(-5.591, df = 46, lower.tail = TRUE)
##GRAFICAMOS LOS ERRORES

residuales <- residuals(modelo)
jarque.bera.test(residuales)
plot(modelo)
plot(
  fitted(modelo), 
  residuales^2,
  main = "Relación entre valores ajustados y cuadrados de los residuales", # Título del gráfico
  xlab = "Valores ajustados (Fitted Values)", # Etiqueta del eje X
  ylab = "Residuales al cuadrado (Squared Residuals)", # Etiqueta del eje Y
  pch = 16, # Cambiar estilo de los puntos
  col = "blue" # Color de los puntos
)
##ANALISIS GGPLOT heterosedasticidad

ggplot(data = datos_sin_primera_fila, aes(x = fitted(modelo), y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm", color = "blue") + 
  theme_minimal() +
  labs(
    title = "Relación entre Valores Ajustados y Residuales al Cuadrado MODELO FINAL",
    x = "Valores Ajustados (Fitted Values)",
    y = "Residuales al Cuadrado (Squared Residuals)"
  )


bptest(modelo, studentize = F)


white_test <- bptest(modelo, ~ fitted(modelo) + I(fitted(modelo)^2), studentize = F)

# Mostrar los resultados
print(white_test)


###EVALUACION POR VARIABLES
#### PRECIO
ggplot(data = datos_sin_primera_fila, aes(x = inex, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm")
### PRECIO GASOLINA
ggplot(data = datos_sin_primera_fila, aes(x = PCANAD, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm")
###VARIACION TIPO CAMBIO REAL
ggplot(data = datos_sin_primera_fila, aes(x = INDEX_BRENT, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm")

ggplot(data = datos_sin_primera_fila, aes(x = VAR_TC_COR, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm")

ggplot(data = datos_sin_primera_fila, aes(x = ANUNCIO, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm")

library(gridExtra)

# Crear los gráficos individualmente
grafico1 <- ggplot(data = datos_sin_primera_fila, aes(x = inex, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm") +
  ggtitle("Relación INEX vs Residuales^2")

grafico2 <- ggplot(data = datos_sin_primera_fila, aes(x = PCANAD, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm") +
  ggtitle("Relación PCANAD vs Residuales^2")

grafico3 <- ggplot(data = datos_sin_primera_fila, aes(x = INDEX_BRENT, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm") +
  ggtitle("Relación INDEX_BRENT vs Residuales^2")

grafico4 <- ggplot(data = datos_sin_primera_fila, aes(x = VAR_TC_COR, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm") +
  ggtitle("Relación VAR_TC_COR vs Residuales^2")

grafico5 <- ggplot(data = datos_sin_primera_fila, aes(x = ANUNCIO, y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm") +
  ggtitle("Relación ANUNCIO vs Residuales^2")

# Organizar los gráficos en un único lienzo
grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, ncol = 2)

corrplot(cor(datos_sin_primera_fila |>
               mutate(inex = P/PRECIO_GASOLINA_USA) |>
               select(QUSA, inex , PCANAD  , INDEX_BRENT , IP_INDEX, VAR_TC_COR, ANUNCIO)), method= "number")

vif(modelo)


dwtest(modelo)

###PROBLEMA DE AUTOCORRELACION DE LOS ERRORES
# Graficar el correlograma
acf(residuales, main = "Correlograma de los Residuos")


# Graficar residuos vs. tiempo
plot(residuales, type = "o", col = "blue", main = "Residuos vs. Tiempo",
     xlab = "Índice Temporal", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)


residuos <- residuals(modelo)

# Graficar el correlograma
acf(residuos, main = "Correlograma de los Residuos")


# Graficar residuos vs. tiempo
plot(residuos, type = "o", col = "blue", main = "Residuos vs. Tiempo",
     xlab = "Índice Temporal", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(2, 2))  # Configura la ventana gráfica para mostrar 4 gráficos
plot(modelo)


### HACER LA DEMANDA
#####

# Calcular los valores promedio de PRECIO_GASOLINA_USA y CPIAUCSL
mean_gas <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA, na.rm = TRUE)
mean_tc <- mean(datos_sin_primera_fila$TC_CORREGIDO, na.rm = TRUE)

# Crear un rango de valores para la cantidad demandada (Q)
Q_values <- seq(0, max(datos_sin_primera_fila$QUSA) + 100, length.out = 100)

# Calcular los valores predichos de P (demanda inversa)
P_pred <- (Q_values - 957.421 - 214.905 * mean_gas + 2.201 * mean_cpi) / -5.959

# Crear la gráfica de la curva de demanda inversa con límite mínimo en y = 0
plot(Q_values, P_pred, type = "l", col = "blue", lwd = 2,
     xlab = "Cantidad Demandada (Q)",
     ylab = "Precio (P)",
     main = "Curva de Demanda América Inversa del Petróleo Mexicano",
     ylim = c(0, max(P_pred) + 50))
grid()

# Añadir puntos para los datos observados
points(datos_sin_primera_fila$Q, datos_sin_primera_fila$P, pch = 19, col = "red")

# Añadir la leyenda
legend("topright", legend = c("Curva Estimada Inversa", "Datos Observados"), col = c("blue", "red"), lty = 1, pch = 19)

###COCHRAN ORCUTT

# 1. Ajustar el modelo original
modelo <- lm(Q ~ P + PRECIO_GASOLINA_USA + CPIAUCSL, data = datos_sin_primera_fila)

# 2. Calcular los residuos del modelo original
residuos <- residuals(modelo)

# 3. Calcular el coeficiente de autocorrelación (rho) usando la correlación de los residuos rezagados
rho <- cor(residuos[-1], residuos[-length(residuos)])

# 4. Transformar las variables dependiente e independientes
datos_trans <- datos_sin_primera_fila[-1, ]  # Excluir la primera fila para la transformación
Q_trans <- datos_trans$Q - rho * datos_sin_primera_fila$Q[-nrow(datos_sin_primera_fila)]
P_trans <- datos_trans$P - rho * datos_sin_primera_fila$P[-nrow(datos_sin_primera_fila)]
GAS_trans <- datos_trans$PRECIO_GASOLINA_USA - rho * datos_sin_primera_fila$PRECIO_GASOLINA_USA[-nrow(datos_sin_primera_fila)]
CPI_trans <- datos_trans$CPIAUCSL - rho * datos_sin_primera_fila$CPIAUCSL[-nrow(datos_sin_primera_fila)]

# 5. Ajustar el modelo transformado
modelo_cochran <- lm(Q_trans ~ P_trans + GAS_trans + CPI_trans)

vif(modelo_cochran)

# 6. Mostrar el resumen del modelo transformado
summary(modelo_cochran)

# 7. Verificar la autocorrelación nuevamente con el test de Durbin-Watson

dwtest(modelo_cochran)
bptest(modelo_cochran)



###NUEVA DEMANDA
# Coeficientes estimados del modelo
intercept <- 332.966
coef_P <- -6.400
coef_gasolina <- 227.320
coef_TC <- 6744.309

# Valores constantes para las otras variables
PRECIO_GASOLINA_USA <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA)  # Valor constante (puedes ajustarlo según tus datos)
TC_CORREGIDO <- mean(datos_sin_primera_fila$TC_CORREGIDO)        # Valor constante (puedes ajustarlo según tus datos)

# Generar valores de precio (P)
P <- seq(0, 100, length.out = 100)  # Rango de precios

# Calcular la cantidad demandada (QUSA) usando la ecuación de demanda
QUSA <- intercept + coef_P * P + coef_gasolina * PRECIO_GASOLINA_USA + coef_TC * TC_CORREGIDO

# Crear la gráfica de la curva de demanda
plot(P, QUSA, type = "l", col = "blue", lwd = 2,
     main = "Curva de Demanda",
     xlab = "Precio (P)", ylab = "Cantidad Demandada (QUSA)",
     ylim = c(0, max(QUSA)))

# Agregar líneas para los ejes
abline(h = 0, v = 0, col = "black", lty = 2)

### INVERVSA
# Coeficientes estimados del modelo
intercept <- 332.966
coef_P <- -6.400
coef_gasolina <- 227.320
coef_TC <- 6744.309

# Valores constantes para las otras variables
PRECIO_GASOLINA_USA <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA)  # Valor constante (puedes ajustarlo según tus datos)
TC_CORREGIDO <- mean(datos_sin_primera_fila$TC_CORREGIDO)    # Valor constante (puedes ajustarlo según tus datos)

# Generar valores de cantidad demandada (QUSA)
QUSA <- seq(0, 400, length.out = 100)  # Rango de cantidades demandadas

# Calcular el precio (P) usando la ecuación de demanda inversa
P <- (QUSA - intercept - coef_gasolina * PRECIO_GASOLINA_USA - coef_TC * TC_CORREGIDO) / coef_P

# Asegurarse de que los precios sean no negativos
P[P < 0] <- 0

# Crear la gráfica de la curva de demanda inversa
plot(QUSA, P, type = "l", col = "blue", lwd = 2,
     main = "Curva de demanda inversa de barriles de
Petróleo Mexicano diarios",
     xlab = "Cantidad Demandada (QUSA)", ylab = "Precio (P)",
     ylim = c(0, max(P, na.rm = TRUE)))
grid()
# Agregar líneas para los ejes
abline(h = 0, v = 0, col = "black", lty = 2)

# GRAFICAS PROYECTO

ggplot(data = datos_sin_primera_fila, aes(x = MES, y = QUSA)) +
  geom_line(group = 1, color = "blue", size = 1) +  # Línea
  geom_point(color = "red", size = 2) +            # Puntos
  labs(
    title = "Demanda diaria de barriles de petroleo mexicano 
de EUA por mes",
    x = "Mes",
    y = "Miles de barriles diarios"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6), # Rotar etiquetas del eje X
    plot.title = element_text(hjust = 0.5)            # Centrar el título
  )

#####MODELO 1 

mod1 <- lm(QUSA ~ P + PRECIO_GASOLINA_USA + PCANAD + VAR_PIB + TC_CORREGIDO + IP_INDEX, data = DEMANDA_AMERICA)
summary(mod1)
stargazer(mod1, type = "html", out = "HTMLS")


residuales <- residuals(mod1)
jarque.bera.test(residuales)


ggplot(data = DEMANDA_AMERICA, aes(x = fitted(mod1), y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm", color = "blue") + 
  theme_minimal() +
  labs(
    title = "Relación entre Valores Ajustados y Residuales al Cuadrado",
    x = "Valores Ajustados (Fitted Values)",
    y = "Residuales al Cuadrado (Squared Residuals)"
  )

bptest(mod1, studentize = F)

par(mfrow = c(2, 2))  # Configura la ventana gráfica para mostrar 4 gráficos

plot(mod1)

corrplot(cor(DEMANDA_AMERICA |>
               select(QUSA, P, PRECIO_GASOLINA_USA , PCANAD  , VAR_PIB, TC_CORREGIDO, IP_INDEX, )), method= "number")
vif(mod1)


# Graficar residuos vs. tiempo
plot(residuales, type = "o", col = "blue", main = "Residuos vs. Tiempo",
     xlab = "Índice Temporal", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)
dwtest(mod1)

#####MODELO 2
mod2 <- lm(QUSA ~ inex + PCANAD + IP_INDEX + INDEX_BRENT + VAR_TC_COR, data = datos_sin_primera_fila)
summary(mod2)
stargazer(mod2, type = "text")


residuales <- residuals(mod2)
jarque.bera.test(residuales)


ggplot(data = datos_sin_primera_fila, aes(x = fitted(mod2), y = residuales^2)) + 
  geom_point(color = "red") + 
  stat_smooth(method = "lm", color = "blue") + 
  theme_minimal() +
  labs(
    title = "Relación entre Valores Ajustados y Residuales al Cuadrado 
MODELO 2",
    x = "Valores Ajustados (Fitted Values)",
    y = "Residuales al Cuadrado (Squared Residuals)"
  )

bptest(mod2, studentize = F)

par(mfrow = c(2, 2))  # Configura la ventana gráfica para mostrar 4 gráficos

plot(mod2)

corrplot(cor(datos_sin_primera_fila |>
               select(QUSA,  PRECIO_GASOLINA_USA , PCANAD  , VAR_PIB, TC_CORREGIDO, IP_INDEX )), method= "number")
vif(mod2)



# Graficar residuos vs. tiempo
plot(residuales, type = "o", col = "blue", main = "Residuos vs. Tiempo MODELO 2",
     xlab = "Índice Temporal", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)
dwtest(mod2)
#######
#######
#######
#######
#######
# Valores arbitrarios para las variables exógenas
P_GASUSA <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA)  # Precio base GASUSA
PSUST <- mean(datos_sin_primera_fila$PCANAD)  # Sustitutos
IP_INDEX <- mean(datos_sin_primera_fila$IP_INDEX)  # Índice de poder adquisitivo
BRENT_ratio <- mean(datos_sin_primera_fila$INDEX_BRENT)  # BRENT_1 / BRENT_0
TIPO_CAMBIO_REAL_ratio <- mean(datos_sin_primera_fila$VAR_TC_COR)  # ΔTIPO_CAMBIO_REAL / TIPO_CAMBIO_REAL_0
ANUNCIO <- 0  # Variable anuncio (0 o 1)

mean(datos_sin_primera_fila$QUSA)
# Parámetros de la ecuación
constante <- -2149.822
coef_PBM <- -42.081
coef_PSUST <- 0.264
coef_IP_INDEX <- 33.080
coef_BRENT <- 444.980
coef_TIPO_CAMBIO <- -747.444
coef_ANUNCIO <- -215.687

# Función para calcular P_BM en función de Q
P_BM <- function(Q) {
  127.4117-0.077015*Q
}

# Valores de Q
Q_values <- seq(-1000, 2000, length.out = 500)

# Calcular P_BM
P_BM_values <- sapply(Q_values, P_BM)

interseccion_Q <- (17.80 - 127.4117) / -0.077015  # Resolviendo P_BM(Q) = 17.80
interseccion_P <- 17.80  # El precio es igual al costo marginal

# Graficar
plot(
  Q_values,
  P_BM_values,
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Cantidad Demandada (Q) de barriles diarios",
  ylab = "Precio internacional FOB",
  xlim = c(0, max(Q_values)),
  ylim = c(0, max(P_BM_values)),
  main = "Curva de Demanda Inversa"
  
)
abline(h = 0, col = "black", lty = 2)  # Línea horizontal en y = 0
abline(v = 0, col = "black", lty = 2)  # Línea vertical en x = 0
abline(h = 17.80, col = "green", lty = 2, lwd = 2)
grid()
points(datos_sin_primera_fila$QUSA, datos_sin_primera_fila$P, pch = 19, col = "red")
points(interseccion_Q, interseccion_P, pch = 19, col = "purple", cex = 1.5)

abline(v = interseccion_Q, col = "purple", lty = 3)  # Proyección en el eje x
abline(h = interseccion_P, col = "purple", lty = 3)  # Proyección en el eje y

text(interseccion_Q, 0, labels = paste("Q =", round(interseccion_Q, 2)), pos = 3, col = "purple")
text(0, interseccion_P, labels = paste("P =", round(interseccion_P, 2)), pos = 4, col = "purple")


legend("topright", 
       legend = c("Curva Estimada Inversa", "Datos Observados", "Costo Marginal ($17.80)"), 
       col = c("blue", "red", "green"), 
       lty = c(1, NA, 2), 
       pch = c(NA, 19, NA), 
       lwd = c(2, NA, 2))
####
##
####
### GRAFICA SITUACIÓN ACTUAL VS COMPETENCIA PERFECTA

mean(datos_sin_primera_fila$QUSA)
# Parámetros de la ecuación
constante <- -2149.822
coef_PBM <- -42.081
coef_PSUST <- 0.264
coef_IP_INDEX <- 33.080
coef_BRENT <- 444.980
coef_TIPO_CAMBIO <- -747.444
coef_ANUNCIO <- -215.687

# Función para calcular P_BM en función de Q
P_BM <- function(Q) {
  127.4117-0.077015*Q
}

# Valores de Q
Q_values <- seq(-1000, 2000, length.out = 500)

# Calcular P_BM
P_BM_values <- sapply(Q_values, P_BM)

interseccion_Q <- (17.80 - 127.4117) / -0.077015  # Resolviendo P_BM(Q) = 17.80
interseccion_P <- 17.80  # El precio es igual al costo marginal

# Graficar
plot(
  Q_values,
  P_BM_values,
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Cantidad Demandada (Q) en miles de barriles diarios",
  ylab = "Precio internacional FOB",
  xlim = c(0, max(Q_values)),
  ylim = c(0, max(P_BM_values)),
  main = "Curva de Demanda Inversa"
  
)
abline(h = 0, col = "black", lty = 1)  # Línea horizontal en y = 0
abline(v = 0, col = "black", lty = 1)  # Línea vertical en x = 0
abline(h = 17.80, col = "green", lty = 2, lwd = 2)
grid()

points(interseccion_Q, interseccion_P, pch = 19, col = "blue", cex = 1.5)
points(770, P_BM(770), pch = 19, col = "red", cex = 1.5)



abline(v = interseccion_Q, col = "black", lty = 3)  # Proyección en el eje x
abline(h = interseccion_P, col = "black", lty = 3)  # Proyección en el eje 

###PARA LA SIUTACIÓN ACTUAL

abline(v = 770, col = "black", lty = 3)  # Proyección en el eje x
abline(h = P_BM(770), col = "black", lty = 3) 


text(interseccion_Q, 0, labels = paste("Q =", round(interseccion_Q, 2)), pos = 3, col = "black")
text(0, interseccion_P, labels = paste("P =", round(interseccion_P, 2)), pos = 4, col = "black")

text(770, 0, labels = paste("Q =", round(770, 2)), pos = 3, col = "black")
text(0, P_BM(770), labels = paste("P =", round(P_BM(770), 2)), pos = 4, col = "black")

rect(
  xleft = 0, 
  xright = 770, 
  ybottom = 17.8, 
  ytop = P_BM(770), 
  col = rgb(0.1, 0.5, 0.8, 0.3),  # Color azul con transparencia
  border = NA
)

polygon(
  x = c(interseccion_Q, 770, 770), # Coordenadas x de los vértices
  y = c(17.80, 17.80, P_BM(770)), # Coordenadas y de los vértices
  col = rgb(0.8, 0.2, 0.2, 0.3),  # Color rojo con transparencia
  border = NA
)

polygon(
  x = c(0, 0, 770), # Coordenadas x de los vértices
  y = c(127.4117, P_BM(770), P_BM(770)), # Coordenadas y de los vértices
  col = rgb(0, 1, 0, 0.3),  # Color rojo con transparencia
  border = NA
)



legend("topright", 
       legend = c("Curva Estimada Inversa","Costo Marginal ($17.80)"), 
       col = c("blue", "green"), 
       lty = c(1, 2), 
       pch = c(NA, NA), 
       lwd = c(2, 2))
