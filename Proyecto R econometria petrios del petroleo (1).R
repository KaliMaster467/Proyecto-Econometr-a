data <- read.csv("ruta/ECONOMETRIA.csv")
library(readr)
ECONOMETRIA <- read_csv("ECONOMETRIA.csv")
View(ECONOMETRIA)
data <- read.csv("ruta/ECONOMETRIA.csv")
getwd()
data <- datos_sin_primera_fila
summary(data)

boxplot(data$BRENT, main="Distribución de BRENT", ylab="Precio")
 

hist(data$INPC, main="Distribución del INPC", xlab="INPC", col="lightblue", breaks=10)

t.test(data$BRENT, mu=50)

1. Relación entre el tipo de cambio y los precios del petróleo
Hipótesis nula (H₀): No hay correlación entre el tipo de cambio (MXN/USD) y el precio del petróleo (BRENT o WTI).
Hipótesis alternativa (H₁): Existe una correlación significativa entre el tipo de cambio y el precio del petróleo.
cor.test(data$MXN_USD, data$BRENT, use="complete.obs")
cor.test(data$MXN_USD, data$WTI, use="complete.obs")
RESULTADOS
Valor de la correlación:
  
  La correlación entre el tipo de cambio (MXN/USD) y el precio del petróleo (WTI) es -0.538.
Esto indica una relación negativa moderada: cuando el precio del WTI sube, el tipo de cambio MXN/USD tiende a bajar (es decir, el peso mexicano se fortalece frente al dólar).
Significancia estadística (p-value):
  
  El p-valor es 0.00001902, que es muchísimo menor que 0.05 (el nivel típico de significancia).
Esto significa que la correlación observada no es producto del azar. En otras palabras, hay evidencia estadística fuerte para decir que la relación entre estas variables es real.
Intervalo de confianza (95%):
  
  La correlación verdadera en la población está entre -0.702 y -0.320.
Esto reafirma que la relación es negativa, aunque la fuerza exacta puede variar dentro de ese rango.



2. Diferencia en precios antes y durante la pandemia
Hipótesis nula (H₀): El precio promedio del petróleo (BRENT) antes de la pandemia es igual al precio durante la pandemia.
Hipótesis alternativa (H₁): Hay una diferencia significativa en el precio promedio antes y durante la pandemia.

t.test(data$BRENT[data$PANDEMI == 0], data$BRENT[data$PANDEMI == 1])
RESULTADOS:
Valor de la prueba t:
  
  La estadística t es 2.478, lo que indica que hay una diferencia observable entre los precios promedio del petróleo (BRENT) antes y durante la pandemia.
Valores promedio:
  
  Antes de la pandemia (PANDEMI = 0): El precio promedio del BRENT fue 83.03 USD.
Durante la pandemia (PANDEMI = 1): El precio promedio del BRENT fue 72.46 USD.
Esto sugiere que los precios bajaron durante la pandemia.
Significancia estadística (p-value):
  
  El p-valor es 0.01689, menor que 0.05 (nivel típico de significancia).
Esto significa que hay evidencia estadística significativa para rechazar la hipótesis nula. Es decir, los precios promedio de BRENT antes y durante la pandemia son diferentes.
Intervalo de confianza (95%):
  
  La diferencia verdadera entre los promedios está entre 1.99 USD y 19.15 USD.
Esto confirma que los precios antes de la pandemia fueron, en promedio, mayores





3. Relación entre inflación y tipo de cambio real
Hipótesis nula (H₀): No existe una relación significativa entre el índice de precios al consumidor (INPC) y el tipo de cambio real (TIPO_CAMBIO_REAL).
Hipótesis alternativa (H₁): Existe una relación significativa entre INPC y TIPO_CAMBIO_REAL.
cor.test(data$INPC, data$TIPO_CAMBIO_REAL, use="complete.obs")
RESULTADOS:
  Valor de la correlación:
  
  La correlación entre el índice de precios al consumidor (INPC) y el tipo de cambio real (TIPO_CAMBIO_REAL) es -0.075.
Este valor indica una relación negativa extremadamente débil (prácticamente inexistente). No hay evidencia de que un aumento o disminución en el INPC esté relacionado con el tipo de cambio real.
Significancia estadística (p-value):
  
  El p-valor es 0.5836, mucho mayor que 0.05.
Esto significa que no hay suficiente evidencia estadística para concluir que la correlación observada es diferente de 0. La relación es muy probablemente producto del azar.
Intervalo de confianza (95%):
  
  La correlación verdadera en la población podría estar entre -0.331 y 0.192.
Esto refuerza la idea de que no existe una relación fuerte ni significativa entre estas dos variables.


4. Impacto del anuncio en el tipo de cambio
Hipótesis nula (H₀): Los anuncios (ANUNCIO) no tienen impacto en el tipo de cambio (TIPO_CAMBIO_REAL).
Hipótesis alternativa (H₁): Los anuncios tienen un impacto significativo en el tipo de cambio.
t.test(data$TIPO_CAMBIO_REAL[data$ANUNCIO == 0], data$TIPO_CAMBIO_REAL[data$ANUNCIO == 1])
RESULTADOS:
Valores promedio:
  
  Sin anuncio (ANUNCIO = 0): El tipo de cambio real promedio fue 0.3902.
Con anuncio (ANUNCIO = 1): El tipo de cambio real promedio fue 0.2786.
Esto sugiere que el tipo de cambio real es más bajo cuando hay anuncios.
Diferencia significativa (p-value):
  
  El p-valor es 1.777e-05, muchísimo menor que 0.05.
Esto indica que hay evidencia estadística muy fuerte para rechazar la hipótesis nula. Es decir, hay una diferencia significativa en el tipo de cambio real dependiendo de la presencia de un anuncio.
Intervalo de confianza (95%):
  
  La diferencia promedio entre ambos grupos (sin anuncio y con anuncio) está entre 0.067 y 0.156.
Esto significa que el tipo de cambio real sin anuncios es consistentemente más alto que con anuncios dentro de este rango.
Estadística t:
  
  El valor t de 5.091 indica que la diferencia observada entre las medias de los dos grupos es bastante grande en relación con la variabilidad de los datos.
Estadística t:

  Conclusión:
  El tipo de cambio real es significativamente mayor en promedio cuando no hay anuncios. Esto podría indicar que los anuncios tienen algún impacto (directo o indirecto) que reduce el tipo de cambio real

5. Diferencia en varianza de precios entre Brent y WTI
Hipótesis nula (H₀): Las varianzas de los precios de BRENT y WTI son iguales.
Hipótesis alternativa (H₁): Las varianzas de BRENT y WTI son diferentes.

var.test(data$BRENT, data$WTI)
RESULTADOS.
Razón de varianzas:
  
  La razón de varianzas (F) entre BRENT y WTI es 1.129.
Esto significa que la varianza de BRENT es aproximadamente 1.13 veces la varianza de WTI. Sin embargo, esta diferencia es pequeña.
Significancia estadística (p-value):
  
  El p-valor es 0.6504, mucho mayor que 0.05.
Esto indica que no hay suficiente evidencia para rechazar la hipótesis nula de que las varianzas son iguales. Es decir, las varianzas de BRENT y WTI son estadísticamente similares.
Intervalo de confianza (95%):
  
  El intervalo de confianza para la razón de varianzas está entre 0.665 y 1.917.
Esto incluye el valor 1, lo que refuerza que no hay una diferencia estadísticamente significativa entre las varianzas.
Conclusión de la prueba F:
  
  No se encontraron diferencias significativas en la dispersión (varianza) de los precios de BRENT y WTI.
Conclusión final:
  Las varianzas de los precios de BRENT y WTI son estadísticamente similares. Esto sugiere que la volatilidad de ambos mercados tiende a ser comparable en este conjunto de datos.

6. Tendencia de los precios del petróleo
Hipótesis nula (H₀): Los precios de BRENT no muestran una tendencia significativa a lo largo del tiempo.
Hipótesis alternativa (H₁): Los precios de BRENT muestran una tendencia significativa a lo largo del tiempo.

time <- 1:nrow(data)
lm_brent <- lm(data$BRENT ~ time)
summary(lm_brent)
1. Ecuación del modelo
El modelo ajustado es:
  
  BRENT
=
  51.8764
+
  0.8141
⋅
time
BRENT=51.8764+0.8141⋅time
Esto significa:
  
  El precio inicial estimado de BRENT (cuando el tiempo es 0) es 51.88 USD.
Por cada unidad de tiempo (podría ser días, semanas, o cualquier medida usada), el precio de BRENT aumenta en promedio 0.8141 USD.
2. Coeficientes del modelo
Intercepto (51.8764): Representa el valor estimado del precio de BRENT al inicio de la serie temporal.
Pendiente (0.8141): Muestra que hay una tendencia positiva significativa en el tiempo, con un p-valor de 3.98e-07 (muy menor a 0.05). Esto significa que el aumento en los precios del BRENT a lo largo del tiempo no es producto del azar.
3. Evaluación del modelo
Residuales:
  Los valores residuales (errores de predicción) están distribuidos entre -35.939 y 47.225, con una desviación estándar residual de 17.57 USD.
R-cuadrado (R²):
  El valor de 0.3759 indica que el modelo explica aproximadamente el 37.6% de la variabilidad en los precios del BRENT.
Aunque hay una tendencia significativa, otros factores (62.4%) también afectan los precios.
4. Prueba de significancia del modelo (F-statistic)
La estadística F de 33.12 y su p-valor de 3.984e-07 confirman que el modelo es estadísticamente significativo. Esto implica que el tiempo tiene un impacto notable en los precios del BRENT.
Conclusión
El precio del petróleo BRENT muestra una tendencia creciente significativa en el tiempo, aunque el modelo no explica completamente la variabilidad de los precios. Esto sugiere que otros factores adicionales, como oferta, demanda, o eventos económicos, podrían estar influyendo


7. Relación entre la inflación en EE.UU. y el precio de la gasolina
Hipótesis nula (H₀): No existe relación significativa entre la inflación en EE.UU. (CPIAUCSL) y el precio de la gasolina en EE.UU. (PRECIO_GASOLINA_USA).
Hipótesis alternativa (H₁): Existe una relación significativa entre CPIAUCSL y PRECIO_GASOLINA_USA.

cor.test(data$CPIAUCSL, data$PRECIO_GASOLINA_USA, use="complete.obs")
RESULTADOS:
 1. Valor de la correlación:
  El valor de la correlación es 0.7163, lo que indica una relación positiva moderada entre el índice de precios al consumidor (CPI) y el precio de la gasolina en EE. UU. Esto sugiere que cuando el CPI aumenta, los precios de la gasolina tienden a aumentar también.
2. Significancia estadística (p-value):
  El p-valor es 5.405e-10, mucho menor que el umbral estándar de 0.05.
Esto significa que hay evidencia estadística muy fuerte para rechazar la hipótesis nula (que dice que no hay relación). En otras palabras, es muy probable que la relación observada entre el CPI y los precios de la gasolina no sea producto del azar.
3. Intervalo de confianza (95%):
  El intervalo de confianza de la correlación está entre 0.5586 y 0.8240.
Esto sugiere que la correlación entre ambas variables es positiva, y que la relación es confiable dentro de este rango.
Conclusión:
  Hay una relación positiva moderada y significativa entre el índice de precios al consumidor en EE. UU. y el precio de la gasolina. Esto podría reflejar que los aumentos en la inflación general (CPI) tienden a estar asociados con aumentos en los precios de la gasolina, posiblemente debido a factores de costos más generales como el transporte y la producción de combustible.

