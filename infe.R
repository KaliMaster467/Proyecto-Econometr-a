####GRAFICAS INFERENCIA 

##PRECIO CANADA 
# Calcular el promedio y estimación por intervalos
# Cargar la biblioteca adicional
library(patchwork)

# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$PCANAD, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$PCANAD, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$PCANAD)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = PCANAD, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Precio histórico PFOB del petróleo canadiense 2020-2024",
    x = "MES",
    y = "Precio FOB del petróleo canadiense"
  ) +
  annotate("text", x = 10, y = promedio + 100, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = PCANAD)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Precio FOB del Petróleo Canadiense",
    x = NULL,
    y = "Precio FOB del petróleo canadiense"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)


##### TIPO CABMIO REAL
# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$TC_CORREGIDO, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$TC_CORREGIDO, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$TC_CORREGIDO)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = TC_CORREGIDO, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Tipo de cambio real EUA-MEX 2020-2024",
    x = "MES",
    y = "Tipo de cambio real"
  ) +
  annotate("text", x = 10, y =.027, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = TC_CORREGIDO)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Tipo de cambio real",
    x = NULL,
    y = "Tipo de cambio real"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)

###VAR TIPO DE CAMBIO TRIMESTRAL

# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$VAR_PIB, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$VAR_PIB, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$VAR_PIB)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = VAR_PIB, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Variación del PIB histórico Trimestral 2020-2024",
    x = "MES",
    y = "Variación del PIB"
  ) +
  annotate("text", x = 15, y = .2, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = VAR_PIB)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Variación del PIB Trimestral",
    x = NULL,
    y = "Variación del PIB"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)


###PRECIO GASOLINA USA 

# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$PRECIO_GASOLINA_USA, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$PRECIO_GASOLINA_USA)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = PRECIO_GASOLINA_USA, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Precio histórico de la gasolina en USA 2020-2024",
    x = "MES",
    y = "Precio de la gasolina en USA"
  ) +
  annotate("text", x = 10, y = 4, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = PRECIO_GASOLINA_USA)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Precio de la gasolina en USA",
    x = NULL,
    y = "Precio de la gasolina en USA"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)

###INDICE DESARROLLO INDUSTRIAL 

# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$IP_INDEX, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$IP_INDEX, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$IP_INDEX)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = IP_INDEX, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Índice de Producción Industrial (IP Index) 2020-2024",
    x = "MES",
    y = "Índice de Producción Industrial"
  ) +
  annotate("text", x = 10, y = promedio + 5, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = IP_INDEX)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Índice de Producción Industrial",
    x = NULL,
    y = "Índice de Producción Industrial"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)

###P

# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$P, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$P, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$P)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = P, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Histórico del precio del barril mexicano 2020-2024",
    x = "MES",
    y = "Precio FOB"
  ) +
  annotate("text", x = 10, y = promedio  + 7, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = P)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot Precio del barril mexicano",
    x = NULL,
    y = "Precio FOB"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)

##GRAFICA DUAL

# Convertir MES a formato Date especificando el formato actual
datos_sin_primera_fila$MES <- as.Date(datos_sin_primera_fila$MES, format = "%d/%m/%Y")

# Verificar si todas las conversiones fueron exitosas
if (any(is.na(datos_sin_primera_fila$MES))) {
  stop("Algunas fechas no se pudieron convertir. Verifica el formato y los datos.")
}

# Continuar con el gráfico una vez que MES esté en el formato correcto
datos_sin_primera_fila <- datos_sin_primera_fila[order(datos_sin_primera_fila$MES), ]

# Calcular estadísticas y proceder con el resto del análisis
promedio_p <- mean(datos_sin_primera_fila$P, na.rm = TRUE)
promedio_gas <- mean(datos_sin_primera_fila$PRECIO_GASOLINA_USA, na.rm = TRUE)
factor_escala <- max(datos_sin_primera_fila$P, na.rm = TRUE) / max(datos_sin_primera_fila$PRECIO_GASOLINA_USA, na.rm = TRUE)
datos_sin_primera_fila$PRECIO_GASOLINA_USA_ESCALADO <- datos_sin_primera_fila$PRECIO_GASOLINA_USA * factor_escala

grafica_dual <- ggplot(datos_sin_primera_fila, aes(x = MES)) +
  geom_line(aes(y = P, color = "Variable P", group = 1), size = 1) +
  geom_line(aes(y = PRECIO_GASOLINA_USA_ESCALADO, color = "Precio Gasolina USA", group = 1), size = 1) +
  scale_y_continuous(
    name = "Variable P",
    sec.axis = sec_axis(~ . / factor_escala, name = "Precio Gasolina USA")
  ) +
  labs(title = "Histórico de P y Precio Gasolina USA", x = "MES") +
  scale_color_manual(name = "Leyenda", values = c("Variable P" = "blue", "Precio Gasolina USA" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), legend.position = "top")

print(grafica_dual)


# GRAFICA VAARIACION TIPO DE CAMBIO REAL
# Calcular estadísticas necesarias
promedio <- mean(datos_sin_primera_fila$VAR_TC_COR, na.rm = TRUE)
desviacion <- sd(datos_sin_primera_fila$VAR_TC_COR, na.rm = TRUE)
n <- sum(!is.na(datos_sin_primera_fila$VAR_TC_COR)) # Cantidad de datos no NA
z <- 1.96 # Valor crítico para un intervalo del 95%

# Cálculo del intervalo de confianza
limite_inferior <- promedio - z * (desviacion / sqrt(n))
limite_superior <- promedio + z * (desviacion / sqrt(n))

# Crear gráfica de línea con promedio
grafica_linea <- ggplot(datos_sin_primera_fila, aes(x = MES, y = VAR_TC_COR, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = promedio, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite inferior
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "orange", size = 1) + # Línea del límite superior
  labs(
    title = "Histórico tasa de cambio del tipo de cambio real mes a mes 2020-2024",
    x = "MES",
    y = "Precio FOB"
  ) +
  annotate("text", x = 14, y = promedio  + .06, 
           label = paste("Promedio:", round(promedio, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# Gráfica de boxplot
grafica_boxplot <- ggplot(datos_sin_primera_fila, aes(y = VAR_TC_COR)) +
  geom_boxplot(color = "black", fill = "gray80", outlier.color = "red", width = 0.3) +
  labs(
    title = "Boxplot tasa de cambio del tipo de cambio real mes a mes",
    x = NULL,
    y = "Precio FOB"
  ) +
  theme_minimal()

# Combinar ambas gráficas
library(patchwork)
grafica_comb <- grafica_linea / grafica_boxplot
print(grafica_comb)

# Crear la tabla con las estadísticas
tabla_estadisticas <- data.frame(
  "Indicador" = c("Promedio", "Límite Inferior (95%)", "Límite Superior (95%)"),
  "Valor" = c(round(promedio, 2), round(limite_inferior, 2), round(limite_superior, 2))
)

# Mostrar la tabla como resultado
print(tabla_estadisticas)

