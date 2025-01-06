#OUTLIERS
library(haven)


# URL "raw" del archivo .sav (REEMPLAZA CON TU URL REAL)
url_sav <- "https://raw.githubusercontent.com/NahuelDuhalde/Deteccion-de-Outliers/main/BASE%20AFE.sav"
BASE_AFE <- read_sav(url_sav)


X <- 
  BASE_AFE[,c(
     "NCC_1_U" , "NCC_3_U" , "NCC_5_U" , "NCC_7_U" , "NCC_9_U" , "NCC_11_U" ,"NCC_13_U"
    
  )]


dim (X)

na.omit(X)

dim(X)

#Comprobar Normalidad
library(MVN)
result.MVN <-mvn(data = X, 
                 mvnTest = "mardia", univariateTest = "SW")
result.MVN
result.MVN$multivariateNormality

psych::describe(X)


#EJEMPLO 1

#Puntaje Z y Bloxpot
set.seed(123)
X <- rowSums(BASE_AFE[,c(
  "NCC_1_U" , "NCC_3_U" , "NCC_5_U" , "NCC_7_U" , "NCC_9_U" , "NCC_11_U" ,"NCC_13_U"
  
)])

# Función para detectar outliers usando boxplot y puntuaciones Z (versión mejorada)
detectar_outliers <- function(X) {
  # Convertir a numérico si es haven_labelled
  if (inherits(X, "haven_labelled")) {
    datos_numericos <- as.numeric(X)
    warning("Los datos eran de clase 'haven_labelled'. Se han convertido a numéricos para el análisis.")
  } else {
    datos_numericos <- X
  }
  
  # 1. Boxplot
  boxplot(datos_numericos, main = "Boxplot con detección de Outliers", ylab = "Valores")
  limite_inferior <- boxplot.stats(datos_numericos)$stats[1]
  limite_superior <- boxplot.stats(datos_numericos)$stats[5]
  
  # 2. Puntuaciones Z
  media <- mean(datos_numericos, na.rm = TRUE)
  desviacion_estandar <- sd(datos_numericos, na.rm = TRUE)
  puntuaciones_z <- (datos_numericos - media) / desviacion_estandar
  
  # 3. Detección de outliers basada en puntuaciones Z
  outliers_z <- abs(puntuaciones_z) > 3
  
  # 4. Combinando ambos criterios
  outliers_combinado <- (datos_numericos < limite_inferior | datos_numericos > limite_superior) | outliers_z
  
  # Obtener los índices de los outliers
  indices_boxplot <- which(datos_numericos < limite_inferior | datos_numericos > limite_superior)
  indices_z <- which(outliers_z)
  indices_combinado <- which(outliers_combinado)
  
  # Imprimir resultados (con índices)
  cat("Resumen de datos:\n")
  print(summary(datos_numericos))
  cat("\nLímite inferior del Boxplot:", limite_inferior, "\n")
  cat("Límite superior del Boxplot:", limite_superior, "\n")
  
  cat("\nValores atípicos detectados por Boxplot:\n")
  if (length(indices_boxplot) > 0) {
    print(data.frame(Valor = datos_numericos[indices_boxplot], Indice = indices_boxplot))
  } else {
    cat("No se encontraron outliers por Boxplot.\n")
  }
  
  cat("\nPuntuaciones Z:\n")
  print(puntuaciones_z)
  
  cat("\nValores atípicos detectados por Puntuación Z (usando |Z| > 3):\n")
  if (length(indices_z) > 0) {
    print(data.frame(Valor = datos_numericos[indices_z], Indice = indices_z))
  } else {
    cat("No se encontraron outliers por Puntuación Z.\n")
  }
  
  cat("\nValores atípicos detectados combinando ambos métodos:\n")
  if (length(indices_combinado) > 0) {
    print(data.frame(Valor = datos_numericos[indices_combinado], Indice = indices_combinado))
  } else {
    cat("No se encontraron outliers combinando ambos métodos.\n")
  }
  
  resultados <- list(
    limite_inferior_boxplot = limite_inferior,
    limite_superior_boxplot = limite_superior,
    outliers_boxplot = data.frame(Valor = datos_numericos[indices_boxplot], Indice = indices_boxplot),
    puntuaciones_z = puntuaciones_z,
    outliers_z = data.frame(Valor = datos_numericos[indices_z], Indice = indices_z),
    outliers_combinado = data.frame(Valor = datos_numericos[indices_combinado], Indice = indices_combinado)
  )
  return(invisible(resultados))
}


#EJEMPLO 2
#MCD

library(devtools)
install_github("mdelacre/Routliers")
library(Routliers)
NCC_U <- rowMeans(BASE_AFE[, c("NCC_1_U", "NCC_3_U", "NCC_5_U", "NCC_7_U", "NCC_9_U", "NCC_11_U", "NCC_13_U"), drop = FALSE], na.rm = TRUE)

res1 <- outliers_mad(x = NCC_U)
res1
res1$outliers_pos
outliers_index_NCC_U <- res1$outliers_pos
MFQ_sin_outliers_NCC_U <- MFQ_sin_outliers_c[-outliers_index1_NCC_U, ]

NCC_P <- rowMeans(
  BASE_AFE[,c(
     "NCC_2_P", "NCC_4_P", "NCC_6_P", "NCC_8_P", 
     "NCC_10_P", "NCC_12_P", "NCC_14_P"
  )]
)
res2 <- outliers_mad(x = NCC_P)
res2
res2$outliers_pos
outliers_index_NCC_P <- res2$outliers_pos
MFQ_sin_outliers_NCC_U_ni_NCC_P <- MFQ_sin_outliers_c[-outliers_index_NCC_P, ]

#EJEMPLO 3 

X<-  BASE_AFE[,c(
  "NCC_2_P", "NCC_4_P", "NCC_6_P", "NCC_8_P", 
  "NCC_10_P", "NCC_12_P", "NCC_14_P"
)]

#Distancia de Mahalanobis
detectar_outliers_mahalanobis <- function(X, alpha = 0.05, na.rm = TRUE) {
  # Manejo de NAs (opcional)
  if(na.rm){
    X <- na.omit(X)
    if(nrow(X) == 0){
      stop("Todos los datos son NA")
    }
    warning("Se han eliminado filas con NA para calcular la distancia de Mahalanobis")
  } else if(any(is.na(X))){
    stop("Hay NA en los datos. Para continuar, o elimine las filas con NA o ponga na.rm = TRUE")
  }
  
  # 1. Calcular la Distancia de Mahalanobis
  tryCatch({
    distancias_mahalanobis <- mahalanobis(X, colMeans(X), cov(X))
  }, error = function(e){
    stop(paste("Error al calcular la distancia de Mahalanobis:", e))
  })
  
  # 2. Determinar el umbral (usando la distribución Chi-cuadrado)
  alpha <- 0.05
  umbral <- qchisq(1 - alpha, df = ncol(X))
  
  # 3. Identificar los outliers
  outliers <- distancias_mahalanobis > umbral
  indices_outliers <- which(outliers)  # Indices de las filas con outliers
  
  # Imprimir resumen (opcional, se puede comentar)
  cat("Umbral de Chi-cuadrado (alpha =", alpha, "):", umbral, "\n")
  cat("Número de outliers detectados:", sum(outliers), "\n")
  
  # Resultados
  resultados <- list(
    distancias_mahalanobis = distancias_mahalanobis,
    umbral = umbral,
    outliers = outliers,
    indices_outliers = indices_outliers,  # Añadir indices de outliers
    X_sin_na = X # Incluir los datos sin NA en el resultado
  )
  return(invisible(resultados))
}

#Detectar filas
print(resultados$indices_outliers)

# Detectar outliers
resultados <- detectar_outliers_mahalanobis(X, alpha = 0.05)

# Mostrar resultados
print(resultados$distancias_mahalanobis)
print(resultados$outliers)

# Visualizar las distancias de Mahalanobis
plot(1:nrow(X), resultados$distancias_mahalanobis,
     main = "Distancias de Mahalanobis y Umbral de Outliers",
     xlab = "Índice de la Observación",
     ylab = "Distancia de Mahalanobis",
     pch = 16, # Forma de los puntos
     col = "blue") # Color de los puntos

abline(h = resultados$umbral, col = "red", lwd = 2) # Línea del umbral más gruesa
legend("topleft", # Posición de la leyenda
       legend = paste("Umbral (α =", 0.05, ")"),
       col = "red", lwd = 2)

#Resaltar los outliers
points(which(resultados$outliers), resultados$distancias_mahalanobis[resultados$outliers], col="red", pch=17, cex=1.5)
legend("topright", legend="Outliers", col="red", pch=17)



#EJEMPLO 4
#MCD 
#Metodo MCD para outliers multivariados
set.seed(1234)

resMCD<- outliers_mcd(x = cbind (NCC_U, NCC_P), h = 0.75)
resMCD
outlier_index_MCD<- resMCD$outliers_pos
X_sin_outliers <- X[-outlier_index_MCD, ]

