#Instalamos los paquetes necesarios
install.packages("readxl")
install.packages("plotly")
install.packages("modeest")
install.packages("dplyr")
install.packages("qcc")

#Cargamos nuestros paquetes
library(readxl)
library(plotly)
library(modeest)
library(dplyr)
library(qcc)

#Usamos este código para encontrar la ruta de nuestro archivo de datos
file.choose()

#Realizamos la lectura del archivo
tienda1 <- read_excel("C:\\Users\\CAFRAL SSJ\\Desktop\\PROYECTO FINAL\\MOCK_DATA.xlsx")


#Procedemos a realizar Medidas de Tendencia Central

# Calculamos la media de los precios
media_precio <- mean(tienda1$Precio)
# Calculamos la mediana de los precios
mediana_precio <- median(tienda1$Precio)
# Calculamos la moda de los precios
moda_precio <- mlv(tienda1$Precio, method = "shorth")
# Calculamos la moda de los productos
moda_producto <- mlv(tienda1$Producto, method = "shorth")
# Calculamos la moda de los métodos de pago
moda_metodo_pago <- mlv(tienda1$Método_Pago, method = "shorth")

# Mostramos las medidas de tendencia central

media_precio
mediana_precio
moda_precio
moda_producto
moda_metodo_pago

#MEDIDAS DE DISPERCION SOBRE LOS PRECIOS

# Calculamos la varianza de los precios
varianza_precio <- var(tienda1$Precio)
# Calculamos la desviación estándar de los precios
desviacion_estandar_precio <- sd(tienda1$Precio)
# Calculamos el rango de los precios
rango_precio <- max(tienda1$Precio) - min(tienda1$Precio)

# Mostramos las medidas de dispersión

varianza_precio
desviacion_estandar_precio
rango_precio


# Calculamos el total de ventas por tienda
total_ventas <- tapply(tienda1$Precio, tienda1$Tienda, sum)

print(total_ventas)

# Creamos un gráfico de pastel para visualizar las ventas por tienda
plot_ly(
  type = "pie",
  labels = tienda1$Tienda,
  values = total_ventas,
  textinfo = "label+percent",
  insidetextorientation = "radial",
  marker = list(
    line = list(color = "white", width = 2)
    ),
  hoverinfo = "text",
  text = paste("Tienda:", tienda1$Tienda,
               "<br>Total de Ventas: $", total_ventas)
  ) %>%
  layout(
    title = list(
      text = "Ventas por tienda"
      )
    )


# Top 10 Productos Más Vendidos
conteo_productos <- table(tienda1$Producto)
top_10 <- sort(conteo_productos, decreasing = TRUE)[1:10]

#HISTOGRAMA DE LOS 10 PRODUCTOS MAS VENDIDOS
barplot(top_10, 
        col = c("blue","red","yellow","orange","green","lightblue",
                "lightgreen","pink","purple","skyblue"),
        border = "black",
        xlab = "",
        ylab = "Productos vendidos",
        main = "Top 10 Productos Más Vendidos",
        names.arg = names(top_10),
        las = 2,
        cex.lab = 1,
        cex.names = 1)

# Calcular frecuencias acumulativas
regiones <- table(tienda1$Región)
frecuencias_acum <- cumsum(sort(regiones, decreasing = TRUE))

# Crear el diagrama de Pareto de productos
pareto.chart(frecuencias_acum, col = c("skyblue","pink","orange","yellow"),
             main = "Ventas acumuladas por región",
             ylab = "Cantidad de ventas",
             xlab = "Regiones",
             ylab2 = "Porcentajes",
             cex.lab = 1.2,
)
