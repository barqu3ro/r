# Actividad 1 
# Jorge Barquero Villagra
# Herramientas para Data Science - Lenguaje R

# Borrar el workspace
rm(list = ls())

# seed ? verificar al final si es necesario
set.seed(123)
# Organizar los gráficos en una cuadrícula de 4x3
par(mfrow=c(2,2))  # Organizar gráficos en una cuadrícula


# activo el directorio de trabajo
setwd('/Users/jorgebarquero/gitrepos/r/actividad 1')

# cargar el objeto mtcars 
data(mtcars)

# 1. Seleccionar únicamente la columna hp del data frame
columna_hp <- mtcars$hp
print(columna_hp)

# 2. Seleccionar todas las columnas menos hp del data frame 
indice_hp <-  which(names(mtcars) == "hp")
columnas_sin_hp <- mtcars[, -indice_hp]
print (columnas_sin_hp)

# 3.	Crea un nuevo objeto mycars que contenga solo las 
# columnas mpg y hp pero renombradas como miles_per_galon y horse_power.
mycars <- mtcars[, c("mpg", "hp")]
names(mycars) <- c("miles_per_gallon","horse_power")
print(mycars)

# 5.	Añade una columna a mycars llamada km_per_litre (1 mpg= 0.425 km/l)
mycars$km_per_litre <- mycars$miles_per_gallon * 0.425
print(mycars)

# 6.	Crea un nuevo objeto mycars2 con las filas que cumplan que mpg está entre 20 y 100.
mycars2 <- mycars[mycars$miles_per_gallon >= 20 & mycars$miles_per_gallon <= 100,]
print(mycars2)


# 7.	Obtén el histograma de cada una de las columnas numéricas de mtcars


# for para recorrer las columnas del dataframe
for (col in names(mtcars)) {
  # Evaluar si la columna es numérica
  if (is.numeric(mtcars[[col]])) {
    # Histograma de columna
    hist(mtcars[[col]], main=paste("Histogram of ",col), xlab=col, col="skyblue", border="black")
  }
}

# 8. Obtén el boxplot de cada una de las columnas numéricas de mtcars
for (col in names(mtcars)) {
  # Evaluar si la columna es numérica
  if (is.numeric(mtcars[[col]])) {
    boxplot(mtcars[[col]], main=paste("Boxplot of", col), col="skyblue", border="black")
  }
}

# 9.	Escribe y prueba una función llamada km.to.miles que una velocidad en km/h 
# y la convierta en millas/h. Úsala para convertir los datos del siguiente vector 
# v (expresado en millas) a km y almacénalo en otro vector k.
# v <- c(90,70,50,71,53)

# Definir la función 
km.to.miles <- function(velocidad_kmh) {
  velocidad_mph <- velocidad_kmh * 1.60934
  return(velocidad_mph)
}

v <- c(90,70,50,71,53)
k <- km.to.miles(v)

print ("Vector original")
print (v)
print ("Vector con función aplicada")
print (k)

# 10. 10.	Genera un objeto mycars3 que tenga todas las filas y columnas de 
# mtcars y 2 nuevas filas con modelos de coches nuevos (puede ser inventados)

# Crear dos nuevas filas para modelos de coches (Tesla Model S y Tesla Cybertruck)
nuevos_modelos <- data.frame(
  mpg = c(80, 15),
  cyl = c(0, 12),
  disp = c(200, 600),
  hp = c(500, 700),
  drat = c(4.0, 3.5),
  wt = c(2.2, 3.0),
  qsec = c(12, 14),
  vs = c(1, 0),
  am = c(1, 0),
  gear = c(6, 6),
  carb = c(2, 4),
  row.names = c("Tesla_Model_S", "Tesla_Cybertruck")  # Cambié el nombre del Ferrari a "Tesla_Cybertruck"
)

# Crear el objeto mycars3 uniendo mtcars con los nuevos modelos
mycars3 <- rbind(mtcars, nuevos_modelos)

# Imprimir el nuevo objeto mycars3
print(mycars3)


