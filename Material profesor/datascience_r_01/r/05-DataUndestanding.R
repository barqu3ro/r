# ************************************************************************************
# DATA UNDERSTANDING
# ************************************************************************************

# ************************************************************************************
# Preparacion del entorno
# ************************************************************************************
rm(list=ls())
setwd('C:/Users/usuario/OneDrive - Universidad Complutense de Madrid (UCM)/_TODO/_PROYECTOS/Docentia/datascience_r_01/')

# ************************************************************************************
# Carga de paquetes
# ************************************************************************************
if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

if(!require('ggrepel')) install.packages('ggrepel')
library(ggrepel)

if(!require('Hmisc')) install.packages('Hmisc')
library(Hmisc)


# -----------------------------------------------------------------
# Exploración del contenido
# -----------------------------------------------------------------
class(iris)  # Clase del objeto
typeof(iris) # Tipo del objeto
str(iris)    # Información sobre su estructura
View(iris)

head(iris)  # Primeras filas 
tail(iris)  # Últimas filas

# Selección de filas y columnas: which
iris$Sepal.Length[which(iris$Species == 'versicolor')] 

# Subset
subset(iris, Species=='versicolor')

# Separar en lista de data.frames
iris.por.especie <- split(iris,iris$Species)
str(iris.por.especie)

# estadísticas descriptivas
summary(iris) 

# con Hmisc
describe(iris)

# -----------------------------------------------------------------
# Gráficos con R Base
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Nubes de puntos
# -----------------------------------------------------------------
plot(iris$Sepal.Length)

plot(iris$Sepal.Length, iris$Sepal.Width)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)

plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species,  
     xlab = 'Longitud del pétalo', ylab = 'Ancho del pétalo')
title(main = 'IRIS', 
      sub = 'Exploración de los pétalos según especie', 
      col.main = 'blue', col.sub = 'blue')
	  
# con leyenda color
mycolor <- c("red","green3","blue")[as.factor(iris$Species)]
plot(iris$Petal.Length, iris$Petal.Width, pch = 8, col = mycolor,
	main = "Edgar Anderson's Iris Data", xlab = "Petal Length", ylab = "Petal Width",
	xlim = c(0,10), ylim= c(0,10))
legend('topright', legend = unique(iris$Species), col = c("red","green3","blue"), pch = 8)

# -----------------------------------------------------------------
# Gráfica de cajas
# -----------------------------------------------------------------
boxplot(iris$Sepal.Length)

boxplot(iris$Petal.Length ~ iris$Species)
title(main = 'IRIS', ylab = 'Longitud pétalo', sub = 'Análisis de pétalo por familia')


# -----------------------------------------------------------------
# Histograma
# -----------------------------------------------------------------
hist(quakes$depth,prob=TRUE)
lines(density(quakes$depth,bw=50))


# -----------------------------------------------------------------
# Bar plot
# -----------------------------------------------------------------
ventas <- rnorm(12,1000,100)
names(ventas) <- month.abb

barplot(ventas)
barplot(ventas, main="ventas de vetusta", 
                xlab="eur",
				ylab="mes",
				col="green",
				horiz=T)
				

cod.cliente <- 1:100
edad.cliente <- as.integer(rnorm(100,55,15))
sexo.cliente <- c(rep('H',60),rep('M',40))
cliente <- data.frame(cod.cliente,edad.cliente,sexo.cliente, stringsAsFactors =T)
edades <- table(cliente$edad.cliente)
barplot(edades)

sexo.edad <- table(cliente$sexo,cliente$edad)
barplot(sexo.edad, col=c("red","blue"))
legend("topleft",c("Hombre","Mujer"),fill=c("red","blue"))

# -----------------------------------------------------------------
# Pie
# -----------------------------------------------------------------
sexo <- table(cliente$sexo)
pie(sexo, col=c("red","blue"))
legend("topleft",c("Hombre","Mujer"),fill=c("red","blue"))

# -----------------------------------------------------------------
# Stripchart
# -----------------------------------------------------------------
stripchart(airquality$Ozone)

stripchart(airquality[,c(2,4)])
stripchart(airquality[,c(2,4)], method="jitter")



# -----------------------------------------------------------------
# ggplot vs base
# -----------------------------------------------------------------

# leemos dataset
housing <- read.csv("in/landdata-states.csv")
str(housing)
head(housing)

# -----------------------------------------------------------------
# histogramas sobre Home.Value
# -----------------------------------------------------------------
# base
hist(housing$Home.Value)

# ggplot
ggplot(housing, aes(x = Home.Value)) +
    geom_histogram()

# con intervalos mas estrechos	
ggplot(housing, aes(x = Home.Value)) +
    geom_histogram(stat = "bin", binwidth=4000)	

# -----------------------------------------------------------------
# boxplot sobre Home.Value
# -----------------------------------------------------------------
	
# base
boxplot(housing$Home.Value)

# ggplot
ggplot(housing, aes(y = Home.Value)) +
    geom_boxplot()

# -----------------------------------------------------------------
# boxplot sobre Specie en iris
# -----------------------------------------------------------------
	
# base
boxplot(iris$Petal.Length ~ iris$Species)

# ggplot
ggplot(iris, aes(y = Petal.Length, x=Species)) +
    geom_boxplot()
		
	
# -----------------------------------------------------------------
# Evolución de Home.Value  para dos estados: DC FL
# -----------------------------------------------------------------
# base
plot(Home.Value ~ Date,
   data=subset(housing, State == "DC"))
points(Home.Value ~ Date, col="red",
	 data=subset(housing, State == "FL"))
legend(19750, 400000,
	 c("DC", "FL"), title="State",
	 col=c("black", "red"),
	 pch=c(1, 1))
	 
# ggplot
  ggplot(subset(housing, State %in% c("DC", "FL")),
         aes(x=Date,
             y=Home.Value,
             color=State))+
    geom_point()
	
# ayuda sobre geometrias
help.search("geom_", package = "ggplot2")	


# -----------------------------------------------------------------
# Scatterplot ggplot
# -----------------------------------------------------------------

# en q1 de 2001 como varía Structure.Cost vs Land.Value
hp2001Q1 <- subset(housing, Date == 20011) 
ggplot(hp2001Q1,
	 aes(y = Structure.Cost, x = Land.Value)) +
geom_point()

# predicción futura: regresion lineal
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ Land.Value, data = hp2001Q1))
str(hp2001Q1)

p1 <- ggplot(hp2001Q1, aes(x = Land.Value, y = Structure.Cost))

p1 + geom_point(aes(color = Home.Value)) +
	geom_line(aes(y = pred.SC))

# Smoothers
p1 +
	geom_point(aes(color = Home.Value)) +
	geom_smooth()

# añadir texto a los puntos con el estado
p1 + 
    geom_text(aes(label=State), size = 3)
	
# mejorar dicha visualizacion
p1 + 
	geom_point() + 
	geom_text_repel(aes(label=State), size = 3)

# geometria de puntos con color Home.Value y forma por region
p1 +
    geom_point(aes(color=Home.Value, shape = region))	
	
# tendencia de precios en cada estado
p5 <- ggplot(housing, aes(x = Date, y = Home.Value))

p5 + geom_line(aes(color = State)) 

# separando graficos por estado
p5 + geom_line() +
     facet_wrap(~State, ncol = 10)
	 
# temas
p5 + geom_line() +
  facet_wrap(~State, ncol = 10) +
  theme_linedraw()

p5 + geom_line() +
  facet_wrap(~State, ncol = 10) +
  theme_light()
