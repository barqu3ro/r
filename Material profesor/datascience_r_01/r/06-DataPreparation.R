# ************************************************************************************
# PREPARACION BASICA DE DATOS
# ************************************************************************************

# ************************************************************************************
# Preparacion del entorno
# ************************************************************************************
rm(list=ls())
setwd('C:/Users/usuario/OneDrive - Universidad Complutense de Madrid (UCM)/_TODO/_PROYECTOS/Docentia/datascience_r_01/')

# -----------------------------------------------------------------
# Introducción tratamiento strings
# -----------------------------------------------------------------
nombre <- "Juan"
apellido1 <- "Perez"
apellido2 <- "Rodriguez"

paste(nombre,apellido1,apellido2)
paste(nombre,apellido1,apellido2, sep = "-")
paste(nombre,apellido1,apellido2, sep = "")
paste0(nombre,apellido1,apellido2)
nombre_cliente <- paste0(apellido1," ",apellido2, " ", nombre)

paste(nombre,1:10)
paste(nombre,1:10, sep="-", collapse = "")

toupper(nombre_cliente)
tolower(nombre_cliente)

substr(nombre_cliente, 7, 15)
substr(nombre_cliente, 17, nchar(nombre_cliente))

strsplit(nombre_cliente," ")

# -----------------------------------------------------------------
# Tratar missing values
# -----------------------------------------------------------------

# generamos un vector con las horas trabajadas por un empleado que tiene NA
horas_trabajadas <- as.integer(runif(52,1,10)) # Número de horas trabajadas semanalmente en una encuesta
indices <- as.integer(runif(5,1,52))  # Sin respuesta 5 casos
horas_trabajadas[indices] <- NA

# comprobamos el efecto que provocan los NA en la gestión del vector
horas_trabajadas
horas_trabajadas > 5 # Los valores NA no pueden ser comparados
horas_trabajadas + 1 # Ni se puede operar con ellos
mean(horas_trabajadas)

is.na(horas_trabajadas)
summary(horas_trabajadas)

# gestión básica de NA

# aviso
if (any(is.na(horas_trabajadas)))
  message("existen semanas sin horas trabajadas")

# warning
if (any(is.na(horas_trabajadas)))
  warning("existen semanas sin horas trabajadas")

warnings()

# error
if (any(is.na(horas_trabajadas)))
  stop("existen semanas sin horas trabajadas")

na.fail(horas_trabajadas)

# generar vector sin NA
horas_trabajadas2 <- na.omit(horas_trabajadas)

horas_trabajadas2 > 5 
horas_trabajadas2 + 1
mean(horas_trabajadas2)

# imputar la media a los missing values
promedio <- mean(horas_trabajadas, na.rm = TRUE)
promedio
horas_trabajadas[is.na(horas_trabajadas)] <- promedio
mean(horas_trabajadas)

# Manejo de data.frames
nrow(airquality)
nrow(na.omit(airquality))
nrow(airquality[complete.cases(airquality),])

# las funciones suele llevar opciones para gestión de los NA
lm(Solar.R ~ Temp, airquality, na.action=na.omit)

# -----------------------------------------------------------------
# Ordenar 
# -----------------------------------------------------------------

# dataset de ejemplo mtcars 
data(mtcars)

# Ordenar un vector
sort(mtcars$mpg)

# Obtenemos un vector de indices 
order(mtcars$mpg)

# Ordenamos por la variable mpg 
newdata <- mtcars[order(mtcars$mpg),]

# Ordenamos por la variables mpg y cyl
newdata <- mtcars[order(mtcars$mpg, mtcars$cyl),]

# Ordenamos por la variable mpg (ascendente) y cyl (descendente)
newdata <- mtcars[order(mtcars$mpg, -mtcars$cyl),]
 

# -----------------------------------------------------------------
# Agregar 
# -----------------------------------------------------------------

# agregamos el data frame mtcars por las columnas cyl y vs,
# devolviendo la media y eliminando los NA 
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl, vs), FUN=mean, na.rm=TRUE)


# -----------------------------------------------------------------
# Join
# -----------------------------------------------------------------
clientes <- read.csv('in/DP-clients.csv',header=T,sep='\t')
tx_tarjetas <- read.csv('in/DP-Transactions OUT.txt',header=T,sep=',')
tx_clientes<-merge(tx_tarjetas,clientes,by="CardID") # inner join
nrow(tx_tarjetas)
nrow(tx_clientes)

tx_clientes2<-merge(tx_tarjetas,clientes,by="CardID",all.x=T) # left outer join
nrow(tx_clientes2)

tx_sin_cliente <- tx_clientes2[!complete.cases(tx_clientes2),]

# -----------------------------------------------------------------
# Concatenar filas y columnas
# -----------------------------------------------------------------
#Ejemplo usando el paro registrado por municipio
#http://datos.gob.es/es/catalogo/e00142804-paro-registrado-por-municipios

paro2018<-read.table("https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_2018_csv.csv", 
                     skip=1,header = TRUE, sep=";", quote = "\"", fileEncoding="latin1")
paro2017<-read.table("https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_2017_csv.csv", 
                     skip=1,header = TRUE, sep=";", quote = "\"", fileEncoding="latin1")
paro2016<-read.table("https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_2016_csv.csv", 
                     skip=1,header = TRUE, sep=";", quote = "\"", fileEncoding="latin1")
paro<-rbind(paro2016,paro2017,paro2018)

fecha <- paste0(paro[,1],"01")
fecha <- as.Date(fecha ,"%Y%m%d")
fecha_desc <- format(fecha, "%y %a %b")

paro <- cbind(fecha, fecha_desc, paro[, c(-1,-2)])


# -----------------------------------------------------------------
# Filtrar Proyectar
# -----------------------------------------------------------------
df <- data.frame(Nombre=c("Pedro","María","José","Marta"), 
                 Edad=c(27,34,40,39),
                 Poblacion=c("Zaragoza","Madrid","Valencia",
                             "Barcelona"), 
                 Sexo=c("H","M","H","M"),
                 Casado=c(F,T,T,F))
View(df)
df[4,1]
df$Nombre
df[3,3]
df[1,3]
df[df$Sexo=="H",]
subset(df, Sexo=="H")
df[df$Sexo!="H",]
df[df$Edad>30,c("Poblacion","Sexo","Casado")]
df[df$Sexo=="M",c("Nombre","Edad", "Casado")]
df[df$Casado==F, c("Edad","Poblacion","Sexo")]
df[df$Edad>30,]
df[df$Sexo=="M" & df$Edad>35, c("Nombre","Poblacion")]


# -----------------------------------------------------------------
# SQL SOBRE DATA.FRAMES
# -----------------------------------------------------------------
# carga de paquete
if(!require('sqldf')) install.packages('sqldf')
library(sqldf)

# inner join
clientes <- read.csv('in/DP-clients.csv',header=T,sep='\t')
tx_tarjetas <- read.csv('in/DP-Transactions OUT.txt',header=T,sep=',')
tx_clientes<-merge(tx_tarjetas,clientes,by="CardID") # inner join
sql <- paste ("
				SELECT A.*,[Client.ID]
				FROM tx_tarjetas A JOIN clientes B 
				ON A.CardID=B.CardID
			",sep="")
tx_clientes_sql<-sqldf(sql)


# sqldf Permite manipular los data frames mediante SQL

clientes <- data.frame(id_cliente=c(1,2,3,4),
                       Nombre=c("Pedro","María","José","Marta"),
                       Edad=c(27,34,40,39))
demografico<-data.frame(id_cliente=c(3,4,1,2),
                        Poblacion=c("Zaragoza","Madrid","Valencia",
                                    "Barcelona"),
                        Sexo=c("H","M","H","M"),
                        Casado=c(F,T,T,F))
clientedemografico<-sqldf("select t1.nombre, t1.edad, t2.*
                          from clientes t1
                          inner join demografico t2
                          on t1.id_cliente=t2.id_cliente")      

demografico2<-data.frame(id_cliente=c(3,4,1),
                         Poblacion=c("Zaragoza","Madrid","Valencia"),
                         Sexo=c("H","M","H"),
                         Casado=c(F,T,T))
clientedemografico2<-sqldf("select t1.nombre, t1.edad, t2.*
                           from clientes t1
                           inner join demografico2 t2
                           on t1.id_cliente=t2.id_cliente")
clientedemografico3<-sqldf("select t1.nombre, t1.edad, t2.*
                           from clientes t1
                           left join demografico2 t2
                           on t1.id_cliente=t2.id_cliente")   



