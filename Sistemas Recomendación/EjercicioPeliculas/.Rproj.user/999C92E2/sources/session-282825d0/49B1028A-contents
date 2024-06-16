## -------------------------------------------------------------------------
## SCRIPT: Sistema de Recomendacion.R
## CURSO: Workshop Sistema de Recomendación
## PROFESOR: Antonio Pita Lozano
## PAQUETES NECESARIOS: tm
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

if(!require("tm")){
  install.packages("tm")
  library("tm")
}

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####


setwd("/Users/jorgebarquero/GitRepos/python/Estudio/Data Science/Sistemas de Recomendación/EjercicioPeliculas")

## -------------------------------------------------------------------------

##### 3. Bloque de carga de informacion #####

Peliculas=read.csv2("data/dataset Peliculas.csv", stringsAsFactors = FALSE,encoding = "WINDOWS-1252",fileEncoding = "WINDOWS-1252")

## -------------------------------------------------------------------------

##### 4. Bloque de analisis del dataset #####

str(Peliculas)
Peliculas$Titulo[1:10]
Peliculas$Rating[1:10]
Peliculas$Sinopsis[1:3]


ListadoPeliculas=Peliculas$Titulo
ListadoPeliculas[1:10]

## -------------------------------------------------------------------------

##### 5. Bloque de tratamiento de la información #####

dim(Peliculas)[1]
Muestra=1000
#Muestra=6000
#Muestra=10000

corpus<-Corpus(VectorSource(Peliculas$Sinopsis[1:Muestra]))
corpus[[1]]$content
corpus[[1]]$meta

corpus2<-tm_map(corpus, PlainTextDocument)
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, content_transformer(tolower))
corpus2[[1]]$content[1]

#stopwords("spanish")
corpus2<-tm_map(corpus2, removeWords, stopwords("spanish"))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, removeWords, c("pelicula","actor","film"))           
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, removePunctuation)
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, removeNumbers)
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('\\b\\w{1,2}\\b','',x)}))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('¡','',x)}))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('á','a',x)}))
corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('é','e',x)}))
corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('í','i',x)}))
corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('ó','o',x)}))
corpus2<-tm_map(corpus2, content_transformer(function(x) {gsub('ú','u',x)}))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, stripWhitespace)
corpus2[[1]]$content[1]

## -------------------------------------------------------------------------

##### 6. Bloque de creacion de matrices de documentos y terminos (tf) #####

tdm<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
tdm$dimnames$Docs=Peliculas$Titulo[1:Muestra]
dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
dtm$dimnames$Docs=Peliculas$Titulo[1:Muestra]
tdm
str(tdm)
dtm
str(dtm)

## -------------------------------------------------------------------------

##### 7. Bloque de análisis de términos #####

length(tdm$dimnames$Terms)

Terminos=tdm$dimnames$Terms[order(tdm$dimnames$Terms)]
Terminos

## -------------------------------------------------------------------------

##### 8. Bloque de creacion de matrices de documentos y terminos (tf-idf) #####

tdm2<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
tdm2$dimnames$Docs=Peliculas$Titulo[1:Muestra]
dtm2<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
dtm2$dimnames$Docs=Peliculas$Titulo[1:Muestra]
tdm2
str(tdm2)
dtm2
str(dtm2)

## -------------------------------------------------------------------------

##### 9. Bloque de inspeccion de matriz de documentos y terminos #####

dim(tdm2)
inspect(tdm2[1:10,1:5])

Pelicula= 7 # numero o nombre completo
inspect(tdm2[,Pelicula])

Pelicula= 20 # numero o nombre completo
inspect(tdm2[,Pelicula])

ListadoPeliculas[5]
Pelicula= "El señor de los anillos: La comunidad del anillo" # numero o nombre completo
inspect(tdm2[,Pelicula])

termino="guerra"
inspect(tdm2[termino,])

termino="boda"
inspect(tdm2[termino,])

termino="mafia"
inspect(tdm2[termino,])

termino="princesa"
inspect(tdm2[termino,])

## -------------------------------------------------------------------------

##### 10. Bloque de calculo de similitudes item-item #####

inicio <- proc.time()
N=tdm2$ncol
dim(tdm2)

ProductosEscalares=as.matrix(dtm2)%*%as.matrix(tdm2)
proc.time()-inicio 
## Para N=6000 
## user  system elapsed 
## 814.33    6.17  840.25 
normas=sqrt(diag(ProductosEscalares))
Simil=(1/normas)*t(ProductosEscalares*(1/normas))

## -------------------------------------------------------------------------

##### 11. Bloque de similitud de objetos #####

Top=10
Num_Pelicula=15
colnames(Simil)[Num_Pelicula]
Simil[order(Simil[,Num_Pelicula],decreasing=TRUE)[2:(Top+1)],Num_Pelicula]

## -------------------------------------------------------------------------

##### 12. Bloque de filtrado colaborativo Item-Item #####

Peliculas$Valoracion = NA

colnames(Simil)[1:100]

Num_Pelicula=15
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 4

Num_Pelicula=1
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 9

Num_Pelicula=2
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 9

Num_Pelicula=25
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 3

Num_Pelicula=45
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 5

Num_Pelicula=56
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 2

Num_Pelicula=65
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 10

Num_Pelicula=98
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 9

Num_Pelicula=100
colnames(Simil)[Num_Pelicula]
Peliculas$Valoracion[Num_Pelicula]= 3

ValoracionIndividual=Peliculas[1:Muestra,c("Titulo","Valoracion")]

str(ValoracionIndividual)
summary(ValoracionIndividual)
PeliculasValorados=which(!is.na(ValoracionIndividual$Valoracion))

MediaIndividual=mean(ValoracionIndividual$Valoracion,na.rm=TRUE)
ValoracionIndividual$ValoracionNorm=ValoracionIndividual$Valoracion-MediaIndividual
ValoracionIndividual$ValoracionNorm[is.na(ValoracionIndividual$ValoracionNorm)]=0

PuntuacionesIndividuales=Simil %*% ValoracionIndividual$ValoracionNorm
PrioridadIndividual=order(PuntuacionesIndividuales, decreasing=TRUE)
PrioridadFiltrada=PrioridadIndividual[!(PrioridadIndividual %in% PeliculasValorados)]

RecomendacionesIndividuales=ListadoPeliculas[PrioridadFiltrada[1:20]]

RecomendacionesIndividuales

write.csv2(RecomendacionesIndividuales,"recomendaciones individuales.csv",row.names = FALSE)
## -------------------------------------------------------------------------