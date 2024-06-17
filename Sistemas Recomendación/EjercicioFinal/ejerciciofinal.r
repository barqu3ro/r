# This code snippet is setting up the initial environment for text mining analysis in R. Here's a
# breakdown of what each block is doing:
##### 1. Bloque de inicializacion de librerias #####
if (!require ("tm")){
  install.packages("tm")
  library("tm")
}

##### 2. Bloque de parametros iniciales #####
setwd("/Users/jorgebarquero/GitRepos/r/Sistemas Recomendación/EjercicioFinal")

##### 3. Bloque de carga de informacion #####
productos = read.csv2("data/productos.csv", stringsAsFactors = FALSE)


##### 4. Bloque de analisis del dataset #####
str (productos)
productos$Titulo[1:10]
productos$Descripcion[1:10]
productos$Tipo[1:10]


##### 5. Bloque de tratamiento de la información #####

dim(productos)[1]
Muestra=1000
#Muestra=6000
#Muestra=10000

# Antes de crear el corpus, leerlo en UTF-8
productos$Descripcion <- iconv(productos$Descripcion, "latin1", "UTF-8")

# Crear el corpus
corpus <- Corpus(VectorSource(productos$Descripcion[1:Muestra]))
corpus[[1]]$content
corpus[[1]]$meta

# Aplicar una función de transformación de contenido que maneje adecuadamente los caracteres especiales
corpus2 <- tm_map(corpus, content_transformer(function(x) iconv(x, "UTF-8", sub = "byte")))
corpus2 <- tm_map(corpus2, content_transformer(tolower))

corpus2[[1]]$content
corpus2[[1]]$meta

corpus2<-tm_map(corpus2, content_transformer(tolower))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, removeWords, stopwords("spanish"))
corpus2[[1]]$content[1]

corpus2<-tm_map(corpus2, removeWords, c("productos"))           
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



##### 6. Bloque de creacion de matrices de documentos y terminos (tf) #####

tdm<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
tdm$dimnames$Docs=productos$Titulo [1:Muestra]
dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
dtm$dimnames$Docs=productos$Titulo[1:Muestra]
tdm
str(tdm)
dtm
str(dtm)

##### 7. Bloque de análisis de términos #####

length(tdm$dimnames$Terms)

Terminos=tdm$dimnames$Terms[order(tdm$dimnames$Terms)]
Terminos

##### 8. Bloque de creacion de matrices de documentos y terminos (tf-idf) #####

tdm2<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
tdm2$dimnames$Docs=productos$Titulo[1:Muestra]
dtm2<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
dtm2$dimnames$Docs=productos$Titulo[1:Muestra]
tdm2
str(tdm2)
dtm2
str(dtm2)

##### 9. Bloque de inspeccion de matriz de documentos y terminos #####

dim(tdm2)
inspect(tdm2[1:10,1:5])

# Consulta 2
Producto = "My Little Pony - Pack Ponys Medianoche en Canterlot" # numero o nombre completo
inspect(tdm2[,Producto])

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


# Consulta 4 
# LEGO Angry Birds - Castillo del Rey Cerdo - 75826
Producto = 347
colnames(Simil)[Producto]
Simil[order(Simil[,Producto],decreasing=TRUE)[2:(Top+1)],Producto]


# Consulta 6 
Top <- 10

# Lista de productos comprados por el cliente
productos_comprados <- c(312, 927, 867, 787, 425)

# Inicializar un vector para almacenar las recomendaciones
recomendaciones <- numeric()

# Para cada producto comprado, encontrar los productos más similares
for (producto in productos_comprados) {
  # Ordenar los productos por similitud, excluyendo el producto actual
  similares <- order(Simil[,producto], decreasing = TRUE)
  
  # Añadir los productos más similares a la lista de recomendaciones
  recomendaciones <- c(recomendaciones, similares)
}

# Eliminar duplicados y productos ya comprados
recomendaciones <- unique(recomendaciones)
recomendaciones <- recomendaciones[!recomendaciones %in% productos_comprados]

# Seleccionar los Top 10 productos más similares
recomendaciones_final <- recomendaciones[1:Top]

# Mostrar las recomendaciones
recomendaciones_final

productos_recomendados <- productos[recomendaciones_final, ]

# Print the productos
productos_recomendados[2]
