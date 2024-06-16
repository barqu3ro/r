# Borrar el workspace
rm(list = ls())
set.seed(123)

# activo el directorio de trabajo
setwd('/Users/jorgebarquero/gitrepos/r/Hello-r')

# Código del cliente
Codigo.cliente <- paste("COD",1:1000,sep="_")

# Edad
Edad <- as.integer( rnorm(n=1000, mean= 45, sd=10))
Saldo <- rnorm(n=1000, mean= 10000, sd=1000)

# Dato categórico
# yo lo hice así
#Genero <- sample(c(1,2), size = 10000, replace = TRUE, prob = c(0.5,0.5))

# el lo hizo asi
Sexo <- as.integer(runif(n=1000,min=1, max=2.99))
Sexo <- factor(Sexo)                   
levels(Sexo) <- c("HOM","MUJ")
                 

# Crear el data frame
df_clientes <- data.frame(Codigo.cliente,Edad,Saldo,Sexo)

# Primeras inspecciones
# Ver los primeros
head(df_clientes)
# Ver los últimos
tail(df_clientes)


# Nombre columnas y filas
colnames(df_clientes)
rownames(df_clientes)

rownames (df_clientes) <- df_clientes$Codigo.cliente
df_clientes <- df_clientes[,-1]

seleccion_hombres <- df_clientes$Sexo == "HOM" & df_clientes$Edad <= 35
df_hombres_jov <- df_clientes[seleccion_hombres,]
seleccion_mujeres <- df_clientes$Sexo == "MUJ" & df_clientes$Edad <= 35
df_mujeres_jov <- df_clientes[seleccion_mujeres,]

ls_df <- list(df_clientes, df_hombres_jov,df_mujeres_jov)


length(ls_df)
names (ls_df)


# repasamos funciones de listas

save(file="out/ls_df.rdata", list=c("ls_df"))







