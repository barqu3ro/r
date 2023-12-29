# ************************************************************************************
# PREPARACION BASICA DE DATOS CON IMPUTACION OUTLIERS
# ************************************************************************************

# ************************************************************************************
# Preparacion del entorno
# ************************************************************************************
rm(list=ls())
setwd('C:/Users/usuario/OneDrive - Universidad Complutense de Madrid (UCM)/_TODO/_PROYECTOS/Docentia/datascience_r_01/')

# ************************************************************************************
# Carga de paquetes
# ************************************************************************************
if(!require('Hmisc')) install.packages('Hmisc')
library(Hmisc)

if(!require('ellipse')) install.packages('ellipse')
library(ellipse)

# ##############################
# business undestanding
# ##############################

# Obtención de datos
clientes <- read.csv('in/CAL-anomalias-clientes.csv')
clientes$CONTACT_ID <- factor(clientes$CONTACT_ID)
clientes$EDUCATIONLEVEL_ID <- factor(clientes$EDUCATIONLEVEL_ID)


# ##############################
# data undestanding
# ##############################
summary(clientes)
describe(clientes)
hist(clientes$AGE)
boxplot_edad <- boxplot(clientes$AGE)

# ##############################
# data preparation
# ##############################

# quitar outliers por metodo boxplot
outliers <- c(clientes$AGE<boxplot_edad$stats[1] | clientes$AGE>boxplot_edad$stats[5])
clientes_conout <- clientes[outliers,]
clientes_sinout <- clientes[!outliers,]

summary(clientes_sinout)
hist(clientes_sinout$AGE,prob=TRUE)
lines(density(clientes_sinout$AGE))

# identificar correlaciones altas
cor_data <- cor(clientes_sinout[, c(-1,-6)])
plotcorr(cor_data,col=heat.colors(10))
plot(clientes_sinout$YEAREXPERIENCE, clientes_sinout$AGE)

# imputar outliers con rl
reg <- lm(AGE~YEAREXPERIENCE, data=clientes_sinout)
summary(reg)

# se compone dataset final con todos (outliers y no outliers)
pred<-predict(reg, newdata=clientes_conout)
clientes_conout$AGE <- round(pred,0)

clientes_final <- rbind(clientes_conout,clientes_sinout)
summary(clientes_final)

hist(clientes_final$AGE,prob=TRUE)
lines(density(clientes_final$AGE))

# ##############################
# data preparation
# ##############################
saveRDS(clientes_final, "out/clientes_final.rdata")

