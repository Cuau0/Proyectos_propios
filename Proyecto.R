#1.- Limpieza de datos 

datos <- read.csv("./datos_covid.csv", header = TRUE)
names(datos)
#Numero de positivos por Covid al 19/05
sum(datos$RESULTADO == 1)

defunciones <- datos[!(datos$FECHA_DEF %in% "9999-99-99") & datos$RESULTADO == 1,]

#Generamos los casos positivos 
positivos1 <- datos[!(datos$FECHA_DEF %in% "9999-99-99") & datos$RESULTADO == 1,c("FECHA_SINTOMAS","SEXO","EDAD","NEUMONIA",
                                           "DIABETES","EPOC","ASMA","INMUSUPR","HIPERTENSION",
                                           "CARDIOVASCULAR","OBESIDAD","RENAL_CRONICA","OTRA_COM", "FECHA_DEF")]
positivos1$FECHA_SINTOMAS <- as.Date(positivos1$FECHA_SINTOMAS,
                                     format = "%d/%m/%Y")
positivos1$FECHA_DEF <- as.Date(positivos1$FECHA_DEF, format = "%d/%m/%Y")

#Contar cormobidades
positivos1$NEUMONIA[positivos1$NEUMONIA %in% c(2,99)] <- 0
positivos1$DIABETES[positivos1$DIABETES %in% c(2,99,98)] <- 0
positivos1$EPOC[positivos1$EPOC %in% c(2,99,98)] <- 0
positivos1$ASMA[positivos1$ASMA %in% c(2,99,98)] <- 0
positivos1$INMUSUPR[positivos1$INMUSUPR %in% c(2,99,98)] <- 0
positivos1$HIPERTENSION[positivos1$HIPERTENSION %in% c(2,99,98)] <- 0
positivos1$CARDIOVASCULAR[positivos1$CARDIOVASCULAR %in% c(2,99,98)] <- 0
positivos1$OBESIDAD[positivos1$OBESIDAD %in% c(2,99,98)] <- 0
positivos1$RENAL_CRONICA[positivos1$RENAL_CRONICA %in% c(2,99,98)] <- 0
positivos1$OTRA_COM[positivos1$OTRA_COM %in% c(2,99,98)] <- 0

positivos1$COMORBILIDAD <- rowSums(positivos1[,c("NEUMONIA","DIABETES","EPOC",
                                               "ASMA","INMUSUPR","HIPERTENSION",
                     "CARDIOVASCULAR","OBESIDAD","RENAL_CRONICA","OTRA_COM")])

positivos <- positivos1[,c(1,2,3,14,15)]

# Histograma de los pacientes positivos que presentaron sintomas a 
# la fecha mencionada

barplot(table(positivos$FECHA_SINTOMAS),ylim = c(0,2300), col = "blue", 
        xlab = "Tiempo", ylab = "No de casos", 
        main = "Casos confirmados hasta 19/Mayo")
abline(h =max(table(positivos$FECHA_SINTOMAS)),col="Red")

#Histograma de frecuencias acumuladas absoluta y relativa 
par(mfrow = c(1,1))
barplot(cumsum(table(positivos$FECHA_SINTOMAS)), col = "red", main = "Casos acumulados hasta el 19 de mayo")
barplot(cumsum(prop.table(table(positivos$FECHA_SINTOMAS))), col = "yellow", main = "Casos acumulados relativos hasta el 19 de mayo")

#Distribucion de casos positivos por edad
barplot(xtabs(positivos$SEXO ~ positivos$EDAD, data = positivos), las=1, col = "red",
        main = "Distribución por edad \n de casos positivos", xlab = "Edad", ylab = "Frecuencia")

#Asimetria
Asimetria <- function(datos){
  n <- length(datos)
  m3 <- (1/(n-1))*(sum((datos - mean(datos))^3))
  s <- sqrt(1/(n-1))*(sum((datos - mean(datos))^2))
  print(m3/s^3)
}

Asimetria(positivos$EDAD)
## Es asimetrica a la derecha

Curtosis <- function(datos){
  n <- length(datos)
  m4 <- (1/(n-1))*(sum((datos - mean(datos))^4))
  s <- sqrt(1/(n-1))*(sum((datos - mean(datos))^2))
  print(m4/s^4)
}

Curtosis(positivos$EDAD)
#ES leptocurtica

quantile(positivos$EDAD)
library(modeest) 
mfv(positivos$EDAD)


#Grafica de pie sobre division de hombres y mujeres 
tabla_HM <- table(positivos$SEXO)
tabla_HM_prop <- prop.table(tabla_HM)

pie(tabla_HM, 
    main = "Casos positivos por sexo", 
    labels = paste(c("Mujeres","Hombres"),":",round(tabla_HM_prop,2)), 
    radius = 1, col = c("red","blue"),
    border = "black")

#Dividir los datos en mujeres y hombres
covid_mujer <- positivos[positivos$SEXO == 1, ]
covid_hombre <- positivos[positivos$SEXO == 2, ]

#Histograma de positivos por edad y por sexo
par(mfrow = c(1,2))
hist(covid_mujer$EDAD, breaks = 113, main = "Histograma de positivos de edad \n de hombres", xlab = "Edad", ylab = "Positivos", col = "blue")
hist(covid_hombre$EDAD, breaks = 114, main = "Histograma de positivos de edad \n de mujeres", xlab = "Edad", ylab = "Positivos", col = "red")

#Distribución de positivos por edad y por sexo
hist(covid_mujer$EDAD,probability = T, breaks = 113, main = "Distribución de positivos por edad \n de hombres", xlab = "Edad", ylab = "Positivos", col = "blue")
lines(density(covid_mujer$EDAD), lwd = 4)
hist(covid_hombre$EDAD, probability = T, breaks = 114, main = "Distribución de positivos por edad \n de mujeres", xlab = "Edad", ylab = "Positivos", col = "red")
lines(density(covid_hombre$EDAD), lwd = 4)

# Distribución de casos positivos de hombres y mujeres
barplot(table(covid_hombre$FECHA_SINTOMAS), col = "red", main = "Casos confirmados en Hombres", xlab = "Fecha de sintomas", ylab = "No. de positivos")
barplot(table(covid_mujer$FECHA_SINTOMAS), col = "blue", main = "Casos confirmados en mujeres", xlab = "Fecha de sintomas", ylab = "No. de positivos")

#Grafica con comorbilidades
#Histograma de positivos por numero de comorbilidad 
par(mfrow = c(1,1))
barplot(table(positivos$COMORBILIDAD), main = "Distribución de Número de comorbilidades de casos positivos",
        xlab = "Número de comorbilidades",
        ylab = "Número de positivos", 
        col = "orange")
Asimetria(positivos$COMORBILIDAD)
Curtosis(positivos$COMORBILIDAD)
quantile(positivos$COMORBILIDAD)

par(mfrow = c(1,1))
boxplot(positivos$COMORBILIDAD, main = "Diagrama de caja del número de comorbilidades", horizontal = T, xlab = "Número de comorbilidades", col = "green")

par(mfrow = c(1,2))
barplot(table(covid_hombre$COMORBILIDAD), 
     main = "Total de personas por número de \n comorbilidades en hombres",
     xlab = "Numero de comorbilidades",
     ylab = "Total de hombres", 
     col = "green")

barplot(table(covid_mujer$COMORBILIDAD),
     main = "Total de personas por número de \n comorbilidades en mujeres",
     xlab = "Numero de comorbilidades",
     ylab = "Total de mujeres", 
     col = "blue")

library(ggplot2)

ggplot(positivos,
       aes(x= factor(COMORBILIDAD), EDAD))+
  labs(y = "Edad", x = "Número de comorbilidades") +
  geom_boxplot(fill = "red")

table(positivos$COMORBILIDAD)
barplot(table(positivos$COMORBILIDAD))
################################################################################
#################################        2        ############################## 
################################################################################

library(MASS)

####################### Si se ajusta a una normal #######################
comor_2 <- positivos[positivos$COMORBILIDAD == 2,c(3)]

par(mfrow = c(1,1))
y <- positivos[positivos$COMORBILIDAD == 2,c(3)]
(fit <- fitdistr(comor_2, densfun="normal"))
hist(comor_2, breaks = max(comor_2)-min(comor_2), probability = T)
curve(dnorm(x, fit$estimate[1],fit$estimate[2]), col="red", lwd=2, add=T)

###################################################################################
########################## Distribución de casos por edad #########################
###################################################################################

barplot(xtabs(positivos$SEXO ~ positivos$EDAD, data = positivos), las=1, col = "red",
        main = "Distribución de edad \n de casos positivos", xlab = "Edad", ylab = "Frecuencia")
hist(positivos$EDAD, breaks = 114, probability = T, main = "Distribución de Casos positivos por edad", col = "pink", xlab = "Edad")

## probamos con la NORMAL primero
positivos_0 <- positivos[positivos$EDAD %in% seq(1,114),]

fit_edad_1 <- fitdistr(positivos_0$EDAD, densfun = "normal")
curve(dnorm(x, fit_edad_1$estimate[1],fit_edad_1$estimate[2]), col = "orange", add = T, lwd = 2.5)
legend(69,.025, legend = c("Normal"), fill = c("orange"), cex = .8)


### Probar con la WEIBULL (Tenemos que sacrificar la edad 0)

fit_edad_3 <- fitdistr(positivos_0$EDAD, densfun = "weibull")
curve(dweibull(x,fit_edad_3$estimate[1],fit_edad_3$estimate[2]), col = "green", add = T)
legend(69,.025, legend = c("Weibull"), fill = c("green"), cex = .8)


### Probar con la GAMMA

### estimamos los parametros por momentos
n <- length(positivos_0$EDAD)
num <- n*mean(positivos_0$EDAD)^2
den <- (sum(positivos_0$EDAD^2)) - (n*mean(positivos_0$EDAD)^2)
alfa_estimada <- num/den
beta_estimada <- mean(positivos$EDAD)/alfa_estimada

#### Por maxima verosimilitud

#Debemos quitar la edad 0 para el logaritmo
x <- positivos_0$EDAD

fit_edad_4 <- fitdistr(x, densfun = "gamma", list(shape = alfa_estimada, scale = 1/beta_estimada), lower = .001)

curve(dgamma(x, alfa_estimada, 1/beta_estimada),col = "blue" ,add = T)
curve(dgamma(x,fit_edad_4$estimate[1],1/fit_edad_4$estimate[2]), col = "red", add = T)
legend(69,.025, legend = c("Gamma Mom","Gamma fitdistr"), fill = c("blue","red"), cex = .8)


legend(69,.025, legend = c("Normal","Weibull","Gamma Mom","Gamma fitdistr"), fill = c("orange","green","blue","red"), cex = .8)

log_ver <- function(tetha){
  al <- tetha[1]
  be <- tetha[2]
  f1 <- log(x^(al-1)*exp(-x/be))
  f2 <- sum(f1)
  f3 <- log(gamma(al)) + al*log(be)
  f4 <- -f3 + sum(f1)
  return(-f4)
}
log_mm <- log_ver(c(alfa_estimada,beta_estimada))

##########################################
### Que modelo es el mejor
n <- length(positivos$EDAD)
k=2   # numero de parametros a estimar

AIC_Normal = 2*k-(2*fit_edad_1$loglik)
BIC_Normal = log(n)*k-(2*fit_edad_1$loglik)

AIC_weibull = 2*k-(2*fit_edad_3$loglik)
BIC_weibull = log(n)*k-(2*fit_edad_3$loglik)

AIC_gamma_mom = 2*k-(2*log_mm)
BIC_gamma_mom = log(n)*k-(2*log_mm)

AIC_gamma_MV = 2*k-(2*fit_edad_4$loglik)
BIC_gamma_MV = log(n)*k-(2*fit_edad_4$loglik)

AIC_Normal;AIC_weibull;AIC_gamma_mom;AIC_gamma_MV
BIC_Normal;BIC_weibull;BIC_gamma_mom;BIC_gamma_MV

#####################################################################################
#####################################################################################
##Estime para la variable cualitativa, la proporcion muestral ( ^ pk) en cada categora k. Recuerdeque:
#####################################################################################

covid_mujer_00 <- positivos_0[positivos_0$SEXO == 1, ]
covid_hombre_00 <- positivos_0[positivos_0$SEXO == 2, ]

covid_hombre_00$SEXO[covid_hombre_00$SEXO == 2] <- 1
p_barra_1 <- sum(covid_hombre_00$SEXO)/length(positivos_0$SEXO)

p_barra_2 <- sum(covid_mujer_00$SEXO)/length(positivos_0$SEXO)

p_barra_1 + p_barra_2

#########################################################################################
###############################################################################
#########5. Estime para las dos variables cuantitativas la media muestral (x), la varianza muestral (s2)
#########y el coeciente de correlacion muestral (r). Recuerde que:
###########################################################################################
#############################################################################################

##### Crear una muestra del 3/4 de los datos y veremos que sucede con los parametros muestrales
set.seed(2)
datos_muestra <-  positivos_0[sample(nrow(positivos_0),.75*nrow(positivos_0),replace = F),]

###########################################################################################################

m1_muestra <- sum(datos_muestra$EDAD)/length(datos_muestra$EDAD)
m2_muestra <- sum(datos_muestra$COMORBILIDAD)/length(datos_muestra$COMORBILIDAD)

m1_muestra;m2_muestra
## Varianza muestral 
n_muestra <- length(datos_muestra$COMORBILIDAD)
fac1_muestra <- 1/(n_muestra-1)

va1_muestra <- (sum(datos_muestra$EDAD^2)-(n_muestra*m1_muestra^2))*fac1_muestra
va2_muestra <- (sum(datos_muestra$COMORBILIDAD^2)-(n_muestra*m2_muestra^2))*fac1_muestra

var(datos_muestra$COMORBILIDAD)
var(positivos_0$EDAD)
## Coeficiente de correlacion muestral
#
cov_xy_muestra <- fac1_muestra*(sum(datos_muestra$EDAD*datos_muestra$COMORBILIDAD) - n_muestra*m1_muestra*m2_muestra)
cov_xy_muestra/(va1_muestra*va2_muestra)

## En R
r_muestra <- cov(datos_muestra$EDAD, datos_muestra$COMORBILIDAD)/(va1_muestra*va2_muestra)

##############################################################################
#####################Estimación de intervalos de confianza#####################
###############################################################################
###############################################################################
covid_mujer_0 <- positivos_0[positivos_0$SEXO == 1, ]
covid_hombre_0 <- positivos_0[positivos_0$SEXO == 2, ]

muestra_hombres <- covid_hombre_0[sample(nrow(covid_hombre_0),.75*nrow(covid_hombre_0), replace = F),]
muestra_mujeres <- covid_mujer_0[sample(nrow(covid_mujer_0),.75*nrow(covid_mujer_0), replace = F), ]

# Histograma de distribución por edad 
par(mfrow = c(1,2))
hist(covid_mujer_0$EDAD, breaks = 113, main = "Histograma de positivos de edad \n de hombres", xlab = "Edad", ylab = "Positivos", col = "blue", probability = T)
hist(covid_hombre_0$EDAD, breaks = 114, main = "Histograma de positivos de edad \n de mujeres", xlab = "Edad", ylab = "Positivos", col = "red", probability = T)

#Estimacion de los parametros por maxima verosimilitud (Poblacional)
fit_edad_h <- fitdistr(covid_hombre_0$EDAD, densfun = "normal")
curve(dnorm(x,fit_edad_h$estimate[1],fit_edad_h$estimate[2]), add = T)

fit_edad_m <- fitdistr(covid_mujer_0$EDAD, densfun = "normal")
curve(dnorm(x, fit_edad_m$estimate[1], fit_edad_m$estimate[2]), add = T)

######################################################################################
######################### Intervalos de la media y la varianza para una población ####
######################################################################################

# Aqui tomamos a nuestros estadisticos como poblacionales y entonces tomamos los intervalos
# de confianza de los datos muestrales

Int_conf_med <- function(nc, datos){
n <- length(datos)
me <- mean(datos)
dev <- sd(datos)
alfa <- 1-nc
alfa2 <- alfa/2
tStudent <- qt(alfa2, df = n-1)
intervalo <- c(me + dev/sqrt(n)*tStudent, me - tStudent*dev/sqrt(n))
intervalo
}

Int_conf_med(.83, datos_muestra$EDAD)
Int_conf_med(.97, datos_muestra$EDAD)

# En la muestra creada, la media poblacional si cae en el intervalo 
#de confianza de la muestra

mean(covid_hombre_0$EDAD)

mean(covid_mujer_0$EDAD)

mean(muestra_hombres$EDAD)
Int_conf_med(.83, muestra_hombres$EDAD)
Int_conf_med(.97, muestra_hombres$EDAD)

mean(muestra_mujeres$EDAD)
Int_conf_med(.83, muestra_mujeres$EDAD)
Int_conf_med(.97, muestra_mujeres$EDAD)

#La media poblacional no cae en ninguno de los intervalos separados por la variable cuantitiva 
# Pero la media poblacional de las variables separadas por hombre y mujer si caen en sus respectivos intervalos 

# Diferencias de medias

mean(covid_hombre_0$EDAD) - mean(covid_mujer_0$EDAD)
t.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, mu = 0, conf.level = .83)$conf.int
t.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, mu = 0, conf.level = .97)$conf.int

#####################################################################################
#####################################################################################
#####################################################################################
#Intervalos de confianza tomando las var muuestrales para la variable EDAD, 83% y 97%

Int_conf_var <- function(datos, nc){
  n <- length(datos)
  k <- n-1
  s <- var(datos)
  alfa <- 1- nc
  alfa2 <- alfa/2
  chi_alfa <- qchisq(1-alfa2, df = k)
  chiUnoMenosAlfa2 <- qchisq(alfa2, df = k)
  intevalo <- s*(k/c(chi_alfa,chiUnoMenosAlfa2))
  return(intevalo)
}

#var poblacional
var(positivos_0$EDAD)

#Var de una muestra muestral
Int_conf_var(datos_muestra$EDAD, .83)
Int_conf_var(datos_muestra$EDAD, .97)

## segunda pregunta
#Var de una muestra hombres
var(covid_hombre_0$EDAD)
Int_conf_var(muestra_hombres$EDAD, .83)
Int_conf_var(muestra_hombres$EDAD, .97)

# Var de una muestra mujeres
var(covid_mujer_0$EDAD)
Int_conf_var(muestra_mujeres$EDAD, .83)
Int_conf_var(muestra_mujeres$EDAD, .97)

# cociente de varianzas 

var(covid_hombre_0$EDAD)/var(covid_mujer_0$EDAD)
var.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, conf.level = .83)$conf.int
var.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, conf.level = .97)$conf.int

#El cociente se encuentra dentro del intervalo de confianza en ambas ocaciones 
####################################################################################

################################################################################################
################################################################################################
################################ Pruebas de hipotesis ##########################################
################################################################################################
################################################################################################

# Vamos a suponer una media y una varianza poblacional para la muestra de nuestros datos
# Para una media como nuestro estimado de la muestra es de 46.6845 , yo propongo que la media 
# poblacional debe ser de 46.683
# y para la varianza poblacional como el estimado para la muestra es de 255.2369, propongo que la desviación estandar 
# poblacional debe ser de 15.761.
# suponiendo estos casos haremos una prueba de hipotesis
# Sabemos como de distribuye la poblacional entonces usamos un estadistico Z 
library(PASWR)

#Prara una población
#################################################################
########################### Para la media #######################
#################################################################

# Calculamos la region critica de Z con alfa = .03
re_critica1 <- qnorm(.03/2, mean = 0, sd = 1, lower.tail=T) 
re_critica2 <- qnorm(.03/2, mean = 0, sd = 1, lower.tail=F) 
c(re_critica1,re_critica2)

set.seed(1)
u <- rnorm(10000)
plot(u, dnorm(u))
abline(v = c(re_critica1,re_critica2), col = "red")


# Estadistico 
z <- (mean(datos_muestra$EDAD)-mean(positivos_0$EDAD))/(sd(positivos_0$EDAD)/sqrt(length(datos_muestra$EDAD)))

# Valor P
2*pnorm(-abs(z))
2*(1-pnorm(-z))

2*pnorm(z)
z.test(datos_muestra$EDAD, mu = mean(positivos_0$EDAD), sigma.x = sd(positivos_0$EDAD) , alternative="two.sided", conf.level = .83)

#No se rechaza!!

##########################################################
############################### Para la varianza #########
##########################################################

# Region critica

R_critica_chi1 <- qchisq(.03/2, length(datos_muestra$EDAD)-1)
R_critica_chi2 <- qchisq(1-.015, length(datos_muestra$EDAD-1))

c(R_critica_chi1,R_critica_chi2)
u1 <- rchisq(10000, length(datos_muestra$EDAD)-1)
plot(u1, dchisq(u1, length(datos_muestra$EDAD)-1))
abline(v = c(R_critica_chi1,R_critica_chi2))

#Estadistico
j <- ((length(datos_muestra$EDAD)-1)*var(datos_muestra$EDAD))/var(positivos_0$EDAD)
abline(v = j)
# Valor p PENDIENTE
pchisq(j, length(datos_muestra$EDAD)-1)

#NO se rechaza!!!

#### Para dos muestras ##

###############################################################
################################ Media ########################
###############################################################
# Para el 3%
length(muestra_hombres$EDAD)+length(muestra_mujeres$EDAD)
mean(muestra_hombres$EDAD)- mean(muestra_mujeres$EDAD)
Dif_medias_3 <- t.test(x = muestra_hombres$EDAD, y = muestra_mujeres$EDAD, conf.level = .97, mu = 0, alternative = "two.side")

Dif_medias_3$statistic
Dif_medias_3$p.value
Dif_medias_3$conf.int

#Sí se rechaza!!

#region de rechazo
R_critica_t1 <- qt(.03/2, df = 40700 - 1)
R_critica_t2 <- qt(.03/2, df = 40700 - 1, lower.tail = F)

c(R_critica_t1, R_critica_t2)
u2 <- rt(10000, df = 40700 - 1)
plot(u2, dt(u2, df = 40700 - 1))
abline(v = c(R_critica_t1, R_critica_t2))

#Para el 17%
Dif_medias_17 <- t.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, conf.level = .83, mu = 0)

Dif_medias_17$statistic
Dif_medias_17$p.value
Dif_medias_17$conf.int


#region de rechazo
R_critica_t1 <- qt(.17/2, df = 40700 - 1)
R_critica_t2 <- qt(.17/2, df = 40700 - 1, lower.tail = F)

u2 <- rt(10000, df = 40700 - 1)
plot(u2, dt(u2, df = 40700 - 1))
abline(v = c(R_critica_t1, R_critica_t2))




#Sí se rechaza!!!
####################################################################
########################### Varianzas ##############################
####################################################################

#Para el 3%
Vares <- var.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, alternative = "two.sided", conf.level = .97, ratio = 1)
Vares$statistic ; var(muestra_hombres$EDAD)/var(muestra_mujeres$EDAD)
Vares$p.value

# región de rechazo
R_critica_F1 <- qf(.03/2, length(muestra_hombres$EDAD) - 1, length(muestra_mujeres$EDAD)- 1)
R_critica_F2 <- qf(1- .015, length(muestra_hombres$EDAD)- 1, length(muestra_mujeres$EDAD)- 1, lower.tail = F)
c(R_critica_F1, R_critica_F2)

#Para el 17%
Vares <- var.test(muestra_hombres$EDAD, muestra_mujeres$EDAD, alternative = "two.sided", conf.level = .83, ratio = 1)
Vares$statistic
Vares$p.value


# región de rechazo
R_critica_F1 <- qf(.17/2, length(muestra_hombres$EDAD) - 1, length(muestra_mujeres$EDAD)- 1)
R_critica_F2 <- qf(1- .085, length(muestra_hombres$EDAD)- 1, length(muestra_mujeres$EDAD)- 1)
c(R_critica_F1, R_critica_F2)
