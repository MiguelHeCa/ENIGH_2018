
# 1. Cálculo de principales indicadores usando el paquete R ---------------

# A continuación, se presentan los códigos para el cálculo de los principales 
# indicadores de la ENIGH 2018. Están escritos para que el usuario los ejecute
# sin necesidad de cambio e incluyen comentarios que explican parte del código. 
# En la primera sección se incluye el código necesario para leer las tablas de 
# datos que contienen la información recabada por la encuesta, y en las 
# subsecuentes los códigos utilizados.
# 
# Antes de ejecutar los códigos es necesario que el usuario cargue las librerías 
# foreign y survey. La primera se utiliza para leer y escribir archivos de bases 
# de datos y la segunda para el cálculo de las estimaciones, errores estándar, 
# coeficientes de variación, intervalos de confianza, etcétera, en diseños de 
# muestreo complejos como el estratificado y por conglomerados. Así mismo se deben
# cargar la librería doBy que nos permitirá ordenar de menor a mayor los ingresos,
# esto con el fin de la creación de deciles de ingreso, y también la librería 
# reldist, la cual nos ayudara para mandar llamar la función que nos calculará el 
# coeficiente de GINI.
# 
# Para ello, deben ejecutarse los siguientes comandos:

# carga la librería foreign la cual nos auxiliará para leer los datos en 
# diferentes formatos (.dbf, .sav, etc.)
library(foreign)

# carga la librería survey, ésta nos sirve para el cálculo del diseño muestral
library(survey)

# carga la librería doBy que nos permite ordenar los datos de la tabla según el ingreso
library(doBy)

# carga la librería reldist, ésta incluye la función para el cálculo del GINI
library(reldist)

# opción para tratar los casos de los estratos con una sola una UPM
options(survey.lonely.psu="adjust")

# Lectura de las tablas de datos ------------------------------------------

# Esta parte del código es la única que requiere un ajuste por el usuario. Antes 
# de ejecutar los comandos que leen las tablas con la información, el usuario 
# debe escribir el directorio con la ubicación de las bases publicadas. Por
# ejemplo, si se encuentran en el directorio C:\Documentos, la instrucción 
# adecuada sería:
  
setwd("C:/Documentos")

# o bien

setwd("C:\\Documentos")

# El resto del código no requiere modificación.

# 2. Cálculo de indicadores -----------------------------------------------


# 2.1 Ingreso corriente total promedio trimestral por hogar ---------------
# en deciles de hogares y su coeficiente de GINI

# A continuación, se presenta el código para calcular el Ingreso corriente
# total promedio trimestral por hogar en deciles de hogares y su coeficiente
# de GINI.
# 
# El coeficiente de GINI es una medida de concentración del ingreso: toma
# valores entre cero y uno. Cuando el valor se acerca a uno, indica que 
# hay mayor concentración del ingreso; en cambio cuando el valor se acerca
# a cero la concentración del ingreso es menor.

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

# carga lista de librerías que necesitaremos
library(foreign) # librería que nos ayuda a leer las tablas en diferentes formatos
library(doBy) # librería que nos permite ordenar los datos de la tabla según el ingreso
library(reldist) # librería que incluye la función para el cálculo del GINI

# establece el directorio donde se encuentran nuestras bases de datos
setwd("D:/ENIGH NS 2018/BASE DE DATOS/DBF")

# abrimos la tabla concentradohogar
Conc<- read.dbf("concentradohogar.dbf",as.is = T)

# selección de las variables de interés
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]

# se crea una variable para agregar la entidad federativa
Conc$entidad <- substr(Conc$folioviv,1,2)

# se define la columna con el nombre de las entidades federativas
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

# se crea una bandera para numerar a los hogares
Conc$Nhog <- 1

##################### DECILES DE INGRESO ###################
# deja activa la tabla Conc
attach(Conc)

# ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc)

# suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor,to.data.frame=TRUE)

# se divide la suma de factores entre diez para sacar el tamaño del decil
# se debe de truncar el resultado quitando los decimales.
tam_dec<-trunc(tot_hogares/10)

# muestra la suma del factor en variable hog.
Conc$tam_dec=tam_dec

############### CREACION DE DECILES DE INGRESO #######################
# se renombra la tabla concentrado a BD1.
BD1 <- Conc

# dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor

# se ordena de menor a mayor según la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]

# se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)

# entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
             BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9)
{
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10

##################################################################
# TOTAL HOGARES
x<-tapply(BD1$factor,BD1$Nhog,sum)

# DECILES
y<-tapply(BD1$factor,BD1$DECIL,sum)

# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t<-tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ing_cormed_d<-tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y

########################## C U A D R O S #################################
# guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))

# agregamos el nombre a las filas
row.names(prom_rub)<-Numdec

############### Cálculo del GINI #############
# GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3],
                                           ing_cormed_d[4],ing_cormed_d[5],ing_cormed_d[6],
                                           ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],
                                           ing_cormed_d[10]))

# se efectua la función Gini y se guarda en nuestro vector a.
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)

# se renombran las variables (columnas)
names(prom_rub)=c("INGRESO CORRIENTE")
names(a)="GINI"

##### Mostramos el resultado en pantalla #####
round(prom_rub)
round(a,3)
## NOTA: El cálculo de las precisiones estadísticas de deciles y del coeficiente
# de GINI no son mostradas debido a que requieren el uso de técnicas de remuestreo ##

# 2.2 Promedio de las principales fuentes de ingreso ----------------------
# por entidad federativa

# A continuación, se presenta el código para calcular la composición de las 
# principales fuentes del ingreso corriente total promedio trimestral por 
# entidad federativa. Después de ejecutar las estimaciones, se calculan sus 
# errores estándar, sus coeficientes de variación, los límites inferiores de 
# sus intervalos de confianza y los correspondientes límites de confianza, se 
# encontrarán en estructuras de datos de tipo data.frame llamadas c_ent_ES, 
# c_ent_SE, c_ent_CV, c_ent_LI y c_ent_LS, respectivamente.

# Cuadro2
# Promedio de las principales fuentes de ingreso por entidad federativa
# 2018

# carga lista de librerías que necesitaremos
library(foreign) # librería que nos ayuda a leer las tablas en diferentes formatos
library(survey) # librería para calcular el diseño muestral

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

# establece el directorio donde se encuentran nuestras bases de datos
setwd("D:/ENIGH NS 2018/BASE DE DATOS/DBF")

# abre la tabla concentradohogar
Conc <- read.dbf("concentradohogar.dbf",as.is = T)

# se selecciona solo las variables de interés para nuestro cálculo
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]

# se crea una variable para agregar la entidad federativa
Conc$entidad <- substr(Conc$folioviv,1,2)

# Se define la columna con el nombre de las entidades federativas
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur",
             "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "México", "Durango",
             "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
             "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
             "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán",
             "Zacatecas")

####### HOGARES #########
# se crea una bandera para numerar a los hogares
Conc$Nhog <- 1

# se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)

######## ingreso Corriente #######
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) # Total promedio
Ming_corEnt <- svyby(~ing_cor,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

### ingreso del trabajo ###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabEnt <- svyby(~ingtrab,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoEnt <- svyby(~trabajo,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioEnt <- svyby(~negocio,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabEnt <- svyby(~otros_trab,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

### renta de la propiedad ###
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasEnt <- svyby(~rentas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadEnt <- svyby(~utilidad,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaEnt <- svyby(~arrenda,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

### Transferencias ###
MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferEnt <- svyby(~transfer,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### jubilacion
MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionEnt <- svyby(~jubilacion,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

###### becas
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasEnt <- svyby(~becas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### donativos
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosEnt <- svyby(~donativos,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

###### remesas
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasEnt <- svyby(~remesas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

###### bene_gob
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobEnt <- svyby(~bene_gob,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

###### transf_hog
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogEnt <- svyby(~transf_hog,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

###### trans_inst
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instEnt <- svyby(~trans_inst,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
promedio

### estim_alqu ###
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquEnt <- svyby(~estim_alqu,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

### otros_ing ###
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingEnt <- svyby(~otros_ing,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

########## Estimaciones ##########
ES_Ming_corTot <- Ming_corTot[[1]]
ES_Ming_corEnt <- Ming_corEnt[[2]]
ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabEnt <- MingtrabEnt[[2]]
ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoEnt <- MtrabajoEnt[[2]]
ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioEnt <- MnegocioEnt[[2]]
ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabEnt <- Motros_trabEnt [[2]]
ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasEnt <- MrentasEnt [[2]]
ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadEnt <- MutilidadEnt [[2]]
ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaEnt <- MarrendaEnt [[2]]
ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferEnt <- MtransferEnt[[2]]
ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionEnt <- MjubilacionEnt [[2]]
ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasEnt <- MbecasEnt [[2]]
ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosEnt <- MdonativosEnt[[2]]
ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasEnt <- MremesasEnt[[2]]
ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobEnt <- Mbene_gobEnt [[2]]
ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogEnt <- Mtransf_hogEnt [[2]]
ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instEnt <- Mtrans_instEnt[[2]]
ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquEnt <- Mestim_alquEnt [[2]]
ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingEnt <- Motros_ingEnt [[2]]

########## Error Estándar ##########
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corEnt <- SE (Ming_corEnt)
SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabEnt <- SE (MingtrabEnt)
SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoEnt <- SE (MtrabajoEnt)
SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioEnt <- SE (MnegocioEnt)
SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabEnt <- SE (Motros_trabEnt)
SE_MrentasTot <- SE (MrentasTot)
SE_MrentasEnt <- SE (MrentasEnt)
SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadEnt <- SE (MutilidadEnt)
SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaEnt <- SE (MarrendaEnt)
SE_MtransferTot <- SE (MtransferTot)
SE_MtransferEnt <- SE (MtransferEnt)
SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionEnt <- SE (MjubilacionEnt)
SE_MbecasTot <- SE (MbecasTot)
SE_MbecasEnt <- SE (MbecasEnt)
SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosEnt <- SE (MdonativosEnt)
SE_MremesasTot <- SE (MremesasTot)
SE_MremesasEnt <- SE (MremesasEnt)
SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobEnt <- SE (Mbene_gobEnt)
SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogEnt <- SE (Mtransf_hogEnt)
SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instEnt <- SE (Mtrans_instEnt)
SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquEnt <- SE (Mestim_alquEnt)
SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingEnt <- SE (Motros_ingEnt)

########## Coeficiente de variación ##########
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corEnt <- cv(Ming_corEnt)
CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabEnt <- cv(MingtrabEnt)
CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoEnt <- cv(MtrabajoEnt)
CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioEnt <- cv(MnegocioEnt)
CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabEnt <- cv(Motros_trabEnt)
CV_MrentasTot <- cv(MrentasTot)
CV_MrentasEnt <- cv(MrentasEnt)
CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadEnt <- cv(MutilidadEnt)
CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaEnt <- cv(MarrendaEnt)
CV_MtransferTot <- cv(MtransferTot)
CV_MtransferEnt <- cv(MtransferEnt)
CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionEnt <- cv(MjubilacionEnt)
CV_MbecasTot <- cv(MbecasTot)
CV_MbecasEnt <- cv(MbecasEnt)
CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosEnt <- cv(MdonativosEnt)
CV_MremesasTot <- cv(MremesasTot)
CV_MremesasEnt <- cv(MremesasEnt)
CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobEnt <- cv(Mbene_gobEnt)
CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogEnt <- cv(Mtransf_hogEnt)
CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instEnt <- cv(Mtrans_instEnt)
CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquEnt <- cv(Mestim_alquEnt)
CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingEnt <- cv(Motros_ingEnt)

########## Limite inferior ##########
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,1]
LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,1]
LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,1]
LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,1]
LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,1]
LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasEnt <- confint(MrentasEnt,level=0.90)[,1]
LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,1]
LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,1]
LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferEnt <- confint(MtransferEnt,level=0.90)[,1]
LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1] 
LI_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,1]
LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasEnt <- confint(MbecasEnt,level=0.90)[,1]
LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,1]
LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasEnt <- confint(MremesasEnt,level=0.90)[,1]
LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,1]
LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,1]
LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,1]
LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,1]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,1]

########## Limite superior ##########
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,2]
LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,2]
LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,2]
LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,2]
LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,2]
LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasEnt <- confint(MrentasEnt,level=0.90)[,2]
LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,2]
LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,2]
LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferEnt <- confint(MtransferEnt,level=0.90)[,2]
LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,2]
LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasEnt <- confint(MbecasEnt,level=0.90)[,2]
LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,2]
LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasEnt <- confint(MremesasEnt,level=0.90)[,2]
LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,2]
LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,2]
LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,2]
LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,2]
LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,2]

##############################C R E A C I Ó N C U A D R O S ###############################.
##### ESTIMACIONES
c_ent_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corEnt),c(ES_MingtrabTot,ES_MingtrabEnt),c(ES_MtrabajoTot,ES_MtrabajoEnt),c(ES_MnegocioTot,ES_MnegocioEnt)
             ,c(ES_Motros_trabTot,ES_Motros_trabEnt),c(ES_MrentasTot,ES_MrentasEnt),c(ES_MutilidadTot,ES_MutilidadEnt)
             ,c(ES_MarrendaTot,ES_MarrendaEnt)
             ,c(ES_MtransferTot,ES_MtransferEnt),c(ES_MjubilacionTot,ES_MjubilacionEnt),c(ES_MbecasTot,ES_MbecasEnt),
             c(ES_MdonativosTot,ES_MdonativosEnt)
             ,c(ES_MremesasTot,ES_MremesasEnt),c(ES_Mbene_gobTot,ES_Mbene_gobEnt),c(ES_Mtransf_hogTot,ES_Mtransf_hogEnt),c(ES_Mtrans_instTot,ES_Mtrans_instEnt)
             ,c(ES_Mestim_alquTot,ES_Mestim_alquEnt),c(ES_Motros_ingTot,ES_Motros_ingEnt))
##### ERROR ESTANDAR
c_ent_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corEnt),c(SE_MingtrabTot,SE_MingtrabEnt),c(SE_MtrabajoTot,SE_MtrabajoEnt),c(SE_MnegocioTot,SE_MnegocioEnt)
             ,c(SE_Motros_trabTot,SE_Motros_trabEnt),c(SE_MrentasTot,SE_MrentasEnt),c(SE_MutilidadTot,SE_MutilidadEnt)
             ,c(SE_MarrendaTot,SE_MarrendaEnt)
             ,c(SE_MtransferTot,SE_MtransferEnt),c(SE_MjubilacionTot,SE_MjubilacionEnt),c(SE_MbecasTot,SE_MbecasEnt),
             c(SE_MdonativosTot,SE_MdonativosEnt)
             ,c(SE_MremesasTot,SE_MremesasEnt),c(SE_Mbene_gobTot,SE_Mbene_gobEnt),c(SE_Mtransf_hogTot,SE_Mtransf_hogEnt),c(SE_Mtrans_instTot,SE_Mtrans_instEnt)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquEnt),c(SE_Motros_ingTot,SE_Motros_ingEnt))
##### COEFICIENTE DE VARIACION
c_ent_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corEnt),c(CV_MingtrabTot,CV_MingtrabEnt),c(CV_MtrabajoTot,CV_MtrabajoEnt),c(CV_MnegocioTot,CV_MnegocioEnt)
             ,c(CV_Motros_trabTot,CV_Motros_trabEnt),c(CV_MrentasTot,CV_MrentasEnt),c(CV_MutilidadTot,CV_MutilidadEnt
             ),c(CV_MarrendaTot,CV_MarrendaEnt)
             ,c(CV_MtransferTot,CV_MtransferEnt),c(CV_MjubilacionTot,CV_MjubilacionEnt),c(CV_MbecasTot,CV_MbecasEnt)
             ,c(CV_MdonativosTot,CV_MdonativosEnt)
             ,c(CV_MremesasTot,CV_MremesasEnt),c(CV_Mbene_gobTot,CV_Mbene_gobEnt),c(CV_Mtransf_hogTot,CV_Mtransf_hogEnt),c(CV_Mtrans_instTot,CV_Mtrans_instEnt)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquEnt),c(CV_Motros_ingTot,CV_Motros_ingEnt))
##### LIMITE INFERIOR AL 90%
c_ent_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corEnt),c(LI_MingtrabTot,LI_MingtrabEnt),c(LI_MtrabajoTot,LI_MtrabajoEnt),
             c(LI_MnegocioTot,LI_MnegocioEnt)
             ,c(LI_Motros_trabTot,LI_Motros_trabEnt),c(LI_MrentasTot,LI_MrentasEnt),c(LI_MutilidadTot,LI_MutilidadEnt),c(LI_MarrendaTot,LI_MarrendaEnt)
             ,c(LI_MtransferTot,LI_MtransferEnt),c(LI_MjubilacionTot,LI_MjubilacionEnt),c(LI_MbecasTot,LI_MbecasEnt),c(LI_MdonativosTot,LI_MdonativosEnt)
             ,c(LI_MremesasTot,LI_MremesasEnt),c(LI_Mbene_gobTot,LI_Mbene_gobEnt),c(LI_Mtransf_hogTot,LI_Mtransf_hogEnt),c(LI_Mtrans_instTot,LI_Mtrans_instEnt)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquEnt),c(LI_Motros_ingTot,LI_Motros_ingEnt))

### LIMITE SUPERIOR AL 90%
c_ent_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corEnt),c(LS_MingtrabTot,LS_MingtrabEnt),c(LS_MtrabajoTot,LS_MtrabajoEnt),c(LS_MnegocioTot,LS_MnegocioEnt)
             ,c(LS_Motros_trabTot,LS_Motros_trabEnt),c(LS_MrentasTot,LS_MrentasEnt),c(LS_MutilidadTot,LS_MutilidadEnt),c
             (LS_MarrendaTot,LS_MarrendaEnt)
             ,c(LS_MtransferTot,LS_MtransferEnt),c(LS_MjubilacionTot,LS_MjubilacionEnt),c(LS_MbecasTot,LS_MbecasEnt),c(
               LS_MdonativosTot,LS_MdonativosEnt)
             ,c(LS_MremesasTot,LS_MremesasEnt),c(LS_Mbene_gobTot,LS_Mbene_gobEnt),c(LS_Mtransf_hogTot,LS_Mtransf_hogEnt),c(LS_Mtrans_instTot,LS_Mtrans_instEnt)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquEnt),c(LS_Motros_ingTot,LS_Motros_ingEnt))

# se agregan los nombres de las entidades a las filas
row.names(c_ent_ES)<-row.names(c_ent_SE)<-row.names(c_ent_CV)<-row.names(c_ent_LI)<-
  row.names(c_ent_LS)<-Entidades

# se renombran las variables
names(c_ent_ES)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS",
                  "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENE
GOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_SE)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS",
                  "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENE
GOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_CV)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS",
                  "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENE
GOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_LI)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS",
                  "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENE
GOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS",
                  "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENE
GOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variación redondea a 4
# decimales y luego multiplica por cien.

# Mostramos el resultado en pantalla #####
round(c_ent_ES)
round(c_ent_SE)
round(c_ent_CV,4)*100
round(c_ent_LI)
round(c_ent_LS)

# 2.3 Promedio del gasto corriente monetario trimestral -------------------
# por entidad federativa y grandes rubros del gasto

# A continuación, se presenta el código que calcula el promedio de los 
# grandes rubros de gasto por entidad federativa. Al igual que en el cuadro2,
# después de ejecutar las estimaciones, se calculan sus errores estándar, sus
# coeficientes de variación, los límites inferiores de sus intervalos de 
# confianza y los correspondientes límites de confianza.

# Cuadro3
# Promedio del gasto corriente monetario trimestral por entidad federativa y grandes rubros del gasto
# 2018
# carga lista de librerías que necesitaremos

library(foreign) # librería que nos ayuda a leer las tablas en diferentes formatos
library(survey) # librería para calcular el diseño muestral

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

# establece el directorio donde se encuentran nuestras bases de datos
setwd("D:/ENIGH NS 2018/BASE DE DATOS/DBF")

# abrimos la tabla concentradohogar
Conc<- read.dbf("concentradohogar.dbf",as.is = T)

# seleccionamos solo las variables de interes para realizar nuestros cálculos
Conc <- Conc [ c("folioviv", "foliohog", "tot_integ","gasto_mon", "alimentos", "vesti_calz", "vivienda", "limpieza",
                 "salud", "transporte", "educa_espa", "personales", "transf_gas","factor","upm","est_dis")]

# se crea una variable para agragar la entidad federativa
Conc$entidad <-substr(Conc$folioviv,1,2)

# se define la columna con el nombre de las entidades federativas
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur",
             "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
             "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
             "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
             "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

#se crea una bandera para numerar a los hogares
Conc$Nhog <- 1

#se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)

## Se comienzan a preparar las variables para su explotación ##

##### GASTO CORRIENTE MONETARIO
M_gasto_monTot <- svyratio(~gasto_mon,denominator=~Nhog,mydesign)#Total promedio
M_gasto_monEnt <- svyby(~gasto_mon,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN ALIMENTOS
M_alimentosTot <- svyratio(~alimentos,denominator=~Nhog,mydesign)#Total promedio
M_alimentosEnt <- svyby(~alimentos,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN VESTIDO Y CALZADO
M_vesti_calzTot <- svyratio(~vesti_calz,denominator=~Nhog,mydesign)#Total promedio
M_vesti_calzEnt <- svyby(~vesti_calz,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN VIVIENDA Y SERVICIOS DE CONSERVACIÓN
M_viviendaTot <- svyratio(~vivienda,denominator=~Nhog,mydesign)#Total promedio
M_viviendaEnt<-svyby(~vivienda,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN ARTÍCULOS Y SERVICIOS PARA LA LIMPIEZA
M_limpiezaTot <- svyratio(~limpieza,denominator=~Nhog,mydesign)#Total promedio
M_limpiezaEnt<-svyby(~limpieza,denominator=~Nhog,by=~entidad,mydesign,svyratio)#Nacional promedio

##### GASTO EN CUIDADOS DE LA SALUD
M_saludTot <- svyratio(~salud,denominator=~Nhog,mydesign)#Total promedio
M_saludEnt <- svyby(~salud,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN TRANSPORTE
M_transporteTot <- svyratio(~transporte,denominator=~Nhog,mydesign)#Total promedio
M_transporteEnt <- svyby(~transporte,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### GASTO EN SERVICIOS DE EDUCACIÓN
M_educa_espaTot <- svyratio(~educa_espa,denominator=~Nhog,mydesign)#Total promedio
M_educa_espaEnt<-svyby(~educa_espa,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional
promedio

##### GASTO EN CUIDADOS PERSONALES
M_personalesTot <- svyratio(~personales,denominator=~Nhog,mydesign)#Total promedio
M_personalesEnt <- svyby(~personales,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

##### TRANSFERENCIAS DE GASTO
M_transf_gasTot <- svyratio(~transf_gas,denominator=~Nhog,mydesign)#Total promedio
M_transf_gasEnt <- svyby(~transf_gas,denominator=~Nhog,by=~entidad ,mydesign,svyratio)#Nacional promedio

########################## E S T A D Í S T I C O S ##########################
########## Promedios ##########
ES_M_gasto_monTot<-M_gasto_monTot[[1]]
ES_M_gasto_monEnt<-M_gasto_monEnt[[2]]
ES_M_alimentosTot<-M_alimentosTot[[1]]
ES_M_alimentosEnt<-M_alimentosEnt[[2]]
ES_M_vesti_calzTot<-M_vesti_calzTot[[1]]
ES_M_vesti_calzEnt<-M_vesti_calzEnt[[2]]
ES_M_viviendaTot<-M_viviendaTot[[1]]
ES_M_viviendaEnt<-M_viviendaEnt[[2]]
ES_M_limpiezaTot<-M_limpiezaTot[[1]]
ES_M_limpiezaEnt<-M_limpiezaEnt[[2]]
ES_M_saludTot<-M_saludTot[[1]]
ES_M_saludEnt<-M_saludEnt[[2]]
ES_M_transporteTot<-M_transporteTot[[1]]
ES_M_transporteEnt<-M_transporteEnt[[2]]
ES_M_educa_espaTot<-M_educa_espaTot[[1]]
ES_M_educa_espaEnt<-M_educa_espaEnt[[2]]
ES_M_personalesTot<-M_personalesTot[[1]]
ES_M_personalesEnt<-M_personalesEnt[[2]]
ES_M_transf_gasTot<-M_transf_gasTot[[1]]
ES_M_transf_gasEnt<-M_transf_gasEnt[[2]]

########## Error Estandar ##########
SE_M_gasto_monTot<- SE(M_gasto_monTot)
SE_M_gasto_monEnt<- SE(M_gasto_monEnt)
SE_M_alimentosTot<- SE(M_alimentosTot)
SE_M_alimentosEnt<- SE(M_alimentosEnt)
SE_M_vesti_calzTot<- SE(M_vesti_calzTot)
SE_M_vesti_calzEnt<- SE(M_vesti_calzEnt)
SE_M_viviendaTot<- SE(M_viviendaTot)
SE_M_viviendaEnt<- SE(M_viviendaEnt)
SE_M_limpiezaTot<- SE(M_limpiezaTot)
SE_M_limpiezaEnt<- SE(M_limpiezaEnt)
SE_M_saludTot<- SE(M_saludTot)
SE_M_saludEnt<- SE(M_saludEnt)
SE_M_transporteTot<- SE(M_transporteTot)
SE_M_transporteEnt<- SE(M_transporteEnt)
SE_M_educa_espaTot<- SE(M_educa_espaTot)
SE_M_educa_espaEnt<- SE(M_educa_espaEnt)
SE_M_personalesTot<- SE(M_personalesTot)
SE_M_personalesEnt<- SE(M_personalesEnt)
SE_M_transf_gasTot<- SE(M_transf_gasTot)
SE_M_transf_gasEnt<- SE(M_transf_gasEnt)

########## coeficiente de Variación ##########
CV_M_gasto_monTot<- cv( M_gasto_monTot)
CV_M_gasto_monEnt<- cv( M_gasto_monEnt)
CV_M_alimentosTot<- cv(M_alimentosTot)
CV_M_alimentosEnt<- cv(M_alimentosEnt)
CV_M_vesti_calzTot<- cv( M_vesti_calzTot)
CV_M_vesti_calzEnt<- cv( M_vesti_calzEnt)
CV_M_viviendaTot<- cv(M_viviendaTot)
CV_M_viviendaEnt<- cv(M_viviendaEnt)
CV_M_limpiezaTot<- cv(M_limpiezaTot)
CV_M_limpiezaEnt<- cv(M_limpiezaEnt)
CV_M_saludTot<- cv(M_saludTot)
CV_M_saludEnt<- cv(M_saludEnt)
CV_M_transporteTot<- cv(M_transporteTot)
CV_M_transporteEnt<- cv(M_transporteEnt)
CV_M_educa_espaTot<- cv(M_educa_espaTot)
CV_M_educa_espaEnt<- cv(M_educa_espaEnt)
CV_M_personalesTot<- cv(M_personalesTot)
CV_M_personalesEnt<- cv(M_personalesEnt)
CV_M_transf_gasTot<- cv(M_transf_gasTot)
CV_M_transf_gasEnt<- cv(M_transf_gasEnt)

########## Limite inferior ##########
LI_M_gasto_monTot<- confint( M_gasto_monTot,level=0.90)[,1]
LI_M_gasto_monEnt<- confint( M_gasto_monEnt,level=0.90)[,1]
LI_M_alimentosTot<- confint(M_alimentosTot,level=0.90)[,1]
LI_M_alimentosEnt<- confint(M_alimentosEnt,level=0.90)[,1]
LI_M_vesti_calzTot<- confint( M_vesti_calzTot,level=0.90)[,1]
LI_M_vesti_calzEnt<- confint( M_vesti_calzEnt,level=0.90)[,1]
LI_M_viviendaTot<- confint(M_viviendaTot,level=0.90)[,1]
LI_M_viviendaEnt<- confint(M_viviendaEnt,level=0.90)[,1]
LI_M_limpiezaTot<- confint(M_limpiezaTot,level=0.90)[,1]
LI_M_limpiezaEnt<- confint(M_limpiezaEnt,level=0.90)[,1]
LI_M_saludTot<- confint(M_saludTot,level=0.90)[,1]
LI_M_saludEnt<- confint(M_saludEnt,level=0.90)[,1]
LI_M_transporteTot<- confint(M_transporteTot,level=0.90)[,1]
LI_M_transporteEnt<- confint(M_transporteEnt,level=0.90)[,1]
LI_M_educa_espaTot<- confint(M_educa_espaTot,level=0.90)[,1]
LI_M_educa_espaEnt<- confint(M_educa_espaEnt,level=0.90)[,1]
LI_M_personalesTot<- confint(M_personalesTot,level=0.90)[,1]
LI_M_personalesEnt<- confint(M_personalesEnt,level=0.90)[,1]
LI_M_transf_gasTot<- confint(M_transf_gasTot,level=0.90)[,1]
LI_M_transf_gasEnt<- confint(M_transf_gasEnt,level=0.90)[,1]

########## Limite superior ##########
LS_M_gasto_monTot<- confint( M_gasto_monTot,level=0.90)[,2]
LS_M_gasto_monEnt<- confint( M_gasto_monEnt,level=0.90)[,2]
LS_M_alimentosTot<- confint(M_alimentosTot,level=0.90)[,2]
LS_M_alimentosEnt<- confint(M_alimentosEnt,level=0.90)[,2]
LS_M_vesti_calzTot<- confint( M_vesti_calzTot,level=0.90)[,2]
LS_M_vesti_calzEnt<- confint( M_vesti_calzEnt,level=0.90)[,2]
LS_M_viviendaTot<- confint(M_viviendaTot,level=0.90)[,2]
LS_M_viviendaEnt<- confint(M_viviendaEnt,level=0.90)[,2]
LS_M_limpiezaTot<- confint(M_limpiezaTot,level=0.90)[,2]
LS_M_limpiezaEnt<- confint(M_limpiezaEnt,level=0.90)[,2]
LS_M_saludTot<- confint(M_saludTot,level=0.90)[,2]
LS_M_saludEnt<- confint(M_saludEnt,level=0.90)[,2]
LS_M_transporteTot<- confint(M_transporteTot,level=0.90)[,2]
LS_M_transporteEnt<- confint(M_transporteEnt,level=0.90)[,2]
LS_M_educa_espaTot<- confint(M_educa_espaTot,level=0.90)[,2]
LS_M_educa_espaEnt<- confint(M_educa_espaEnt,level=0.90)[,2]
LS_M_personalesTot<- confint(M_personalesTot,level=0.90)[,2]
LS_M_personalesEnt<- confint(M_personalesEnt,level=0.90)[,2]
LS_M_transf_gasTot<- confint(M_transf_gasTot,level=0.90)[,2]
LS_M_transf_gasEnt<- confint(M_transf_gasEnt,level=0.90)[,2]

########### C U A D R O S ################
##### Estimaciones
c_gas_ES<-
  data.frame(c(ES_M_gasto_monTot,ES_M_gasto_monEnt),c(ES_M_alimentosTot,ES_M_alimentosEnt),c(ES_M_vesti_calzTot,ES_M_vesti_calzEnt),c(ES_M_viviendaTot,ES_M_viviendaEnt)
             ,c(ES_M_limpiezaTot,ES_M_limpiezaEnt),c(ES_M_saludTot,ES_M_saludEnt),c(ES_M_transporteTot,ES_M_transporteEnt),c(ES_M_educa_espaTot,ES_M_educa_espaEnt),c(ES_M_personalesTot,ES_M_personalesEnt),c(ES_M_transf_gasTot,ES_M_transf_gasEnt))

##### Error Estándar
c_gas_SE<-
  data.frame(c(SE_M_gasto_monTot,SE_M_gasto_monEnt),c(SE_M_alimentosTot,SE_M_alimentosEnt),c(SE_M_vesti_calzTot,SE_M_vesti_calzEnt),c(SE_M_viviendaTot,SE_M_viviendaEnt)
             ,c(SE_M_limpiezaTot,SE_M_limpiezaEnt),c(SE_M_saludTot,SE_M_saludEnt),c(SE_M_transporteTot,
                                                                                    SE_M_transporteEnt),c(SE_M_educa_espaTot,SE_M_educa_espaEnt)
             ,c(SE_M_personalesTot,SE_M_personalesEnt),c(SE_M_transf_gasTot,SE_M_transf_gasEnt))

##### Coeficiente de variación
c_gas_CV<-
  data.frame(c(CV_M_gasto_monTot,CV_M_gasto_monEnt),c(CV_M_alimentosTot,CV_M_alimentosEnt),c(CV_M_vesti_calzTot,CV_M_vesti_calzEnt),c(CV_M_viviendaTot,CV_M_viviendaEnt)
             ,c(CV_M_limpiezaTot,CV_M_limpiezaEnt),c(CV_M_saludTot,CV_M_saludEnt),c(CV_M_transporteTot
                                                                                    ,CV_M_transporteEnt),c(CV_M_educa_espaTot,CV_M_educa_espaEnt)
             ,c(CV_M_personalesTot,CV_M_personalesEnt),c(CV_M_transf_gasTot,CV_M_transf_gasEnt))

##### Limite inferior
c_gas_LI<-
  data.frame(c(LI_M_gasto_monTot,LI_M_gasto_monEnt),c(LI_M_alimentosTot,LI_M_alimentosEnt),c(LI_M_vesti_calzTot,LI_M_vesti_calzEnt),c(LI_M_viviendaTot,LI_M_viviendaEnt)
             ,c(LI_M_limpiezaTot,LI_M_limpiezaEnt),c(LI_M_saludTot,LI_M_saludEnt),c(LI_M_transporteTot,LI_M_transporteEnt),c(LI_M_educa_espaTot,LI_M_educa_espaEnt)
             ,c(LI_M_personalesTot,LI_M_personalesEnt),c(LI_M_transf_gasTot,LI_M_transf_gasEnt))

##### Limite superior
c_gas_LS<-
  data.frame(c(LS_M_gasto_monTot,LS_M_gasto_monEnt),c(LS_M_alimentosTot,LS_M_alimentosEnt),c(LS_M_vesti_calzTot,LS_M_vesti_calzEnt),c(LS_M_viviendaTot,LS_M_viviendaEnt)
             ,c(LS_M_limpiezaTot,LS_M_limpiezaEnt),c(LS_M_saludTot,LS_M_saludEnt),c(LS_M_transporteTot,LS_M_transporteEnt),c(LS_M_educa_espaTot,LS_M_educa_espaEnt)
             ,c(LS_M_personalesTot,LS_M_personalesEnt),c(LS_M_transf_gasTot,LS_M_transf_gasEnt))

# se renombran las variables
names(c_gas_ES)=c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA", "SALUD",
                  "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
names(c_gas_SE)=c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA", "SALUD",
                  "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
names(c_gas_CV)=c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA", "SALUD",
                  "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
names(c_gas_LI)=c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA", "SALUD",
                  "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
names(c_gas_LS)=c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA", "SALUD",
                  "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")

# se agregan los nombres de las entidades a las filas
names=(row.names(c_gas_ES)<-row.names(c_gas_SE)<-row.names(c_gas_CV)<-row.names(c_gas_LI)<-
         row.names(c_gas_LS)<-Entidades)

##### Mostramos el resultado en pantalla #####
round(c_gas_ES)
round(c_gas_SE)
round(c_gas_CV,4)*100
round(c_gas_LI)
round(c_gas_LS)
