setwd("/home/saul/Documents/FAVAR")
source("./Codigo/source.R")
usePackage("ggthemes")
usePackage("x13binary") 
usePackage("seasonal")  
usePackage("tseries")   
usePackage("dplyr") 
usePackage("forecast")  
usePackage("readr")
usePackage("ggplot2")
usePackage("tibble")
usePackage("vars")
usePackage("tidyr")
##### Estados Unidos: Actividad Real ####
#Real Gross Domestic Product, at 2009 prices
US_GDP <- read_csv("./Datos Desestacionalizados/GDP IFS (USA).csv")[164:255,2] %>% 
  estandarizacion(freq = 4, s=c(1990,4)) 
#Gross National Income
US_GNI <-read_csv("./Datos Desestacionalizados/GNI IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#Personal Consumption Expenditures: Services: Household Consumption Expenditures
US_HCE <- read_csv("./Datos Desestacionalizados/HCE IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4, s =c(1990,3),n=1)
#Real Government Consumption Expenditures and Gross Investment
US_GCE <-read_csv("./Datos Desestacionalizados/GCE IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#Exports of Goods and Services
US_EXP <- read_csv("./Datos Desestacionalizados/EXP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Imports of Goods and Services
US_IMP <- read_csv("./Datos Desestacionalizados/IMP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Industrial Production: Total Index
US_INDP <- read_csv("./Datos Desestacionalizados/INDP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Non-durable Manufacturing and Crude Petroleum Products
US_PETRP<- read_csv("./Datos/PETRP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),des=TRUE,n=1)
#Employment Rate: Aged 15-64: All persons for the United States
US_EMP <- read_csv("./Datos Desestacionalizados/EMP FRED (USA).csv")[55:147,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#Componentes Principales
Real_US     <- as_tibble(cbind(US_GDP,US_GNI,US_HCE,US_GCE,US_EXP,US_INDP,US_PETRP,US_IMP,US_EMP))              #Crea matriz con componentes monetarios
Real_US_PC  <- prcomp(Real_US)                                      #Obtiene los Componentes Principales
Real_US_PCP <- Real_US_PC %>% predict %>% as_tibble                 #Obtiene los valores predecidos de los Componentes Principales
Real_US_COR <-as_tibble(t(cor(Real_US_PCP,Real_US)))                #Matriz de correlaciones con componentes principales
Real_US_PCP <- Real_US_PCP %>%                                      #Modificamos la matriz
  mutate(Date = US_GDP %>% time)                                       #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Real_US_PCP,aes(y=PC1,x=Date))+                              #Grafica x:Años, y=Prediccion                                       
  geom_line(color = 'blue')+                                        #Tipo de grafica: linea
  labs(title="Actividad Real, EE.UU.",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist() 
#Elimina variables
rm(US_GDP,US_GNI,US_HCE,US_GCE,US_EXP,US_IMP,US_INDP,US_EMP,US_PETRP)
##### Estados Unidos: Inflacion ####
#Consumer Price Index: Total All Items for the United States
US_CPI <- read_csv("./Datos/CPI IFS (USA).csv")[133:227,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),des=TRUE)
#Gross Domestic Product: Implicit Price deflator
US_GDPDEF <-read_csv("./Datos Desestacionalizados/GDPDEF IFS (USA).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),n=1)
#Producer Price Index for All Commodities
US_PPI <- read_csv("./Datos/PPI IFS (USA).csv")[133:227,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE)
#Import Price, All Commodities
US_IMPPRICE <- read_csv("./Datos/IMPPRICE IFS (USA).csv")[133:227,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE)
#S&P Index
US_SP <- read_csv("./Datos/SP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq=4,s=c(1990,3),des=TRUE,n=1)
#Real Residential Property Prices for United States
US_RHP <- read_csv("./Datos/RHP FRED (USA).csv")[58:152,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE, n=1)
#US_WAGES
US_WAGES <- read_csv("./Datos Desestacionalizados/WRATES FRED (USA).csv")[313:598,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),n=1)
#Componentes Principales
Inf_US     <- as_tibble(cbind(US_CPI,US_GDPDEF,US_PPI,US_IMPPRICE,US_SP,US_RHP,US_WAGES))    #Crea matriz con componentes monetarios
Inf_US_PC  <- prcomp(Inf_US)                                      #Obtiene los Componentes Principales
Inf_US_PCP <- Inf_US_PC %>% predict %>% as_tibble                 #Obtiene los valores predecidos de los Componentes Principales
Inf_US_COR <-as_tibble(t(cor(Inf_US_PCP,Inf_US)))                 #Matriz de correlaciones con componentes principales
Inf_US_PCP <- Inf_US_PCP %>%                                      #Modificamos la matriz
  mutate(Date = time(US_CPI))                                     #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Inf_US_PCP,aes(y=-PC1,x=Date))+                             #Grafica x:Años, y=Prediccion
  geom_line(color= 'blue')+                                       #Tipo de grafica: linea
  labs(title="Inflación, EE.UU.",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist()
#Elimina variables
rm(US_CPI,US_GDPDEF,US_PPI,US_RHP,US_IMPPRICE,US_SP,US_WAGES)
##### Estados Unidos: Crecimiento del Dinero ####
#Base Money 
US_MB <- read_csv("./Datos Desestacionalizados/MB IFS (USA).csv")[42:135,2] %>% 
  estandarizacion(freq=4,s=c(1990,2),n=2)      
#M1
US_M1 <- read_csv("./Datos Desestacionalizados/M1 IFS (USA).csv")[126:219,2] %>% 
  estandarizacion(freq=4,s=c(1990,2))   
#M2
US_M2 <- read_csv("./Datos Desestacionalizados/M2 IFS (USA).csv")[126:219,2] %>% 
  estandarizacion(freq=4,s=c(1990,2)) 
#M3
US_M3 <- read_csv("./Datos Desestacionalizados/M3 FRED (USA).csv")[379:657,2] %>% 
  estandarizacion(freq=12,s=c(1990,7))  
#Componentes Principales
Money_US     <- as_tibble(cbind(US_MB,US_M1,US_M2))              #Crea matriz con componentes monetarios
Money_US_PC  <- prcomp(Money_US)                                       #Obtiene los Componentes Principales
Money_US_PCP <- Money_US_PC %>% predict %>% as_tibble  #Obtiene los valores predecidos de los Componentes Principales
Money_US_COR <-as_tibble(t(cor(Money_US_PCP,Money_US)))                #Matriz de correlaciones con componentes principales
Money_US_PCP <- Money_US_PCP %>%                                       #Modificamos la matriz
  mutate(Date = time(US_MB))                                           #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Money_US_PCP,aes(y=PC1,x=Date))+                                #Grafica x:Años, y=Prediccion
  geom_line(color= 'blue')+                                            #Tipo de grafica: linea
  labs(title="Crecimiento del Dinero, EE.UU.",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist()
#Elimina variables
rm(US_MB,US_M1,US_M2,US_M3)
##### Estados Unidos: Tasa de Interes ####
#Bank Prime Loan Rate
#US_BPLRATE <-read_csv("./Datos/BPLRATE FRED (USA).csv")[499:780,2] %>% 
#  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
#Interest Rates, Central Bank Rate
US_CBRATE <- read_csv("./Datos/CBRATE IFS (USA).csv")[32:125,2] %>% 
  estandarizacion(freq = 4,s=c(1990,2),int =TRUE,des = TRUE,n=1) %>% 
  as_tibble
#Interest Rates, Discount Rate for United States
#US_DRATE <- read_csv("./Datos/DRATE FRED (USA).csv")[487:768,2] %>% 
#  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
#Effective Federal Funds Rate
#US_FFRATE <- read_csv("./Datos/FFRATE FRED (USA).csv")[433:715,2] %>% 
#  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
# Interest Rates, Government Securities, Treasury Bills for United States
#US_TBRATE <- read_csv("./Datos/TBRATE FRED (USA).csv")[487:766,2] %>% 
#  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE,n=1)
#Componentes Principales
#Int_US     <- as_tibble(cbind(US_BPLRATE,US_CBRATE,US_DRATE,US_FFRATE,US_TBRATE))     #Crea matriz con componentes monetarios
#Int_US_PC  <- prcomp(Int_US)                                                #Obtiene los Componentes Principales
#Int_US_PCP <- as_tibble(t(t(Int_US_PC$rotation) %*%t(Int_US)))              #Obtiene los valores predecidos de los Componentes Principales
#Int_US_COR <-as_tibble(t(cor(Int_US_PCP,Int_US)))                           #Matriz de correlaciones con componentes principales
#Int_US_PCP <- Int_US_PCP %>%                                                #Modificamos la matriz
#  mutate(Date = time(US_BPLRATE))                                           #Agrega la fecha
#Grafica de la prediccion de componentes principales
#ggplot(Int_US_PCP,aes(y=PC1,x=Date))+                                #Grafica x:Años, y=Prediccion
#  geom_line(color= 'blue')+                                            #Tipo de grafica: linea
#  labs(title="Tasa de Interés, EE.UU.",x="",y="",subtitle="Primer Componente Principal")+
#  theme_economist()
#Elimina variables
#rm(US_BPLRATE,US_DRATE,US_FFRATE,US_TBRATE,US_CBRATE)

##### Mexico: Actividad Real ####
#Gross Domestic Product, Real
MX_GDP <- read_csv("./Datos Desestacionalizados/GDP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),1)
#Indice General de Actividad Economica
MX_IGAE <- read_csv("./Datos/IGAE.csv")[1:249,1] %>% 
  estandarizacion(freq = 12,s=c(1993,1),des=TRUE,n=1)
#Household Consumption Expenditure
MX_HCE <- read_csv("./Datos Desestacionalizados/HCE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s =c(1990,1))
#Real Government Consumption Expenditures and Gross Investment
MX_GCE <- read_csv("./Datos Desestacionalizados/GCE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1))
#Exports of Goods and Services
MX_EXP <- read_csv("./Datos Desestacionalizados/EXP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1))
#Industrial Production: Total Index
MX_INDP <- read_csv("./Datos/INDP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),des = TRUE)
#Manufacturing
MX_MANUFP <- read_csv("./Datos/MANUFP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),des = TRUE)
#Non-durable Manufacturing and Crude Petroleum Products
MX_PETRP <- read_csv("./Datos/PETRP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),des = TRUE)
#Mining
MX_MINP <- read_csv("./Datos/MINP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),des = TRUE)
#Componentes Principales
Real_MX     <- as_tibble(cbind(MX_GDP,MX_HCE,MX_GCE,MX_EXP,MX_INDP,MX_MANUFP,MX_MINP,MX_PETRP))              #Crea matriz con componentes monetarios
Real_MX_PC  <- prcomp(Real_MX)                                      #Obtiene los Componentes Principales
Real_MX_PCP <- Real_MX_PC %>% predict %>% as_tibble    #Obtiene los valores predecidos de los Componentes Principales
Real_MX_COR <-as_tibble(t(cor(Real_MX_PCP,Real_MX)))                #Matriz de correlaciones con componentes principales
Real_MX_PCP <- Real_MX_PCP %>%                                      #Modificamos la matriz
  mutate(Date = time(MX_GDP))                                   #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Real_MX_PCP,aes(y=-PC1,x=Date))+                              #Grafica x:Años, y=Prediccion                                       
  geom_line(color = 'blue')+                                        #Tipo de grafica: linea
  labs(title="Actividad Real, Mexico",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist() 
#Elimina variables
rm(MX_GDP,MX_IGAE,MX_HCE,MX_GCE,MX_EXP,MX_INDP,MX_MANUFP,MX_MINP,MX_PETRP)
##### Mexico: Inflacion ####
#Consumer Prices, All Items
MX_CPI <- read_csv("./Datos/CPI IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),des=TRUE,n=1)
#autoplot(MX_CPI)
#Gross Domestic Product: Implicit Price Deflator
MX_GDPDEF <- read_csv("./Datos Desestacionalizados/GDPDEF IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),n=1)
#autoplot(MX_GDPDEF)
#Producer Price Index for All Commodities
MX_PPI <- read_csv("./Datos/PPI IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE,n=1)
#autoplot(MX_PPI)
#Import Price, All Commodities,
MX_IMPPRICE <- read_csv("./Datos/IMPPRICE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE,n=1)
#autoplot(MX_IMPPRICE)
#Indice de Precios y Cotizaciones
MX_IPC <- read_csv("./Datos/IPC IFS (MEX).csv")[1:285,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),des=TRUE,n=1)
#MX_WAGES
MX_WAGES <- read_csv("./Datos/WAGES IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1)
#autoplot(MX_WAGES)
#Componentes Principales
Inf_MX     <- as_tibble(cbind(MX_CPI,MX_GDPDEF,MX_PPI,MX_IMPPRICE,MX_IPC,MX_WAGES))    #Crea matriz con componentes monetarios
Inf_MX_PC  <- prcomp(Inf_MX)                                      #Obtiene los Componentes Principales
Inf_MX_PCP <- Inf_MX_PC%>% predict %>% as_tibble                  #Obtiene los valores predecidos de los Componentes Principales
Inf_MX_COR <-as_tibble(t(cor(Inf_MX_PCP,Inf_MX)))                 #Matriz de correlaciones con componentes principales
Inf_MX_PCP <- Inf_MX_PCP %>%                                      #Modificamos la matriz
  mutate(Date = time(MX_CPI))                                     #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Inf_MX_PCP,aes(y=PC1,x=Date))+                             #Grafica x:Años, y=Prediccion
  geom_line(color= 'blue')+                                       #Tipo de grafica: linea
  labs(title="Inflación, Mexico",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist()
#Elimina variables
rm(MX_CPI,MX_GDPDEF,MX_PPI,MX_IMPPRICE,MX_IPC,MX_WAGES)
##### Mexico: Crecimiento del Dinero ####
#M1
MX_M1 <- read_csv("./Datos/Money IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1) 
#M2
MX_M2 <- read_csv("./Datos/Money IFS (MEX).csv")[1:95,3] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1) 
#M3
MX_M3 <- read_csv("./Datos/Money IFS (MEX).csv")[1:95,4] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1) 
#M4
MX_M4 <- read_csv("./Datos/Money IFS (MEX).csv")[1:95,5] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1) 
#Componentes Principales
Money_MX     <- as_tibble(cbind(MX_M1,MX_M2,MX_M3,MX_M4))        #Crea matriz con componentes monetarios
Money_MX_PC  <- prcomp(Money_MX)                                       #Obtiene los Componentes Principales
Money_MX_PCP <- Money_MX_PC %>% predict %>% as_tibble   #Obtiene los valores predecidos de los Componentes Principales
Money_MX_COR <- as_tibble(t(cor(Money_MX_PCP,Money_MX)))                #Matriz de correlaciones con componentes principales
Money_MX_PCP <- Money_MX_PCP %>%                                       #Modificamos la matriz
  mutate(Date = time(MX_M1))                                           #Agrega la fecha
#Grafica de la prediccion de componentes principales
ggplot(Money_MX_PCP,aes(y=PC1,x=Date))+                                #Grafica x:Años, y=Prediccion
  geom_line(color= 'blue')+                                            #Tipo de grafica: linea
  labs(title="Crecimiento del Dinero, Mexico",x="",y="",subtitle="Primer Componente Principal")+
  theme_economist()
#Elimina variables
rm(MX_M1,MX_M2,MX_M3,MX_M4)
##### Mexico: Tasa de Interes ####
#Interest Rates, Lending Rate
#MX_LRATE <- read_csv("./Datos/LRATE IFS (MEX).csv")[1:80,2] %>% 
#  estandarizacion(freq = 4,s=c(1993,4),int =TRUE,des = TRUE,n=1) %>% 
#  as_tibble
#Interest Rates, Money Market Rate
#MX_MMRATE <- read_csv("./Datos/MMRATE IFS (MEX).csv")[1:95,2] %>% 
#  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE)
#Interest Rates, Deposit Rate
#MX_DRATE <- read_csv("./Datos/DRATE IFS (MEX).csv")[1:95,2]   %>% 
#  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE)
#Interest Rates, Government Securities, Treasury Bills
MX_TBRATE <- read_csv("./Datos/TBRATE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE) %>% 
  as_tibble
#Componentes Principales
#Int_MX      <- as_tibble(cbind(MX_MMRATE,MX_DRATE,MX_TBRATE)) #Crea matriz con componentes monetarios
#Int_MX_PC  <- prcomp(Int_MX)                                      #Obtiene los Componentes Principales
#Int_MX_PCP <- as_tibble(t(t(Int_MX_PC$rotation) %*%t(Int_MX)))   #Obtiene los valores predecidos de los Componentes Principales
#Int_MX_COR <-as_tibble(t(cor(Int_MX_PCP,Int_MX)))                #Matriz de correlaciones con componentes principales
#Int_MX_PCP <- Int_MX_PCP %>%                                      #Modificamos la matriz
#  mutate(Date = time(MX_DRATE))                                   #Agrega la fecha
##Grafica de la prediccion de componentes principales
#ggplot(Int_MX_PCP,aes(y=PC1,x=Date))+                              #Grafica x:Años, y=Prediccion                                       
#  geom_line(color = 'blue')+                                        #Tipo de grafica: linea
#  labs(title="Tasa de Interés, México",x="",y="",subtitle="Primer Componente Principal")+
#  theme_economist() 
#Elimina variables
#rm(MX_LRATE,MX_MMRATE,MX_DRATE,MX_TBRATE)
##### Tipo de Cambio ####
MXN_USD <- read_csv("./Datos/MXN_USD1.csv")[1:288,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),des=TRUE)   
autoplot(MXN_USD)+                                                                  
  geom_line(color = 'blue')+                                        
  labs(title="Tipo de Cambio",x="",y="",subtitle="Pesos por Dólar")+
  theme_economist() 
##### VAR ####
## Las variables se orden la de las más exógenas a las más endógenas
## Estados Unidos
### 1. Tasa de interés 
### 2. Crecimiento Monetario
### 3. Inflación 
### 4. Actividad Real
##  5. Variable dummy (Primer y segundo trimestre de 1995)
##  6. Tipo de cambio nominal peso/dólar
## México
### 7. Tasa de interés 
### 8. Crecimiento Monetario
### 9. Inflación 
### 10.Actividad Real
y <- Money_US_PCP %>% 
  dplyr::select(Date) %>% 
  mutate(Int_US      = scale(US_CBRATE$x),
         Money_US    = Money_US_PCP$PC1,
         Inf_US      = Inf_US_PCP$PC1,
         Real_US     = Real_US_PCP$PC1,
         Dummy       = if_else(Date==1995.00 | Date ==1995.25,1,0),
         Tipo_Cambio = MXN_USD,
         Int_MX      = scale(MX_TBRATE$x),
         Money_MX    = Money_MX_PCP$PC1,
         Inf_MX      = Inf_MX_PCP$PC1,
         Real_MX     = Real_MX_PCP$PC1)

model <- VAR(y[,2:ncol(y)])
##### Impulso respuesta ####
impulse  <- c("Int_US","Int_MX")
response <- c("Tipo_Cambio","Int_MX","Money_MX","Inf_MX","Real_MX")
nombres  <- c("Tipo de Cambio", "Tasa de Interés","Base Monetaria","Inflación","Actividad Real")

df <- get_irf(model,impulse,response,nombres)
  
ggplot(data=df %>% filter(shock==impulse[1]),aes(y=value,x=cons))+
  geom_line(colour="blue")+
  geom_line(aes(y=value_up),colour="red",linetype = "dashed")+
  geom_line(aes(y=value_low),colour="red",linetype = "dashed")+
  geom_ribbon(aes(x=cons,ymin=value_low,ymax=value_up),alpha=.25,show.legend=FALSE)+
  labs(title="Shock a la Tasa de Interés de E.U.A.",x="",y="",subtitle="Funciones Impulso-Respuesta")+
  facet_wrap(~nombres,ncol=2)+
  theme_economist() 

ggplot(data=df %>% filter(shock==impulse[2]),aes(y=value,x=cons))+
  geom_line(colour="blue")+
  geom_line(aes(y=value_up),colour="red",linetype = "dashed")+
  geom_line(aes(y=value_low),colour="red",linetype = "dashed")+
  geom_ribbon(aes(x=cons,ymin=value_low,ymax=value_up),alpha=.25,show.legend=FALSE)+
  labs(title="Shock a la Tasa de Interés de México",x="",y="",subtitle="Funciones Impulso-Respuesta")+
  facet_wrap(~nombres,ncol=2)+
  theme_economist() 


##### Descomposición de la varianza ####
var_dec <- get_fevd(model)
##### Predicción ####
pred <- get_pred(model)

ggplot(data=pred ,aes(y=pred_puntual,x=cons))+
  geom_line(colour="blue")+
  geom_line(aes(y=lower),colour="red",linetype = "dashed")+
  geom_line(aes(y=upper),colour="red",linetype = "dashed")+
  geom_ribbon(aes(x=cons,ymin=lower,ymax=upper),alpha=.25,show.legend=FALSE)+
  labs(title="Prediccion",x="",y="")+
  facet_wrap(~variable,ncol=2)+
  theme_economist() 
