setwd("/home/saul/Repos/FAVAR")
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
US_GDP <- read_csv("./Datos_Desestacionalizados/GDP IFS (USA).csv")[164:255,2] %>% 
  estandarizacion(freq = 4, s=c(1990,4)) 
#Gross National Income
US_GNI <-read_csv("./Datos_Desestacionalizados/GNI IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#Personal Consumption Expenditures: Services: Household Consumption Expenditures
US_HCE <- read_csv("./Datos_Desestacionalizados/HCE IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4, s =c(1990,3),n=1)
#Real Government Consumption Expenditures and Gross Investment
US_GCE <-read_csv("./Datos_Desestacionalizados/GCE IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#Exports of Goods and Services
US_EXP <- read_csv("./Datos_Desestacionalizados/EXP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Imports of Goods and Services
US_IMP <- read_csv("./Datos_Desestacionalizados/IMP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Industrial Production: Total Index
US_INDP <- read_csv("./Datos_Desestacionalizados/INDP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3))
#Non-durable Manufacturing and Crude Petroleum Products
US_PETRP<- read_csv("./Datos/PETRP IFS (USA).csv")[163:255,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),des=TRUE,n=1)
#Employment Rate: Aged 15-64: All persons for the United States
US_EMP <- read_csv("./Datos_Desestacionalizados/EMP FRED (USA).csv")[55:147,2] %>% 
  estandarizacion(freq = 4,s=c(1990,3),n=1)
#rm(US_GDP,US_GNI,US_HCE,US_GCE,US_EXP,US_IMP,US_INDP,US_EMP,US_PETRP)
##### Estados Unidos: Inflacion ####
#Consumer Price Index: Total All Items for the United States
US_CPI <- read_csv("./Datos/CPI IFS (USA).csv")[133:227,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),des=TRUE)
#Gross Domestic Product: Implicit Price deflator
US_GDPDEF <-read_csv("./Datos_Desestacionalizados/GDPDEF IFS (USA).csv")[1:95,2] %>% 
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
US_WAGES <- read_csv("./Datos_Desestacionalizados/WRATES FRED (USA).csv")[313:598,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),n=1)
#Elimina variables
#rm(US_CPI,US_GDPDEF,US_PPI,US_RHP,US_IMPPRICE,US_SP,US_WAGES)
##### Estados Unidos: Crecimiento del Dinero ####
#Base Money 
US_MB <- read_csv("./Datos_Desestacionalizados/MB IFS (USA).csv")[42:135,2] %>% 
  estandarizacion(freq=4,s=c(1990,2),n=2)      
#M1
US_M1 <- read_csv("./Datos_Desestacionalizados/M1 IFS (USA).csv")[126:219,2] %>% 
  estandarizacion(freq=4,s=c(1990,2))   
#M2
US_M2 <- read_csv("./Datos_Desestacionalizados/M2 IFS (USA).csv")[126:219,2] %>% 
  estandarizacion(freq=4,s=c(1990,2)) 
#M3
US_M3 <- read_csv("./Datos_Desestacionalizados/M3 FRED (USA).csv")[379:657,2] %>% 
  estandarizacion(freq=12,s=c(1990,7))  
#Elimina variables
#rm(US_MB,US_M1,US_M2,US_M3)
##### Estados Unidos: Tasa de Interes ####
#Bank Prime Loan Rate
US_BPLRATE <-read_csv("./Datos/BPLRATE FRED (USA).csv")[499:780,2] %>% 
  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
#Interest Rates, Central Bank Rate
US_CBRATE <- read_csv("./Datos/CBRATE IFS (USA).csv")[32:125,2] %>% 
  estandarizacion(freq = 4,s=c(1990,2),int =TRUE,des = TRUE,n=1) 
#Interest Rates, Discount Rate for United States
US_DRATE <- read_csv("./Datos/DRATE FRED (USA).csv")[487:768,2] %>% 
  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
#Effective Federal Funds Rate
US_FFRATE <- read_csv("./Datos/FFRATE FRED (USA).csv")[433:715,2] %>% 
  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE)
# Interest Rates, Government Securities, Treasury Bills for United States
US_TBRATE <- read_csv("./Datos/TBRATE FRED (USA).csv")[487:766,2] %>% 
  estandarizacion(freq = 12,s=c(1990,7),int =TRUE,des = TRUE,n=1)
#Elimina variables
#rm(US_BPLRATE,US_DRATE,US_FFRATE,US_TBRATE,US_CBRATE)
##### Mexico: Actividad Real ####
#Gross Domestic Product, Real
MX_GDP <- read_csv("./Datos_Desestacionalizados/GDP IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),1)
#Indice General de Actividad Economica
MX_IGAE <- read_csv("./Datos/IGAE.csv")[1:249,1] %>% 
  estandarizacion(freq = 12,s=c(1993,1),des=TRUE,n=1)
#Household Consumption Expenditure
MX_HCE <- read_csv("./Datos_Desestacionalizados/HCE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s =c(1990,1))
#Real Government Consumption Expenditures and Gross Investment
MX_GCE <- read_csv("./Datos_Desestacionalizados/GCE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1))
#Exports of Goods and Services
MX_EXP <- read_csv("./Datos_Desestacionalizados/EXP IFS (MEX).csv")[1:95,2] %>% 
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
#Elimina variables
#rm(MX_GDP,MX_IGAE,MX_HCE,MX_GCE,MX_EXP,MX_INDP,MX_MANUFP,MX_MINP,MX_PETRP)
##### Mexico: Inflacion ####
#Consumer Prices, All Items
MX_CPI <- read_csv("./Datos/CPI IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),des=TRUE,n=1)
#Gross Domestic Product: Implicit Price Deflator
MX_GDPDEF <- read_csv("./Datos_Desestacionalizados/GDPDEF IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4, s=c(1990,1),n=1)
#Producer Price Index for All Commodities
MX_PPI <- read_csv("./Datos/PPI IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE,n=1)
#Import Price, All Commodities,
MX_IMPPRICE <- read_csv("./Datos/IMPPRICE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des = TRUE,n=1)
#Indice de Precios y Cotizaciones
MX_IPC <- read_csv("./Datos/IPC IFS (MEX).csv")[1:285,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),des=TRUE,n=1)
#MX_WAGES
MX_WAGES <- read_csv("./Datos/WAGES IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq=4,s=c(1990,1),des=TRUE,n=1)
#Elimina variables
#rm(MX_CPI,MX_GDPDEF,MX_PPI,MX_IMPPRICE,MX_IPC,MX_WAGES)
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
#Elimina variables
#rm(MX_M1,MX_M2,MX_M3,MX_M4)
##### Mexico: Tasa de Interes ####
#Interest Rates, Lending Rate
MX_LRATE <- read_csv("./Datos/LRATE IFS (MEX).csv")[1:80,2] %>% 
  estandarizacion(freq = 4,s=c(1993,4),int =TRUE,des = TRUE,n=1) 
#Interest Rates, Money Market Rate
MX_MMRATE <- read_csv("./Datos/MMRATE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE)
#Interest Rates, Deposit Rate
MX_DRATE <- read_csv("./Datos/DRATE IFS (MEX).csv")[1:95,2]   %>% 
  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE)
#Interest Rates, Government Securities, Treasury Bills
MX_TBRATE <- read_csv("./Datos/TBRATE IFS (MEX).csv")[1:95,2] %>% 
  estandarizacion(freq = 4,s=c(1990,1),int =TRUE,des = TRUE) 
#Elimina variables
#rm(MX_LRATE,MX_MMRATE,MX_DRATE,MX_TBRATE)
##### Tipo de Cambio ####
MXN_USD <- read_csv("./Datos/MXN_USD1.csv")[1:288,2] %>% 
  estandarizacion(freq=12,s=c(1990,1),des=TRUE)   
#autoplot(MXN_USD)+                                                                  
#  geom_line(color = 'blue')+                                        
#  labs(title="Tipo de Cambio",x="",y="",subtitle="Pesos por Dólar")+
#  theme_economist() 
##### Componentes Principales ####
info <- cbind(MX_CPI,MX_DRATE,MX_EXP,MX_GCE,MX_GDP,MX_GDPDEF,MX_HCE,MX_IMPPRICE,MX_INDP,MX_IPC,MX_M1,MX_M2,MX_M3,MX_M4,MX_MANUFP,MX_MINP,MX_MMRATE,MXN_USD,MX_PETRP,MX_PPI,MX_WAGES,US_BPLRATE,US_CPI,US_DRATE,US_EMP,US_EXP,US_FFRATE,US_GCE,US_GDP,US_GDPDEF,US_GNI,US_HCE,US_IMP,US_IMPPRICE,US_INDP,US_M1,US_M2,US_M3,US_MB,US_PETRP,US_PPI,US_RHP,US_SP,US_TBRATE,US_WAGES) %>% as_tibble
comp <- info %>% prcomp(scale=T) %>% predict %>% .[,1:10] %>% as_tibble
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
y <- comp %>% 
  mutate(Int_US=US_CBRATE,
         Int_MX=MX_TBRATE)

model <- VAR(y)
##### Impulso respuesta ####
impulse  <- c("Int_US","Int_MX")
response <- names(y)
nombres  <- names(y)

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
