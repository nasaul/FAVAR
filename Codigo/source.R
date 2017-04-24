#Instala Automaticamente los paquetes
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
#Funcion que hace:
  #1.Convierte a serie de tiempo
  #2.Agrega la serie (si es que no es trimestral)
  #3.Desestacionaliza si des = TRUE
  #4.Calcula el numero de diferencias para ser estacionaria (Augmented Dickey-Fuller)
  #5.Aplica el numero de diferencias
  #6.Estandariza los datos
estandarizacion  <- function(vec,freq,s,des =FALSE, int= FALSE,n=NULL){
  serie <- ts(vec,frequency = freq ,start = s) 
  
  if (freq !=4){
    help1 <- aggregate(serie, nfrequency = 4)*(freq/4)
  } else {help1 <- serie}     
  
  if(int == TRUE){    
    if(des == TRUE){  help1 <- help1 %>% seasonal::seas() %>% seasonal::final()}
    if(is.null(n)){n     <- help1 %>% ndiffs(test="adf")}
    if(n != 0){help1  <- help1 %>% diff(differences = n)}
    help1 <- .25 * log(1+help1/100)
    serie <- help1  %>% scale %>%  ts(start=time(help1)[1],frequency = 4) %>% window(start=1991.0,end=2013.50)
    
  } else {                                                
    if(des==TRUE){help1 <- help1    %>%  seasonal::seas() %>% seasonal::final()}
    if(is.null(n)){n    <- log(help1)  %>% ndiffs(test="adf")}
    if(n!=0){serie1  <- log(help1)  %>% diff(differences = n)
    } else {serie1   <- log(help1)}  
    serie <- serie1 %>% 
      scale %>% 
      ts(start=time(serie1)[1],frequency = 4) %>%
      window(start=1991.0,end=2013.50)
  }
  return(serie)}
#Consigue un tibble con irf de un var
get_irf <- function(model,impulse,response=NULL,nombres){
  #
  impulse_response <- vars::irf(model,impulse=impulse,response=response)
  
  conv <- response %>% 
    as_tibble %>% rename(var=value) %>% 
    mutate(nombres=nombres) 
  
  
  get_data <- function(i){
    resp <- impulse_response$irf[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.))) %>% 
      gather_("var","value",nombres) %>% 
      left_join(conv,by="var") %>% 
      mutate(shock=rep(impulse[i],nrow(.)))
    
    resp_up <- impulse_response$Upper[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.)))%>% 
      gather_("var","value_up",nombres) %>% 
      left_join(conv,by="var")
    
    resp_low <- impulse_response$Lower[[i]]%>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.)))%>% 
      gather_("var","value_low",nombres) %>% 
      left_join(conv,by="var")
    
    df <- left_join(resp,resp_up, by = c("cons", "var", "nombres")) %>% 
      left_join(resp_low, by = c("cons", "var", "nombres"))
  }
  
  df <- do.call(
    "rbind",
    lapply(1:length(impulse_response$irf),get_data) 
  )
  return(df)
}
#Consigue un tibble con predicciones de un var
get_pred <-  function(model){
  pred <- predict(model)
  
  get_data <- function(i){
    df <- pred$fcst[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.))) %>% 
      rename(pred_puntual=fcst) %>% 
      mutate(variable=names(pred$fcst[i]))
  }
  df <- do.call(
    "rbind",
    lapply(1:length(pred$fcst),get_data) 
  )
  
  return(df)
}
#Consigue un tibble con descomposición de varianza de un var
get_fevd <- function(model){
  var_dec <- fevd(model) 
  
  get_data <- function(i){
    df <- var_dec[[i]] %>% as_tibble %>% mutate(Shock=names(var_dec[i]))
  }
  
  df <- do.call(
    "rbind",
    lapply(1:length(var_dec),get_data) 
  )
  
}