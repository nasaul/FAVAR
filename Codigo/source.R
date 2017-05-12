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
standar  <- function(vec,freq,s,des =FALSE, int= FALSE,n=NULL){
  serie <- ts(vec,frequency = freq ,start = s) 
  
  if (freq !=4){
    help1 <- aggregate(serie, nfrequency = 4)*(freq/4)
  } else {help1 <- serie}     
  
  if(int == TRUE){    
    if(des == TRUE){  help1 <- help1 %>% seasonal::seas() %>% seasonal::final()}
    help1 <- .25 * log(1+help1/100)
    if(is.null(n)){n     <- help1 %>% ndiffs(test="adf")}
    if(n != 0){help1  <- help1 %>% diff(differences = n)}
    serie <- help1  %>%
      scale %>%
      ts(start=time(help1)[1],frequency = 4) %>%
      window(start=1991.0,end=2013.50)
    
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
get_irf <- function(model,impulse,pca,n.ahead=10){
  
  response <- model$y %>% as_tibble %>% names
  nombres  <- model$y %>% as_tibble %>% names
  
  impulse_response <- vars::irf(model,impulse=impulse,response=response,n.ahead=n.ahead)
  
  conv <- response %>% 
    as_tibble %>% rename(var=value) %>% 
    mutate(nombres=nombres) 
  
  
  get_data <- function(i){
    resp <- impulse_response$irf[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.))) %>% 
      gather_("var","pred",nombres) %>% 
      left_join(conv,by="var") %>% 
      dplyr::select(-nombres)
    
    resp_up <- impulse_response$Upper[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.)))%>% 
      gather_("var","upper",nombres) %>% 
      left_join(conv,by="var") %>% 
      dplyr::select(-nombres)
    
    resp_low <- impulse_response$Lower[[i]]%>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.)))%>% 
      gather_("var","lower",nombres) %>% 
      left_join(conv,by="var") %>% 
      dplyr::select(-nombres)
    
    comp <- resp %>% 
      left_join(resp_up, by = c("cons", "var")) %>% 
      left_join(resp_low, by = c("cons", "var")) %>% 
      pred_fac(pca)
    
    real_var<- resp %>% 
      left_join(resp_up, by = c("cons", "var")) %>% 
      left_join(resp_low, by = c("cons", "var")) %>% 
      dplyr::filter(!grepl("PC",var))
    
    df <- comp %>% 
      full_join(real_var, 
                by = c("cons", "var", "pred", "lower", "upper")) %>% 
      mutate(shock=rep(impulse[i],nrow(.)))
  }
  
  df <- do.call(
    "rbind",
    lapply(1:length(impulse_response$irf),get_data) ) %>% 
    dplyr::select(shock,var,cons,pred,upper,lower)
  return(df)
}
#Consigue un tibble con predicciones de un var
get_pred <-  function(model,pca){
  pred <- predict(model)
  
  get_data <- function(i){
    df <- pred$fcst[[i]] %>% as_tibble %>%
      mutate(cons=as.numeric(row.names(.))) %>% 
      rename(pred=fcst) %>% 
      mutate(var=names(pred$fcst[i]))
  }
  df <- do.call(
    "rbind",
    lapply(1:length(pred$fcst),get_data) 
  ) %>% dplyr::select(-CI)
  df1 <- pred_fac(df,pca) %>% 
    full_join(df %>% 
                filter(!grepl("PC",var)),
              by = c("cons", "var", "pred", "lower", "upper"))
  return(df1)
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
#Consigue un tibble con predicciones de las variables reales
pred_fac<- function(pred,pca){
  df_punt <- pred %>% 
    filter(grepl("PC",var)) %>% 
    dplyr::select(var,pred,cons) %>% 
    spread(key=var,
           value=pred) %>% 
    dplyr::select(-cons) %>% 
    as.matrix %*% t(pca$rotation[,1:nfac]) %>%  
    as_tibble %>% 
    mutate(cons=row.names(.) %>% as.numeric) %>% 
    gather(key=var,
           value=pred,
           -cons) 
  
  df_lower <- pred %>% 
    filter(grepl("PC",var)) %>% 
    dplyr::select(var,lower,cons) %>% 
    spread(key=var,
           value=lower) %>% 
    dplyr::select(-cons) %>%
    as.matrix %*% t(pca$rotation[,1:nfac]) %>%  
    as_tibble %>% 
    mutate(cons=row.names(.) %>% as.numeric)%>% 
    gather(key=var,
           value=lower,
           -cons) 
  
  df_upper <- pred %>% 
    filter(grepl("PC",var)) %>% 
    dplyr::select(var,upper,cons) %>% 
    spread(key=var,
           value=upper) %>% 
    dplyr::select(-cons) %>%
    as.matrix %*% t(pca$rotation[,1:nfac]) %>%  
    as_tibble %>% 
    mutate(cons=row.names(.) %>% as.numeric) %>% 
    gather(key=var,
           value=upper,
           -cons) 
  
  df <- df_punt %>% 
    full_join(df_lower, by = c("cons", "var")) %>% 
    full_join(df_upper, by = c("cons", "var"))
  
  return(df)
}