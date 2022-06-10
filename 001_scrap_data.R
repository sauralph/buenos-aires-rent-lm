rm(list = ls())
library(rvest)
library(stringr)
simple <- read_html("https://www.argenprop.com/departamento-venta-barrio-br-norte")



extraerDatos = function(nodo){
  Y = nodo %>% html_nodes("p.card__price") %>% html_text() 
  Y = str_trim(gsub("\\r\\n","",Y))  #card__common-data
  predictores = nodo %>% html_nodes("ul.card__main-features") 
  
  X = rep(NA,length(Y))
  
  for (i in 1:length(Y)) {
    predictor = predictores[i]
    X[i]=paste(predictor %>% html_nodes("span") %>% html_text(),collapse = "|")
  }
  return(data.frame(X,Y))
}

proximaPagina = function(nodo){
  paste0("https://www.argenprop.com",nodo %>% html_nodes(".pagination__page-next.pagination__page>a") %>% html_attr("href"))
}


out = extraerDatos(simple)
nodo = read_html(proximaPagina(simple))
out = rbind(out,extraerDatos(nodo))

for (i in 1:366) {
  print(paste0("Trayendo Pagina:",proximaPagina(nodo)))
  nodo = read_html(proximaPagina(nodo))
  out = rbind(out,extraerDatos(nodo))
  Sys.sleep(1)
}


dim(out)

out$moneda=ifelse(str_detect(out$Y,pattern = "USD"),"USD","ARS")
table(out$moneda)
out$importe = as.numeric(str_replace_all(out$Y,"[^\\d]",""))
out$importe[out$moneda=="USD"]*200
mean(out$importe[out$moneda=="ARS"],na.rm = T)
out$importe_normalizado = ifelse(str_detect(out$Y,pattern = "USD"),
                                 out$importe * 200,
                                 out$importe
)
boxplot(importe_normalizado~moneda,data=out,outline=F)

extraerCaracteristica = function(X,pattern){
  unlist(lapply(str_split(X,pattern = "\\|"),function(x){
    posicion = str_detect(x,pattern)
    if(sum(posicion)>0){
      mean(as.numeric(str_replace_all(str_replace_all(x[posicion],"\\,","."),"[^\\d\\.]","")),na.rm = T)
    }else{
      return(NA)
    }
  }))
}

out$m2 = extraerCaracteristica(out$X,"m²")
out$dormitorios = extraerCaracteristica(out$X,"dormitorio")
head(out$X)
out$baño = extraerCaracteristica(out$X,"baño")
head(out$X)
out$antiguedad = extraerCaracteristica(out$X,"año")
out$interno = str_detect(out$X,"Interno")
out$contrafrente = str_detect(out$X,"Contrafrente")
out$monoambiente = str_detect(out$X,"Monoambiente")
out$ap = str_detect(out$X,"Apto Profesional")

summary(out)
out$X
ventas = out[,c(-1,-2)]
nodo

saveRDS(ventas,"ventas2.RDS")

