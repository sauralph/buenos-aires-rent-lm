# Load Libraries ----------------------------------------------------------

rm(list = ls())
library(rvest)
library(stringr)



# Set Remote end-point ----------------------------------------------------


simple <- read_html("https://www.argenprop.com/departamento-venta-barrio-br-norte")
exchange_rate <- 200


# Auxiliary Functions -----------------------------------------------------



extractData = function(node){
  Y = node %>% html_nodes("p.card__price") %>% html_text() 
  Y = str_trim(gsub("\\r\\n","",Y))  #card__common-data
  predictores = node %>% html_nodes("ul.card__main-features") 
  
  X = rep(NA,length(Y))
  
  for (i in 1:length(Y)) {
    predictor = predictores[i]
    X[i]=paste(predictor %>% html_nodes("span") %>% html_text(),collapse = "|")
  }
  return(data.frame(X,Y))
}

nextPage = function(node){
  paste0("https://www.argenprop.com",node %>% html_nodes(".pagination__page-next.pagination__page>a") %>% html_attr("href"))
}


# First Extraction --------------------------------------------------------


out = extractData(simple)
node = read_html(nextPage(simple))
out = rbind(out,extractData(node))

last_page = simple %>% html_nodes(".pagination__page") %>% html_text()
last_page = as.integer(last_page[length(last_page)-1])


# Extraction loop ---------------------------------------------------------


for (i in seq(1,last_page)) {
  print(paste0("Downloding Page: ",nextPage(node)))
  node = read_html(nextPage(node))
  out = rbind(out,extractData(node))
  Sys.sleep(1)
}


dim(out)
head(out)

out$currency=ifelse(str_detect(out$Y,pattern = "USD"),"USD","ARS")
table(out$currency)
out$amount = as.numeric(str_replace_all(out$Y,"[^\\d]",""))
out$amount[out$currency=="USD"]*exchange_rate
mean(out$amount[out$currency=="ARS"],na.rm = T)
out$amount_normalized = ifelse(str_detect(out$Y,pattern = "USD"),
                                 out$amount * 200,
                                 out$amount
)
boxplot(amount_normalized~currency,data=out,outline=F)

parseCharacteristics = function(X,pattern){
  unlist(lapply(str_split(X,pattern = "\\|"),function(x){
    posicion = str_detect(x,pattern)
    if(sum(posicion)>0){
      mean(as.numeric(str_replace_all(str_replace_all(x[posicion],"\\,","."),"[^\\d\\.]","")),na.rm = T)
    }else{
      return(NA)
    }
  }))
}

out$m2 = parseCharacteristics(out$X,"m²")
out$dorms = parseCharacteristics(out$X,"dorm")
out$bath = parseCharacteristics(out$X,"baño")
out$age = parseCharacteristics(out$X,"año")
#out$interno = table(str_detect(out$X,"Int"))
out$backside = str_detect(out$X,"Contra")
out$loft = str_detect(out$X,"Monoam")
out$office = str_detect(out$X,"Apto P")

summary(out)
#out$X
write.csv(out[,c(-1,-2)],"HouseMarket.csv")
