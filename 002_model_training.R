# En este analisis, se extrajeron desde el sitio argenprop
# los valores de alquileres de departamentos en la Ciudad 
# Autonoma de Buenos aires se intentara predecir el precio 
# de alquiler mediante 1 regresion multiple dependiendo de 
# las covariables disponibles. Luego se intentará realizar 
# un regresion logistica para ver si existe algun patron 
# para aquellos q pide el valor del alquiler en USD


# Diccionario de datos ----------------------------------------------------
# "X"                   = Identificador secuencial
# "moneda"              = Moneda{ARS,USD}
# "importe"             = Importe en moneda original
# "importe_normalizado" = Importe en pesos (se asume tipo de cambio de 86)
# "m2"                  = Metros cuadrados de superficie total
# "dormitorios"         = Cant de Dormitorios
# "baño"                = Cant de baños
# "antiguedad"          = Antiguedad en años
# "interno"             = Es interno{T,F}
# "contrafrente"        = Es contrafrente{T,F}
# "monoambiente"        = Es monoambiente{T,F}
# "ap"                  = Apto profesional{T,F}



# Extraer Dataset ---------------------------------------------------------
library(rvest)
library(stringr)
simple <- read_html("https://www.argenprop.com/departamento-alquiler-barrio-br-norte-orden-masnuevos")

extraerDatos = function(nodo){
  Y = nodo %>% html_nodes("p.card__price") %>% html_text()
  Y = str_trim(gsub("\\r\\n","",Y))  #card__common-data
  predictores = nodo %>% html_nodes("p.card__common-data")

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

#while (nodo) {
#  print(paste0("Trayendo Pagina:",proximaPagina(simple)))
#}

for (i in 1:50) {
  print(paste0("Trayendo Pagina:",proximaPagina(nodo)))
  nodo = read_html(proximaPagina(nodo))
  out = rbind(out,extraerDatos(nodo))
  Sys.sleep(5)
}
dim(out)

out$moneda=ifelse(str_detect(out$Y,pattern = "USD"),"USD","ARS")
table(out$moneda)
out$importe = as.numeric(str_replace_all(out$Y,"[^\\d]",""))
out$importe[out$moneda=="USD"]*86
mean(out$importe[out$moneda=="ARS"],na.rm = T)
out$importe_normalizado = ifelse(str_detect(out$Y,pattern = "USD"),
                                 out$importe * 60,
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

out[,c(1,2,3)]
alquileres = out[,c(-1,-2)]
write.csv(alquileres,"Alquileres.csv")

# Cargar alquileres CSV ---------------------------------------------------

rm(list = ls())
alquileres=read.csv("Alquileres.csv")

head(alquileres)

library("lightgbm")
library(caTools)

numericos = unlist(lapply(alquileres,FUN = is.numeric))
X = alquileres[,numericos]
summary(X)

library(mice)
library(VIM)

md.pattern(X)
aggr_plot <- aggr(X, 
                  col=c('steelblue','indianred'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(X), 
                  cex.axis=.7, gap=3, 
                  ylab=c("Histograma de datos faltantes","Patron"))

modeloImp <- mice(X,m=5,maxit=50,meth='pmm',seed=1)
summary(modeloImp)

#armamos un dataset completo
dataset =  complete(modeloImp,1)

sub50 = subset(alquileres,importe_normalizado<50000)

m1 = lm(importe_normalizado~m2,data=sub50)

plot(sub50$m2,sub50$importe_normalizado)
abline(m1,col="indianred",lwd=2)
plot(m1)

pca = prcomp(dataset[,-1])

plot(pca)

plot(pca$x[,"PC1"],dataset[,1])

idx = sample.split(alquileres$importe_normalizado)
table(idx)
alquileres$importe = NULL
train = alquileres[idx,]
test  = alquileres[!idx,]

dtrain  <- lgb.Dataset( data  = data.matrix(  train[ , -2]),
                        label = train$importe_normalizado,
                        free_raw_data= TRUE,
                        feature_pre_filter=FALSE)
modelo  <- lgb.cv(data= dtrain,
                  objective= "regression",
                  #metric = "auc",
                  max_depth=2,
                  bagging_fraction=0.5,
                  min_data_in_leaf=10,
                  feature_fraction=0.5,
                  min_data_in_leaf=2,
                  min_sum_hessian_in_leaf=0) 

fitness = function(x){
  modelo  <- lgb.cv(data= dtrain,
                    objective= "binary",
                    metric = "auc",
                    max_depth=round(x[1]),
                    bagging_fraction=x[2],
                    min_data_in_leaf=round(x[3]),
                    feature_fraction=x[4],
                    min_data_in_leaf=round(x[5]),
                    min_sum_hessian_in_leaf=x[6],
                    verbose = -1) 
  return(modelo$best_score)  
}




#ranger::ranger(importe_normalizado~.,data=alquileres)


# Carga de dataset --------------------------------------------------------
set.seed(123)
alquileres = read.csv("Alquileres.csv",stringsAsFactors = T)
summary(alquileres)

# descartamos los q no tienen importe
alquileres = alquileres[!is.na(alquileres$importe),]
summary(alquileres)


# Imputacion de faltantes -------------------------------------------------

library(mice)
library(VIM)

md.pattern(alquileres)
aggr_plot <- aggr(alquileres, 
                  col=c('steelblue','indianred'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(alquileres), 
                  cex.axis=.7, gap=3, 
                  ylab=c("Histograma de datos faltantes","Patron"))

modeloImp <- mice(alquileres,m=5,maxit=50,meth='pmm',seed=1)
summary(modeloImp)

#armamos un dataset completo
dataset =  complete(modeloImp,1)

summary(dataset)

# Regresion lineal multiple -----------------------------------------------

fit1 = lm(importe_normalizado~moneda+m2+dormitorios+baño+antiguedad+interno+contrafrente+monoambiente+ap,data=dataset)
summary(fit1)
#plot(fit1)

#455,630 y 947 son problematicos
dataset[c(455,630,947),]
#los importes son absurdos, se asumen q son errores
dataset2 = dataset[-c(455,630,947),]

fit2 = lm(importe_normalizado~moneda+m2+dormitorios+baño+antiguedad+interno+contrafrente+monoambiente+ap,data=dataset2)
summary(fit2)
#plot(fit2)

#claramente hay dispersion dependiente de nivel
shapiro.test(fit2$residuals) 
#y el error no es normal...

fit3 = lm(log(importe_normalizado)~moneda+m2+dormitorios+baño+antiguedad+interno+contrafrente+monoambiente+ap,data=dataset2)
summary(fit3)
#plot(fit3)
shapiro.test(fit3$residuals)
#mejora el R2, pero el error sigue sin ser normal
#vamos a simplificar el modelo utilizando BIC y algoritmos geneticos para la 
#seleccion de variables

# Seleccion de Variables --------------------------------------------------
library(GA)
x = model.matrix(fit2)
y = model.response(fit2$model)

fitness = function(string)
{ 
  tryCatch({
    mod <- lm(y ~ x[,string==1])
    return(-BIC(mod)) # Uso este criterio pq en genaral selecciona menos variables
  },error=function(excep){
    return(-1e10)
  })
}

#probamos funcion de fitness
string = rep(1,ncol(x))
fitness(rep(1,ncol(x)))
BIC(fit2)
set.seed(123)
GA1 = ga("binary", fitness = fitness, nBits = ncol(x), 
         popSize = 100, maxiter = 100, seed = 1, monitor = TRUE)
plot(GA1)
#se optimizo antes de la generacion 20, no hace falta seguir...
GA1@solution
colnames(x)[GA1@solution[1,]==1]

fit4 = lm(y~x[,GA1@solution[1,]==1]-1)
BIC(fit2,fit4)
summary(fit4)
#plot(fit4)
pairs(dataset2[,c("importe_normalizado","moneda","m2","dormitorios","baño")])

# Analisis PCA ------------------------------------------------------------


PCA1 = prcomp(dataset2[,c("m2","dormitorios","baño")])
summary(PCA1)
plot(log(PCA1$x[,1]),log(dataset2$importe_normalizado))
yprima = (dataset2$importe_normalizado)
xprima = (PCA1$x[,1])
fit5 = lm(yprima~xprima)
summary(fit5)
#plot(fit5)
#sigo sin logarar normalizar el error, paso a una regresion robusta


# Metodos Robustos --------------------------------------------------------

library(MASS)
library(grDevices)

fit6 = rlm(importe_normalizado~moneda+m2+dormitorios+baño,data = dataset2)
summary(fit6)
BIC(fit2,fit4,fit6)
#como era de esperarse el metodo robusto da un peor BIC que los OLS,
#pero al menos podemos asumir q los coeficientes estimados son validos
#plot(fit6)
yhat = predict(fit6)
plot(model.response(fit6$model),yhat,xlab="Y",ylab=bquote(hat(Y)),pch=19,
     col=adjustcolor("gray",0.5))
abline(0,1,col="indianred",lwd=2)
#Hay una pequeña caida...
plot(sqrt(model.response(fit6$model)),yhat,xlab="Y",ylab=bquote(hat(Y)),pch=19,
     col=adjustcolor("gray",0.5))
abline(lm(yhat~sqrt(model.response(fit6$model))),col="indianred",lwd=2)
# q se linealiza al utilizar raiz cuadrada....

# Regresion Multiple, modelo FINAL ----------------------------------------
fit7 = rlm(sqrt(importe_normalizado)~moneda+m2+dormitorios+baño,data = dataset2)
summary(fit7)
#plot(fit7)
yhat = predict(fit7)
plot(model.response(fit7$model),yhat,xlab="Y",ylab=bquote(hat(Y)),pch=19,
     col=adjustcolor("gray",0.5))
abline(0,1,col="indianred",lwd=2)



# PARTE 2 LOGISTICA -------------------------------------------------------
# Quiero ver q caracteristicas tiene un departamento para que
# sea cotizado en USD
# omitimos el importe normalizado pq esta fuertemente influenciado

lfit1 = glm(moneda~m2+dormitorios+baño+antiguedad+
              antiguedad+interno+contrafrente+monoambiente+ap
              ,data = dataset2,family = "binomial")
summary(lfit1)

# Seleccion de variables --------------------------------------------------


x = model.matrix(lfit1)
#escalamos para anular efecto de las unidades
y = model.response(lfit1$model)

fitness = function(string)
{ 
  tryCatch({
    mod <- glm(y ~ x[,string==1]-1,family = "binomial")
    return(-BIC(mod))
  },error=function(excep){
    return(-1e10) #si tira excepcion, poner un valor de perdida alto..
  })
}

#probamos funcion de fitness
fitness(rep(1,ncol(x)))
BIC(lfit1)
set.seed(123)
GA2 = ga("binary", fitness = fitness, nBits = ncol(x), 
         popSize = 100, maxiter = 50, seed = 1, monitor = TRUE)
plot(GA2)
#se optimizo antes de la generacion 20, no hace falta seguir...
GA2@solution
colnames(x)[GA2@solution[1,]==1]

lfit2 = glm(moneda~m2,data = dataset2,family = "binomial")
BIC(lfit1,lfit2)
AIC(lfit1,lfit2)


# Diagnosticos del modelo Logistico ---------------------------------------

library(ROCR)

pred1 = prediction(predict(lfit1),labels = model.response(lfit1$model))
pred2 = prediction(predict(lfit2),labels = model.response(lfit2$model))

plot(performance(pred1,"tpr","fpr"),col="gray",lwd=2)
plot(performance(pred2,"tpr","fpr"),col="indianred",lwd=2,add=T)
# con la curva ROC no es tan claro q lfit2 sea un mejor modelo, 
# tal vez dependa del punto de corte
# pero al menos son muy parecidos y lfit2 es mas parsimonioso
plot(performance(pred1,"acc"),col="gray",lwd=2)
plot(performance(pred2,"acc"),col="indianred",lwd=2,add=T)

perf2 = performance(pred2,"acc")
cutoff = pred2@cutoffs[[1]][which.max(perf2@y.values[[1]])]
abline(v=cutoff,col="steelblue",lwd=2,lty="dashed")


# matriz de confusion -----------------------------------------------------

yhat = ifelse(predict(lfit2)>cutoff,"pred USD","pred ARS")
y = model.response(lfit1$model)
CM = table(y,yhat)
CM
cat("Precision:" ,sum(diag(CM))/sum(CM))
cat("VPP:" ,prop.table(CM,2)[2,2])

summary(lfit2)

# A mayor cantidad de m2 se incrementa la probabiliad que el departemento
# sea cotizado en USD, lo cual es compatible con lo esperado

