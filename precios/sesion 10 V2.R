## -------------------------------------------------------------------------
## SCRIPT: Sesion 10 Regresion Lineal y Regresion Logistica.R
## CURSO: Master en Data Science
## ASIGNATURA: Regresion lineal y logistica. Modelos lineales generalizados
## PROFESOR: Antonio Pita Lozano
## FECHA: 24/06/2016
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(ggplot2)
library(effects)
library(plyr)
library(ROCR)
library(stats)
library(randomForest)

options(java.parameters = "-Xmx4g" )
library(XLConnect)

## -------------------------------------------------------------------------
##       PARTE 1: REGRESION LINEAL
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 2. Bloque de carga de datos #####

getwd()         
setwd("~/R/_Estadistica")      # ORIGINAL
dir()   
creditos=read.csv("data/creditos.csv",stringsAsFactors = FALSE)
#creditos<-creditos[1:20,]

getwd()         
setwd("~/_PFM")      # GASO
creditosG=read.table("data/creditosN.txt",stringsAsFactors = TRUE, header =TRUE, sep = "\t",
                   colClasses = c("integer", "double", "double","double", "double", "double"))
#apply(creditosG,3,factor)


precio<- c(dfp[which(dfp$zipp == 282),"Gasolina.95.sin.plomo"])

precio<-precio[127]
pre1<-pre1[82]
pre2<-pre2[82]

pre1<- c(dfp[which(dfp$zipp == 179),"Gasolina.95.sin.plomo"])
pre2<- c(dfp[which(dfp$zipp == 180),"Gasolina.95.sin.plomo"])
creditosG<-data.frame(precio, pre2)



## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

str(creditos)
head(creditos)
tail(creditos)
summary(creditos)

## -------------------------------------------------------------------------

##### 4. Bloque de tratamiento de variables #####

creditos$Gender=as.factor(creditos$Gender)
creditos$Mortgage=as.factor(creditos$Mortgage)
creditos$Married=as.factor(creditos$Married)
creditos$Ethnicity=as.factor(creditos$Ethnicity)

summary(creditos)

## -------------------------------------------------------------------------

##### 5. Bloque de test de diferencia de medias mediante regresion lineal #####

t.test(Income ~ Age, data = creditos)
t.test(precio ~ pre1, data = creditosG)

# mediante un modelo lineal
modeloT=lm(Income ~ Age, data = creditos)
summary(modeloT)
modeloT=lm(Income ~ Rating, data = creditos)
summary(modeloT)

modeloTG=lm(pre2 ~ pre4, data = creditosG)
summary(modeloTG)
modeloTG=lm(precio +litros ~ pre1, data = creditosG)
summary(modeloTG)

modeloTG=lm(precio ~ ., data = creditosG)
summary(modeloTG)
modeloTG=lm(litros ~ pre2, data = creditosG)
summary(modeloTG)
modeloTG=lm(litros ~ pre3, data = creditosG)
summary(modeloTG)
modeloTG=lm(litros ~ pre4, data = creditosG)
summary(modeloTG)

modeloTG=lm( litros  ~ precio, data = creditosG)
summary(modeloTG)
modeloTG=lm( litros  ~ pre1, data = creditosG)
summary(modeloTG)
modeloTG=lm( litros  ~ pre2, data = creditosG)
summary(modeloTG)
modeloTG=lm( litros  ~ pre3, data = creditosG)
summary(modeloTG)
modeloTG=lm( litros  ~ pre3, data = creditosG)
summary(modeloTG)

modeloTG=lm( litros + precio ~ pre4, data = creditosG)
summary(modeloTG)




modeloTG=lm( precio ~ pre1+pre2+pre3, data = creditosG)
summary(modeloTG)
modeloTG=lm( precio ~ pre1+pre2+pre4, data = creditosG)
summary(modeloTG)
modeloTG=lm( precio ~ pre1+pre3+pre4, data = creditosG)
summary(modeloTG)
modeloTG=lm( precio ~ pre2+pre3+pre4, data = creditosG)
summary(modeloTG)

modeloTG=lm(precio~. -pre4, data = creditosG)
summary(modeloTG)

nuevas.edades <- data.frame(pre1 = seq(1.230, 1.260))
predict(modeloTG, nuevas.edades)

residuos <- rstandard(modeloTG)
valores.ajustados <- fitted(modeloTG)
plot(valores.ajustados, residuos)
qqnorm(residuos)
qqline(residuos)

plot(creditosG$litros, creditosG$pre1, xlab = "Edad", ylab = "Grasas")
abline(modeloTG)

pairs(creditosG)

plot(creditosG$precio, creditosG$pre1, xlab = "Edad", ylab = "Grasas")
plot(creditos$income, creditos$Rating, xlab = "Edad", ylab = "Grasas")
abline(modeloTG)

nuevasG <- seq(1245, 1275)
predict(creditosG, nuevasG)

# interpretar saldo en función del sexo no aplica. No diferencias significativas
# Caso base. Interpretacion de vaiables categoricas. Caso Base = man.
# 
# No podemos asegurar que el sexo influye en el salario -> podemos asegurar que el sexo NO influye en el salario ? SI  !!!!!
# p-value está muy por encima del 0.05 (0.7345) y R2 tiende a cero


## -------------------------------------------------------------------------

##### 6. Bloque de regresion lineal individual #####

t.test(Income ~ Rating, data = creditos) # Error in t.test.formula(Income ~ Rating, data = creditos) : grouping factor must have exactly 2 levels

modeloInd1=lm(Income ~ Rating, data = creditos)
summary(modeloInd1)

# podemos asegurar que el Rating NO influye en el salario ? NO  !!!!!
# a mayor rating mayores ingresos es hipotesis cierta. Valor extremo a la derecha (Max)

modeloInd2=lm(Income ~ Products, data = creditos)
summary(modeloInd2)

# correlacion entre productos contratados e ingresos ? -> pValue. 61.8%
# más productos tinen más ingresos ?
# Primero veo si hay efecto (pValue) y luego la cuantía

modeloInd3=lm(Income ~ Age, data = creditos)
summary(modeloInd3)

# No podemos asegurar que la edad influye en el salario -> podemos asegurar que la edad NO influye en el salario ? NO  !!!!!
# p-value está muy por debajo del 0.05 (0.032) y R2 tiende a UNO

# Hay correlación ? Hay un 3% al 68% entre 0.13 y 0.35
# no podemos estimar el salario en funcion a la edad (por el r2) ?????


modeloInd4=lm(Income ~ Education, data = creditos)
summary(modeloInd4)

modeloInd5=lm(Income ~ Gender, data = creditos)
summary(modeloInd5)

modeloInd6=lm(Income ~ Mortgage, data = creditos)
summary(modeloInd6)

modeloInd7=lm(Income ~ Married, data = creditos)
summary(modeloInd7)

modeloInd8=lm(Income ~ Ethnicity, data = creditos)
summary(modeloInd8)

# la raza es categórica. analisis previo 

modeloInd9=lm(Income ~ Balance, data = creditos)
summary(modeloInd9)

## -------------------------------------------------------------------------

##### 7. Bloque de regresion lineal multiple #####

modeloMul1=lm(Income ~ ., data = creditos)
summary(modeloMul1)

modeloMulG=lm(litros ~ ., data = creditosG)
summary(modeloMulG)

# solo tres variables significativas

## -------------------------------------------------------------------------

##### 8. Bloque de comparacion de modelos #####

anova(modeloInd1,modeloMul1)

## -------------------------------------------------------------------------

##### 9. Bloque de Ejercicio #####

## ¿Cuales serian las variables que incluiriamos en el modelo?

modeloMul2=lm(Income ~   Gender                            , data = creditos)
summary(modeloMul2)

anova(modeloInd1,modeloMul2)
anova(modeloMul2,modeloMul1)

null=lm(Income~1, data=creditos)
full=lm(Income~., data=creditos)
step(null, scope=list(lower=null, upper=full), direction="forward")


head (modeloFinal)
## -------------------------------------------------------------------------

##### 10. Bloque de analisis del modelo #####

modeloFinal=lm(Income ~ Rating+Mortgage+Balance, data = creditos)
summary(modeloFinal)
plot(modeloFinal$residuals)
hist(modeloFinal$residuals)
qqnorm(modeloFinal$residuals); qqline(modeloFinal$residuals,col=2)
confint(modeloFinal,level=0.95) # intervalo de confianza. Se decide

anova(modeloFinal,modeloMul1)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

## -------------------------------------------------------------------------

##### 11. Bloque de analisis de interacciones #####

modeloInter1=lm(Income ~ Balance+Rating*Mortgage+Balance:Mortgage, data = creditos)
summary(modeloInter1)

modeloInter2=lm(Income ~ Rating*Mortgage+Balance, data = creditos)
summary(modeloInter2)

modeloInter3=lm(Income ~ Rating:Mortgage+Balance, data = creditos)
summary(modeloInter3)

efecto1 <- effect("Rating*Mortgage", modeloInter1, xlevels = 10)
plot(efecto1)

efecto2 <- effect("Balance*Mortgage", modeloInter1, xlevels = 10)
plot(efecto2)

efecto3 <- effect("Rating*Mortgage", modeloInter2, xlevels = 10)
plot(efecto3)

efecto4 <- effect("Rating:Mortgage", modeloInter3, xlevels = 10)
plot(efecto4)

modeloInter5=lm(Income ~ Rating*Mortgage, data = creditos)
summary(modeloInter5)

efecto5 <- effect("Rating*Mortgage", modeloInter5, xlevels = 10)
plot(efecto5)

## -------------------------------------------------------------------------

##### 12. Bloque de analisis de variable Balance #####

modeloBalance=lm(Balance ~ ., data = creditos)
summary(modeloBalance)

## -------------------------------------------------------------------------

##### 13. Bloque de ejercicio #####

## ¿Cuales serian las variables que incluiriamos en el modelo?

modeloBalanceFin=lm(Balance ~                    , data = creditos)
summary(modeloBalanceFin)

## -------------------------------------------------------------------------
##       PARTE 2: MODELOS LINEALES GENERALIZADOS: REGRESION LOGISTICA
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 14. Bloque de carga de datos #####

BANK=read.csv2("data/bank-full.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

## -------------------------------------------------------------------------

##### 15. Bloque de revisión basica del dataset #####

str(BANK)
head(BANK)
summary(BANK)

## -------------------------------------------------------------------------

##### 16. Bloque de formateo de variables #####

BANK$day=as.factor(BANK$day)
BANK$campaign=as.factor(BANK$campaign)
BANK$IND_PREVIO=as.factor(as.numeric(BANK$pdays!=-1))

str(BANK)
head(BANK)
summary(BANK)

## -------------------------------------------------------------------------

##### 17. Bloque de modelo de regresión logistica #####

model_logit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="logit"))
summary(model_logit1)

levels(BANK$marital)  <- c("single","married","divorced") # prueba de Antonio
        
model_probit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="probit"))
summary(model_probit1)



# Diferencia entre el logit y el probit
X=seq(from=-4,to=4,by=0.1)
sigmoide=1/(1+exp(-X))
cumulative<-pnorm(X, 0, 1)
plot(sigmoide,type="l",col="red")
lines(cumulative,col="blue")

## -------------------------------------------------------------------------

##### 18. Bloque de evaluación del modelo #####

BANK$prediccion=predict(model_logit1,type="response")
Pred_auxiliar= prediction(BANK$prediccion, BANK$y, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_model_logit1_train = as.numeric(auc.tmp@y.values)
auc_model_logit1_train

CURVA_ROC_model_logit1_train <- performance(Pred_auxiliar,"tpr","fpr")
plot(CURVA_ROC_model_logit1_train,colorize=TRUE)
abline(a=0,b=1)

## Capacidad del Modelo
mean(as.numeric(BANK$y)-1)
aggregate(BANK$prediccion~BANK$y,FUN=mean)

## -------------------------------------------------------------------------

##### 19. Bloque de puesta en valor de un modelo: Fijación del Threshold #####

ALPHA=0.5 # a partir de donde quiero llamar
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.2
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.8
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

# Criterio maximizar F1-Score

BANK_KS$Accuracy=(BANK_KS$EXITOS_ACUM+BANK_KS$FRACASOS_TOT-BANK_KS$FRACASOS_ACUM)/BANK_KS$TOTAL
BANK_KS$Precision=BANK_KS$EXITOS_ACUM/BANK_KS$N
BANK_KS$Cobertura=BANK_KS$EXITOS_ACUM/BANK_KS$EXITOS_TOT
BANK_KS$F1Score=2*(BANK_KS$Precision*BANK_KS$Cobertura)/(BANK_KS$Precision+BANK_KS$Cobertura)
plot(BANK_KS$F1Score)
max(BANK_KS$F1Score)
which(BANK_KS$F1Score==max(BANK_KS$F1Score))
BANK_KS[3648,]

## -------------------------------------------------------------------------
##       PARTE 3: MODELOS LINEALES GENERALIZADOS: REGRESION POISSON
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 20. Bloque de carga de datos #####

BICIS=read.csv("data/hour.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

## -------------------------------------------------------------------------

##### 21. Bloque de revisión basica del dataset #####

str(BICIS)
head(BICIS)
summary(BICIS)

## -------------------------------------------------------------------------

##### 22. Bloque de modelos de regresión poisson #####

hist(BICIS$cnt)
mean(BICIS$cnt)
sd(BICIS$cnt)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

## -------------------------------------------------------------------------

##### 23. Bloque de formateo de variables #####

BICIS$season=as.factor(BICIS$season)
BICIS$yr=as.factor(BICIS$yr)
BICIS$mnth=as.factor(BICIS$mnth)
BICIS$hr=as.factor(BICIS$hr)
BICIS$holiday=as.factor(BICIS$holiday)
BICIS$weekday=as.factor(BICIS$weekday)
BICIS$workingday=as.factor(BICIS$workingday)
BICIS$weathersit=as.factor(BICIS$weathersit)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

model_poisson=glm(cnt~.-workingday-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

## -------------------------------------------------------------------------


BANK[1:2,]
BANK$CosteLlamada = 5
BANK$BeneficioVenta = 25
BANK$BeneficioEstimado = 25*BANK$prediccion-5

sum(BANK$BeneficioEstimado>0)
BANK2 = BANK[BANK$BeneficioEstimado>0,]

# con un threashold de 0.20 lo voy a llamar
sum(BANK2$prediccion) # lo que voy a ganar


