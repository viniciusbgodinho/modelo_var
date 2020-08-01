## Verificar o mecanismo de variáveis macroeconômicas na agricultura
## Preço real dos grãos (milho, soja, café, cana-de-açúcar)
## Taxa de juros real
## Taxa de câmbio real
## Preço real da energia (petróleo)
##Dados em número índice   base 100 = média dos meses de 2010
# Importando do diretório

dados <- read.csv2("", header = TRUE, dec = '.')
#install.packages('tseries')
require('tseries')
library(tseries)
attach(dados)
#install.packages('timeDate')
require('timeDate')
library(timeDate)
#Renomeando e Transformando em série temporal
dados <- ts(dados, freq = 12, start = c(1995,1), end = c(2016, 11))
graos1 <- ts(graos, freq = 12, start = c(1995,1), end = c(2016, 11))
juros1 <- ts(juros, freq = 12, start = c(1995,1), end = c(2016, 11))
cambio1 <- ts(cambio, freq = 12, start = c(1995,1), end = c(2016, 11))
energia1 <- ts(energia, freq = 12, start = c(1995,1), end = c(2016, 11))
inflacao1 <- ts(inflacao, freq = 12, start = c(1995,1), end = c(2016, 11))

## Estatísiticas descritivas
#grãos
summary(graos1)
sd(graos1)
coef.var=sd(graos1)/mean(graos1)*100
skewness(graos1)
kurtosis(graos1)

# Gráficos dos dados brutos
ts.plot(graos1,gpars=list(xlab="Ano", main="Índece de Preço Grãos", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Grãos'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

dev.off
# Teste de nomalidade:
## Jarque-Bera
require('tseries')
library(tseries)
jarque.bera.test(graos1)
### A série rejeita H0, ou seja, a série não segue a normal.

## Confirmação: Histograma
hgraos1<-hist(graos1, breaks=10, col="gray", xlab="", ylab="", main="Grãos") 
graos1fit<-seq(min(graos1),max(graos1),length=40) 
ygraos1fit<-dnorm(graos1fit,mean=mean(graos1),sd=sd(graos1)) 
ygraos1fit <- ygraos1fit*diff(hgraos1$mids[1:2])*length(graos1) 
lines(graos1fit, ygraos1fit, col="black", lwd=2)

# Decomposição gráfica
plot(decompose(graos1))

#### Juros

summary(juros1)
sd(juros1)
coef.var=sd(juros1)/mean(juros1)*100
skewness(juros1)
kurtosis(juros1)
# Gráficos dos dados brutos
ts.plot(juros1,gpars=list(xlab="Ano", main="Número Índice - Juros Real", ylab="", lty=c(1:12)))
legend('topleft', c('Juros'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

dev.off
# Teste de nomalidade:
## Jarque-Bera
jarque.bera.test(juros1)
### A série rejeita H0, ou seja, a série não segue a normal.

## Confirmação: Histograma
hjuros1<-hist(juros1, breaks=10, col="gray", xlab="", ylab="", main="Juros") 
juros1fit<-seq(min(juros1),max(juros1),length=40) 
yjuros1fit<-dnorm(juros1fit,mean=mean(juros1),sd=sd(juros1)) 
yjuros1fit <- yjuros1fit*diff(hjuros1$mids[1:2])*length(juros1) 
lines(juros1fit, yjuros1fit, col="black", lwd=2)



# Decomposição gráfica
plot(decompose(juros1))

#### Cambio

summary(cambio1)
sd(cambio1)
coef.var=sd(cambio1)/mean(cambio1)*100
skewness(cambio1)
kurtosis(cambio1)
# Gráficos dos dados brutos
ts.plot(cambio1,gpars=list(xlab="Ano", main="Número Índice - Câmbio Real", ylab="", lty=c(1:12)))
legend('topleft', c('Câmbio'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

dev.off
# Teste de nomalidade:
## Jarque-Bera
jarque.bera.test(cambio1)
### A série rejeita H0, ou seja, a série não segue a normal.

## Confirmação: Histograma
hcambio1<-hist(cambio1, breaks=10, col="gray", xlab="", ylab="", main="Cambio") 
cambio1fit<-seq(min(cambio1),max(cambio1),length=40) 
ycambio1fit<-dnorm(cambio1fit,mean=mean(cambio1),sd=sd(cambio1)) 
ycambio1fit <- ycambio1fit*diff(hcambio1$mids[1:2])*length(cambio1) 
lines(cambio1fit, ycambio1fit, col="black", lwd=2)



# Decomposição gráfica
plot(decompose(cambio1))

#### Energia

summary(energia1)
sd(energia1)
coef.var=sd(energia1)/mean(energia1)*100
skewness(energia1)
kurtosis(energia1)
# Gráficos dos dados brutos
ts.plot(energia1,gpars=list(xlab="Ano", main="Número Índice - Preço do Petróleo", ylab="", lty=c(1:12)))
legend('topleft', c('Petróleo'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

dev.off
# Teste de nomalidade:
## Jarque-Bera
jarque.bera.test(energia1)
### A série rejeita H0, ou seja, a série não segue a normal.

## Confirmação: Histograma
henergia1<-hist(energia1, breaks=10, col="gray", xlab="", ylab="", main="Energia") 
energia1fit<-seq(min(energia1),max(energia1),length=40) 
yenergia1fit<-dnorm(energia1fit,mean=mean(energia1),sd=sd(energia1)) 
yenergia1fit <- yenergia1fit*diff(henergia1$mids[1:2])*length(energia1) 
lines(energia1fit, yenergia1fit, col="black", lwd=2)

#####Gráficos Dados Brutos
ts.plot(graos1,gpars=list(xlab="Ano", main="Preço  dos Grãos", ylab="Número Índice", lty=c(1:12)))
legend('topleft', c('Grãos'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

ts.plot(juros1,gpars=list(xlab="Ano", main="Taxa de Juros", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Taxa de Juros'), lty=c(1:12), bty='n')

ts.plot(energia1,gpars=list(xlab="Ano", main="Preço  da Energia", ylab="Número Índice", lty=c(1:12)))
legend('topleft', c('Energia'), lty=c(1:12), bty='n')
legend('bottomright', c('Base: Média 2010=100'))

ts.plot(cambio1,gpars=list(xlab="Ano", main="Taxa de Câmbio", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Câmbio'), lty=c(1:12), bty='n')

ts.plot(inflacao1,gpars=list(xlab="Ano", main="Taxa de Inflação", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Inflação'), lty=c(1:12), bty='n')

# Decomposição gráfica
plot(decompose(energia1))


#Tratamento dos dados

###Verificar sazonalidade

##Grãos

#Criando dummies mensais, sazonalidade determinística
mensal1 <- factor(cycle(graos1))
mensal1 <- model.matrix(~mensal1)[,-1]
# Regressão das séries contra as dummies
graos1.saz <- lm(graos1~mensal1)
summary(graos1.saz)
##Não há sazonalidade

##Juros

#Criando dummies mensais, sazonalidade determinística
mensal2 <- factor(cycle(juros1))
mensal2 <- model.matrix(~mensal2)[,-1]
# Regressão das séries contra as dummies
juros1.saz <- lm(juros1~mensal2)
summary(juros1.saz)
##Não há sazonalidade
#

##Câmbio

#Criando dummies mensais, sazonalidade determinística
mensal3 <- factor(cycle(cambio1))
mensal3 <- model.matrix(~mensal3)[,-1]
# Regressão das séries contra as dummies
cambio1.saz <- lm(cambio1~mensal3)
summary(cambio1.saz)
##Não há sazonalidade

##Energia

#Criando dummies mensais, sazonalidade determinística
mensal4 <- factor(cycle(energia1))
mensal4 <- model.matrix(~mensal4)[,-1]
# Regressão das séries contra as dummies
energia1.saz <- lm(energia1~mensal4)
summary(energia1.saz)
##Não há sazonalidade


##inflação

#Criando dummies mensais, sazonalidade determinística
mensal5 <- factor(cycle(inflacao1))
mensal5 <- model.matrix(~mensal5)[,-1]
# Regressão das séries contra as dummies
inflacao1.saz <- lm(inflacao1~mensal5)
summary(inflacao1.saz)
##Não há sazonalidade

### Para retirar tendência, vamos tirar a primeira diferença 
##do preços dos grãos e do preço da energia
## juros e o câmbio já estão em taxa

dgraos <- diff(graos1)

denergia <- diff(energia1)

## Selecionando o período de tempo que será trabalhado
## devido algumas quebras estruturais pegamos a partir de 2000

graos <- window(dgraos, start = c(2000,01))
juros <- window(juros1, start = c(2000,01))
cambio <- window(cambio1, start = c(2000,01))
energia <- window(denergia, start = c(2000,01))
inflacao <- window(inflacao1, start = c(2000,01))

####Gráficos dados tratados 

ts.plot(graos,gpars=list(xlab="Ano", main="Preço  dos Grãos", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Grãos'), lty=c(1:12), bty='n')

ts.plot(juros,gpars=list(xlab="Ano", main="Taxa de Juros", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Taxa de Juros'), lty=c(1:12), bty='n')

ts.plot(energia,gpars=list(xlab="Ano", main="Preço  da Energia", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Energia'), lty=c(1:12), bty='n')

ts.plot(cambio,gpars=list(xlab="Ano", main="Taxa de Câmbio", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Câmbio'), lty=c(1:12), bty='n')

ts.plot(inflacao,gpars=list(xlab="Ano", main="Taxa de Inflação", ylab="Porcentagem", lty=c(1:12)))
legend('topleft', c('Inflação'), lty=c(1:12), bty='n')


###Teste de Raíz unitária
####Dickey Fuller Aumentado Ho = possui raíz unitária
##install.packages("urca")
require('urca')
library(urca)

###grãos
########em nível
##Sem tendência
adfg1_nivel <- ur.df(graos1, type = "none", selectlags = "AIC")
summary(adfg1_nivel)
## Com tendência
adfg2_nivel <- ur.df(graos1, type = "trend", selectlags = "AIC")
summary(adfg2_nivel)
## Com constante
adfg3 <- ur.df(graos, type='drift', selectlags = "AIC")
summary(adfg3)
acf(adfg3@res, ci.type='ma',
    main='Resíduos de teste ADF Preço dos Grãos',
    xlab='Defasagem')
##### primeira diferença
##Sem tendência
adfg1 <- ur.df(graos, type = "none", selectlags = "AIC")
summary(adfg1)
## Com tendência
adfg2 <- ur.df(graos, type = "trend", selectlags = "AIC")
summary(adfg2)
## Com constante
adfg3 <- ur.df(graos, type='drift', selectlags = "AIC")
summary(adfg3)
acf(adfg3@res, ci.type='ma',
    main='Resíduos de teste ADF Preço dos Grãos',
    xlab='Defasagem')

###juros
##Sem tendência
adfj1 <- ur.df(juros, type = "none", selectlags = "AIC")
summary(adfj1)
## Com tendência
adfj2 <- ur.df(juros, type = "trend", selectlags = "AIC")
summary(adfj2)
## Com constante
adfj3 <- ur.df(juros, type='drift', selectlags = "AIC")
summary(adfj3)
acf(adfj3@res, ci.type='ma',
    main='Resíduos de teste ADF Taxa de Juros',
    xlab='Defasagem')

###cambio
##Sem tendência
adfc1 <- ur.df(cambio, type = "none", selectlags = "AIC")
summary(adfc1)
## Com tendência
adfc2 <- ur.df(cambio, type = "trend", selectlags = "AIC")
summary(adfc2)
## Com constante
adfc3 <- ur.df(cambio, type='drift', selectlags = "AIC")
summary(adfc3)
acf(adfc3@res, ci.type='ma',
    main='Resíduos de teste ADF Taxa de Câmbio',
    xlab='Defasagem')

###Energia
#### em nível
##Sem tendência
adfe1_nivel <- ur.df(energia1, type = "none", selectlags = "AIC")
summary(adfe1_nivel)
## Com tendência
adfe2_nivel <- ur.df(energia1, type = "trend", selectlags = "AIC")
summary(adfe2_nivel)
## Com constante
adfe3_nivel <- ur.df(energia1, type='drift', selectlags = "AIC")
summary(adfe3_nivel)
acf(adfe3_nivel@res, ci.type='ma',
    main='Resíduos de teste ADF Preço da Energia',
    xlab='Defasagem')
### primeira diferença
##Sem tendência
adfe1 <- ur.df(energia, type = "none", selectlags = "AIC")
summary(adfe1)
## Com tendência
adfe2 <- ur.df(energia, type = "trend", selectlags = "AIC")
summary(adfe2)
## Com constante
adfe3 <- ur.df(energia, type='drift', selectlags = "AIC")
summary(adfe3)
acf(adfe3@res, ci.type='ma',
    main='Resíduos de teste ADF Preço da Energia',
    xlab='Defasagem')


###inflação
##Sem tendência
adfi1 <- ur.df(inflacao, type = "none", selectlags = "AIC")
summary(adfi1)
## Com tendência
adfi2 <- ur.df(inflacao, type = "trend", selectlags = "AIC")
summary(adfi2)
## Com constante
adfi3 <- ur.df(inflacao, type='drift', selectlags = "AIC")
summary(adfi3)
acf(adfi3@res, ci.type='ma',
    main='Resíduos de teste ADF Taxa de Inflação',
    xlab='Defasagem')


#install.packages('lmtest')
require('lmtest')
library(lmtest)


####Calculando as defasagens ótimas
#install.packages('vars')
require('vars')
library(vars)
####trasnformar as variáveis em data frame e em série temporal
dados_var <- data.frame(graos,energia,cambio,juros,inflacao)
dados_var <- ts(dados_var, freq = 12, start = c(2000,1), end = c(2016, 11))

var.selec1 <- VARselect(dados_var,
                        type = 'both',
                        lag.max = 7)

var.selec1

### um lag

#####################################################
####### Procedimento de Johansen ####################

jo.eigen <- ca.jo(dados_var, type='eigen', K=6, ecdet='const', 
                  spec='transitory')

jo.trace <- ca.jo(dados_var, type='trace', K=6, ecdet='const', 
                  spec='transitory')


summary(jo.eigen)




####Teste de Granger
grangertest(graos ~ energia, order = 1)
grangertest(energia ~ graos, order = 1)

grangertest(graos ~ cambio, order = 1)
grangertest(cambio ~ graos, order = 1)

grangertest(graos ~ juros, order = 1)
grangertest(juros ~ graos, order = 1)

grangertest(graos ~ inflacao, order = 1)
grangertest(inflacao ~ graos, order = 1)


####estimando o var
var1 <- VAR(dados_var,
             p = 1,
             type = 'both')
summary(var1)

install.packages('dynlm')
require('dynlm')
library('dynlm')

###estimando a matriz robusta
# Estimando somente a 1a equação
tend <- 1:nrow(dados_var) # Criando tendência constante
var1a <- dynlm(graos ~ L(juros,1:1) +
                 L(cambio,1:1) + L(energia,1:1) + L(inflacao,1:1) + tend,
               data = dados_var)
# Montando matriz robusta
install.packages('sandwich')
require('sandwuich')
library(sandwich)
vrob <- sqrt(diag(vcovHAC(var1a)))
summary(vrob)

## teste de autocorrelação
seriala <- serial.test(var1, lags.pt=16, type="PT.adjusted")
seriala$serial
plot(seriala)


test_normalidade <- normality.test(var1)
test_normalidade$jb.mul

## Prova da heterocedasticidade 

arch1<-arch.test(var1)
arch1$arch.mul 

####Função de impulso e resposta
###Para impulso resposta do preço dos grãos


plot(irf(var1,
         impulse = 'graos',
         ortho = FALSE,
         n.ahead = 12))

plot(irf(var1,
         impulse = 'inflacao',
         ortho = FALSE,
         n.ahead = 12))


plot(irf(var1,
         impulse = 'juros',
         ortho = FALSE,
         n.ahead = 12))


plot(irf(var1,
         impulse = 'cambio',
         ortho = FALSE,
         n.ahead = 12))


plot(irf(var1,
         impulse = 'energia',
         ortho = FALSE,
         n.ahead = 12))

#Decomposição da variança
#graos.
var1_fevd_graos<-fevd(var1, n.ahead=12)$graos
var1_fevd_graos 




