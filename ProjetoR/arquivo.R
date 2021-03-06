#Preparando os dados

#Bibliotecas importadas para o projeto
library(ggplot2)
library(ggExtra)
library(gridExtra)

#Cria��o do data frame
nomes <- c("horario", "temperatura", "vento", "umidade", "sensacao")
dados <- read.csv("D:/Documentos/GitHub/AnaliseR/ProjetoR/cepagri.csv", header=FALSE, sep=";", col.names = nomes)

#Transforma ca coluna de horario em tipo POSIXct
dados$horario <- as.POSIXct(dados$horario, format="%d/%m/%Y-%H:%M")

#Seleciona os dados dos anos entre 2015 e 2020
d <- dados[dados$horario >= "2015-01-01" & dados$horario < "2020-01-01",]

#Deleta as linhas que tem dados faltantes
d <- na.omit(d)

#Transforma a coluna de temperatura em numeric
d$temperatura <- as.numeric(d$temperatura)

#Resumo das informa��es do data frame
summary(d)

#Verifica se h� duplicado
anyDuplicated(d)

#dadosOficial ser� o data frame usado durante o projeto, sem nenhum dado duplicado em hor�rio
dadosOficial <- d[!duplicated(d),]

#Resumo das informa��es sem informa��es repetidas
summary(dadosOficial)



# Normalizar dados absurdos


#Gr�fico do tipo pairs para observar os dados e suas correla��es graficamente
pairs(dadosOficial[,], pch = 19,lower.panel = NULL)

#Normaliza a sensa��o t�rmica de 99.9 para o valor do hor�rio anterior
for(i in 1:nrow(dadosOficial)){

  if(dadosOficial$sensacao[i] == 99.9)
  {
    dadosOficial$sensacao[i] <- mean(dadosOficial$sensacao[dadosOficial$horario >= dadosOficial$horario[i-1] & dadosOficial$horario < dadosOficial$horario[i]])
  }
  
}

#Normaliza a umidade de 0 para o valor do hor�rio anterior
for(i in 1:nrow(dadosOficial)){
  
  if(dadosOficial$umidade[i] == 0)
  {
    dadosOficial$umidade[i] <- mean(dadosOficial$umidade[dadosOficial$horario >= dadosOficial$horario[i-1] & dadosOficial$horario < dadosOficial$horario[i]])
  }
  
}


#Adiciona a coluna de estacao do ano usando hardcode

dadosOficial[as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') >=as.POSIXct("3-20", format="%m-%d", tz='America/Sao_Paulo') & as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') < as.POSIXct("6-20" , format="%m-%d", tz='America/Sao_Paulo'), "estacao"] <-"outono"
dadosOficial[as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') >=as.POSIXct("6-20", format="%m-%d", tz='America/Sao_Paulo') & as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') < as.POSIXct("9-22" , format="%m-%d", tz='America/Sao_Paulo'), "estacao"] <-"inverno"
dadosOficial[as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') >=as.POSIXct("9-22", format="%m-%d", tz='America/Sao_Paulo') & as.POSIXct(format(strptime(dadosOficial$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d') < as.POSIXct("12-21" , format="%m-%d", tz='America/Sao_Paulo'), "estacao"] <-"primavera"
dadosOficial[is.na(dadosOficial$estacao), "estacao"] <- "verao"



# Tabelas: 

#Ap�s o tratamento dos dados faz novamente o pair plot para ver se os dados ficaram normalizados

panel.points<-function(x,y)
{
  points(x,y,cex=0.5)
}

pairs(dadosOficial[,],lower.panel=NULL,upper.panel=panel.points)




#Sensa��o t�rmica e temperatura, line plot

plot <- ggplot(dadosOficial, aes(y=temperatura, x=sensacao))
plot <- plot + geom_line()
plot


#Temperatura e hor�rio, line plot

plot <- ggplot(dadosOficial, aes(y=temperatura, x=horario, colour=horario))
plot <- plot + geom_line()
plot


#Varia��o por m�s de temperatura, boxplot

meses <- factor(month.abb[as.integer(format(as.Date(dadosOficial$horario),"%m"))], levels = month.abb, ordered = TRUE) #separa em meses o data frame
g <- ggplot(dadosOficial,aes(x=meses,y=temperatura,group=meses,fill=meses))+geom_boxplot()+scale_fill_brewer(palette="Set3")
g


#Sensa��o e umidade, marginal plot

SenUmi <- ggplot(dadosOficial, aes(x=sensacao, y=umidade, color=umidade)) + geom_point()
SenUmiHist <- ggMarginal(SenUmi, type="density")
SenUmiHist


#Sensa��o e vento, smooth plot

SenV <- ggplot(dadosOficial, aes(x = sensacao, y = vento)) +
  geom_point(color="lightsalmon") +
  geom_smooth(color = "red")

SenV

#Umidade por hor�rio durante um dia de um ano especifico, line plot

dia <- dadosOficial[format(dadosOficial$horario, "%m-%d-%Y") == "05-04-2018",] #Seleciona o dia 4 de maio de 2018
UmidadeHoraNoDia <- ggplot(dia, aes(y=umidade, x=horario)) + geom_line()
UmidadeHoraNoDia

#Umidade por horario durante os 5 anos, line plot


diaEscolhido <- dadosOficial[format(dadosOficial$horario, "%m-%d") == "05-07",] #Seleciona o dia 7 de maio

dia <- factor(as.array(as.Date(diaEscolhido$horario, format="%d", tz='America/Sao_Paulo'))) #Separa o dia de cada ano e coloca como fator

horas <- as.POSIXct(format(strptime(diaEscolhido$horario,"%Y-%m-%d %H:%M:%S"), '%H:%M'), format='%H:%M') #Separa as horas daquele dia especifico do horario todo

diaEscolhido <- cbind(diaEscolhido, dia) #Adiciona a coluna de dia no diaEscolhido

rm(dia) # Remove a vari�vel dia para n�o ter confus�o do programa quando for criar o gr�fico

diaEscolhido <- cbind(diaEscolhido,horas) # Adiciona a coluna de horas no diaEscolhido

rm(horas) # Remove a vari�vel horas para n�o ter confus�o do programa quando for criar o gr�fico

UmidadeNoDia <- ggplot(diaEscolhido, aes(y=umidade, x=horas, colour= dia))      
UmidadeNoDia <- UmidadeNoDia + geom_line() + scale_x_datetime(date_label = "%H:%M")
UmidadeNoDia



#Tabelas

#Todas as tabelas usam valores das m�dias de temperatura, vento, umidade e sensa��o t�rmica em cada esta��o durante os 5 anos da an�lise

tabelaTempEstacao <- tapply(dadosOficial$temperatura, dadosOficial$estacao, mean)
tabelaUmiEstacao <- tapply(dadosOficial$umidade, dadosOficial$estacao, mean)
tabelaSenEstacao <- tapply(dadosOficial$sensacao, dadosOficial$estacao, mean)
tabelaVenEstacao <- tapply(dadosOficial$vento, dadosOficial$estacao, mean)


#Tabela de temperatura e umidade durante as esta��es
tabelaUmiTempEstacao <- cbind(tabelaTempEstacao, tabelaUmiEstacao)
colnames(tabelaUmiTempEstacao) <- c("temperatura", "umidade")
tabelaUmiTempEstacao


#Tabela de sensa��o t�rmica, umidade e vento durante as esta��es

tabelaSenUmiVenEstacao <- cbind(tabelaSenEstacao, tabelaUmiEstacao, tabelaVenEstacao)
colnames(tabelaSenUmiVenEstacao) <- c("sensa��o", "umidade", "vento")
tabelaSenUmiVenEstacao
