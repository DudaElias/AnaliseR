nomes <- c("horario", "temperatura", "vento", "umidade", "sensacao")
dados <- read.csv("D:/Documentos/GitHub/AnaliseR/ProjetoRcepagri.csv", header=FALSE, sep=";", col.names = nomes)
library(ggplot2)
library(ggExtra)
library(gridExtra)

dados$horario <- as.POSIXct(dados$horario, format="%d/%m/%Y-%H:%M")

d <- dados[dados$horario >= "2015-01-01" & dados$horario < "2020-01-01",]
d <- na.omit(d)
d$temperatura <- as.numeric(d$temperatura)

plot <- ggplot(d, aes(y=temperatura, x=sensacao))
plot <- plot + geom_line()
plot


summary(d)

head(d, n = 20L)



anyDuplicated(d$horario)
anyDuplicated(d)


d[2831:2832,]

d[duplicated(d),]

d[25205:25206,]


d[129232:129233,]

d[189519:189520,]

dadosOficial <- d[!duplicated(d),]


summary(dadosOficial)


plot <- plot + geom_area()
plot

plot2 <- ggplot(dadosOficial, aes(x=temperatura))
plot2 <- plot2 + geom_histogram()
plot2

# Normalizar dados absurdos


summary(dTeste)

pairs(dadosOficial[,], pch = 19,lower.panel = NULL)


dadosOficial$umidade[dadosOficial$umidade < 5] <- mean(dadosOficial$umidade[dadosOficial$horario >= "2019-01-05" & dadosOficial$horario < "2019-01-06"])
dTeste

mean(dadosOficial$umidade[dadosOficial$horario >= "2019-01-05" & dadosOficial$horario < "2019-01-06"])



mean(dadosOficial$sensacao[dadosOficial$horario >= "2016-08-29 00:00:00" & dadosOficial$horario < "2016-08-30" & dadosOficial$horario != "2016-08-29 07:10:00"])
mean(dadosOficial$sensacao[dadosOficial$horario >= "2016-07-20 00:00:00" & dadosOficial$horario < "2016-07-21" & dadosOficial$horario != "2016-07-20 07:10:00"])
dadosOficial[dadosOficial$horario >= "2016-08-29 00:00:00" & dadosOficial$horario < "2016-08-30" & dadosOficial$horario != "2016-08-29 07:10:00",]
dadosOficial[dadosOficial$horario >= "2016-07-20 00:00:00" & dadosOficial$horario < "2016-07-21" & dadosOficial$horario != "2016-07-20 07:10:00",]


for(i in 1:nrow(dadosOficial)){

  if(dadosOficial$sensacao[i] == 99.9)
  {
    dadosOficial$sensacao[i] <- mean(dadosOficial$sensacao[dadosOficial$horario >= dadosOficial$horario[i-1] & dadosOficial$horario < dadosOficial$horario[i]])
  }
  
}

for(i in 1:nrow(dadosOficial)){
  
  if(dadosOficial$umidade[i] == 0)
  {
    dadosOficial$umidade[i] <- mean(dadosOficial$umidade[dadosOficial$horario >= dadosOficial$horario[i-1] & dadosOficial$horario < dadosOficial$horario[i]])
  }
  
}




# Montar os gráficos por tempo, temperatura e umidade, 
# Tabelas: 
# Aquecimento ok, Variação por mês ok, temperatura e umidade, sensação térmica é influenciada, umidade e horário

#Sensação térmica

plot <- ggplot(dadosOficial, aes(y=temperatura, x=sensacao))
plot <- plot + geom_line()
plot


#pair plot de todas as informações

panel.points<-function(x,y)
{
  points(x,y,cex=0.5)
}

pairs(dadosOficial[,],lower.panel=NULL,upper.panel=panel.points)

#Aquecimento

plot <- ggplot(dadosOficial, aes(y=temperatura, x=horario, colour=horario))
plot <- plot + geom_line()
plot


#Variação por mês

meses <- factor(month.abb[as.integer(format(as.Date(dadosOficial$horario),"%m"))], levels = month.abb, ordered = TRUE)
g <- ggplot(dadosOficial,aes(x=meses,y=temperatura,group=meses,fill=meses))+geom_boxplot()+scale_fill_brewer(palette="Set3")


#Temperatura e umidade

TempUmi <- ggplot(dadosOficial, aes(x=temperatura, y=umidade, color=umidade)) + geom_point()
TempUniHist <- ggMarginal(TempUmi, type="density")
TempUniHist

#Sensação e umidade

SenUmi <- ggplot(dadosOficial, aes(x=sensacao, y=umidade, color=umidade)) + geom_point()
SenUmiHist <- ggMarginal(SenUmi, type="density")
SenUmiHist


#Sensação e vento

SenV <- ggplot(dadosOficial, aes(x = sensacao, y = vento)) +
  geom_point(color="lightsalmon") +
  geom_smooth(color = "red")

SenV


#Umidade por horario


diaEscolhido <- dadosOficial[format(dadosOficial$horario, "%m-%d") == "05-07",]
dia <- factor(as.array(as.Date(diaEscolhido$horario, format="%d", tz='America/Sao_Paulo')))
horas <- as.POSIXct(format(strptime(diaEscolhido$horario,"%Y-%m-%d %H:%M:%S"), '%H:%M'), format='%H:%M')

diaEscolhido <- cbind(diaEscolhido, dia)
rm(dia)
diaEscolhido <- cbind(diaEscolhido,horas)
rm(horas)
UmidadeNoDia <- ggplot(diaEscolhido, aes(y=umidade, x=horas, colour= dia))
UmidadeNoDia <- UmidadeNoDia + geom_line() + scale_x_datetime(date_label = "%H:%M")
UmidadeNoDia



#Tabelas

head(dadosOficial)


estacao <- as.POSIXct(tz="America/Sao_Paulo", origin= '2015-01-01',sapply(dadosOficial$horario, function(x){
  x
}))

estacao <- dadosOficial$horario

diaMeses <- as.POSIXct(format(strptime(diaEscolhido$horario,"%Y-%m-%d %H:%M:%S"), '%m-%d'), format='%m-%d')



dadosOficial[dadosOficial$horario >= "03-20" & dadosOficial$horario < "06-20", "estacao"] <-"outono"
dadosOficial[dadosOficial$horario >= "06-20" & dadosOficial$horario < "09-22", "estacao"] <-"inverno"
dadosOficial[dadosOficial$horario >= "09-22" & dadosOficial$horario < "12-21", "estacao"] <-"primavera"
dadosOficial[is.na(dadosOficial$estacao), "estacao"] <- "verao"


options(max.print=1000000)
table(dadosOficial$temperatura[dadosOficial$horario >= '2015-01-01' & dadosOficial$horario < '2016-01-01'], dadosOficial$estacao[dadosOficial$horario >= '2015-01-01' & dadosOficial$horario < '2016-01-01'])
tabela2 <- table(dadosOficial$temperatura[dadosOficial$horario >= '2019-01-01' & dadosOficial$horario < '2020-01-01'], dadosOficial$estacao[dadosOficial$horario >= '2019-01-01' & dadosOficial$horario < '2020-01-01'])
setwd("D:/Documentos/GitHub/AnaliseR/")

tabela <- table(dadosOficial$temperatura[dadosOficial$horario >= '2015-01-01' & dadosOficial$horario < '2016-01-01'], dadosOficial$estacao[dadosOficial$horario >= '2015-01-01' & dadosOficial$horario < '2016-01-01'])
names(dimnames(tabela)) <- c("temperatura", "estações")
names(dimnames(tabela2)) <- c("temperatura", "estações")

tabela
tabela2

tabela3 <- tapply(dadosOficial$temperatura, dadosOficial$estacao, mean)
tabela4 <- tapply(dadosOficial$umidade, dadosOficial$estacao, mean)

tabela3 <- cbind(tabela3, tabela4)
colnames(tabela3) <- c("temperatura", "umidade")
tabela3


tabela5 <- tapply(dadosOficial$sensacao, dadosOficial$estacao, mean)
tabela6 <- tapply(dadosOficial$umidade, dadosOficial$estacao, mean)
tabela7 <- tapply(dadosOficial$vento, dadosOficial$estacao, mean)

tabela5 <- cbind(tabela5, tabela6)
tabela5 <- cbind(tabela5, tabela7)
colnames(tabela5) <- c("sensação", "umidade", "vento")
tabela5





