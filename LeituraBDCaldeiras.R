#LIMPA AMBIENTE DE TRABALHO
rm(list=ls())

#Biblioteca
#install.packages("readxl")
#install.packages("ggplot")
library(readxl)
library(ggplot2) # graficos
library(plotly) # graficos
library(tidyverse) # um pouco de tudo
library(MASS)
library(EnvStats)
#install.packages("ggcorrplot")
library(ggcorrplot)

######### LEITURA DE DADOS ##################
getwd()
#setwd("C://Users//iuri.santos//Downloads")
setwd("C://Users//raque//Downloads")
#my_data <- read_excel("AnaliseAtualizada-BDCaldeiras_03Mar_Raquel.xlsx", sheet = "Andrea_OFFHistFiltrada (VALOR)") 
#my_data<-read.csv2("AnaliseAtualizada-BDCaldeiras_03Mar_Raquel.csv")

my_data<-read.csv2("AnaliseAtualizada-BDCaldeiras_16Mar_Raquel.xlsx - Raquel.csv")


#glimpse(my_data) #visualizaÁ„o de um resumo de dados
#plot(my_data$SomaDeHorasApontadas, my_data$QuantidadePedida) Gr·fico de dispers„o 
View(my_data)

########### tratamento de dados ############

my_data[my_data$Andrea_DEPARTAMENTO.Descricao=="TRA√???AGEM",]$Andrea_DEPARTAMENTO.Descricao<-"TRACAGEM"
my_data$Andrea_DEPARTAMENTO.Descricao<-toupper(my_data$Andrea_DEPARTAMENTO.Descricao)

my_data<-my_data %>%mutate(OFFEscopo=as.factor(paste(OFFEscopo)),
                  OFFCod=as.factor(paste(OFFCod)),
                  Andrea_DEPARTAMENTO.Descricao=as.factor(paste(Andrea_DEPARTAMENTO.Descricao)),
                  TipodeProduto=as.factor(paste(TipodeProduto)),
                  PRODUTOS.Codigo=as.factor(paste(PRODUTOS.Codigo)),
                  Produto.Nome=as.factor(paste(Produto.Nome)),
                  ITENS.Codigo=as.factor(paste(ITENS.Codigo)),
                  ITENS.Descricao=as.factor(paste(ITENS.Descricao)),
                  SomaDeHorasApontadas=as.numeric(SomaDeHorasApontadas),
                  Poder.Linear=as.numeric(Poder.Linear),
                  Poder.Z=as.numeric(Poder.Z),
                  Cap.Normal.Linear=as.numeric(Cap.Normal.Linear),
                  Cap.Normal.Z=as.numeric(Cap.Normal.Z),
                  Evap.Normal.Linear=as.numeric(Evap.Normal.Linear),
                  Evap.Normal.Z=as.numeric(Evap.Normal.Z),
                  Evapora√.√.o.T.H=as.numeric(Evapora√.√.o.T.H.))

my_data<-my_data[!is.na(my_data$SomaDeHorasApontadas),]

my_data$SomaDeHorasApontadasUnitario<-my_data$SomaDeHorasApontadas/my_data$QuantidadePedida
#my_data$SomaDeHorasApontadasUnitario<-NULL #Para apagar

summary(my_data)

###### GRAFICOS ##################
ggplot(my_data, aes(x=QuantidadePedida)) + geom_histogram() #aes associa a qtdpedida ao eixo 'x'

### histograma usando plotly###
plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$Andrea_DEPARTAMENTO,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")
plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$TipodeProduto,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")

### densidades usando plotly###  #Passei para soma de hr apont. uni.
densidadeCaldeira<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Caldeira'])
densidadeTrocador<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Trocador de Calor'])
densidadVasos<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Vasos'])
plot_ly(x = ~densidadeTrocador$x, y = ~densidadeCaldeira$y,, type = 'scatter', mode = 'lines', name = 'Caldeira', fill = 'tozeroy') %>%
  add_trace(x = ~densidadeTrocador$x, y = ~densidadeTrocador$y, name = 'Trocador', fill = 'tozeroy') %>%
  add_trace(x = ~densidadVasos$x, y = ~densidadVasos$y, name = 'Vasos', fill = 'tozeroy') %>%
  layout(title="DuraÁ„o",xaxis=list(title=FALSE),
         yaxis = list(title = 'Densidade'))


### boxplot usando plotly###  #Passei para soma de hr apont. uni.
plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
        color = my_data$TipodeProduto,type = "box") %>%
  layout(yaxis = list(title="DuraÁ„o"), title="Boxplot")#, showlegend = FALSE)

plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
        color = my_data$Andrea_DEPARTAMENTO.Descricao,type = "box") %>%
  layout(boxmode = "group",yaxis = list(title="DuraÁ„o"), title="Boxplot")#, showlegend = FALSE)


#### pontos ####

#soma de horas num eixo
#quantidade pedida
#poder linear

# 3 lugares solda, tracaem, montagem
# 3 produtos caldeiras, trocador de calor e vasos
clean_data <- my_data[!is.na(my_data$Poder.Linear),]
clean_data <- clean_data %>% filter((paste(TipodeProduto)=="Caldeira" | 
                                      paste(TipodeProduto)=="Trocador de Calor" |
                                      paste(TipodeProduto)=="Vasos") & 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="MONTAGEM" | 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="SOLDA" |
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="TRACAGEM")

clean_data <- droplevels(clean_data)
plot_ly(clean_data, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
        color = ~TipodeProduto, size = ~QuantidadePedida, symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
        mode = 'markers')

clena_media<-clean_data%>%
  group_by(TipodeProduto,Andrea_DEPARTAMENTO.Descricao,QuantidadePedida)%>%
  summarise(SomaDeHorasApontadasUnitario=mean(SomaDeHorasApontadasUnitario),Poder.Linear=mean(Poder.Linear))

plot_ly(clena_media, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
        color = ~TipodeProduto, size = ~QuantidadePedida, symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
        mode = 'markers')

summary(my_data)

### correlograma #######

#ggcorrplot(my_data, hc.order = TRUE, type = "lower",
#           lab = TRUE)
