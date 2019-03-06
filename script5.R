#analizar 3 series
#1-tendencia
#2-estacional
#3-sin patron
library(ggplot2)
library(forecast)
library(fpp2)
serie01<-read.csv("C:/Users/ALUMNO-D4/Documents/IPC.csv")
serie02<-read.csv("C:/Users/ALUMNO-D4/Documents/nasdaq.csv")
serie03<-read.csv("C:/Users/ALUMNO-D4/Documents/dowj.csv")
serie01[(1:520),1]<-serie01[(520:1),1]
serie01[(1:520),2]<-serie01[(520:1),2]
serie02[(1:521),1]<-serie02[(521:1),1]
serie02[(1:521),2]<-serie02[(521:1),2]
serie03[(1:588),1]<-serie03[(588:1),1]
serie03[(1:588),2]<-serie03[(588:1),2]
###creacion de la serie de tiempo
ts01<-ts(serie01$Serie,start = 2017,frequency = 260)
ts02<-ts(serie02$Serie,start = 2017,frequency = 260)
ts03<-ts(serie03$Serie,start = 2017,frequency = 294)
###descomposicion
comp01<-decompose(ts01)
comp02<-decompose(ts02)
comp03<-decompose(ts03)
####analisis grafico de los componentes
autoplot(comp01)#tendencia a la baja del ipc mexicano, periodo en el que hay muchas variaciones 
#dadas las situaciones politicas y electorales en México. Ingenuo estacional
autoplot(comp02)#tendencia a la alza, se registran fuertes producciones del sector industrial de 
#la region de la bolsa de NY. Ingenuo estacional 
autoplot(comp03)#tendencia a la alza, se registra alza en el indice accionario dow jones, que 
#representa las acciones de empresas de tecnologia que cotizan en NY
###pronosticos: para los 3 casos se ocupara snaive
#corte de la serie
corte1<-window(ts01,start=2017,end=c(2018,156))
corte2<-window(ts02,start=2017,end=c(2018,156))
corte3<-window(ts03,start=2017,end=c(2018,176))
c01<-window(ts01,start=c(2018,157),end=2019)
c02<-window(ts02,start=c(2018,157),end=2019)
c03<-window(ts03,start=c(2018,157),end=2019)
#generacion del modelo
n01<-snaive(corte1,h=156)
n02<-snaive(corte2,h=156)
n03<-snaive(corte3,h=176)
#calculo de los errores
e01<-accuracy(n01,c01)
e02<-accuracy(n02,c02)
e03<-accuracy(n03,c03)
#Suavizado exponencial simple
ses01<-ses(c01,alpha = 0.8,initial = "simple",h=156)
ses02<-ses(c02,alpha = 0.8,initial = "simple",h=156)
ses03<-ses(c03,alpha = 0.8,initial = "simple",h=176)
#errores ses
se01<-accuracy(ses01,c01)
se02<-accuracy(ses02,c02)
se03<-accuracy(ses03,c03)
###graficas
autoplot(n01)+ggtitle("IPC")+xlab("Periodo")+ylab("IPC")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))
autoplot(n02)+ggtitle("IPC")+xlab("Periodo")+ylab("NASDAQ")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))
autoplot(n03)+ggtitle("IPC")+xlab("Periodo")+ylab("Dow Jones")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))
#ses
autoplot(ts01)+ggtitle("IPC")+autolayer(ses01$fitted)+xlab("Periodo")+ylab("SES IPC")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))
autoplot(ts02)+ggtitle("IPC")++autolayer(ses02$fitted)+xlab("Periodo")+ylab("SES NASDAQ")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))
autoplot(ts03)+ggtitle("IPC")++autolayer(ses03$fitted)+xlab("Periodo")+ylab("SES Dow Jones")+
  theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "red"))

n
