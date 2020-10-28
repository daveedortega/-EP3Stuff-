setwd("/Users/DOrtega/Documents/Rstuff/EP3Stuff/Data")
data<-read.csv("dips.csv")
l<-1:300

data_2006<-data[2701:3000,]
row.names(data_2006)<-l
data_2009<-data[3001:3300,]
row.names(data_2009)<-l
data_2012<-data[3301:3600,]
row.names(data_20012)<-l
data_2015<-data[3601:3900,]
row.names(data_2015)<-l
#2018 values DIFERENT
data_2018<-data[3901:4200,]
row.names(data_2018)<-l



#Reduced Data Frame
data2s09<-data_2009[,c('cab','edo','ncand','win','v01','l01','v02','l02','v03','efec')]
data2s12<-data_2012[,c('cab','edo','ncand','win','v01','l01','v02','l02','v03','efec')]
data2s15<-data_2015[,c('cab','edo','ncand','win','v01','l01','v02','l02','v03','efec')]

#Tabla revisando si abandonaron el tercer lugar



#Hipótesis 1: Regla de duverger
abandono12<-round((data2s12$v03/data2s12$v02),2)
abandono09<-round((data2s09$v03/data2s09$v02),2)

share_ganador=round((data2s09$v01/data2s09$efec),2)
share_ganador1=round((data2s12$v01/data2s12$efec),2)

tabla1<-cbind.data.frame(segundo=data2s09$l01,tercer=data2s09$l02,abandono09,data2s09$win,share_ganador)
tabla2<-cbind.data.frame(segundo=data2s12$l01,tercer=data2s12$l02,abandono12,data2s12$win,share_ganador1)

class(tabla2$abandono12)

#Media, Mediana


summary(tabla1$abandono09)
summary(tabla1$share_ganador)

summary(tabla2$abandono12)
summary(tabla2$share_ganador1)

#Interpretación: 
#En promedio el voto que recibió el tercer lugar fue casi la mitad que el segundo
#Mismo que aumentó en el 2012.
#por lo que no se puede hablar de un abandono total a los demás candidatos;
#no obstante, el promedio de votos totales que recibió el primero lugar 
#representa casi la mitad de los votos efectivos. Por lo que sí 
#Existe una concentración del voto fuerte en los primeros dos lugares 
#en los distritos uninominales de mayoría relativa en general. 

#H2: Gráfica bonita ggplot2
#Número de candidatos 

#Interpretación: Pato
#La reforma en 2012(???) sí incrementó el número de candidatos hacia 2015
#Pero, al no permitir las coaliciones,(NOTAR QUE EL PRI SÍ SE ALIÓ CON PVEM) 
#tuvo un efecto cercano a nulo en abrir la representatividad de la cámara
#esto se ve en la cantidad de 

View(data_2015)
ncand_06<-data_2006[,'ncand']
ncand_09<-data_2009[,'ncand']
ncand_12<-data_2012[,'ncand']
ncand_15<-data_2015[,'ncand']
#2018
ncand_18<-data_2018[,'ncand']


library(ggplot2)
Candidatos<-as.numeric(c(sum(ncand_06),sum(ncand_09),sum(ncand_12),sum(ncand_15),sum(ncand_18)))
data_candidatos<-data.frame(Año=c('2006','2009','2012','2015','2018'),
                            Candidatos)

p<-ggplot(data_candidatos,aes(x=Año,y=Candidatos))+geom_bar(fill="dodgerblue2",stat='identity')

p<-p+ggtitle('Número de Candidatos Total por Año')+ geom_hline(yintercept=mean(Candidatos))

data_candidatos

p



#H3: Cantidad de partidos y su share de votos, y share de votos del PRI en los 
#que ganó 2009, 2012, 2015

split09<-split(data2s09,data2s09$win)
split12<-split(data2s12,data2s12$win)
split15<-split(data2s15,data2s15$win)

nop<-c(Partidos09=length(split09),Partidos12=length(split12),Partidos15=length(split15))

pri09<-merge(split09$pri,split09$`pri-pvem`,all=TRUE)
sharepri09<-round(c(pri09$v01/pri09$efec),2)
prisolo09<-split12$pri
pripv09<-split09$`pri-pvem`
comp09<-c(length(prisolo09$win),length(pripv09$win))
pri09<-cbind(pri09[,c(1,2,4,5,10)],sharepri09)

pri12<-merge(split12$pri,split12$`pri-pvem`,all=TRUE)
prisolo12<-split12$pri
pripv12<-split12$`pri-pvem`
comp12<-c(length(prisolo12$win),length(pripv12$win))
sharepri12<-round(c(pri12$v01/pri12$efec),2)
pri12<-cbind(pri12[,c(1,2,4,5,10)],sharepri12)

pri15<-merge(split15$pri,split15$`pri-pvem`,all=TRUE)
prisolo15<-split15$pri
pripv15<-split15$`pri-pvem`
sharepri15<-round(c(pri15$v01/pri15$efec),2)
comp15<-c(length(prisolo15$win),length(pripv15$win))
pri15<-cbind(pri15[,c(1,2,4,5,10)],sharepri15)

escapri<-c(length(pri09$win),length(pri12$win),length(pri15$win))
sovpri<-c(mean(pri09$sharepri09),mean(pri12$sharepri12),mean(pri15$sharepri15))
prisolo<-c(comp09[1],comp12[1],comp15[1])
pricompuesto<-c(comp09[2],comp12[2],comp15[2])

resumen<-cbind(Numero_De_Partidos=nop,Escaños_Pri=escapri,Escaños_PRI=prisolo,
               Escaños_PRI_PVEM=pricompuesto,Porcentaje_del_Voto=sovpri)
row.names(resumen)<-c('2009','2012','2015')
resumen
#Número de partidos que ganaron un escaño, Número de escaños del Pri, compuesto
#por escaños solo y con pvem en promedio que consiguió el pri

#Interpretación: Sí incrementó la cantidad de partidos en el congreso hacia 2015, 
#El PRI se mantuvo estable durante estos años, consiguiendo más o menos 
#El mismo porcentaje del voto y los mismos escaños
#Sin embargo en 2012 NO tuvo efecto la entrada de EPN para que aumentaran dichos
#Escaños, Al mismo tiempo incrementaron los escaños que consiguió con el apoyo
#del PVEM... INTERPRETACIÓN CONTINUAR


