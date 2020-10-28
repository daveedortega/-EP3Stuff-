setwd("/Users/DOrtega/Documents/Rstuff/EP3Stuff/Data")
data<-read.csv("dips.csv")
l<-1:300

#election data
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

#incumbent data
inc<-read.csv("incumbents.csv")
inc_2006<-inc[901:1200,]
row.names(inc_2006)<-l
inc_2009<-inc[1201:1500,]
row.names(inc_2009)<-l
inc_2012<-inc[1501:1800,]
row.names(inc_2012)<-l
inc_2015<-inc[1801:2100,]
row.names(inc_2015)<-l
#2018 values DIFERENT
inc_2018<-inc[2101:2400,]
row.names(inc_2018)<-l


#getting share of effective vote

sov<-function(x){
  y<-matrix(0,nrow(x),3)
  for (i in 1:nrow(x)){
    y[i,1]<-(x[i,14]/x[i,36])
    y[i,2]<-(x[i,16]/x[i,36])
    y[i,3]<-(1-(y[i,1]+y[i,2]))
  } #reduces decimal places
  cien<-matrix(100,300,3)
  y<-y*cien
  y = formatC(y, digits = 2, format = "f")
  return(y)
}

#transforming character matrix to numeric in order to operate
to_numeric<-function(sov, data){
  a=sov[,1]
  b=sov[,2]
  c=sov[,3]
  l=list(a,b,c)
  sovf<-sapply(l,FUN=as.numeric)
  colnames(sovf)<-c("Ganador", "Segundo", "Suma del resto")
  row.names(sovf)<-c(data[,9])
  return(sovf)
}

#sov in character
sov2006<-sov(data_2006)
sov2009<-sov(data_2009)
sov2012<-sov(data_2012)
sov2015<-sov(data_2015)
#2018
sov2018<-sov(data_2018)

#share final in numeric
sf2006<-to_numeric(sov2006,data_2006)
sf2009<-to_numeric(sov2009,data_2009)
sf2012<-to_numeric(sov2012,data_2012)
sf2015<-to_numeric(sov2015,data_2015)

#2018
sf2018<-to_numeric(sov2018,data_2018)

#differences in shares  inbetween terms last-initial
dif0609<-sf2009-sf2006
dif0912<-sf2012-sf2009
dif1215<-sf2015-sf2012

#2018
dif1518<-sf2018-sf2012

#can call summary in order to get mean, median, max and min, EX.
summary(dif0609)

#Checking for incumbency advantage, checks lists of winners in t1, t2, with
#INCUMBENT LISTIS, bcause it uses single party text, not coalition
incadv<-function(inc1,inc2){
  res<-vector()
  for (i in 1:300){
    a<-inc1[i,'part']
    b<-inc2[i,'part']
    matchab<-pmatch(a,b)
    
    if (!is.na(matchab)){
      res<-c(res,1)
    }else{
      res<-c(res,0)
    }
    
  }
  return(res)
}

#resulting 3 vectors from comparing 4 elections, 0 1 vectors
adv0609<-incadv(inc_2006,inc_2009)
adv0912<-incadv(inc_2009,inc_2012)
adv1215<-incadv(inc_2012,inc_2015)
#2018
adv1518<-incadv(inc_2015,inc_2018)

#resulting 3 correlations in checking 4 vectors
coradv<-c(cor(adv0609,adv0912),cor(adv0912,adv1215),cor(adv1215,adv1518))
summary(coradv)
#NOTE, NEGATIVE CORRELATION WHEN ATTESTING 15 18!
coradv

#number of monopolies

monop<-function(t1,t2,t3,t4,dat){
  res<-vector()
  p<-'part'
  for (i in 1:300){
    if (t1[i,p]==t2[i,p] & t2[i,p]==t3[i,p] & t3[i,p]==t4[i,p]){
      res<-c(res,i)
    }
  }
  nom<-dat[c(res),'cab']
  edo<-dat[c(res),'edo']
  part<-t1[res,'part']
  df<-data.frame(Estado=edo,Ciudad=nom,Cacique=part)
  return(df)
}

#returning monopolies in these years
m06_15<-monop(inc_2006,inc_2009,inc_2012,inc_2015,data_2006)

#2018 historic election

monop18<-function(t1,t2,t3,t4,t5,dat){
  res<-vector()
  p<-'part'
  for (i in 1:300){
    if (t1[i,p]==t2[i,p] & t2[i,p]==t3[i,p] & t3[i,p]==t4[i,p] & t4[i,p]==t5[i,p]){
      res<-c(res,i)
    }
  }
  nom<-dat[c(res),'cab']
  edo<-dat[c(res),'edo']
  part<-t1[res,'part']
  df<-data.frame(Estado=edo,Ciudad=nom,Cacique=part)
  return(df)
}
m06_18<-monop18(inc_2006,inc_2009,inc_2012,inc_2015,inc_2018,data_2006)

#Número de candidatos 
ncand_06<-data_2006[,'ncand']
ncand_09<-data_2009[,'ncand']
ncand_12<-data_2012[,'ncand']
ncand_15<-data_2015[,'ncand']
#2018
ncand_18<-data_2018[,'ncand']
#dataframe showing of no. candidatos
no_candidatos<-data.frame(c(data_2006[,'cab']),candidatos_2006=ncand_06,candidatos_2009=ncand_09,
                          candidatos_2012=ncand_12,candidatos_2015=ncand_15,
                          candidatos_2018=ncand_18)




#TO SHOW:

#SHARE OF EFFECTIVE VOTE MATRIX

sf2006
sf2009
sf2012
sf2015
sf2018


#DIFFERENCES IN EFFECTIVE VOTE MATRIX
dif0609
dif0912
dif1215
dif1518

summary(dif0912)
summary(dif1518)

#MONOPOLIES DATA.FRAME.
m06_15
m06_18


#NO. CANDIDATES DATA.FRAME.

View(no_candidatos)
c1215<-ncand_15-ncand_12
c1215

#H1 reforma electoral y número de partidos en 2015 hace que se postulen más
#(Jeff)
library(ggplot2)

data_candidatos<-data.frame(year=c('2006','2009','2012','2015','2018'),
                candidatos=c(sum(ncand_06),sum(ncand_09),sum(ncand_12),sum(ncand_15),sum(ncand_18)))

ggplot(data_candidatos,aes(x='Year',y='Candidatos'))+geom_bar(stat = "identity")

data_country <- data.frame(country = c("China", "Germany", "UK", "US"), 
                conversion_rate = c(0.001331558,0.062428188, 0.052612025, 0.037800687))
ggplot(data_country, aes(x=country,y = conversion_rate)) +geom_bar(stat = "identity")
#H2
#Revisar si los candidatos del PRI que ganaron en 2009 ganaron por menos o más 
#de lo que ganaron en 2012
#CHECAR SI FORTALECE AL PRI LOCAL (Jeff) Y comparar cuántos sí ganaron
#
#splitting into smaller df's with lesser variables
data2s09<-data_2009[,c('cab','edo','ncand','win','v01','efec')]
data2s12<-data_2012[,c('cab','edo','ncand','win','v01','efec')]
#getting only the ones for pri
split2009<-split(data2s09,data2s09$win,order)
split2012<-split(data2s12,data2s12$win,order)


#one for each coallition
pri09<-split2009$pri
pri_pvem09<-split2009$`pri-pvem`
pri12<-split2012$pri 
pri_pvem12<-split2012$`pri-pvem`

#single df 
pri09t<-merge(split2009$pri,split2009$`pri-pvem`,all=T)
pri12t<-merge(split2012$pri,split2012$`pri-pvem`,all = T)
View(pri09t)
View(pri12t)
#rate


vc<-is.element(pri09t[,1],pri12t[,1])
vci<-is.element(pri12t[,1],pri09t[,2])


pri09t[vc,]
dim(pri09t[vc,])
pri12t[vc,]
dim(pri12t[vci,])

#H3, PARTIDOS UNINOMINALES HACEN LA ELECCIÓN ENTRE DOS PARTIDOS (nivel local)
#(Cox)



