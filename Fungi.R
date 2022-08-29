
library(stats)
library(readr)

#histoplasmosis 
histo<-read_csv("histoplasmosis_data.csv")
histo=histo[complete.cases(histo), ]
histo$health_index<-as.numeric(histo$health_index)
histo$SPI<-as.numeric(histo$SPI)
histo$SMI<-as.numeric(histo$SMI)
histo$precipitation<-as.numeric(histo$precipitation)

health_index.mc=histo$health_index-mean(histo$health_index,na.rm=T)
SPI.mc=histo$SPI-mean(histo$SPI,na.rm=T)
SMI.mc=histo$SMI-mean(histo$SMI,na.rm=T)
precipitation.mc=histo$precipitation-mean(histo$precipitation,na.rm=T)
av.temp.mc=histo$av.temp-mean(histo$av.temp,na.rm=T)
America.mc=histo$America-mean(histo$America,na.rm=T)
log_incidence.mc=histo$log_incidence-mean(histo$log_incidence,na.rm = T)



America.mc=America-mean(America,na.rm=T)
notAmerica=1-histo$America

model1<- lm(histo$log_incidence~histo$av.temp+health_index.mc+SPI.mc+SMI.mc+
              precipitation.mc+histo$av.temp*notAmerica+notAmerica)
summary(model1)

p=-9.15+0.21*histo$av.temp-0.17*histo$av.temp*notAmerica

plot(histo$av.temp,p+resid(model9),col="blue", xlab = "Average Temperature (°C)", ylab = "log(odds of infection)",
     ,main = "The effcts of average temperature on the incidence of histoplasmosis")

points(histo$av.temp[which(America==1.0)],(p+resid(model9))[which(America==1.0)] ,col="orange")

abline(-9.15,0.21,,col="orange")
abline(-9.15,0.21-0.17,col="blue")

hist(histo$log_incidence, xlab = "Incidence", main = "Incidence of Histoplasmosis")



                        
#aspergillosis
asper<-read.csv("aspergillosis_data.csv")
asper=asper[complete.cases(asper), ]
asper$SPI<-as.numeric(asper$SPI)
asper$SMI<-as.numeric(asper$SMI)
asper$precipitation<-as.numeric(asper$precipitation)
asper$log_incidence<-as.double(asper$log_incidence)
asper$disease[which(asper$disease=="IA ")]="IA"
asper$disease[which(asper$disease=="aspergillosisi")]="aspergillosis"
focal.data=asper[which(asper$disease=="CPA"),]
focal.data=asper[which(asper$disease!="aspergillosis"),]
focal.data=asper


library(lme4)
model2<-lmer(log_incidence~health_index+SPI+SMI+precipitation+av.temp+disease+(1|country),data= focal.data)
summary(model2)

plot(log_incidence~av.temp, data= focal.data, col="blue", xlab = "Average Temperature (°C)", 
     ylab = "log(odds of infection)",main = "The effcts of average temperature on the incidence of IA and CPA")

points(focal.data$av.temp[which(focal.data$disease=="IA")],focal.data$log_incidence[which(focal.data$disease=="IA")], col="orange")

hist(focal.data$av.temp[which(focal.data$disease=="IA")],xlab = "Incidence of IA", main = "Histogram of IA Incidence")
hist(focal.data$av.temp[which(focal.data$disease=="CPA")],xlab = "Incidence of CPA", main = "Histogram of CPA Incidence")


library(nlme)
model4<-lme(log_incidence~health_index+SPI+SMI+precipitation+av.temp+disease, random=~1|country,data= asper)
summary(model4)
anova(model4)
library(MuMIn)
r.squaredGLMM(model4)



