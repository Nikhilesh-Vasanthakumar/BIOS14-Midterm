library(jtools) #Used in summ() function
library(sjPlot) #Used in plot_sums()
library(car) #Used in avPlots()
plants = read.csv("alpineplants.csv")

x1=(plants$mean_T_winter)
z1=x1^1/3 #Normalizing the mean winter temperature.

x2=(plants$mean_T_summer)
z2=x2^1/3 #Normalizing the mean summer temperature.

x3=(plants$light)
z3=x3^1/3 #Normalizing the light received.

x4=(plants$snow)
z4=x4^1/3 #Normalizing the amount of snow fall.

x5=(plants$soil_moist)
z5=x5^1/3 #Normalizing the amount of soil moisture.

x6=(plants$altitude)
z6=x6^1/3 # Normalizing the altitude.

modp1=plants$Carex.bigelowii #Getting the count data of Carex.bigelowii 
modp2=plants$Thalictrum.alpinum # Getting the count data of Thalictrum.alpinum 

for (index in 1:length(plants$Carex.bigelowii)){
  if (modp1[index]==0.5){
    modp1[index]=modp1[index]+0.5
  }                                  #Rounding off the count data to 1 incase of 0.5 plants.
  if (modp2[index]==0.5){
    modp2[index]=modp2[index]+0.5
  }
}

#Data Analysis #Analysing the dependence of the factors in the data
a<-lm(x2~x1)
plot(x=x1,y=x2,col="blue",las=1,xlab="Mean winter temp",ylab="Mean summer temp")
xx=seq(-4,-0.1,by=0.5)
yhat=summary(a)$coef[1,1]+summary(a)$coef[2,1]*xx
lines(x=xx,y=yhat,col="red")

#VIF calculation
r2 = summary(a)$r.squared
cat(1/(1-r2)) #1.2021

#Models
model1=glm(modp1~z1+z2+z6,family='poisson') 
summary(model1)
summ(model1)
avPlots(model1,ylab="Carex.bigelowii | others")

model2=glm(modp2~z1+z2+z4,family='poisson')
summary(model2)
summ(model2)
avPlots(model2,ylab="Thalictrum.alpinum  | others")

modelc=glm(modp1+modp2~z1+z2,family ='poisson' )
summary(modelc)
summ(modelc)
avPlots(modelc,ylab="Carex.bigelowii+Thalictrum.alpinum | others")

plot_summs(model1,model2,modelc)