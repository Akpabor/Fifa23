
###Packages used###
library(readxl)
library(corrplot)


### Model to predict viable cell density for a biopharmaceutical manufacturing process. 
##The available variables are : Viability, Glucose concentration and Lactate Concentration.


### Part 1 Read Data into R####
Biomass_data = read_excel("C:\\Users\\tiver\\OneDrive\\Documents\\Portfolio project\\Biomass data.xlsx")

str(Biomass_data)

summary(Biomass_data)


######Create plots for insights######

cor(Biomass_data)   

corrplot(cor(Biomass_data), method = "circle") 

plot(Biomass_data$Glucose,Biomass_data$VCD,xlab="Glucose", ylab="VCD",pch='.',cex=10,main="Scatterplot with Linear Model",cex.main=1.7,cex.axis=2,cex.lab=2)
abline(model,lwd=2,col="red")


cor.test(Biomass_data$VCD,Biomass_data$Glucose)

cor.test(Biomass_data$VCD,Biomass_data$Lactate)



#######Fitting a Regression model###########

lm(Biomass_data$VCD~Biomass_data$Glucose)


model=summary(lm(Biomass_data$VCD~Biomass_data$Glucose))
model


#######Does our model work#######


###########Is there a pattern in the Residuals####

x11();
plot(model$residuals,xlab="Index", ylab="Residual",pch='.',cex=10,main="Residuals",cex.main=1.7,cex.axis=2,cex.lab=2);abline(h=0)


#####Do we Need Residuals to be Normal?###

x11();hist(model$residuals,xlab="Residual",cex=10,main="Histogram of Residuals",cex.main=1.7,cex.axis=2,cex.lab=2,col="grey")
x11();qqnorm(model$residuals,pch='.',cex=10,cex.main=1.7,cex.axis=2,cex.lab=2)
qqline(model$residuals,lwd=2,col="red")
shapiro.test(model$residuals)


#######Predict using model############

#######Fitting a Regression model part 2 ###########

lm(Biomass_data$VCD~Biomass_data$Glucose +Biomass_data$Lactate +Biomass_data$Viability)
model_2=summary(lm(Biomass_data$VCD~Biomass_data$Glucose +Biomass_data$Lactate +Biomass_data$Viability))
model_2


#########
Vialbiliy = .97
Glucose = .015
Lactate= 0.02





##VCD when Viability is .97###
VCD_Viability= 2.098e+01  +  1.220e+00 +9.912e-01 +  2.845e-07 * Vialbiliy
VCD_Viability
##VCD when Glucose is 0.15###

VCD_Glucose= 2.098e+01  +  1.220e+00 * Glucose +9.912e-01 +  2.845e-07
VCD_Glucose


##VCD when Lactate is 0.02####

VCD_Lactate= 2.098e+01  +  1.220e+00 +9.912e-01* Lactate +  2.845e-07 
VCD_Lactate



