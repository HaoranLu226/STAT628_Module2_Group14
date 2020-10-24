# Created by Junxia Zhao,
# Revised by Xinran Miao (responsible)
rm(list=ls())
############################################################################################################
###1.Read data set ###
bodyfat = read.csv("../data/BodyFat.csv")
cleaned = read.csv("../data/cleaned_bodyfat.csv")

############################################################################################################
####2. Model diagnostics ####

#===== (1) Check for constant variance
# Residuals vs fitter values
plot(y=mod5$residuals,x=mod5$fitted.values,xlab = "Fitted Values", ylab = "Residuals",
     main = 'Residuals vs Fitted',cex.lab=1.3)

#===== (2) Check for linearity
# Bodyfat vs fitted values.
plot(y=cleaned$BODYFAT,x=mod5$fitted.values, main = 'Body Fat vs Fitted Values',
     ylab = 'Body fat (%)', xlab = 'Fitted values (%)',cex.lab=1.3)
plot(cleaned$BODYFAT,cleaned$WEIGHT, main = 'Body Fat vs Weight',
     ylab = 'Body fat (%)', xlab = 'Weight (lb)',cex.lab=1.3)
plot(cleaned$BODYFAT,cleaned$ABDOMEN, main = 'Body Fat vs Abdomen',
     ylab = 'Body fat (%)', xlab = 'Abdomen (cm)',cex.lab=1.3)
plot(cleaned$BODYFAT,cleaned$WRIST, main = 'Body Fat vs Wrist',
     ylab = 'Body fat (%)', xlab = 'Wrist (cm)',cex.lab=1.3)

#===== (3) Check for normality
# qqplot
qqnorm(mod5$residuals,main='QQ Norm Plot',cex.lab=1.3)
qqline(mod5$residuals,col="red")

#===== (4) Check for outliers
# Cook distance for our model.
plot(cooks.distance(mod5),type='h', main = 'Cook distances vs Obs idx',
     xlab='Observation indices',
     ylab = 'Cook distances',ylim=c(0,0.05),cex.lab=1.3)
abline(h='0.05',col='red')
text(0,0.046,'0.05',pos = 4,col='red')

# Histogram of leave-one-out cross validation residual
hist(rstandard(mod5), xlab = "Leave-one-out cross validation residuals",
     main = 'Histogram of Leave-one-out CV Residuals',cex.lab=1.3)



