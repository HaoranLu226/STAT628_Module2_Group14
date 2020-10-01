#setwd("/Users/miaoxinran/Documents/WISC/威斯康星课程/628/Module\ 2/Materials/")
library(dplyr)
bodyfat = read.csv("BodyFat.csv")
summary(bodyfat) # 14 variables

# 1. lm on original dataset. ####
mod1 = lm(BODYFAT~., data = bodyfat[,-c(1,3)])
summary(mod1) #	Adjusted R-squared:  0.7342,p-value: < 2.2e-16
MSE = sum(mod1$residuals^2)/(nrow(bodyfat) - 14) # 15.9007
R2 = summary(mod1)$adj.r.squared
p = pf(summary(mod1)$fstatistic[1],
       summary(mod1)$fstatistic[2],
       summary(mod1)$fstatistic[3],lower.tail = FALSE)
plot(mod1)


# 2.lm on original dataset with selected variables. ####
mod2 = step(mod1) # 8 varaibles
summary(mod2) # 0.7383 ,p-value: < 2.2e-16
MSE = c(MSE,sum(mod2$residuals^2)/(nrow(bodyfat) - 8)) # 15.65568
R2 = c(R2,summary(mod2)$adj.r.squared)
p = c(p,pf(summary(mod2)$fstatistic[1],
       summary(mod2)$fstatistic[2],
       summary(mod2)$fstatistic[3],lower.tail = FALSE))
plot(mod2)


# Detect outliers. ####
detect_outlier = function(x, multiplier = 1.5){
  q = quantile(x,probs = 0.75) - quantile(x,probs = 0.25)
  low = mean(x) - multiplier * q
  up = mean(x) + multiplier * q
  
  return(which(x>up | x<low))
}

out = apply(bodyfat[,-c(1,2)],2,detect_outlier,multiplier = 3) %>% unlist() 
out = c(out, detect_outlier(bodyfat$BODYFAT,multiplier = 2))
out = out[duplicated(out) == FALSE]


length(out)


dat2 = bodyfat[-out,]

# 3. lm on modified dataset. ####
mod3 = lm(BODYFAT~., data = dat2[,-c(1,3)])
summary(mod3) #	Adjusted R-squared:  0.7221   ,p-value: < 2.2e-16
MSE = c(MSE,sum(mod3$residuals^2)/(nrow(dat2) - 14)) # 15.47497
R2 = c(R2,summary(mod3)$adj.r.squared)
p = c(p,pf(summary(mod3)$fstatistic[1],
           summary(mod3)$fstatistic[2],
           summary(mod3)$fstatistic[3],lower.tail = FALSE))
plot(mod3)


# 4. lm on modified dataset with selected variables ####
mod4 = step(mod3) # 8个变量
summary(mod4) #  0.7339 ,p-value: < 2.2e-16
MSE = c(MSE,sum(mod4$residuals^2)/(nrow(dat2) - 8)) # 15.31567
R2 = c(R2,summary(mod4)$adj.r.squared)
p = c(p,pf(summary(mod4)$fstatistic[1],
           summary(mod4)$fstatistic[2],
           summary(mod4)$fstatistic[3],lower.tail = FALSE))
plot(mod4)

# Model Diagnostic ####
plot(mod4$fitted.values,mod4$residuals,xlab = "Fitted Values", ylab = "Residuals")
qqnorm(mod4$residuals)
plot(dat2$BODYFAT,mod4$fitted.values)

# 5. on one variable ####
summary(lm(BODYFAT ~ ABDOMEN, data = dat2)) # R2 = 0.6702,p-value: < 2.2e-16
