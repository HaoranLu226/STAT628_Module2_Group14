rm(list=ls())
library(dplyr)
library(MASS)
library(ridge)
library(MASS)
library(car)
library(glmnet)
library(lars)
############################################################################################################
###1.Read the original and cleaned data sets. ####
bodyfat = read.csv("BodyFat.csv")
cleaned = read.csv("cleaned_bodyfat.csv.csv")

############################################################################################################
###2.Model selections ####

#===== (1) lm on original dataset. ####
mod1 = lm(BODYFAT~., data = bodyfat[,-c(1,3)])
summary(mod1) #	14 variables, Ad.R-squared:0.7342058, p-value: < 2.2e-16
MSE = sum(mod1$residuals^2)/(nrow(bodyfat) - 14) # 15.9007
R2 = summary(mod1)$adj.r.squared  # 0.7342058        
p = pf(summary(mod1)$fstatistic[1],
       summary(mod1)$fstatistic[2],
       summary(mod1)$fstatistic[3],lower.tail = FALSE) 
#plot(mod1)

#===== (2) lm on original dataset with selected variables. ####
#AIC (k=2)
mod2.1 = step(mod1,direction = "both", k=2)
summary(mod2.1) # 8 variables, Ad.R-squared: 0.7383, p-value: < 2.2e-16
MSE = c(MSE,sum(mod2.1$residuals^2)/(nrow(bodyfat) - 8)) # 15.65568
R2= c(R2,summary(mod2.1)$adj.r.squared)
p = c(p,pf(summary(mod2.1)$fstatistic[1],
       summary(mod2.1)$fstatistic[2],
       summary(mod2.1)$fstatistic[3],lower.tail = FALSE))
#plot(mod2.1)

#BIC (k=log(length(Y))) 
mod2.2 = step(mod1, direction = "both", k=log(length(bodyfat$BODYFAT)))  #BIC (k=log(length(Y))) 
summary(mod2.2) # 5 variables, Ad.R-squared: 0.7308, p-value: < 2.2e-16
MSE = c(MSE,sum(mod2.2$residuals^2)/(nrow(bodyfat) - 5)) # 16.17130
R2 = c(R2,summary(mod2.2)$adj.r.squared)
p = c(p,pf(summary(mod2.2)$fstatistic[1],
           summary(mod2.2)$fstatistic[2],
           summary(mod2.2)$fstatistic[3],lower.tail = FALSE))
#plot(mod2.2)

#===== (3) lm on cleaned dataset. ####
mod3 = lm(BODYFAT~., data = cleaned[,-c(1,3)])
summary(mod3) #	7 variables,Ad.R-squared: 0.7221, p-value: < 2.2e-16
MSE = c(MSE,sum(mod3$residuals^2)/(nrow(cleaned) - 7)) # 15.02173
R2 = c(R2,summary(mod3)$adj.r.squared)
p = c(p,pf(summary(mod3)$fstatistic[1],
           summary(mod3)$fstatistic[2],
           summary(mod3)$fstatistic[3],lower.tail = FALSE))
#plot(mod3)

#===== (4) lm on modified dataset with selected variables by AIC####
mod4 = step(mod3, direction = "both", k=2)
summary(mod4) #	8 variables, Ad.R-squared: 0.7261, p-value: < 2.2e-16
MSE = c(MSE,sum(mod4$residuals^2)/(nrow(cleaned) - 8)) # 15.31567
R2 = c(R2,summary(mod4)$adj.r.squared)
p = c(p,pf(summary(mod4)$fstatistic[1],
           summary(mod4)$fstatistic[2],
           summary(mod4)$fstatistic[3],lower.tail = FALSE))
#plot(mod4)

#===== (5)lm on modified dataset with selected variables by BIC####
mod5 = step(mod3, direction = "both", k=log(length(cleaned$BODYFAT)))  
summary(mod5)# 3 variables, Ad.R-squared: 0.7187 ,p-value: < 2.2e-16
MSE = c(MSE,sum(mod5$residuals^2)/(nrow(cleaned) - 3)) # 15.66277
R2 = c(R2,summary(mod5)$adj.r.squared)
p = c(p,pf(summary(mod5)$fstatistic[1],
           summary(mod5)$fstatistic[2],
           summary(mod5)$fstatistic[3],lower.tail = FALSE))
#plot(mod5)

#===== (6)  lm with sqare and cube terms ####
dat_x <- cleaned[,-(1:3)]
dat_sq <- dat_x^2
for(i in 1:length(names(dat_sq))){
  names(dat_sq)[i] <- paste0(names(dat_sq)[i], "_sq")
}
dat_cube <- dat_x^3
for(i in 1:length(names(dat_cube))){
  names(dat_cube)[i] <- paste0(names(dat_cube)[i], "_cube")
}
dat_sq_cube <- cbind(cleaned, dat_sq, dat_cube)
dat_sq_cube$BMI <- dat_sq_cube$WEIGHT/dat_sq_cube$HEIGHT_sq
mod6 = lm(BODYFAT~., data = dat_sq_cube[,-c(1,3)])
summary(mod6) #	43 variables, Ad.R-squared: 0.7345, p-value: < 2.2e-16
MSE = c(MSE,sum(mod6$residuals^2)/(nrow(dat_sq_cube) - 43)) #14.77401
R2 = c(R2,summary(mod6)$adj.r.squared)
p = c(p,pf(summary(mod6)$fstatistic[1],
           summary(mod6)$fstatistic[2],
           summary(mod6)$fstatistic[3],lower.tail = FALSE))

#===== (7)  lm and stepped sqrt and cube term ####
mod7 <- step(mod6)
summary(mod7)# 20 variables, Ad. R-squared: 0.747 , p-value: < 2.2e-16
MSE = c(MSE,sum(mod7$residuals^2)/(nrow(dat_sq_cube) - 20)) #14.08733
R2 = c(R2,summary(mod7)$adj.r.squared)
p = c(p,pf(summary(mod7)$fstatistic[1],
           summary(mod7)$fstatistic[2],
           summary(mod7)$fstatistic[3], lower.tail = FALSE))

#===== (8)  RIDGE Regression####
clean_dat <- cleaned[,-c(1,3)]
x = as.matrix(clean_dat[, 2:14]) 
y = as.matrix(clean_dat[,1])

#correlation matrix
cor(clean_dat[ ,c(2:14)])

#check for multicollinearity, vif>10
vif(lm(BODYFAT ~ .,data=clean_dat)) 
#scatterplotMatrix
scatterplotMatrix(bodyfat,spread = F,lty.smooth=2)

# Set the range of lambda firstly/
lambdas <- 10^seq(2, -2, by = -.1)
lasso_reg <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Find best lambda.
lambda_best <- lasso_reg$lambda.min 
mod8 <- linearRidge(BODYFAT ~ ., lambda =lambda_best, data = clean_dat)
summary(mod8) #15 variables lambda: 0.1995262

# Calculate R-square
predictions_train <- predict(mod8, s = lambda_best, newx = x)
SSE <- sum((predictions_train- y)^2)
SST <- sum((y - mean(y))^2)
R_square <- 1 - SSE / SST  # R_square: 0.7326164

replicated_R8 <- replicate(10,
  {lasso_reg <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5);
  lambda_best <- lasso_reg$lambda.min ;
  mod <- linearRidge(BODYFAT ~ ., lambda =lambda_best, data = clean_dat);
  predictions_train <- predict(mod, s = lambda_best, newx = x)
  SSE <- sum((predictions_train- y)^2)
  SST <- sum((y - mean(y))^2)
  R_square <- 1 - SSE / SST 
  c(1 - SSE / SST ,length(mod))}) 
apply(replicated_R8,1,mean) # R2=0.7157213, num of variables = 16

#===== (9)  LASSO ####
clean_dat <- cleaned[,-c(1,3)]
LAS_x <- model.matrix(BODYFAT~. , clean_dat)[,-1]
LAS_y <- clean_dat$BODYFAT
lambda_seq <- 10^seq(2, -2, by = -.1)
train = sample(1:nrow(LAS_x), round(nrow(LAS_x)* 0.8))
x_test = (-train)
y_test = LAS_y[x_test]
cv_lams <- cv.glmnet(LAS_x[train,], LAS_y[train],alpha = 1, lambda = lambda_seq, nfolds = 5)
best_lam <- cv_lams$lambda.min
mod9 <- glmnet(LAS_x[train,], LAS_y[train],alpha = 1, lambda = best_lam)
summary(mod9) #10 variables
coef(mod9)

# Calculate R-square
predict_value<- predict(mod9, s = best_lam, newx = LAS_x)
SSE <- sum((predict_value - LAS_y)^2)
SST <- sum((LAS_y - mean(LAS_y))^2)
R_square <- 1 - SSE / SST #R-square: 0.7215974

replicated_R9 = replicate(10,{
  cv_lams <- cv.glmnet(LAS_x[train,], LAS_y[train],alpha = 1, lambda = lambda_seq, nfolds = 5);
  best_lam <- cv_lams$lambda.min;
  mod <- glmnet(LAS_x[train,], LAS_y[train],alpha = 1, lambda = best_lam);
  predict_value<- predict(mod, s = best_lam, newx = LAS_x);
  SSE <- sum((predict_value - LAS_y)^2);
  SST <- sum((LAS_y - mean(LAS_y))^2);
  R_square <- 1 - SSE / SST;
  c(R_square,length(mod))})
apply(replicated_R9,1,mean) # R2 = 0.7254083, num of variables =  12

#===== (10) LASSO-2 ####
x = as.matrix(clean_dat[, 2:14]) 
y = as.matrix(clean_dat[,1])
mod10<- lars(x,y,type = "lasso")
summary(mod10) #9 variables #R-squared:0.73
mod10$beta
mod10$Cp[which.min(mod10$Cp)]
coef<-coef.lars(mod10,mode="step",s=mod10$Cp[which.min(mod10$Cp)])
coef[coef!=0]

############################################################################################################
#### 3.Summary ####
##The best model we choose
mod5 = step(mod3, direction = "both", k=log(length(cleaned$BODYFAT)))  
summary(mod5)# 3 variables, Ad.R-squared: 0.7187 ,p-value: < 2.2e-16
#Example data
predict(mod5,data_frame(WEIGHT=154.25,ABDOMEN=85.2,WRIST=17.7),interval="confidence", level=0.95)



