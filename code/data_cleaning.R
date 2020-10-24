
############################################################################################################
### 1.Read the data set ####
bodyfat = read.csv("BodyFat.csv")

# Relevant aspects about data 
str(bodyfat)  
summary(bodyfat)
mean(bodyfat$BODYFAT) 
sd(bodyfat$BODYFAT)

############################################################################################################
### 2 Detect outliers.  ####

#===== (1) Find the outliers.
detect_outlier = function(x, multiplier = 1.5){
  # Compute the difference of 75% and 25% quantiles
  q = quantile(x,probs = 0.75) - quantile(x,probs = 0.25)
  
  # Compute the range of our desired non-outliers.
  low = mean(x) - multiplier * q
  up = mean(x) + multiplier * q
  
  # If one observation exceeds the range, it will be detected as an outlier.
  return(which(x>up | x<low)) 
}

# Call the detect_outlier function with different multipliers for different columns.
out = apply(bodyfat[,-c(1,2)],2,detect_outlier,multiplier = 3) %>% unlist()
out = c(out, detect_outlier(bodyfat$BODYFAT,multiplier = 2))

# Choose the unduplicated outliers.
out = out[duplicated(out) == FALSE]


#===== (2) Look into suspicious data.
bodyfat[42,6]  #hight=29.5
bodyfat[39,5]  #weight=363.15
bodyfat[41,7]  #adiposity=39.1
bodyfat[31,14] #ankle=33.9
bodyfat[86,14] #ankle=33.7


# Remove the outliers.
cleaned = bodyfat[-out,]


############################################################################################################
### 3.Export the non-outliers to a .csv file.
write.csv(cleaned,"cleaned_bodyfat.csv",row.names = FALSE)
