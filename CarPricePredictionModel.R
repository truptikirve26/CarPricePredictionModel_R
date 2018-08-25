#Final Project

# import dataset
library(classInt)
library(readxl)
library(corrplot)
library(CCA)
library(yacca)
library(MASS)
library(ggplot2)
library(GGally)

Automobile <- read_excel("C:/Trupti MS/CSC424AdvDataAnalaysis/FinalProject/AutomobileFinal .xlsx")
View(Automobile)


head(Automobile)
str(Automobile)

dim(Automobile)

describe(Automobile)
range(Automobile$normalized_losses)
warnings()

as.numeric(Automobile$normalized_losses)

is.na(Automobile$normalized_losses)

sum(is.na(Automobile))
sum(is.na(Automobile$normalized_losses))

#need to fill in missing values for normalized_losses, checking if the data is normalized
#the data looks skewed to right and hence cannot use mean to fill in missing values.
hist(as.numeric(Automobile$normalized_losses))

# equla depth binning for normalized_losses  65 - 256



## Explanation of how missing values are filled in


# normalized_losses ( used SPSS to remove 37 missing values) used transformation in SPSS, we checked the data is skewed a little to right and hence used SPPS mean of near by points to come up with the missing value

# no. of doors have 1 missing value, we filled that value by checking other values of make,aspiration, engine location, wheel base etc.

# for the value of bore and stroke (displayed in inches) we did some research and found that for the model of mazda, hatchback with rear wheel drive what are the dimensions od bore and stroke and replaced by those values


# for horsepower and peak_rpm, the missing values where for renault car model and we have only two rows of data, hence here as well we just identified some renault model from internet and replaced the missing values




######################### PCA  ###########

#considering all numeric variables
automobilenum = Automobile[,c(26,1,2,10:14,17,19:25)]


describe(automobilenum)
str(automobilenum)


# Box plots
boxplot(Automobile$price~Automobile$make,main="Boxplot of Price vs Make",xlab="Make of the car",ylab="Price of the car")

boxplot(Automobile$curb_weight~Automobile$make,main="Boxplot of Curb_Weight vs Make",xlab="Make of the car",ylab="CUrb-Weight")


boxplot(Automobile$peak_rpm~Automobile$make,main="Boxplot of peak-rpm vs Make",xlab="Make of the car",ylab="peak-rpm")




#correlation of numeric data

cor.pricecitympg = cor(automobilenum$price,automobilenum$city_mpg)
cor.pricecitympg


cor.autonum = cor(automobilenum)
cor.autonum

# plotting the correlations
corrplot(cor.autonum, method="square")

corrplot.mixed(cor.autonum, lower.col = "black", number.cex = .7)


#performing PCA/FA to come up with sets of variables.

pbfi <- prcomp(automobilenum,center=T,scale=T)
print(pbfi)
plot(pbfi)
abline(1,0)

screeplot(pbfi)
abline(1,0)

# carrying out factor analyis to come up with 2 factors which can be used as sets of variables
fit = factanal(automobilenum, 2)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)

#Carrying out CC on factor1 and factor2


## functions of CC


ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

str(automobilenum)

#set of IV

Factor1 = automobilenum[c(1,4,5,6,8,9,10,13,15,16)] 
Factor1

# set of DV

Factor2 = automobilenum[c(2,7,12,14)] 
Factor2

# correlation among the IV and DV factors
ggpairs(Factor1)
ggpairs(Factor2)

#correlations between the IV and DV factors
matcor(Factor1, Factor2)


#carrying oyt simple CCA
ccAutomobile = cc(Factor1, Factor2)

#displays canonical correlations
ccAutomobile$cor

# raw canonical coeffients
ccAutomobile[3:4]
ls(ccAutomobile)



# compute canonical loadings
cc2 <- comput(Factor1,Factor2,ccAutomobile)
cc2[3:6]


# to understand the significance of the variates
wilksCan = ccaWilks(Factor1,Factor2,ccAutomobile)
round(wilksCan, 2)


#compute the scores 
loadingsAuto = comput(Factor1,Factor2,ccAutomobile)
ls(loadingsAuto)
loadingsAuto$corr.X.xscores

loadingsAuto$corr.Y.yscores
loadingsAuto$corr.X.yscores
loadingsAuto$corr.Y.xscores
loadingsAuto$xscores
loadingsAuto$yscores

#standardized coefficients
s1 = diag(sqrt(diag(cov(Factor1))))
s1 %*% ccAutomobile$xcoef

s2 = diag(sqrt(diag(cov(Factor2))))
s2 %*% ccAutomobile$ycoef


str(Factor1)
str(Factor2)
