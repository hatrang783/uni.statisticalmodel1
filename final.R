install.packages("haven")
# Load the haven package
library(haven)

# Specify the path to your Stata dataset
file_path <-"C:/Users/HUONG GIANG/Desktop/UNI/DONE/Econometrics/FINAL/final_data/project4.dta"
#file_path <-"C:/Users/phamm/Downloads/project4.dta"

# Read the Stata dataset
data <- read_dta(file_path)

data
dim(data)
names(data)
str(data)

#Convert to factor
data$region = as.factor(data$region)
levels(data$region)
data$gender = as.factor(data$gender)
levels(data$gender)
data$married = as.factor(data$married)
levels(data$married)
data$SOE = as.factor(data$SOE)
levels(data$SOE)
data$training = as.factor(data$training)
levels(data$training)
data$urban = as.factor(data$urban)
levels(data$urban)

#### DATA PREPROCESSING
# Check missing values
colSums(is.na(data))
data <- na.omit(data) #no missing value
dim(data)

# Duplicated values
sum(duplicated(data))
data <- unique(data)
dim(data)

# Outliers Handling
par(mfrow = c(1,3))
boxplot(data$exper,main = "Work experience")
boxplot(data$wage,main = "Wage (hourly wage in thousand VND)")
boxplot(data$edu,main="Education") # outliers exist

#Remove outliers
##for Exper 
quartiles <- quantile(data$exper, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$exper)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data <- subset(data, data$exper > Lower & data$exper < Upper)
dim(data)


##for Wage 
quartiles <- quantile(data$wage, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$wage)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data <- subset(data, data$wage > Lower & data$wage < Upper)
dim(data)


##for Edu
quartiles <- quantile(data$edu, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$edu)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data <- subset(data, data$edu > Lower & data$edu < Upper)
dim(data)

#### VARIABLE SELECTION
library(stats)
big_model = lm(wage ~ region+edu+exper+urban+married+training+gender+SOE,data=data)
summary(big_model)

model_aic_back = step(big_model, direction  = "backward")
summary(model_aic_back)

#Check collinearity 
library(car)
vif(big_model) 

#model_test = lm(training ~ edu,data = data)
#summary(model_test)
par(mfrow = c(1,5))
#### ASSUMPTIONS
## Check Linearity
par(mfrow = c(1,1))
plot(fitted(model_aic_back), resid(model_aic_back), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_AIC")
abline(h = 0, col = "darkorange", lwd = 2)

## Check Equal Variance (Homoskedasticity)
# H0: Homoscedasticity
# H1: Heteroscedasticity
library(lmtest)
bptest(model_aic_back) # violate

## Check Normality
hist(resid(model_aic_back))

par(mfrow = c(1,1))
qqnorm(resid(model_aic_back), main = "Model_AIC", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2) #violate


library(lmtest)
## Check Independence of error
dwtest(model_aic_back) 

##### TRANSFORMATION
## log dependent
model_aic_back_log = lm(log(wage) ~ region+exper+urban+married+gender+training+edu+SOE, data = data)
summary(model_aic_back_log)

## polynomial
model_aic_back_poly = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+training+gender+SOE,data=data)
summary(model_aic_back_poly)

## log independent
model_aic_back_log2 = lm(log(wage) ~ region + gender + urban +SOE + married  +log(exper+1) +training + log(edu+1) , data = data)
summary(model_aic_back_log2)

## boxcox
library(MASS)
boxcox(model_aic_back, plotit = TRUE)
boxcox(model_aic_back, plotit = TRUE, lambda = seq(0.4, 0.6, by = 0.05)) #0.5
model_aic_back_cox = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE, data = data)
summary(model_aic_back_cox)

########## COMPARATION

par(mfrow = c(2,3))

## Constant variance
plot(fitted(model_aic_back), resid(model_aic_back), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_AIC")
abline(h = 0, col = "darkorange", lwd = 2)
bptest(model_aic_back)

plot(fitted(model_aic_back_log), resid(model_aic_back_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_Log Dependent")
abline(h = 0, col = "darkorange", lwd = 2)
bptest(model_aic_back_log)

plot(fitted(model_aic_back_cox), resid(model_aic_back_cox), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_Boxcox")
abline(h = 0, col = "darkorange", lwd = 2)
bptest(model_aic_back_cox)

plot(fitted(model_aic_back_log2), resid(model_aic_back_log2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_Independence Log")
abline(h = 0, col = "darkorange", lwd = 2)
bptest(model_aic_back_log2)

plot(fitted(model_aic_back_poly), resid(model_aic_back_poly), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Model_AIC_Poly")
abline(h = 0, col = "darkorange", lwd = 2)
bptest(model_aic_back_poly)

## Normality
qqnorm(resid(model_aic_back), main = "Model_AIC", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2)

qqnorm(resid(model_aic_back_log), main = "Model_Log Dependent", col = "darkgrey")
qqline(resid(model_aic_back_log), col = "dodgerblue", lwd =2)

qqnorm(resid(model_aic_back_cox), main = "Model_Boxcox", col = "darkgrey")
qqline(resid(model_aic_back_cox), col = "dodgerblue", lwd =2)

qqnorm(resid(model_aic_back_log2), main = "Model_Independence Log", col = "darkgrey")
qqline(resid(model_aic_back_log2), col = "dodgerblue", lwd =2)

qqnorm(resid(model_aic_back_poly), main = "Model_AIC_Poly", col = "darkgrey")
qqline(resid(model_aic_back_poly), col = "dodgerblue", lwd =2)

## Independence of error
dwtest(model_aic_back)
dwtest(model_aic_back_log)
dwtest(model_aic_back_cox)
dwtest(model_aic_back_log2)
dwtest(model_aic_back_poly)

#######################

#### Model Selection (POLY)
model_aic_back_poly = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+training+gender+SOE,data=data)

model_aic_back_poly_intEduGen = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+training+gender+SOE+edu:gender, data=data)
summary(model_aic_back_poly_intEduGen) #0.2629
anova(model_aic_back_poly, model_aic_back_poly_intEduGen) #0.0002238 ***

model_aic_back_poly_intEduMarried = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+edu:married, data=data)
summary(model_aic_back_poly_intEduMarried) #0.2617
anova(model_aic_back_poly, model_aic_back_poly_intEduMarried) #0.7678

model_aic_back_poly_intEduSOE = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+edu:SOE, data=data)
summary(model_aic_back_poly_intEduSOE) #0.2635 
anova(model_aic_back_poly, model_aic_back_poly_intEduSOE) # 1.453e-12 ***

model_aic_back_poly_intEduTrain = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+edu:training, data=data)
summary(model_aic_back_poly_intEduTrain) # 0.2633
anova(model_aic_back_poly, model_aic_back_poly_intEduTrain) #4.777e-10 ***

model_aic_back_poly_intEduUrban = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+edu:urban, data=data)
summary(model_aic_back_poly_intEduUrban) #0.2633
anova(model_aic_back_poly, model_aic_back_poly_intEduUrban) #3.18e-09 ***

model_aic_back_poly_intEduRegion = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+edu:region, data=data)
summary(model_aic_back_poly_intEduRegion) #0.2636
anova(model_aic_back_poly, model_aic_back_poly_intEduRegion) #5.169e-11 ***

model_aic_back_poly_intExpergender = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:gender, data=data)
summary(model_aic_back_poly_intExpergender) #0.2619 
anova(model_aic_back_poly, model_aic_back_poly_intExpergender) #0.00748 **

model_aic_back_poly_intExpermarried = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:married, data=data)
summary(model_aic_back_poly_intExpermarried) #0.2628
anova(model_aic_back_poly, model_aic_back_poly_intExpermarried) #0.3362

model_aic_back_poly_intExpSoe = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:SOE, data=data)
summary(model_aic_back_poly_intExpSoe) #0.2617
anova(model_aic_back_poly, model_aic_back_poly_intExpSoe)#0.2046

model_aic_back_poly_intExpTrain = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:training, data=data)
summary(model_aic_back_poly_intExpTrain) #0.2639
anova(model_aic_back_poly, model_aic_back_poly_intExpTrain) #2.2e-16 ***

model_aic_back_poly_intExpUrban = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:urban, data=data)
summary(model_aic_back_poly_intExpUrban) #0.2628
anova(model_aic_back_poly, model_aic_back_poly_intExpUrban) #0.0112 *

model_aic_back_poly_intExpRegion = lm(wage ~ region+edu+I(edu^2)+exper+I(exper^2)+urban+married+gender+training+edu+SOE+exper:region, data=data)
summary(model_aic_back_poly_intExpRegion) #0.2643
anova(model_aic_back_poly, model_aic_back_poly_intExpRegion) #2.2e-16 ***

########## model_aic_back_poly_intExpRegion
