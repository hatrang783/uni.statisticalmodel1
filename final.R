install.packages("haven")
# Load the haven package
library(haven)

# Specify the path to your Stata dataset
file_path <-"C:/Users/HUONG GIANG/Desktop/UNI/DONE/Econometrics/FINAL/final_data/project4.dta"
#file_path <-"C:/Users/phamm/Downloads/project4.dta"

# Read the Stata dataset
df <- read_dta(file_path)

df
dim(df)
names(df)
str(df)

#Convert to factor
df$region = as.factor(df$region)
levels(df$region)
df$gender = as.factor(df$gender)
levels(df$gender)
df$married = as.factor(df$married)
levels(df$married)
df$SOE = as.factor(df$SOE)
levels(df$SOE)
df$training = as.factor(df$training)
levels(df$training)
df$urban = as.factor(df$urban)
levels(df$urban)

#### DATA PREPROCESSING
# Check missing values
colSums(is.na(df))
df <- na.omit(df) #no missing value
dim(df)

# Duplicated values
sum(duplicated(df))
df <- unique(df)
dim(df)

# Outliers Handling
par(mfrow = c(1,4))
boxplot(df$log_wage,main = "Log_wage")
boxplot(df$exper,main = "Work experience")
boxplot(df$wage,main = "Wage (hourly wage in thousand VND)")
boxplot(df$edu,main="Education") # outliers exist

#Remove outliers
##for Exper 
quartiles <- quantile(df$exper, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df$exper)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df <- subset(df, df$exper > Lower & df$exper < Upper)
dim(df)


##for Wage - Repeat this step 4 times to remove all outliers.
quartiles <- quantile(df$wage, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df$wage)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df <- subset(df, df$wage > Lower & df$wage < Upper)
dim(df)


##for Edu
quartiles <- quantile(df$edu, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df$edu)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df <- subset(df, df$edu > Lower & df$edu < Upper)
dim(df)

#### TRAIN-TEST SPLIT
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
data  <- df[sample, ]
test   <- df[!sample, ]

#### VARIABLE SELECTION
library(stats)
big_model = lm(wage ~ region+edu+exper+urban+married+training+gender+SOE,data=data)
summary(big_model)

model_aic_back = step(big_model, direction  = "backward")
summary(model_aic_back)

n = length(resid(big_model))
model_back_aic = step(big_model, direction="backward", k = log(n)) #  BIC
summary(model_aic_back)

#Check collinearity 
library(car)
vif(big_model) #remove edu & training because it has vif > 2 
mean(vif(big_model))

#model_test = lm(training ~ edu,data = data)
#summary(model_test)

#### ASSUMPTIONS
## Check Linearity
par(mfrow = c(1,1))
plot(fitted(model_aic_back), resid(model_aic_back), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2) # violate
library(lmtest)
## Check Independence of error
dwtest(model_aic_back) #satisfy

## Check Normality
ks.test(data, 'pnorm')

par(mfrow = c(1,1))
qqnorm(resid(model_aic_back), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2) #satisfy

## Check Equal Variance (Homoskedasticity)
# H0: Homoscedasticity
# H1: Heteroscedasticity
library(lmtest)
bptest(model_aic_back) # violate

###### LOG DEPENDENT
model_aic_back_log = lm(log(wage) ~ region+exper+urban+married+gender+training+edu+SOE, data = data)
summary(model_aic_back_log)
bptest(model_aic_back_log) #violate
plot(fitted(model_aic_back_log), resid(model_aic_back_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2)

shapiro.test(resid(model_aic_back_log)) #p-value small: violated
par(mfrow = c(1,2))
qqnorm(resid(model_aic_back), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2)
qqnorm(resid(model_aic_back_log), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back_log), col = "dodgerblue", lwd =2)

## Check Independence of error
dwtest(model_aic_back_log)

hist(data$wage)
min(data$wage)

###### BOX-COX
library(MASS)
boxcox(model_aic_back, plotit = TRUE)
boxcox(model_aic_back, plotit = TRUE, lambda = seq(0.4, 0.6, by = 0.05)) #0.5
model_aic_back_cox = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE, data = data)

summary(model_aic_back_cox)

bptest(model_aic_back)
bptest(model_aic_back_cox) #violate

par(mfrow = c(1,2))
plot(fitted(model_aic_back), resid(model_aic_back), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2)
plot(fitted(model_aic_back_cox), resid(model_aic_back_cox), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2)

shapiro.test(resid(model_aic_back_cox)) #p-value small: violated
par(mfrow = c(1,2))
qqnorm(resid(model_aic_back), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2)
qqnorm(resid(model_aic_back_cox), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back_cox), col = "dodgerblue", lwd =2)

## Check Independence of error
dwtest(model_aic_back_cox)

####### LOG INDEPENDENCE

model_aic_back_log2 = lm(log(wage) ~ region + gender + urban +SOE + married  +log(exper+1) +training + log(edu+1) , data = data)

summary(model_aic_back_log2)
bptest(model_aic_back_log2)

par(mfrow = c(1,2))
plot(fitted(model_aic_back), resid(model_aic_back), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2)
plot(fitted(model_aic_back_log2), resid(model_aic_back_log2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Check linearity")
abline(h = 0, col = "darkorange", lwd = 2)

shapiro.test(resid(model_aic_back_log2)) #p-value small: violated

par(mfrow = c(1,2))
qqnorm(resid(model_aic_back), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back), col = "dodgerblue", lwd =2)
qqnorm(resid(model_aic_back_log2), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model_aic_back_log2), col = "dodgerblue", lwd =2)
## Check Independence of error
dwtest(model_aic_back_log2)


#### Model Selection
model_aic_back_cox = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE, data = data)

model_aic_back_cox_intEduGen = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:gender, data=data)
summary(model_aic_back_cox_intEduGen)
anova(model_aic_back_cox, model_aic_back_cox_intEduGen) #1*

model_aic_back_cox_intEduMarried = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:married, data=data)
summary(model_aic_back_cox_intEduMarried)
anova(model_aic_back_cox, model_aic_back_cox_intEduMarried) #0.0002317 ***

model_aic_back_cox_intEduSOE = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:SOE, data=data)
summary(model_aic_back_cox_intEduSOE) #0.2373 
anova(model_aic_back_cox, model_aic_back_cox_intEduSOE) #2.2e-16 

model_aic_back_cox_intEduTrain = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:training, data=data)
summary(model_aic_back_intEduTrain) #0.2377 
anova(model_aic_back_cox, model_aic_back_cox_intEduTrain) #2.2e-16 *** 

model_aic_back_cox_intEduUrban = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:urban, data=data)
summary(model_aic_back_cox_intEduUrban) #0.2385 0.2375
anova(model_aic_back_cox, model_aic_back_cox_intEduUrban) #2.2e-16 ***

model_aic_back_cox_intEduRegion = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+edu:region, data=data)
summary(model_aic_back_cox_intEduRegion) #0.2384
anova(model_aic_back_cox, model_aic_back_cox_intEduRegion) #2.2e-16 ***

model_aic_back_cox_intExpergender = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:gender, data=data)
summary(model_aic_back_cox_intExpergender) #0.2361
anova(model_aic_back_cox, model_aic_back_cox_intExpergender) #6.116e-05 ***

model_aic_back_cox_intExpermarried = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:married, data=data)
summary(model_aic_back_cox_intExpermarried) #0.2385 
anova(model_aic_back_cox, model_aic_back_cox_intExpermarried) #2.2e-16 ***

model_aic_back_cox_intExpSoe = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:SOE, data=data)
summary(model_aic_back_cox_intExpSoe) #0.2361 0.2358
anova(model_aic_back_cox, model_aic_back_cox_intExpSoe)#0.009801 **   

model_aic_back_cox_intExpTrain = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:training, data=data)
summary(model_aic_back_cox_intExpTrain) #0.2375  0.237
anova(model_aic_back_cox, model_aic_back_cox_intExpTrain) #2.2e-16 ***  

model_aic_back_cox_intExpUrban = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:urban, data=data)
summary(model_aic_back_cox_intExpUrban) #0.2359
anova(model_aic_back_cox, model_aic_back_cox_intExpUrban) #0.3878 

model_aic_back_cox_intExpRegion = lm(((wage^0.5)-1)/0.5 ~ region+exper+urban+married+gender+training+edu+SOE+exper:region, data=data)
summary(model_aic_back_cox_intExpRegion) #0.2381
anova(model_aic_back_cox, model_aic_back_cox_intExpRegion) #2.2e-16 ***

## best model: model_aic_back_cox_intEduUrban, model_aic_back_cox_intExpermarried
