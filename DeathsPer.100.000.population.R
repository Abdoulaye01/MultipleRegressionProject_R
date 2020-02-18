# import packages
library(corrplot)
library(ggplot2)
library(car)

############### Dataset Intro ####################
# Read the dataset 
carCrashes = read.csv(file.choose() )

# View in form of a table rows and columns
View(carCrashes)

#Structure of the data
str(carCrashes)

# Check for NAs
any(is.na(carCrashes))


#########################SUMMARY OF DATA##########################
# 5- number summaries
# Removing the last two columns because they are empty columns
carCrashes_1 = carCrashes[,2:21]
View(carCrashes_1)

# View First five rows
head(carCrashes_1, 5)


# Correlate the dependent(Deaths.per.100.000.population) against the independent values, 
# using round function to 3 decimal places for better reading
round(cor(carCrashes_1),3)

# Alternative
# Using the sapply function to select the numerical data only
num.cols = sapply(carCrashes, is.numeric)

# Check for correlation 
# We can see strong to moderate correlation on most variables
cor.data = cor(carCrashes[, num.cols])
cor.data
corrplot(cor.data, order = "hclust")

# Checking for linearity and the relationship in all variables
pairs(carCrashes_1, panel = panel.smooth, horOdd=TRUE,cex.labels = 0.8, font.labels = 0.5)

# Improve the view for the correlation using subset
var_relationship = carCrashes_1[c(6,16,1,5,14,3,20,7)]
scatterplotMatrix(var_relationship, diagonal=list(method ="qqplot", breaks="FD", cex.labels = 0.5, font.labels = 0.5))

# Checking for variances
var(var_relationship)

###########################Graphical summaries######################
#A. boxplots B. histograms C. scatterplots D. barplots etc.
#Subsetting The variables that I am using to model the regression to draw my Graphical summary
graphical.summaries = carCrashes_1[c(6,16,1,5,14,3,20,7)]
View(graphical.summaries)

scatterplotMatrix(graphical.summaries, diagonal=list(method ="histogram", breaks="FD", cex.labels = 5, font.labels = 5), main ="Histogram of Variables") # Deaths.per.100.000.population looks symmetrical

scatterplotMatrix(graphical.summaries, diagonal=list(method ="boxplot", breaks="FD"), main ="Boxplot of Variables") # Present of outliers

scatterplotMatrix(graphical.summaries, diagonal=list(method ="density", breaks="FD"), main = " Density of Variables")

scatterplotMatrix(graphical.summaries, diagonal=list(method ="qqplot", breaks="FD"), main = " Qqplot of Variables")

plot(graphical.summaries, main = "Scatter-plot of Variables") # Scatterplot 

# Barplot dimension is too small have to enlarge the screen
# Add the state to make the bins or bars more readable
barplot(carCrashes$Single.vehicle~ carCrashes$State, main="Single Vehicle",
        col =as.factor(carCrashes$State),
        las=2)

barplot(carCrashes$Deaths.per.100.000.population~ carCrashes$State, main="Deaths.per.100.000.population",
        col =as.factor(carCrashes$State),
        las=2)

barplot(carCrashes$Deaths.per.100.million.vehicle.miles.traveled~ carCrashes$State, main="Deaths.per.100.million.vehicle.miles.traveled",
        col =as.factor(carCrashes$State),
        las=2)

barplot(carCrashes$Population ~ carCrashes$State, main="Population",
        col =as.factor(carCrashes$State),
        las=2)

barplot(carCrashes$Fatal.crashes~ carCrashes$State, main="Fatal.crashes",
        col =as.factor(carCrashes$State),
       las=2)

barplot(carCrashes$Unrestrained.fatally.injured.occupants~ carCrashes$State, main="Unrestrained.fatally.injured.occupants",
        col =as.factor(carCrashes$State),
       las=2)

barplot(carCrashes$Rural~ carCrashes$State, main="Rural",
        col =as.factor(carCrashes$State)
        ,las=2)

barplot(carCrashes$Car.occupants~ carCrashes$State, main="Car.occupants",
        col =as.factor(carCrashes$State)
        ,las=2)

####################Model##################
# Due to the dataset having 20 columns ,A random selection of variable was made for the model 
model <- lm(Deaths.per.100.000.population ~ Deaths.per.100.million.vehicle.miles.traveled + 
               Unrestrained.fatally.injured.occupants + Population + Single.vehicle + 
             Car.occupants + Rural + 
            Fatal.crashes, data = carCrashes_1)
summary(model)

# Performing the F-tests,More stars ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05  indicates how significant the variable is
# Anova indicates that only two variables are higly significant 
summary.aov(model)



# I am using the technique stepwise forward regression function to select and subset the predictors for me. 
newcarCrash <- carCrashes[,2:20]
null <- lm(Deaths.per.100.000.population ~ 1, data = newcarCrash) # model only with intercept
full <- lm(Deaths.per.100.000.population ~ ., data = newcarCrash) # model with all variables in the data set
step(null, scope = list(lower=null, upper=full), direction="forward", test = "F")

# Function has given me this predictors
model <- lm(Deaths.per.100.000.population ~ Deaths.per.100.million.vehicle.miles.traveled + 
              Unrestrained.fatally.injured.occupants + Population + Single.vehicle + 
              Fatal.crashes, data = newcarCrash)
summary(model)


#The model is adequate but not good enough so i am going to visualise the variables 
# Going to use the log10() to normalise the dataset 
# which I will assume will give me a good Adjust R-squared
plot(newcarCrash$Deaths.per.100.million.vehicle.miles.traveled, model$residuals)
hist(newcarCrash$Deaths.per.100.million.vehicle.miles.traveled)
plot(newcarCrash$Fatal.crashes, model$residuals)
hist(newcarCrash$Fatal.crashes)
plot(newcarCrash$Population, model$residuals)
hist(newcarCrash$Population)
plot(newcarCrash$Single.vehicle, model$residuals)
plot(newcarCrash$Unrestrained.fatally.injured.occupants, model$residuals)
hist(newcarCrash$Unrestrained.fatally.injured.occupants)

# Defining  and visualising log10() on the variables
hist(log10(newcarCrash$Unrestrained.fatally.injured.occupants))
hist(log10(newcarCrash$Fatal.crashes))


# Running the new model with the adjusted predictors
final.model= lm(Deaths.per.100.000.population ~ Deaths.per.100.million.vehicle.miles.traveled + 
               Unrestrained.fatally.injured.occupants+ log10(Population) + Single.vehicle + 
               log10(Fatal.crashes), data = newcarCrash)
summary(final.model)

#par(mfrow=c(2,2)) # to view all 4 plots at once
plot(final.model)
AIC(final.model)


model <- lm( Deaths.per.100.000.population ~ Deaths.per.100.million.vehicle.miles.traveled + 
               Single.vehicle + log10(Fatal.crashes) +
             + log10(Population)  , data = newcarCrash)
summary(model)

# Output the coefficients for all the independent variables
model

plot(model)


#Remove or keep  Unrestrained.fatally.injured.occupants?
#Since the p-value > 0.05 , we fail to reject the null hypothesis 
#that the means are the same.It looks like  Unrestrained.fatally.injured.occupants does not 
#make any difference in the Deaths.per.100.000.population and we can remove it from the model.
anova(final.model, model)


mean(fitted(model))
AIC(final.model)
AIC(model)






