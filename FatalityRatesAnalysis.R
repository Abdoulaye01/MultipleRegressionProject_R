# import packages
library(ggplot2)
library(ggthemes)
library(dplyr) # easy to work with my data
library(corrgram) # nice packages for correlation
library(corrplot)
library(caTools)


# Reading the dataset 
carCrashes = read.csv(file.choose() )

# viewing in form of a table rows and columns
View(carCrashes)
nrow(carCrashes)
ncol(carCrashes)

#Structure of the data
str(carCrashes)
print(carCrashes)

# Checking for Nas
any(is.na(carCrashes))




#################SUMMARY##########################


# 5- number summaries
# Removing the last two columns because they are not important
carCrashes_1 = carCrashes[,2:21]
View(carCrashes_1)
head(carCrashes_1, n = 5)


# Correlating the dependent and the independent values, using a round function to 2 decimal places
round(cor(carCrashes_1),2)

# Using the sapply function to extract the numerical data only
num.cols = sapply(carCrashes, is.numeric)
nrow(num.cols)
View(num.cols)

# visual correlation , blue indicated strong correlation 
# We can see that correlation on most variables except the 2 
cor.data = cor(carCrashes[, num.cols])
cor.data

corrplot(cor.data, method = 'color')
corrplot(cor.data, order = "hclust")

# Checking for variances
var(carCrashes_1)

hist(num.cols)



###########################Graphical summaries######################
#A. boxplots B. histograms C. scatterplots D. barplots etc.
#Population
boxplot(carCrashes_1$Population)


hist(carCrashes_1$Population, main = "Histogram of Population")

ggplot(carCrashes_1, aes(x = Population)) +
  geom_histogram(binwidth = 1) 

# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Population, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Population %in% outliers,]

plot(Deaths ~ Population, data = carCrashes_1)

barplot(Deaths ~ Population, data = carCrashes_1)

# Vehicles Miles Traveled Per millions

boxplot(carCrashes_1$Vehicle.miles.traveled..millions.)

hist(carCrashes_1$Vehicle.miles.traveled..millions., main = "Histogram of Vehicles MiLes Per Millions")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Vehicle.miles.traveled..millions., plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Vehicle.miles.traveled..millions. %in% outliers,]

plot(carCrashes_1$Vehicle.miles.traveled..millions., data = carCrashes_1)

barplot(carCrashes_1$Vehicle.miles.traveled..millions., data = carCrashes_1)


# Fatal Crashes

boxplot(carCrashes_1$Fatal.crashes)

hist(carCrashes_1$Fatal.crashes, main = "Histogram of Fatal Crashes")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Fatal.crashes, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Fatal.crashes %in% outliers,]

plot(carCrashes_1$Fatal.crashes)

barplot(carCrashes_1$Fatal.crashes)

# Deaths
boxplot(carCrashes_1$Deaths)

hist(carCrashes_1$Deaths, main = "Histogram of Deaths toll")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Deaths, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Deaths %in% outliers,]

plot(carCrashes_1$Deaths)

barplot(carCrashes_1$Deaths)

# Death Per 100.000 Population

boxplot(carCrashes_1$Deaths.per.100.000.population)

hist(carCrashes_1$Deaths.per.100.000.population, main = "Histogram of Death Per 1000.000 million")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Deaths.per.100.000.population, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Deaths.per.100.000.population %in% outliers,]

plot(carCrashes_1$Deaths.per.100.000.population)

barplot(carCrashes_1$Deaths.per.100.000.population)

# Deaths.per.100.million.vehicle.miles.traveled

# Death Per 100.000 Population

boxplot(carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled)

hist(carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled, main = "Histogram of Deaths per 100 million vehicle miles traveled")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled %in% outliers,]

plot(carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled)

barplot(carCrashes_1$Deaths.per.100.million.vehicle.miles.traveled)


# Car.occupants

boxplot(carCrashes_1$Car.occupants)

hist(carCrashes_1$Car.occupants, main = "Histogram of Car occupants")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Car.occupants, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Car.occupants %in% outliers,]

plot(carCrashes_1$Car.occupants)

barplot(carCrashes_1$Car.occupants)

# Pickup.and.SUV.occupants

boxplot(carCrashes_1$Pickup.and.SUV.occupants)

hist(carCrashes_1$Pickup.and.SUV.occupants, main = "Histogram of Pickup and SUV occupants")
# Find outliers from the boxplot
outliers <- boxplot(carCrashes_1$Pickup.and.SUV.occupants, plot=FALSE)$out; 
outliers
# Extract from the dataset
carCrashes_1[carCrashes_1$Pickup.and.SUV.occupants %in% outliers,]

plot(carCrashes_1$Pickup.and.SUV.occupants)

barplot(carCrashes_1$Pickup.and.SUV.occupants)


pairs(carCrashes_1, panel = panel.smooth, horOdd=TRUE,cex.labels = 2, font.labels = 2)



###################Determine the Best Predictive Model#############

model1 <- lm(Deaths ~ Unknown.mode.of.transport + Bicyclists +Population + Vehicle.miles.traveled..millions.  + Large.truck.occupants , data = carCrashes_1)
summary(model1)

model <- lm(Deaths ~  Urban +Pedestrians +Population + Vehicle.miles.traveled..millions.  +Multiple.vehicle + Restrained.fatally.injured.occupants, data = carCrashes_1)
summary(model)

summary.aov(model)
newcarCrash <- carCrashes[,2:20]
null <- lm(Deaths ~ 1, data = newcarCrash) # model only with intercept
full <- lm(Deaths ~ ., data = newcarCrash) # model with all variables in the data set
step(null, scope = list(lower=null, upper=full), direction="forward", test = "F")

model <- lm(Deaths ~ Fatal.crashes + Multiple.vehicle + Single.vehicle + 
              Unrestrained.fatally.injured.occupants, data = newcarCrash)
summary(model)

plot(model)
hist(newcarCrash$Fatal.crashes)
hist(newcarCrash$Unrestrained.fatally.injured.occupants)

final.model <- lm(Deaths ~ log10(Fatal.crashes) + Multiple.vehicle + Single.vehicle + 
              Unrestrained.fatally.injured.occupants, data = newcarCrash)
summary(final.model)

plot(final.model)

newcarCrash = newcarCrash[-2,]
newcarCrash = newcarCrash[-1,]
newcarCrash = newcarCrash[-3,]

nrow(newcarCrash)

final.model <- lm(Deaths ~ log10(Fatal.crashes) + Multiple.vehicle + Single.vehicle + 
                    Unrestrained.fatally.injured.occupants, data = newcarCrash)
summary(final.model)
plot(final.model)

