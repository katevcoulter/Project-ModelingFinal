###R script for checking for leverage points, outliers, and influential points, outliers

# let's look at results for the diamonds data

#load in the library MASS.  This should be automatically installed.  If not, then use 
#install.packages("MASS")

library(MASS)

#let's look at the leverage values -- hatvalues is the command to use
leverage = hatvalues(reglogPonAll)

#and also the Cook's Distance values -- cooks.distance is the command to use
cooks = cooks.distance(reglogPonAll)

#make a new diamonds dataset that appends the leverage and cooks distance values
#I chose to make a new dataset because I didn't want to clutter the original one with stuff I don't care too much about in the end 
d2 = cbind(diamonds, leverage, cooks)

#take a look at the leverage values to pick off a few large ones for examination
hist(leverage, main = "Leverage values for diamonds regression")

#.08 seems like it would give us a few cases with largest leverage values. Let's look at them.
d2[d2$leverage > .08,]

#mostly we are looking for odd values that could be data errors or diamond types we don't care about
#nothing stands out...

#same thing with Cooks disnace
hist(cooks, main = "Cook's distances for diamonds regression")

#.015 gives us a few cases with largest Cooks Distance values.  Let's look at the cases.
d2[d2$cooks > .015,]

#not surprisingly, these are the least and most expensive diamonds.  But no obvious pattern suggesting another transformation or that results driven by unusual diamonds.   

# here is the command for computing the standardized residuals

reglogPonAllstres = rstandard(reglogPonAll)

#use these in plots of residuals versus predictors.  Here is the plot versus Carats 

plot(y = reglogPonAllstres, x = diamonds$Carats, ylab = "Standardized residuals", xlab = "Carats", main = "Standardized residuals versus predictor")
