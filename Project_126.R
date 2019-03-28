library(car)
library(leaps)


##To load data and create In vs. Out of Sample Data
house_data = read.csv("~/Downloads/Real estate valuation data set.csv")
house_data = subset(house_data, select  = -c(No))
house_data_original = house_data
house_data = na.omit(house_data)

colnames(house_data) <- c("Transaction", "House Age", "Distance to the Nearest MRT Station", "# of Convenience Stores Nearby", "Latitude","Longitude", "House Price of Unit Area")

house_data <- house_data[order(house_data$Transaction),]
rownames(house_data) <- 1:nrow(house_data)
house_data_out_of_sample <- house_data[311:414,]
house_data <- house_data[0:310,]


house_data= subset(house_data, select = -c(Transaction))

house_data_with_influential <- house_data 




##Test with Distance as Predictor
plot(((house_data$`Distance to the Nearest MRT Station`)), (house_data$`House Price of Unit Area`),
     main = "House Price vs Distance to MRT",xlab = "Distance to Nearest MRT Station(m)",
     ylab = "House Price of Unit Area", col = "black", pch = 21, bg = "cyan")
mod= lm((house_data$`House Price of Unit Area`) ~ house_data$`Distance to the Nearest MRT Station`)
par(mfrow = c(1,2))
#plot(aov(mod))



par(mfrow = c(1,1))
mod.dffits = dffits(mod)
dffits.influence.mod = 2 * sqrt((2+1)/(length(house_data$`House Age`) -2 -1))
dffits_influential_obs = which(abs(mod.dffits) > dffits.influence.mod)

#Take out influential points
house_data <- house_data_with_influential[-dffits_influential_obs,]

##New Transformed Model with no Influential Points

new.mod = lm(log(house_data$`House Price of Unit Area`) ~ log(house_data$`Distance to the Nearest MRT Station`))
par(mfrow = c(1,2))
plot(log(house_data$`Distance to the Nearest MRT Station`),log(house_data$`House Price of Unit Area`),
     main = "Log of House Price vs Log of Distance to MRT",xlab = "Log of Distance to Nearest MRT Station(m)",ylim = c(2.25,4.5),
     ylab = "Log of House Price of Unit Area", col = "black", pch = 21, bg = "cyan")
abline(new.mod)

#plot(aov(new.mod))
summary(new.mod)
anova(new.mod)

yhat.mod = fitted(new.mod)
e.mod = log(house_data$`House Price of Unit Area`) - yhat.mod
plot(yhat.mod, e.mod,  xlab = 'Fitted Values', ylab = 'Residual', main = 'Residual vs Order')

##Scatterplot and Correlation Matrix
pairs(house_data)  
cor(house_data)

mod.full.log <- lm(log(house_data$`House Price of Unit Area`) ~ (house_data$`House Age`) + log(house_data$`Distance to the Nearest MRT Station`)+
                     (house_data$`# of Convenience Stores Nearby`) + (house_data$Latitude) +(house_data$Longitude)) 

vif(mod.full.log)

mod.reduced <- lm(log(house_data$`House Price of Unit Area`) ~ 1)
step(mod.reduced, scope = list(lower = mod.reduced, upper = mod.full.log))


##Stepwise, Best Subsets


##Stepwise
mod0 = lm(log(house_data$`House Price of Unit Area`) ~ 1)
mod.upper = lm(log(house_data$`House Price of Unit Area`) ~ (house_data$`House Age`) + log(house_data$`Distance to the Nearest MRT Station`)+
                    (house_data$`# of Convenience Stores Nearby`) + (house_data$Latitude) + house_data$Longitude)
step(mod0, scope = list(lower = mod0, upper = mod.upper))


##Best Subsets
mod = regsubsets(cbind(house_data$`House Age`,log(house_data$`Distance to the Nearest MRT Station`),
                 house_data$`# of Convenience Stores Nearby`, house_data$Latitude,house_data$Longitude),log(house_data$`House Price of Unit Area`))
summary.mod = summary(mod)
summary.mod$which
summary.mod$adjr2   

summary.mod$cp
summary.mod$which





##Testing Model with all predictors, with stepwise if one or more predictors is significant
mod.full = lm(log(house_data$`House Price of Unit Area`) ~ (house_data$`House Age`) +
                log(house_data$`Distance to the Nearest MRT Station`)+
                (house_data$`# of Convenience Stores Nearby`) + (house_data$Latitude) + house_data$Longitude)
mod.reduced <- lm(log(house_data$`House Price of Unit Area`) ~ 1)

vif(mod.full)

anova(mod.reduced,mod.full)
par(mfrow = c(1,2))
#plot(aov(mod.full))

## we decide to remove influential points again
mod.dffits.full = dffits(mod.full)
dffits.influence.mod.full = 2 * sqrt((6+1)/(length(house_data$`House Age`) -6 -1))
dffits_influential_obs.full = which(abs(mod.dffits.full) > dffits.influence.mod.full)
house_data_full_mod_no_inf <- house_data[-dffits_influential_obs.full,]

mod.full.no.inf = lm(log(house_data_full_mod_no_inf$`House Price of Unit Area`) ~ (house_data_full_mod_no_inf$`House Age`) +
                       log(house_data_full_mod_no_inf$`Distance to the Nearest MRT Station`)+
                       (house_data_full_mod_no_inf$`# of Convenience Stores Nearby`)
                     + (house_data_full_mod_no_inf$Latitude) + house_data_full_mod_no_inf$Longitude)
mod.reduced.no.inf <- lm(log(house_data_full_mod_no_inf$`House Price of Unit Area`) ~ 1)

vif(mod.full.no.inf)

anova(mod.reduced.no.inf,mod.full.no.inf)
#plot(aov(mod.full.no.inf))

yhat.mod.full.no.inf = fitted(mod.full.no.inf)
e.mod.full.no.inf = log(house_data_full_mod_no_inf$`House Price of Unit Area`) - yhat.mod.full.no.inf
plot(yhat.mod.full.no.inf, e.mod.full.no.inf,  xlab = 'Fitted Values', ylab = 'Residual',
     main = 'Residual vs Order')







##Testing observed vs predicted in in sample data 
estimate_response.in = fitted(mod.full.no.inf)
estimate_response.in = exp(estimate_response.in)

residual.in = house_data_full_mod_no_inf$`House Price of Unit Area` - estimate_response.in
plot( (1:nrow(house_data_full_mod_no_inf)), residual.in,
      main = "Predicted vs Observed House Price In Sample", ylab = 'Residuals',
      xlab= "Index", pch = 21, bg = 'red')

#Testing observed vs predicted in Out of Sample Data
mod.full.out =  lm(log(house_data_out_of_sample$`House Price of Unit Area`) ~ (house_data_out_of_sample$`House Age`) +
                               log(house_data_out_of_sample$`Distance to the Nearest MRT Station`)+
                               (house_data_out_of_sample$`# of Convenience Stores Nearby`) + (house_data_out_of_sample$Latitude) + house_data_out_of_sample$Longitude)

estimate_response.out = fitted(mod.full.out)
estimate_response.out = exp(estimate_response.out)
residual.out = house_data_out_of_sample$`House Price of Unit Area` - estimate_response.out
plot( (1:nrow(house_data_out_of_sample)), residual.out,
      main = "Predicted vs Observed House Price Out of Sample", ylab = 'Residuals',
      xlab= "Index", pch = 21, bg = 'blue')
abline(h = 0, lty = 2)

