### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
library("car")
library("ggplot2")
library("GGally")
df = UN
df
View(df)

cor(df$gdp, df$infant.mortality, use = "complete.obs")

# Write code here to make a new dataframe with incomplete rows omitted
df2 = na.omit(df)

# Write code here to calculate correlations
cor(df2$gdp, df2$infant.mortality, use = "complete.obs")

# Write code here to examine the distribution of the data
scatterplot(df2$gdp, df2$infant.mortality)

# Write code here to take the log transformation of the data
df2$infant.mortality.log = log(df2$infant.mortality, 10)
df2$gdp.log = log(df2$gdp, 10)
# Write code here to examine the distribution of the log-transformed data
scatterplot(df2$gdp.log, df2$infant.mortality.log)


# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df2)
summary(linear_fit)
# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality.log ~ gdp.log, df2)
summary(loglog_fit)
# Plot the linear fit of infant mortality vs. GDP
ggplot(df2, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

# Plot of linear fit residuals
qplot(df2$gdp, linear_fit$residuals)

# Plot of linear fit residuals after log transformation of GDP and infant mortality
qplot(df2$gdp.log, df2$infant.mortality.log - exp(fitted(loglog_fit)))

