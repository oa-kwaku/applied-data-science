# install tidycensus
install.packages("tidycensus")
library("tidycensus")
library("tidyverse")

# get the census data
census_api_key(d0c39077db05593436f5c28e6418f6f5a57683d5, install=TRUE, overwrite = TRUE)
census_api_key("d0c39077db05593436f5c28e6418f6f5a57683d5")

census_data <- get_acs(state = "IL", county = "Cook", geography = "tract", variables = c("DP05_0001E","DP05_0018E","DP03_0062E","DP02_0065PE",'DP03_0096PE','DP03_0128PE','DP04_0047PE'), survey = "acs5", year = 2019, geometry = TRUE, output = "wide")
renaming_pairs <- c(
  "GEOID" = "geoid",
  "NAME" = "name",
  'DP05_0001E' = "total_pop",
  'DP05_0018E' = "med_age",
  'DP03_0062E' = "med_household_income",
  'DP02_0065PE' = "prop_bacc_degree",
  'DP03_0096PE' = "prop_health_insurance",
  'DP03_0128PE' = "prop_poverty",
  'DP04_0047PE' = "prop_occupied_housing_units",
  "geometry" = "geometry"
)

names(census_data) <- ifelse(names(census_data) %in% names(renaming_pairs), renaming_pairs[names(census_data)], names(census_data))

census_data

#4)	Create a map of tract-level baccalaureate attainment rates within Cook County
plot(census_data['prop_bacc_degree'],
     main="Tract-Level Baccalaureate Attainment Rates within Cook County",
)
title(sub="the lighter the color, the higher the percentage of baccalaureate attainment")

#5)	Create a simple linear regression which uses median household income to explain baccalaureate attainment rates at the tract level, using the lm() command.
bacc_degree_model = lm(prop_bacc_degree~med_household_income, census_data)
summary(bacc_degree_model)

#6)	Create an x-y plot showing how median household income can help to explain baccalaureate attainment at the tract-level.  
plot(census_data$med_household_income,
     census_data$prop_bacc_degree,
     xlab ="Median Household Income ($)",
     ylab="Baccalaureate Degree Attainment (%)",
     main="Bacc. Degree Attainment vs. Median Household Income")
abline(bacc_degree_model, col = "red", lty = 2)


#7)	Test the residuals of the simple linear regression for:
## normality, serial correlation, heteroskedasticity
residuals <- residuals(bacc_degree_model)
ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))

  #| Test                                       | Result     |
  #|--------------------------------------------|------------|
  #| Asymptotic one-sample Kolmogorov-Smirnov   |            |
  #| data: residuals                            |            |
  #| D                                          | 0.078957   |
  #| p-value                                    | 1.534e-07  |
  #| alternative hypothesis                     | two-sided  |
  
# There is strong evidence to suggest that the residuals do not follow a normal distribution

qqnorm(bach_degree_model$residuals)
qqline(bach_degree_model$residuals)


# test of heteroskedasticity
bptest(bach_degree_model)

  #| Studentized Breusch-Pagan Test         |             |
  #|----------------------------------------|-------------|
  #| BP (Breusch-Pagan Test Statistic)      | 94.048      |
  #| Degrees of Freedom (df)                | 1           |
  #| p-value                                | < 2.2e-16   |

# plotting to find heteroskedasticity
predicted_values <- predict(bach_degree_model)

plot(predicted_values, residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# the graph shows some clear heteroskedasticity as when we predict higher values, the residuals get higher

# testing for auto-correlation
dwtest(bach_degree_model)

acf(residuals, main = "Autocorrelation Plot of Residuals")

# considering a relatively low p-value of 0.028,
# I conclude that there is a some evidence to suggest some autocorrelation


# 8)	Improve the model as you see fit, using the following techniques:

# lets try applying a log transformation to the response variable to stabilize the variance
census_data$log_prop_bacc_degree = log(census_data$prop_bacc_degree)
census_data$log_prop_bacc_degree[is.infinite(census_data$log_prop_bacc_degree)] = NA

response_log_bach_degree_model = lm(log_prop_bacc_degree~med_household_income, na.omit(census_data))
summary(response_log_bach_degree_model)

qqnorm(response_log_bach_degree_model$residuals)
qqline(response_log_bach_degree_model$residuals)

# autocorrelation
dwtest(response_log_bach_degree_model)

# normality
ks.test(response_log_bach_degree_model$residuals, "pnorm",
        mean = mean(response_log_bach_degree_model$residuals),
        sd = sd(response_log_bach_degree_model$residuals))

census_data_complete.filter(!log_prop_bacc_degree)

# heteroskedasticity
bptest(bach_degree_model)

# heteroskedasticity plot
predicted_values = predict(response_log_bach_degree_model)
residuals = response_log_bach_degree_model$residuals
plot(predicted_values, residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

## lets to try also perfom a lgo transformation on the predictor vars
census_data$log_med_household_income = log(census_data$med_household_income)
census_data$log_med_household_income[is.infinite(census_data$log_med_household_income)] = NA

log_bach_degree_model = lm(log_prop_bacc_degree~log_med_household_income, na.omit(census_data))
summary(response_log_bach_degree_model)

qqnorm(response_log_bach_degree_model$residuals)
qqline(response_log_bach_degree_model$residuals)

# heteroskedasticity
bptest(log_bach_degree_model)

# heteroskedasticity plot
predicted_values = predict(log_bach_degree_model)
residuals = log_bach_degree_model$residuals
plot(predicted_values, residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# autocorrelation
dwtest(log_bach_degree_model)

# normality
ks.test(log_bach_degree_model$residuals, "pnorm",
        mean = mean(log_bach_degree_model$residuals),
        sd = sd(log_bach_degree_model$residuals))


# feature_selection
colnames(census_data_full)
census_data_full = census_data[c("total_pop", "med_age", "med_household_income", "prop_bacc_degree", "prop_health_insurance", "prop_poverty", "prop_occupied_housing_units", "log_prop_bacc_degree", "log_med_household_income")]
full_model = lm(log_prop_bacc_degree ~ total_pop + med_age + med_household_income + prop_health_insurance + prop_poverty + prop_occupied_housing_units + log_med_household_income, na.omit(census_data_full))
stepwise_model =  step(full_model, direction = "forward")

# heteroskedasticity
bptest(stepwise_model)

# heteroskedasticity plot
predicted_values = predict(stepwise_model)
residuals = stepwise_model$residuals
plot(predicted_values, residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# autocorrelation
dwtest(stepwise_model)

# normality
ks.test(stepwise_model$residuals, "pnorm",
        mean = mean(stepwise_model$residuals),
        sd = sd(stepwise_model$residuals))

summary(stepwise_model) # r^2 .6643
summary(log_bach_degree_model) # r^2 .5158
