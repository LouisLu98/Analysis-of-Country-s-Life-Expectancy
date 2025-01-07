# Project ISYE 6414
# Library
library(lmtest)
library(car)
library(MASS)
library(ggplot2)
library(leaps)

# Set the working directory to where your CSV file is located
setwd('/Users/louis/Desktop/6414project')
#import data
file_name <- "Life_Expectancy_Data.csv"
df <- read.csv(file_name)


# There are two approach here, delete the rows without info, or inpute missing values.

#1  Delete Rows
df <- na.omit(df)
cat("Data after removing rows with any missing values:\n")
print(dim(df))
#2  Imputing numeric colums with median
# Imputing numeric columns with median
#numeric_columns <- sapply(df, is.numeric)
#df[, numeric_columns] <- lapply(df[, numeric_columns], function(x) {
#  ifelse(is.na(x), median(x, na.rm = TRUE), x)})


#See the head of the dataframe
head(df)

# Access with the name of the columns
#Cost =df$Cost
Country = df$Country
Year = df$Year
Status = df$Status
Life_expectancy =df$Life.expectancy
Adult_Mortality =df$Adult.Mortality
Infant_deaths=df$infant.deaths
Alcohol=df$Alcohol
Percentage_expenditure=df$percentage.expenditure
Hepatitis_B=df$Hepatitis.B
Measles = df$Measles
BMI = df$BMI
under_five_deaths = df$under.five.deaths
Polio = df$Polio
Total_expenditure = df$Total.expenditure
Diphtheria = df$Diphtheria
HIV_AIDS = df$HIV.AIDS
GDP = df$GDP
Population = df$Population
Thinning_1_19_years = df$thinness..1.19.years
Thinning_5_9_years = df$thinness.5.9.years
Income_composition_of_resources = df$Income.composition.of.resources
Schooling = df$Schooling

#Since the data is too much, I regroup the countries in Continents to improve the assumptions and reduce collinearity.

continent_mapping <- c(
  "Afghanistan" = "Asia",
  "Albania" = "Europe",
  "Algeria" = "Africa",
  "Angola" = "Africa",
  "Antigua and Barbuda" = "North America",
  "Argentina" = "South America",
  "Armenia" = "Asia",
  "Australia" = "Oceania",
  "Austria" = "Europe",
  "Azerbaijan" = "Asia",
  "Bahamas" = "North America",
  "Bahrain" = "Asia",
  "Bangladesh" = "Asia",
  "Barbados" = "North America",
  "Belarus" = "Europe",
  "Belgium" = "Europe",
  "Belize" = "North America",
  "Benin" = "Africa",
  "Bhutan" = "Asia",
  "Bolivia (Plurinational State of)" = "South America",
  "Bosnia and Herzegovina" = "Europe",
  "Botswana" = "Africa",
  "Brazil" = "South America",
  "Brunei Darussalam" = "Asia",
  "Bulgaria" = "Europe",
  "Burkina Faso" = "Africa",
  "Burundi" = "Africa",
  "CÃ´te d'Ivoire" = "Africa",
  "Cabo Verde" = "Africa",
  "Cambodia" = "Asia",
  "Cameroon" = "Africa",
  "Canada" = "North America",
  "Central African Republic" = "Africa",
  "Chad" = "Africa",
  "Chile" = "South America",
  "China" = "Asia",
  "Colombia" = "South America",
  "Comoros" = "Africa",
  "Congo" = "Africa",
  "Cook Islands" = "Oceania",
  "Costa Rica" = "North America",
  "Croatia" = "Europe",
  "Cuba" = "North America",
  "Cyprus" = "Europe",
  "Czechia" = "Europe",
  "Democratic People's Republic of Korea" = "Asia",
  "Democratic Republic of the Congo" = "Africa",
  "Denmark" = "Europe",
  "Djibouti" = "Africa",
  "Dominica" = "North America",
  "Dominican Republic" = "North America",
  "Ecuador" = "South America",
  "Egypt" = "Africa",
  "El Salvador" = "North America",
  "Equatorial Guinea" = "Africa",
  "Eritrea" = "Africa",
  "Estonia" = "Europe",
  "Ethiopia" = "Africa",
  "Fiji" = "Oceania",
  "Finland" = "Europe",
  "France" = "Europe",
  "Gabon" = "Africa",
  "Gambia" = "Africa",
  "Georgia" = "Asia",
  "Germany" = "Europe",
  "Ghana" = "Africa",
  "Greece" = "Europe",
  "Grenada" = "North America",
  "Guatemala" = "North America",
  "Guinea" = "Africa",
  "Guinea-Bissau" = "Africa",
  "Guyana" = "South America",
  "Haiti" = "North America",
  "Honduras" = "North America",
  "Hungary" = "Europe",
  "Iceland" = "Europe",
  "India" = "Asia",
  "Indonesia" = "Asia",
  "Iran (Islamic Republic of)" = "Asia",
  "Iraq" = "Asia",
  "Ireland" = "Europe",
  "Israel" = "Asia",
  "Italy" = "Europe",
  "Jamaica" = "North America",
  "Japan" = "Asia",
  "Jordan" = "Asia",
  "Kazakhstan" = "Asia",
  "Kenya" = "Africa",
  "Kiribati" = "Oceania",
  "Kuwait" = "Asia",
  "Kyrgyzstan" = "Asia",
  "Lao People's Democratic Republic" = "Asia",
  "Latvia" = "Europe",
  "Lebanon" = "Asia",
  "Lesotho" = "Africa",
  "Liberia" = "Africa",
  "Libya" = "Africa",
  "Lithuania" = "Europe",
  "Luxembourg" = "Europe",
  "Madagascar" = "Africa",
  "Malawi" = "Africa",
  "Malaysia" = "Asia",
  "Maldives" = "Asia",
  "Mali" = "Africa",
  "Malta" = "Europe",
  "Marshall Islands" = "Oceania",
  "Mauritania" = "Africa",
  "Mauritius" = "Africa",
  "Mexico" = "North America",
  "Micronesia (Federated States of)" = "Oceania",
  "Monaco" = "Europe",
  "Mongolia" = "Asia",
  "Montenegro" = "Europe",
  "Morocco" = "Africa",
  "Mozambique" = "Africa",
  "Myanmar" = "Asia",
  "Namibia" = "Africa",
  "Nauru" = "Oceania",
  "Nepal" = "Asia",
  "Netherlands" = "Europe",
  "New Zealand" = "Oceania",
  "Nicaragua" = "North America",
  "Niger" = "Africa",
  "Nigeria" = "Africa",
  "Niue" = "Oceania",
  "Norway" = "Europe",
  "Oman" = "Asia",
  "Pakistan" = "Asia",
  "Palau" = "Oceania",
  "Panama" = "North America",
  "Papua New Guinea" = "Oceania",
  "Paraguay" = "South America",
  "Peru" = "South America",
  "Philippines" = "Asia",
  "Poland" = "Europe",
  "Portugal" = "Europe",
  "Qatar" = "Asia",
  "Republic of Korea" = "Asia",
  "Republic of Moldova" = "Europe",
  "Romania" = "Europe",
  "Russian Federation" = "Europe",  # Note: Russia spans both Europe and Asia
  "Rwanda" = "Africa",
  "Saint Kitts and Nevis" = "North America",
  "Saint Lucia" = "North America",
  "Saint Vincent and the Grenadines" = "North America",
  "Samoa" = "Oceania",
  "San Marino" = "Europe",
  "Sao Tome and Principe" = "Africa",
  "Saudi Arabia" = "Asia",
  "Senegal" = "Africa",
  "Serbia" = "Europe",
  "Seychelles" = "Africa",
  "Sierra Leone" = "Africa",
  "Singapore" = "Asia",
  "Slovakia" = "Europe",
  "Slovenia" = "Europe",
  "Solomon Islands" = "Oceania",
  "Somalia" = "Africa",
  "South Africa" = "Africa",
  "South Sudan" = "Africa",
  "Spain" = "Europe",
  "Sri Lanka" = "Asia",
  "Sudan" = "Africa",
  "Suriname" = "South America",
  "Swaziland" = "Africa",
  "Sweden" = "Europe",
  "Switzerland" = "Europe",
  "Syrian Arab Republic" = "Asia",
  "Tajikistan" = "Asia",
  "Thailand" = "Asia",
  "The former Yugoslav republic of Macedonia" = "Europe",
  "Timor-Leste" = "Asia",
  "Togo" = "Africa",
  "Tonga" = "Oceania",
  "Trinidad and Tobago" = "North America",
  "Tunisia" = "Africa",
  "Turkey" = "Asia",  # Note: Turkey is transcontinental, lying in both Asia and Europe
  "Turkmenistan" = "Asia",
  "Tuvalu" = "Oceania",
  "Uganda" = "Africa",
  "Ukraine" = "Europe",
  "United Arab Emirates" = "Asia",
  "United Kingdom of Great Britain and Northern Ireland" = "Europe",
  "United Republic of Tanzania" = "Africa",
  "United States of America" = "North America",
  "Uruguay" = "South America",
  "Uzbekistan" = "Asia",
  "Vanuatu" = "Oceania",
  "Venezuela (Bolivarian Republic of)" = "South America",
  "Viet Nam" = "Asia",
  "Yemen" = "Asia",
  "Zambia" = "Africa",
  "Zimbabwe" = "Africa"
)

#Create new column Continent
df$Continent <- continent_mapping[df$Country]
sum(is.na(df$Continent)) #Sum = 0 everything good
Continent = df$Continent

#Deleting Country
df$Country <- NULL

# Module 3 - Multiple Linear Regression
#3.1 No R code
#3.2
#MLR

model <- lm(Life_expectancy ~ Continent + Year + Status + Adult_Mortality + 
              Infant_deaths + Alcohol + Percentage_expenditure + Hepatitis_B + 
              Measles + BMI + under_five_deaths + Polio + Total_expenditure + 
              Diphtheria + HIV_AIDS + GDP + Population + Thinning_1_19_years + 
              Thinning_5_9_years + Income_composition_of_resources + Schooling,
            data = df)
model_summary <- summary(model)
model_summary
#ANOVA

anova(model)

#3.3 Goodness of fit
par(mfrow=c(3,3))
predictor_vars <- c("Continent", "Year", "Status", "Adult_Mortality", "Infant_deaths", "Alcohol", "Percentage_expenditure", "Hepatitis_B",
                    "Measles", "BMI", "under_five_deaths", "Polio", 
                    "Total_expenditure", "Diphtheria", "HIV_AIDS", "GDP", 
                    "Population", "Thinning_1_19_years", "Thinning_5_9_years",
                    "Income_composition_of_resources", "Schooling"
                    )

for(var in predictor_vars){
  if(is.numeric(df[[var]])) {
  plot(df[[var]], model$residuals, xlab=var,ylab="Residuals")
  abline(h=0, col='red')
  title(paste("Residual vs.", var))
  }}
# Do the transformation for Alcohol, Measles, GDP, Population
df$log_Alcohol <- log(df$Alcohol + 1)
df$log_Measles <- log(df$Measles + 1)
df$log_GDP <- log(df$GDP + 1)
df$log_Population <- log(df$Population + 1)
par(mfrow=c(3,3))
predictor_vars <- c("Continent", "Year", "Status", "Adult_Mortality", "Infant_deaths", 
                    "log_Alcohol", "Percentage_expenditure", "Hepatitis_B",
                    "log_Measles", "BMI", "under_five_deaths", "Polio", 
                    "Total_expenditure", "Diphtheria", "HIV_AIDS", "log_GDP", 
                    "log_Population", "Thinning_1_19_years", "Thinning_5_9_years",
                    "Income_composition_of_resources", "Schooling"
)

for(var in predictor_vars){
  if(is.numeric(df[[var]])) {
    plot(df[[var]], model$residuals, xlab=var,ylab="Residuals")
    abline(h=0, col='red')
    title(paste("Residual vs.", var))
  }}

model_trans <- lm(Life_expectancy ~ Continent + Year + Status + Adult_Mortality + 
              Infant_deaths + log_Alcohol + Percentage_expenditure + Hepatitis_B + 
              log_Measles + BMI + under_five_deaths + Polio + Total_expenditure + 
              Diphtheria + HIV_AIDS + log_GDP + log_Population + Thinning_1_19_years + 
              Thinning_5_9_years + Income_composition_of_resources + Schooling,
            data = df)
model_trans_summary <- summary(model_trans)
model_trans_summary
#3.4 Estimation and Prediction 
# newdata = Add data to predict or evaluate

# predict(model, newdata, interval="confidence") #Confidence Interval
# predict(model, newdata, interval="confidence") #Prediction Interval

#3.5 Model Evaluation

#Generate Matrix Plot

### Linearity Assumption (We can also do it with each predictor, but let do itwhen we choose them.
#plot(df) #Be careful with this plot, it could freeze your computer

mean_residuals <- mean(model$residuals)

# Evaluating model assumptions
par(mfrow = c(2,2)) #Stack figures (2x2)

## Normal Q-Q Plot of Residuals
qqPlot(model$residuals, main = "Normal Q-Q Plot of Residuals", ylab = "Residuals")

### Histogram of Residuals
hist(model$residuals, main = "Histogram of Residuals", xlab = "Residuals")
abline(0,0)

### Constant Variance & Uncorrelated Errors
plot(model$fitted, model$residuals, main="Constant Variance & Uncorrelated Error", xlab="Response Variable",ylab="Residuals",pty=2,lwd=2)
abline(0,0)

cat("The mean of the residuals is", mean_residuals)

#Compare to transformation model
mean_residuals_trans <- mean(model_trans$residuals)

# Evaluating model assumptions
par(mfrow = c(2,2)) #Stack figures (2x2)

## Normal Q-Q Plot of Residuals
qqPlot(model_trans$residuals, main = "Normal Q-Q Plot of Residuals", ylab = "Residuals")

### Histogram of Residuals
hist(model_trans$residuals, main = "Histogram of Residuals", xlab = "Residuals")
abline(0,0)

### Constant Variance & Uncorrelated Errors
plot(model_trans$fitted, model_trans$residuals, main="Constant Variance & Uncorrelated Error", xlab="Response Variable",ylab="Residuals",pty=2,lwd=2)
abline(0,0)

cat("The mean of the residuals is", mean_residuals_trans)
# Looks like the same plot result

### Cooks Distances
resids = stdres(model)
fits = model$fitted
cook = cooks.distance(model)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")

### Correlation
#(I just considered Quantitative)
corr_matrix <- cor(df[, sapply(df, is.numeric)], use = "pairwise.complete.obs")
print(corr_matrix)

### VIF and calculation of High VIFs
vif.results <- vif(model)
vif.results
#print(vif_values)

#The predictors with highest VIF are infant_deaths AND under_five_deaths, so we will delete them
# df <- df[, -infant_deaths] #We delete just thids one since is the largest one


model = lm(Life_expectancy ~ .,data = df)
#R squared
summary(model)$r.squared #We can see an improvement in our R squared

# 3.6 Time Series 
#Time analysis
n <- nrow(df)
time <- 1:n

#I considered Year as a factor given that in the other way I got errors.

timemodel = lm(Life_expectancy ~ I(time)^2+Continent+Status+factor(Year)) 
summary(timemodel)

# Durbin-Watson Test Manually
par(mfrow = c(1,1))
e=resid(timemodel)
time=1:length(e)
d=sum((e[2:n] - e[1:(n-1)])^2)/sum(e^2)
#print(d)
#print(4-d)
#print(length(e))
#plot(time[1:1000], e[1:1000], xlab="t",ylab="e")
#abline(0,0,col="red")

# Test Positive Autocorrelation
dwtest(timemodel)

#Test Negative Autocorrelation
dwtest(timemodel,alternative ="less")

#Two sided test
dwtest(timemodel,alternative="two.sided")


#3.7 Model and Variable Selection 


formula <- as.formula(paste("Life_expectancy ~ ."))

# Run the regsubsets function with a controlled number of maximum predictors (nvmax)
# Setting really.big=TRUE allows for larger datasets but consider computational limitations
regfit.full <- regsubsets(formula, data = df, nbest = 1, nvmax = 10, really.big = TRUE, method = "forward")

# Summarize the results
summary_regfit <- summary(regfit.full)

# Get the Cp values
cp_values <- summary_regfit$cp

# Identify the model with the lowest Cp value
best_model_index <- which.min(cp_values)

# Display the best model's variables and its Cp value
print("Best model based on Cp:")
print(summary_regfit$which[best_model_index, ])
print(paste("Cp value:", cp_values[best_model_index]))


results <- cbind(as.data.frame(summary_regfit$which), Cp = summary_regfit$cp)
print(results)

# Optionally, plot the Cp values
plot(cp_values, type = "o", pch = 20, xlab = "Number of Predictors", ylab = "Mallows' Cp",
     main = "Mallows' Cp for Different Model Sizes")
points(best_model_index, cp_values[best_model_index], col = "red", cex = 2)


# Forward Stepwise Regression

df <- df[, -infant_deaths]
# Starting with the simplest model
start.model.forward <- lm(Life_expectancy ~ 1, data = df)

# Full model to compare against
full.model <- lm(Life_expectancy ~ ., data = df)

# Performing forward stepwise regression
forward.stepwise <- step(start.model.forward,
                         scope = list(lower = formula(start.model.forward), upper = formula(full.model)),
                         direction = "forward",
                         trace = TRUE)  # Set trace=FALSE to reduce output verbosity

# Display the summary of the selected model
summary(forward.stepwise)

#Significant Predictor Variables (AIC):
# Life_expectancy ~ Continent + Adult_Mortality + Schooling + HIV_AIDS + 
#    Income_composition_of_resources + percentage_expenditure + 
#    Alcohol + under_five_deaths + infant_deaths + Year + BMI + 
#    Status + Diphtheria + Polio


#Backward Stepwise Regression

df <- df[, -infant_deaths]
# Starting with the full model
start.model.backward <- lm(Life_expectancy ~ ., data = df)

# Performing backward stepwise regression
backward.stepwise <- step(start.model.backward,
                          direction = "backward",
                          trace = TRUE)  # Set trace=FALSE to reduce output verbosity

# Display the summary of the selected model
summary(backward.stepwise)

#Significant Predictor Variables (AIC):
# Life_expectancy ~ Year + Status + Adult_Mortality + infant_deaths + 
#    Alcohol + percentage_expenditure + BMI + under_five_deaths + 
#    Polio + Diphtheria + HIV_AIDS + Income_composition_of_resources + 
#    Schooling + Continent


modelTest1 = lm(Life_expectancy ~ Continent + Adult_Mortality + Schooling + HIV_AIDS + 
                  Income_composition_of_resources + percentage_expenditure + 
                  Alcohol + under_five_deaths + infant_deaths + Year + BMI + 
                  Status + Diphtheria + Polio, data=df)
summary(modelTest1)

# Evaluating model assumptions
par(mfrow = c(2,2)) #Stack figures (2x2)

## Normal Q-Q Plot of Residuals
qqPlot(modelTest1$residuals, main = "Normal Q-Q Plot of Residuals", ylab = "Residuals")

### Histogram of Residuals
hist(modelTest1$residuals, main = "Histogram of Residuals", xlab = "Residuals")
abline(0,0)

### Constant Variance & Uncorrelated Errors
plot(modelTest1$fitted, modelTest1$residuals, main="Constant Variance & Uncorrelated Error", xlab="Response Variable",ylab="Residuals",pty=2,lwd=2)
abline(0,0)
