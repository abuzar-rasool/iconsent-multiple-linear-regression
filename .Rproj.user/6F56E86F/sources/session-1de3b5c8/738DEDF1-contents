dataset <- read.csv('./dataset.csv')

dataset$signingMethod

selected_data <- data.frame(
  Comprehension_Score = dataset$Score,
  Consent_Method = dataset$signingMethod,
  Level_of_Education = dataset$Highest.Level.of.Education.,
  Native_Language = dataset$What.is.your.native.language.,
  Gender = dataset$Gender.
)

selected_data$Gender <- as.factor(selected_data$Gender)
selected_data$Consent_Method <- as.factor(selected_data$Consent_Method)
selected_data$Level_of_Education <- as.factor(selected_data$Level_of_Education)
selected_data$Native_Language <- as.factor(selected_data$Native_Language)


summary(selected_data)

attach(selected_data)


# Ensuring dummy variables are created
levels(Gender)
levels(Consent_Method)
levels(Level_of_Education)
levels(Native_Language)


# EDA: Summary and Correlation
summary(selected_data)
cor(selected_data$Comprehension_Score, as.numeric(selected_data$Consent_Method))

# Scatterplot for initial visualization
plot(selected_data$Comprehension_Score ~ as.numeric(selected_data$Consent_Method))
boxplot(Comprehension_Score ~ Consent_Method, data = selected_data)


# MLR
mlr_model <- lm(Comprehension_Score ~ Consent_Method + Level_of_Education + Native_Language + Gender)
summary(mlr_model)


# Residual Analysis
myres <- residuals(mlr_model)
myfit <- fitted(mlr_model)

# Linearity and Homoscedasticity Check
plot(myfit, myres, main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Normality Check
qqnorm(myres)
qqline(myres)

# Independence Check
plot(myres, main = "Residuals vs Order")



