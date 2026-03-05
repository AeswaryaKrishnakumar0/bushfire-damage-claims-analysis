# Question 2: Data Pre-processing Steps

# Step 1: Load the dataset
bushfire <- read.csv("BushFireData.csv")

# Step 2: Check the structure of the dataset to identify data types and potential issues
str(bushfire)

# Step 3: Remove rows with missing values to ensure a clean dataset for analysis
bushfire <- na.omit(bushfire)

# Step 4: Convert categorical variables (Construction Quality and Insurance Coverage) to factors
bushfire$Construction_Quality <- as.factor(bushfire$Construction_Quality)
bushfire$Insurance_Coverage <- as.factor(bushfire$Insurance_Coverage)

# Step 5: Set a seed for reproducibility and create a 70-30 train-test split
set.seed(1)
tr.id = sample(1:nrow(bushfire), round(nrow(bushfire) * 0.7, 0))

# Step 6: Define the training data (70% of the dataset) and testing data (remaining 30%)
training_data = bushfire[tr.id, ]  # Training data
testing_data = bushfire[-tr.id, ]  # Test data

# Step 7: Check the dimensions of the training and testing datasets to confirm the split
dim(training_data)
dim(testing_data)

# Step 8: Display the structure of the training dataset after pre-processing
str(training_data)

# Question 3: Data Exploration and Visualization

# Step 1: Plot the distribution of Damage Claims using a histogram
hist(bushfire$Damage_Claims, main="Distribution of Damage Claims", 
     xlab="Damage Claims", col="lightblue")

# Step 2: Plot the distribution of Fire Intensity using a histogram
hist(bushfire$Fire_Intensity, main="Distribution of Fire Intensity", 
     xlab="Fire Intensity", col="lightgreen")

# Step 3: Create a scatter plot to explore the relationship between Fire Intensity and Damage Claims
plot(bushfire$Fire_Intensity, bushfire$Damage_Claims, main="Damage Claims vs Fire Intensity", 
     xlab="Fire Intensity", ylab="Damage Claims", col="blue", pch=19)

# Step 4: Generate a box plot to compare Damage Claims by Construction Quality category
boxplot(Damage_Claims ~ Construction_Quality, data=bushfire, 
        main="Damage Claims by Construction Quality", xlab="Construction Quality", 
        ylab="Damage Claims", col="orange")

# Step 5: Extract numeric variables for correlation analysis
numeric_vars <- bushfire[, sapply(bushfire, is.numeric)]

# Step 6: Calculate the correlation matrix to understand relationships between numeric variables
correlation_matrix <- cor(numeric_vars, method = "pearson")
print(correlation_matrix)

# Step 7: Visualize the correlation matrix with a scatterplot matrix (pairs plot) for deeper insights
pairs(numeric_vars, main = "Scatterplot Matrix of Numeric Variables")






# Question 3: Data Exploration and Insights

# Summary of Exploratory Findings:

# 1. Histogram of Damage Claims
# Observation: The histogram of Damage Claims reveals a roughly normal distribution,
# with most claims centered around the 6–8 range. This suggests a standard level of 
# damage across most claims.

# 2. Histogram of Fire Intensity
# Observation: The histogram of Fire Intensity shows a fairly uniform distribution, 
# indicating varied exposure across properties without any specific concentration.

# 3. Scatter Plot of Damage Claims vs. Fire Intensity
# Observation: The scatter plot comparing Damage Claims and Fire Intensity displays 
# a lack of strong correlation, suggesting that Fire Intensity alone may not be a 
# strong predictor of Damage Claims.

# 4. Box Plot of Damage Claims by Construction Quality
# Observation: The box plot shows similar median claims for both Good and Bad 
# construction quality categories. However, the Good category has more outliers, 
# indicating that high claims can occur across both categories under certain conditions.

# 5. Scatterplot Matrix of Numeric Variables
# Observation: The scatterplot matrix highlights the relationships (or lack thereof) 
# between all numeric variables. No strong linear relationships are evident, which 
# supports the idea that complex interactions may be at play.

# Summary of Findings:
# These visualizations collectively suggest that while each variable offers some insight, 
# no single factor overwhelmingly drives damage claims. This implies that a multivariate 
# approach, potentially exploring interactions or using regression modeling, would be 
# beneficial for deeper insights.






# Question 4: Multiple Linear Regression to Answer the Research Question

# Objective: To build a multiple linear regression model to predict Damage_Claims,
# refining the model for accuracy and interpretability by exploring interaction terms, 
# polynomial terms, and performing variable selection.

# Step 1: Initial Model Building
# Start with all available predictors to establish a baseline model and evaluate the significance of each predictor.
initial_model <- lm(Damage_Claims ~ Fire_Intensity + Distance_from_Fire + 
                      Building_Age + Property_Value + Population_Density + 
                      Emergency_Response_Time + Mitigation_Measures + 
                      Wind_Speed + Humidity, data=training_data)
summary(initial_model)



# Analysis of Output:
# - Significant predictors (p < 0.05): Fire_Intensity, Distance_from_Fire, Property_Value, 
#   Emergency_Response_Time, Mitigation_Measures, and Humidity.
# - R-squared = 0.7308, Adjusted R-squared = 0.7249, indicating that around 73% of the variance in Damage_Claims 
#   is explained by this model.
# Decision: Continue refining the model by exploring interaction and polynomial terms to potentially improve fit.

# Step 2: Model Improvement with Interaction Terms
# Adding an interaction term between Fire_Intensity and Distance_from_Fire to see if the effect of fire intensity 
# on damage claims varies based on distance.
interaction_model <- lm(Damage_Claims ~ Fire_Intensity * Distance_from_Fire + 
                          Building_Age + Property_Value + Population_Density + 
                          Emergency_Response_Time + Mitigation_Measures + 
                          Wind_Speed + Humidity, data=training_data)

summary(interaction_model)
# Analysis of Output:
# - The interaction term Fire_Intensity:Distance_from_Fire is not statistically significant (p = 0.15681).
# - Minimal improvement in R-squared (now 0.7321), suggesting limited added explanatory power from the interaction.
# Decision: Interaction terms do not substantially enhance the model and can be excluded in the final model.

# Step 3: Polynomial Terms for Non-linearity
# Adding a second-degree polynomial term for Fire_Intensity to capture any potential non-linear relationship.
polynomial_model <- lm(Damage_Claims ~ poly(Fire_Intensity, 2) + Distance_from_Fire + 
                         Building_Age + Property_Value + Population_Density + 
                         Emergency_Response_Time + Mitigation_Measures + 
                         Wind_Speed + Humidity, data=training_data)
summary(polynomial_model)
# Analysis of Output:
# - The first polynomial term for Fire_Intensity is significant, but R-squared remains around 0.7314, 
#   with minimal improvement in overall model fit.
# Decision: Adding non-linearity did not yield substantial improvement, so polynomial terms can be excluded 
#   from the final model.

# Step 4: Variable Selection Using Stepwise Regression
# Using stepwise regression to refine the model by selecting only the most significant predictors and removing redundant variables.
library(MASS)
optimized_model <- stepAIC(initial_model, direction="both")
summary(optimized_model)
# Analysis of Output:
# - Final model retains significant predictors: Fire_Intensity, Distance_from_Fire, Property_Value, 
#   Emergency_Response_Time, Mitigation_Measures, and Humidity.
# - R-squared = 0.7304, Adjusted R-squared = 0.7265, maintaining similar explanatory power with fewer predictors.
# Decision: Stepwise selection has successfully streamlined the model, improving interpretability while preserving predictive accuracy.

# Step 5: Model Evaluation with RMSE
# Evaluating the final model's performance on the test dataset using RMSE to assess predictive accuracy.
predictions <- predict(optimized_model, testing_data)
rmse <- sqrt(mean((testing_data$Damage_Claims - predictions)^2))
rmse
# Analysis of Output:
# - RMSE = 1.0024, indicating low prediction error and good accuracy on unseen data.
# Decision: The final model generalizes well and is suitable for predicting Damage_Claims with minimal error.

# Final Summary and Conclusions
# - Initial Model: The initial model with all predictors explained around 73% of the variance in Damage_Claims.
# - Interaction and Polynomial Terms: Adding interaction and polynomial terms provided minimal improvements, 
#   so they were excluded from the final model.
# - Stepwise Selection: This method refined the model by retaining only significant predictors, 
#   enhancing simplicity without sacrificing accuracy.
# - Evaluation: The final model’s RMSE of 1.0024 confirms its predictive accuracy and generalizability.

# This modeling approach ensures that significant factors impacting Damage_Claims are identified, balancing 
# model accuracy and interpretability, and effectively answers the research question.







# Question 5: Final Model Summary for Predicting Damage_Claims

# Final Model Summary:
# After exploring multiple models in Question 4, the final model was selected based on its accuracy and interpretability.
# Stepwise selection identified six significant predictors that contribute to explaining the variance in Damage_Claims.

# Code for the Final Model (as derived in Question 4):
final_model <- lm(Damage_Claims ~ Fire_Intensity + Distance_from_Fire + Property_Value + 
                    Emergency_Response_Time + Mitigation_Measures + Humidity, data = training_data)
summary(final_model)

# Explanation of the Resultant Model:
# - The final model includes the following significant predictors:
#   1. Fire_Intensity: Higher fire intensity correlates with increased damage claims.
#   2. Distance_from_Fire: Closer proximity to the fire source is associated with higher damage claims.
#   3. Property_Value: Properties with higher value often report higher damage claims.
#   4. Emergency_Response_Time: Longer response times increase the severity of damage claims.
#   5. Mitigation_Measures: Implementing preventive measures appears to influence claim amounts.
#   6. Humidity: Environmental factors such as humidity also impact the level of damage.

# Model Performance Evaluation:
# Evaluating the model's predictive performance using RMSE on the test data.
predictions <- predict(final_model, testing_data)
final_rmse <- sqrt(mean((testing_data$Damage_Claims - predictions)^2))
cat("Final Model RMSE:", final_rmse, "\n")

# Interpretation of Final Model:
# - RMSE: The RMSE of 1.0024 on test data suggests that the model has a low prediction error, indicating reliable 
#   performance in predicting Damage_Claims.
# - R-squared: With an R-squared of approximately 0.7304, this model explains around 73% of the variance in 
#   Damage_Claims, demonstrating its effectiveness in capturing significant factors.

# Conclusion:
# - This final model effectively answers the research question by identifying key factors that influence Damage_Claims.
# - The retained predictors provide a robust understanding of the main drivers of property damage costs, which 
#   can assist the insurance company in estimating reserve requirements for future claims.

# Resultant Model Summary for Question 5

# The final model includes six predictors: Fire_Intensity, Distance_from_Fire, Property_Value, 
# Emergency_Response_Time, Mitigation_Measures, and Humidity. This model explains approximately 
# 73% of the variance in Damage_Claims (R-squared = 0.7304) and has an RMSE of 1.0024 on test data, 
# indicating reliable prediction accuracy. The model highlights key factors influencing Damage_Claims, 
# useful for forecasting and decision-making in damage assessment.








