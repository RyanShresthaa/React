library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(scales)
library(caret)

df <- `Applicant.details`
head(df)
sapply(df, function(x) sum(is.na(x)))
sapply(df, class)
summary(df)
table(df$Loan_Default_Risk)
numeric_features <- df %>% 
  select(Annual_Income, Applicant_Age, Work_Experience, 
         Years_in_Current_Employment, Years_in_Current_Residence, 
         Loan_Default_Risk)
corr_data <- cor(numeric_features)
corrplot(corr_data, method = "color", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, col = colorRampPalette(c("blue", "white", "red"))(200))

# Distribution of Risk Flag
ggplot(df, aes(x = Loan_Default_Risk)) + 
  geom_bar(fill = "red") + 
  labs(title = "Distribution of Risk Flag", x = "Risk Flag", y = "Count")

# Distribution of Marital Status
ggplot(df, aes(x = Marital_Status)) + 
  geom_bar(fill = "lightgreen") + 
  labs(title = "Distribution of Marital Status", x = "Marital Status", y = "Count")

#Dropping cols
data <- df %>% select(-Residence_City, -Residence_State, -Applicant_ID)

data <- data %>% 
  mutate(across(c(Marital_Status, House_Ownership, Occupation), 
                ~as.numeric(factor(.))))

# Normalizing
preprocess_params <- preProcess(data, method = c("range"))
data_scaled <- predict(preprocess_params, data)

data_scaled_df <- as.data.frame(data_scaled)
table(data_scaled_df$Loan_Default_Risk)
df$Loan_Default_Risk <- factor(df$Loan_Default_Risk)


#BoxPlot
df$Work_Experience <- factor(df$Work_Experience)
ggplot(df, aes(x = Work_Experience, y = Annual_Income)) +
  geom_boxplot() +
  labs(title = "Box Plot of Annual Income by Work Experience",
       x = "Work Experience (years)",
       y = "Annual Income ($)")

#Scatter Plot
ggplot(df, aes(x = Annual_Income, y = Applicant_Age, color = Loan_Default_Risk)) +
  geom_point() +
  labs(title = "Scatter Plot of Annual Income vs. Applicant Age",
       x = "Annual Income",
       y = "Applicant Age",
       color = "Loan Default Risk") +
  scale_color_viridis_d() +  # Use scale_color_viridis_d() for discrete variables
  theme_minimal()

# Histogram
ggplot(data, aes(x = Annual_Income)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Histogram of Petal Length",
       x = "Annual_Income",
       y = "Work_Experience") +
  theme_minimal()

