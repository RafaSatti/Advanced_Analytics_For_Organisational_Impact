# LSEDA301: ADVANCED ANALYTICS ON ORGANISATIONAL IMPACT
################################################################################
## Scenario:
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends.

## In this R script we will explore Trurtle Games business objectives:
## - what is the impact on sales per product
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales 
################################################################################
# 1. Install and import required packages.
################################################################################
install.packages('tidyverse')
library('tidyverse')

install.packages('dplyr')
library(dplyr)

install.packages('ggplot2')
library(ggplot2)

installed.packages('moments')
library(moments)
################################################################################
# 2. LOAD AND SENSE-CHECK DATA
################################################################################
# Determine working directory
getwd()

# Import the data set
sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print dataset to get a good overview of the data (opens in source tab)
View(sales)       

# View first five rows of dataset
head(sales)

# View structure of the data
str(sales)

# Check percentage of missing values
sum(is.na(sales))
prop.table(table(is.na(sales)))*100 #(0.06% is null)

# Remove rows with missing values 
sales <- na.omit(sales)

# Create a new column 'Other' to calculate (Global - (NA+EU)
sales$Other_Sales <- sales$Global_Sales - (sales$NA_Sales + sales$EU_Sales)

# Create a new data frame from a subset of the sales data frame.
# Remove redundant columns (Year, Ranking) 
sales2 <- select(sales, -Year, -Ranking)

# View the data frame.
head(sales2)

as_tibble(sales2)

# Convert product number to character
sales2$Product <- as.character(sales2$Product)

# View descriptive statistics to determine min, max and mean of sales 
summary(sales2)

################################################################################
# 3. EXPLORATORY DATA ANALYSIS 
################################################################################
## 3a. Determine how many platforms each product is available on
platform_counts <- sales2 %>%
  group_by(Product) %>%
  summarise(Num_Platforms = n_distinct(Platform))

# View the result
View(platform_counts) # Opens in source bar (desc)

# Determine top 10 products sold globally
top_10_products <-sales2 %>%
  group_by(Product) %>%
  summarise(TotalSales = sum(Global_Sales)) %>%
  arrange(desc(TotalSales)) %>%
  head(10)

# Convert product number to character
top_10_products$Product <- as.character(top_10_products$Product)

# Plot bar plot of top 10 sellers
ggplot(top_10_products, aes(x =  TotalSales, 
                            y = Product)) +
geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Product ID", y = "Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Selling Products by Total Sales")
ggsave("top10.png")

# sum of total global sales
total_global = sum(sales2$Global_Sales)
print(total_global)

# Percentage of top 3 selling product

# Product 107
filter_107 <- subset(sales2, Product == "107")
total_sales_107 <- sum(filter_107$Global_Sales)
per_107 <- total_sales_107/total_global*100
# Print percentage
print(per_107) # 3.6% of Global sales

# Product 515
filter_515 <- subset(sales2, Product == "515")
total_sales_515 <- sum(filter_515$Global_Sales)
per_515 <- total_sales_515/total_global*100
# Print percentage
print(per_515) # 2.4% of Global Sales

# Product 123
filter_123 <- subset(sales2, Product == "123")
total_sales_123 <- sum(filter_123$Global_Sales)
per_123 <- total_sales_123/total_global*100
# Print percentage
print(per_123) #  2% of Global Sales


# Percentage of top 10 selling from global sales
top10 <- c("107", "515", "123", "254", "195", 
                       "231", "249", "876", "263", "979")

filter_top10 <- subset(sales2, Product %in% top10)
total_sales_top10 <- sum(filter_top10$Global_Sales)
per_top10 <- total_sales_top10/total_global*100
# Print percentage
print(per_top10) #  18% of Global Sales
total_sales_top10
################################################################################
## 3b. Identify number of categorical variables

unique(sales2$Product) # 175 different products 
unique(sales2$Platform)  # 22 different platforms
unique(sales$Publisher) # 23 different publishers
unique(sales$Genre) # 12 different genres

################################################################################
## 3b. Determine distributions of platforms across regions

# Global
qplot(Global_Sales, Platform, data=sales2, geom='boxplot')

# North America
qplot(NA_Sales, Platform, data=sales2, geom='boxplot')

# Europe
qplot(EU_Sales, Platform, data=sales2, geom='boxplot')

################################################################################
## 3c. Distribution of categorical variables 
# Histogram : Genre distribution
ggplot (sales2, aes (x = Genre)) +
  geom_histogram (stat = "count", fill = "blue") +
  coord_flip() + 
  labs(title = "Genre Distribution")
ggsave('genre_dis.png')

# Histogram : Platform distribution
ggplot (sales2, aes (x = Platform)) +
  geom_histogram (stat = "count", fill = "pink") +
  coord_flip()  + 
  labs(title = "Platform Distibution")
ggsave('plat_dis.png')

# Histogram : Publisher Distribution
ggplot (sales2, aes (x = Publisher)) +
  geom_histogram (stat = "count", fill = "purple") +
  coord_flip() + 
  labs(title = "Publisher Distribution")
ggsave('pub_dis.png')
################################################################################
## 3d. Most popular genres based on Region

# Aggregate sales data by genre
genre_sales_summary <- sales %>%
  group_by(Genre) %>%
  summarise(Total_NA_Sales = sum(NA_Sales),
            Total_EU_Sales = sum(EU_Sales),
            Total_Global_Sales = sum(Global_Sales))

# Bar plot: Most popular genres in North America
ggplot(genre_sales_summary, aes(y = Genre, x = Total_NA_Sales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Most Popular Genre: North America",
       x = "NA Sales",
       y = "Genre")

# Bar plot: Most popular genres in Europe
ggplot(genre_sales_summary, aes(y = Genre, x = Total_EU_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Most Popular Genre: Europe",
       x = "EU Sales",
       y = "Genre")

# Bar plot: Most popular genres Globally
ggplot(genre_sales_summary, aes(y = Genre, x = Total_Global_Sales)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Most Popular Genre: Global",
       x = "Global Sales",
       y = "Genre")
################################################################################
# Observations and insights:

# Most popular product is 107 on Wii platform, making up almost double of the 
# second most selling product 123 on NES platform.
# Product 123 is second-highest selling Globally and in North America, however
# it is unpopular in Europe.

# Action, Shooter and sports are the popular genres in general
# X360, PS3 and PC are the most popular platforms in general
# Nintendo, Electronic Arts and Activision are the most popular publishers

# Product 3645 is available in the most number of platforms (9)
# 102/175 products are only available on 1 platform
################################################################################
# 4. DETERMINE NORMALITY OF THE DATASET
################################################################################
# 4a. Global Sales: 

# Global sales data does not appear to follow a normal distribution
# Skewness is > 1, extremely skewed to the right due to outlier of 67.85M sales
# Kurtosis = 32.5, indicating a heavy tailed distribution
################################################################################
summary(sales2$Global_Sales)

# Interquartile range
IQR <- IQR(sales2$Global_Sales)
Q1 <- 1.045
Q3 <- 6.445

# Identify outliers
lower <- Q1 - (IQR * 1.5)

# Print lower bound
lower 

upper <- Q3 + (IQR * 1.5)
# Print upper bound 
upper
print("Data less than -7.1 and greater than 14.5 are outliers")

# Check for normal distribution
qqnorm(sales2$Global_Sales)
# Add reference line 
qqline(sales2$Global_Sales)
ggsave("normality_global.png")

# Perform Shapiro-Test
shapiro_test <- shapiro.test(sales2$Global_Sales)
cat("Shapiro-Wilk Test Results:\n")
print(shapiro_test)

# Determine skewness and kurtosis
skewness_global_sales <- skewness(sales2$Global_Sales)
kurtosis_global_sales <- kurtosis(sales2$Global_Sales)
cat("Skewness of Global_Sales:", skewness_global_sales, "\n")
cat("Kurtosis of Global_Sales:", kurtosis_global_sales, "\n")

################################################################################
# 4b. North America Sales:

# NA sales data does not appear to follow a normal distribution, and is
# positively skewed to the right
# Skewness is > 1, extremely skewed to the right due to outlier of 34M sales
# Kurtosis = 32.5, indicating a heavy tailed distribution
################################################################################
summary(sales2$NA_Sales)

# Interquartile range
IQR <- IQR(sales2$NA_Sales)
Q1 <- 0.4725
Q3 <- 3.1200

# Identify outliers
# Lower bound
lower2 <- Q1 - (IQR * 1.5)
lower2 

# Upper bound
upper2 <- Q3 + (IQR * 1.5)
upper2 
print("Data less than -3.5 and greater than 7.09 are outliers")

# Check for normal distribution
qqnorm(sales2$NA_Sales)
# Add reference line 
qqline(sales2$NA_Sales)
ggsave("notrmality_na.png")

# Perform Shapiro-Test
shapiro_test_NA <- shapiro.test(sales2$NA_Sales)
cat("Shapiro-Wilk Test Results:\n")
print(shapiro_test)


# Skewness
skewness_na_sales <- skewness(sales2$NA_Sales)
kurtosis_na_sales <- kurtosis(sales2$NA_Sales)
cat("Skewness of NA_Sales:", skewness_na_sales, "\n")
cat("Kurtosis of NA_Sales:", kurtosis_na_sales, "\n")

################################################################################
# 4c. Europe Sales:

# EU sales data does not appear to follow a normal distribution, and is
# positively skewed to the right
# Skewness is > 1, extremely skewed to the right due to outlier of 23.8M sales
# Kurtosis = 44.5, indicating a heavy tailed distribution
################################################################################
summary(sales2$EU_Sales)

# Interquartile range
IQR <- IQR(sales2$EU_Sales)
Q1 <- 0.3925
Q3 <- 2.1600

# 4b. Identify outliers
# Lower bound
lower3 <- Q1 - (IQR * 1.5)
lower3 

# Upper bound
upper3 <- Q3 + (IQR * 1.5)
upper3

print("Data less than -2.26 and greater than 4.81 are outliers")

# Check for normal distribution
qqnorm(sales$EU_Sales)
# Add reference line 
qqline(sales$EU_Sales)
ggsave("normality_eu")

# Perform Shapiro-Test
shapiro_test <- shapiro.test(sales$EU_Sales)
cat("Shapiro-Wilk Test Results:\n")
print(shapiro_test)

# Skewness
skewness_eu_sales <- skewness(sales$EU_Sales)
kurtosis_eu_sales <- kurtosis(sales$EU_Sales)
cat("Skewness of EU_Sales:", skewness_eu_sales, "\n")
cat("Kurtosis of EU_Sales:", kurtosis_eu_sales, "\n")
################################################################################
# 5. Relationship between (NA, EU and Other) and Global Sales
################################################################################
## 5a. Scatterplot: North America and Global Sales

ggplot(sales2, aes(x = Global_Sales, y = NA_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1) +
  labs(title = "Relationship between North America and Global Sales",
       x = "Global Sales",
       y = "North America Sales") +
  theme_bw()


# Correlation coefficient
cor (sales$NA_Sales, sales$Global_Sales)
################################################################################
# 5b. Europe and Global Sales

ggplot(sales2, aes(x = Global_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1) +
  labs(title = "Relationship between Europe and Global Sales",
       x = "Global Sales",
       y = "Europe Sales") + 
  theme_bw()

# Correlation coefficient
cor (sales$EU_Sales, sales$Global_Sales)
################################################################################
# 5c. Other and Global Sales

ggplot(sales2, aes(x = Global_Sales, y = Other_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1) +
  labs(title = "Relationship between North America and Global Sales",
       x = "Global Sales",
       y = "Other Sales")

# Correlation coefficient
cor (sales$Other_Sales, sales$Global_Sales)
################################################################################
# 5d. Europe and North America

ggplot(sales2, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1) +
  labs(title = "Relationship between North America and Europe Sales",
       x = "North America Sales",
       y = "Europe Sales")

# Correlation coefficient
cor (sales$NA_Sales, sales$EU_Sales)
################################################################################
# Report: 
 # - There is a positive linear correlation between NA and Global Sales,
 #   with correlation coeffecient = 0.94, indicating perfect correlation
 # - There is a positive linear correlation between EU and Global Sales,
 #   with correlation coeffecient = 0.88, indicating perfect correlation
 # - There is a positive linear correlation between Other and Global Sales,
 #   with correlation coeffecient = 0.82, indicating perfect correlation

################################################################################
# 6. Predictive Analysis
# y (dependant variable) = Global Sales
# x (independant variable)
################################################################################
# 6a. Create subset of sales2 
sales_sub <- subset(sales2, select = c(NA_Sales, EU_Sales, Global_Sales, 
                                       Other_Sales))

# View subset info
as_tibble(sales_sub)
###############################################################################
# 6b. Simple Linear Regression 
###############################################################################
## i. Correlation between Global and NA Sales 

# Fit a linear regression model
model1 <- lm(Global_Sales ~ NA_Sales, data = sales_sub)

# Determine the correlation between the sales columns
correlation1 <- cor(sales_sub$Global_Sales, sales_sub$NA_Sales)
cat("Correlation between Global_Sales and NA_Sales:", correlation1, "\n")

# View the regression model summary
summary(model1) # R-squared = 87%

# Scatterplot with regression line
ggplot(sales_sub, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot and Linear Regression Line",
       x = "NA_Sales",
       y = "Global_Sales")
ggsave("lm_na.png")
###############################################################################
## ii. Correlation between Global and EU Sales 

# Fit a linear regression model
model2 <- lm(Global_Sales ~ EU_Sales, data = sales_sub)

# Determine the correlation between the sales columns
correlation2 <- cor(sales_sub$Global_Sales, sales_sub$EU_Sales)
cat("Correlation between Global_Sales and EU_Sales:", correlation2, "\n")

# View the regression model summary
summary(model2) # R-squared = 77%

# Scatterplot with regression line
ggplot(sales_sub, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot and Linear Regression Line",
       x = "EU_Sales",
       y = "Global_Sales")
ggsave("lm.eu.png")
###############################################################################
## iii. Correlation between Global and Other(-EU, -NA) Sales 

# Fit a linear regression model
model3 <- lm(Global_Sales ~ Other_Sales, data = sales_sub)

# Determine the correlation between the sales columns
correlation3 <- cor(sales_sub$Global_Sales, sales_sub$Other_Sales)
cat("Correlation between Global_Sales and Other_Sales:", correlation3, "\n")

# View the regression model summary
summary(model3) # R-squared is 67%

# Scatterplot with regression line
ggplot(sales_sub, aes(x = Other_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatterplot and Linear Regression Line",
       x = "Other_Sales",
       y = "Global_Sales")
ggsave('lm_global.png')
###############################################################################
# Observations:
# EU, NA and Other sales are closely correlated to Global Sales
# North America influences Global Sales the most with an R-squared of = 87%
###############################################################################
# 6c. Multiple Linear Regression
###############################################################################
# Fit a multiple linear regression model
model5 <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = sales_sub)

# Determine the correlation between the sales columns
correlation_matrix2 <- cor(sales[, c("Global_Sales", "NA_Sales", "EU_Sales")])
cat("Correlation matrix between sales columns:\n")
print(correlation_matrix2)

# View the regression model summary
summary(model5) # R-squared = 97%
###############################################################################
# Observations: 
# R-squared is very strong = 0.97 (close to 1)
# Indicates that EU and NA sales have a high significance in the model
# Making up to 97% of Global Sales
###############################################################################
# 6d. Predict Global Sales based on given values
###############################################################################
# Define the provided sums of NA_Sales and EU_Sales
pred <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                       EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict Global_Sales using the multiple linear regression model
pred$Predicted_Global_Sales <- predict(model5, newdata = pred)

# Print the predictions
cat("Predicted Global Sales based on provided values:\n")
print(pred)

# Observed Global sales
observed_sales <- c(sales_sub$Global_Sales) 

# Create a comparison data frame
comparison <- data.frame(Observed_Global_Sales = observed_sales,
                         Predicted_Global_Sales = pred$Predicted_Global_Sales)

# Print the comparison
print(comparison)

# Create a scatterplot of observed vs. predicted sales
ggplot(comparison, aes(x = Observed_Global_Sales, y = Predicted_Global_Sales)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Global Sales",
       x = "Observed Global Sales",
       y = "Predicted Global Sales")

###############################################################################
#Observations: 
# NA and Eu data can confidently predict global sales