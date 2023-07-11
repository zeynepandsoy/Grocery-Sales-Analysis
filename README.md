# Dunnhumby Grocery Sales Analysis Group Project

## Introduction

This project involves analyzing grocery retailer sales data to gain insights into performance and the impact of promotions on product sales. The dataset consists of sales and promotion information for selected product categories over a 156-week period. [The data](https://www.kaggle.com/datasets/frtgnn/dunnhumby-the-complete-journey) was collected from a sample of stores and includes sales and promotion details for the top five products from each of the top three brands within four chosen categories (mouthwash, pretzels, frozen pizza, and boxed cereal).

## Objectives
The main objectives of this project are:

1. Visualize the data to gain a better understanding of sales patterns and trends.
2. Explore price elasticity to analyze the responsiveness of demand to price changes.
3. Study seasonal variation in prices to identify pricing dynamics across different categories.
4. Investigate the impact of advertising on product sales.
5. Use prediction models to estimate future sales and assist in sales forecasting.

By achieving these objectives, this project aims to provide valuable insights to the grocery retailer, helping them optimize pricing strategies, tailor advertising efforts, and make informed decisions for improving their sales performance.

## Project Details

- Created using: RStudio
- Dataset: [Complete Journey Dataset](https://www.kaggle.com/datasets/frtgnn/dunnhumby-the-complete-journey)
  
## Project Contributors
- Zeynep Andsoy
- Eugene Ong
- Anna Schyberg
- Gaspard Rosset
- Edoardo Lorenzetti
- Zheng Luo
  
## Table of Contents
1. Introduction
2. Data Visualization
3. Price Elasticity Estimates
4. Demand and Forecasting
5. Reflection
6. Appendix

# Data Visualization
In this section, we visualize the seasonal changes in prices and analyze sales per category to gain a better understanding of the data.

## Seasonal Changes
Time-series plots are used to observe price changes over time and understand the seasonal variations in prices. The prices of all products are sub-plotted into categories over a period of 156 weeks. By examining the plots, we can make generalizations about the price ranges and distributions of each category. For example, bag snacks exhibit a steady price range of around $1-3 with weak oscillations, while oral-hygiene products show more fluctuations with a higher standard deviation between $1-11. To analyze seasonal changes more precisely, we focus on individual years as well.

By subsetting the dataset to the year 2010 and observing changes with respect to months, we find that certain months, such as January, May, and August, show a surge in demand for oral-hygiene products with average prices between $3-4. Similarly, the price of frozen pizza fluctuates steeply in August and November, with an overall range of approximately $2-7. Bag snacks and cold cereals exhibit relatively stable prices with few outliers.

Generalizing these insights for the entire period, we examine the year 2011 and find that bag snacks and cold cereals maintain similar price patterns and ranges as before. However, oral-hygiene products experience a decrease in price, suggesting an annual decrease between 2010 and 2011. From these observations, it can be concluded that prices of bag snacks and cold cereals are relatively more stable, while oral-hygiene products and frozen pizzas exhibit more frequent fluctuations within a year, indicating a seasonal trend.

## Categorizing Sales
In this analysis, we categorize sales per category and compare them to the number of sales and product features to gain insights into the relative performance of each category.

By examining the sales per category, we find that cold cereals have the highest total sales, surpassing other categories with sales over $15M during the analyzed period. However, to understand the complete picture, we need to consider other factors.

The second graph highlights that evaluating the company solely based on sales numbers is not sufficient. Despite cold cereals generating more sales revenue, it requires a significantly higher number of sales to achieve this result. In contrast, frozen pizzas generate less revenue compared to bag snacks but with fewer sales. This analysis supports the notion that oral-hygiene products are less popular among consumers.

The relationship between sales and the number of products displayed is also examined. It is observed that cereals, with a high number of products displayed, generate more sales revenue, which aligns with expectations. However, for frozen pizza, despite being the second-highest category in terms of sales and the third-largest in terms of the number of sales, it has the most products displayed. This raises questions about the effectiveness of the display strategy for frozen pizzas. Conversely, it seems that the number of sales for bag snacks is directly linked to the number of products advertised, suggesting that increasing the number of advertised bag snack products can boost sales.

# Price Elasticity Estimates
This section focuses on estimating price elasticities using linear regression. The goal is to analyze the relationship between price, feature advertising, display advertising, and demand for different product categories.

## Price Elasticity
Price elasticity measures the percentage change in demand given a 1% increase in price. Negative elasticities are expected due to the law of demand. The magnitude of the elasticity indicates the level of demand elasticity, with values between -1 and 0 representing inelastic demand and values greater than -1 indicating elastic demand.

Based on the analysis, bag snacks (-1.4032) and frozen pizza (-1.1430) have elastic demand, while cold cereals (-0.1884) and oral hygiene products (-0.5224) have inelastic demand.

## Feature Advertising
Feature advertising refers to products being included in the in-store circular. The coefficient for feature advertising indicates its impact on demand. Cold cereals have the greatest coefficient (0.6932), indicating that they are most affected by feature advertising. Frozen pizza and oral hygiene categories are also affected by feature advertising, albeit to a lesser extent. Bag snacks show a limited impact.

## Display Advertising
Display advertising refers to products being part of an in-store promotional display. The coefficient for display advertising indicates its influence on demand. Bag snacks (0.8213) are the most affected category, followed by oral hygiene products (0.507), cold cereals (0.68), and frozen pizzas (0.699). However, the impact of display advertising is relatively less pronounced for the latter three categories.

The p-value, which indicates the quality of the model, is below 0.05 for all models, indicating a significant relationship between predictor variables and response variables.

Based on these findings, the following strategies are suggested:

- Bag snacks: Keeping prices low and focusing on display advertising is recommended due to their elasticity and increased demand from display advertising.
- Cold cereals: Focusing on feature advertising is beneficial as it has the greatest impact on demand. Price changes have less effect on sales compared to other products.
- Frozen pizzas: Maintaining affordable prices, along with moderate feature and display advertising, is recommended. Price increases significantly affect demand for frozen pizzas.
- Oral hygiene products: Moderately featuring and displaying products is suggested, as they are less affected by price changes compared to other products.

# Demand and Forecasting
In this section, three regression models (multiple linear regression, regression tree, and random forest) are tested to identify the model that best predicts future unit sales. The selected independent variables for the regression models are PRICE, BASE_PRICE, FEATURE, DISPLAY, and TPR_ONLY, among 12 available variables.

Cross-validation is performed on 26 potential sets of predictor variables for each model, and the best predictive sets are identified based on in-sample root mean squared errors (RMSE) and other selection criteria. The 11th set is determined to be the best for both the regression tree and random forest models, while the 21st set is optimal for the multiple linear regression model (see Appendix, Table 1).

Using the selected sets of predictor variables, out-sample RMSEs are calculated to evaluate the models' performance. The random forest model demonstrates the lowest out-sample RMSE, indicating the best fit and highest accuracy in predicting future unit sales. The selected predictor variables for the random forest model are PRICE, BASE_PRICE, and FEATURE, emphasizing their importance in predicting sales.

By utilizing the Random Forest model, the grocery retailer can predict the impact of potential changes in PRICE, BASE_PRICE, and FEATURE variables on unit sales. This allows for simulating different combinations of these variables to optimize unit sales and make informed decisions.

Please refer to the respective sections for detailed information on each topic covered in this analysis.
