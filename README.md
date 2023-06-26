# NewChic_Retail

# Project Title: Likes_count Prediction using Negative Binomial Regression

## Problem Statement
The objective of this project is to analyze the factors that affect the dependent variable "Likes_count." Since the dependent variable is a count variable, we decided to use a Poisson Regression Model. However, during the analysis, we observed overdispersion in the data, suggesting that the Poisson assumption of equal mean and variance might be violated. To address this issue, we have explored the option of utilizing a Negative Binomial regression model, which allows for more variability than the Poisson model.

## Overdispersion Test Results
We conducted an overdispersion test, which indicated that the actual dispersion parameter exceeds 1. This suggests that there is more variability in the data than what the Poisson model expects. In such cases, the Negative Binomial regression model is a suitable alternative. It accommodates the extra variability in the data and provides more accurate predictions.

## Optimal Model Selection
We are currently in the process of selecting the most appropriate model for our analysis. Based on our research, the Negative Binomial regression model appears to be a promising choice given the overdispersion in the data. This model addresses the issue of unequal mean and variance and can better capture the relationship between the independent variables and the dependent variable.

## Resources
As part of our research, we came across a helpful resource: [UCLA Statistics: Poisson Regression](https://stats.oarc.ucla.edu/r/dae/poisson-regression/). This website provides insights into Poisson Regression and introduces the concept of Negative Binomial Regression as an alternative when dealing with overdispersed data. We found it valuable for understanding and implementing the Negative Binomial regression model in our analysis.

## Conclusion
By leveraging the Negative Binomial regression model, we aim to improve the accuracy of our predictions for the dependent variable, Likes_count. This model accounts for the overdispersion observed in the data, enabling us to better understand the factors influencing Likes_count and make more informed decisions based on the analysis.
