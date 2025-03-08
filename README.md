# Unveiling YouTube Success: Insights from Global Channel Analysis

## Introduction

This project marks my **first deep-dive into machine learning and statistical analysis** applied to a marketing research class, where I explore key factors influencing YouTube channel success. Using a dataset containing nearly 1,000 of the most popular YouTube channels worldwide, I apply various techniques—**clustering, PCA, Market Basket Analysis, and predictive modeling**—to segment high-performing channels, analyze audience preferences, and predict a channel’s likelihood of reaching the **top 35% in subscriber count**.

I welcome any feedback or suggestions for refinement!

## Dataset

The dataset was sourced from Kaggle:  
[**Global YouTube Statistics 2023**](https://www.kaggle.com/datasets/nelgiriyewithana/global-youtube-statistics-2023)

It includes **28 variables**, such as:
- **Performance Metrics**: Number of subscribers, video views, uploads, earnings (monthly/yearly).
- **Channel Information**: Category, country of origin, creation year.
- **Socioeconomic Variables**: Population, urban population, unemployment rate, education enrollment.

## Objectives

1. **Identify key factors influencing YouTube success**: Understand what makes certain channels thrive.
2. **Segment YouTube channels**: Cluster channels based on their performance and socioeconomic characteristics.
3. **Predict a channel’s success**: Build machine learning models to estimate if a channel belongs to the **top 35% of YouTube channels** based on subscribers.

## Methodology

### **Exploratory Data Analysis (EDA)**
- **Data Cleaning & Preprocessing**: Removed redundant/missing values, converted variables into appropriate formats.
- **Visualization**: Used `ggplot2` to explore data distributions and relationships.

### **Clustering Analysis**
- **K-Means Clustering**: Segmented channels based on performance metrics.
- **Hierarchical Clustering**: Explored alternative grouping strategies to improve segmentation.
- **Mixed Data Clustering**: Combined performance and categorical variables to create a refined segmentation.

### **Principal Component Analysis (PCA)**
- Reduced dimensionality to identify **key features driving YouTube success**.
- **Top contributing factors**: Video views, subscribers, urban population, and earnings.

### **Market Basket Analysis (MBA)**
- Uncovered **patterns between channel categories, subscriber counts, and geographic locations**.
- Found that **Music and Entertainment channels dominate high-subscriber categories**.

### **Predictive Modeling**
- **Logistic Regression**: Attempted to predict whether a channel belongs to the top 35% but faced **perfect separation issues**.
- **Decision Tree Model**: Improved interpretability and accuracy (~81%).
- **Random Forest Model**: Achieved the best balance between accuracy (~80%) and robustness.

## Key Findings

- **Subscribers and video views are highly correlated** (0.85), making views a strong predictor of success.
- **High-subscriber channels are often in Music or Entertainment** categories, with a significant presence in the U.S.
- **Socioeconomic factors influence YouTube success**: Urban population and education enrollment impact channel growth.
- **Playlist and chart presence are crucial** for predicting success in other streaming industries (e.g., Spotify).
- **Random Forest was the best-performing predictive model**, highlighting **video views and subscriber growth rate as the strongest indicators of success**.

## Recommendations

1. **Leverage Audience Insights**: Creators should focus on **Music and Entertainment content**, which attracts **higher subscriber counts**.
2. **Optimize Video Production**: Channels with **frequent uploads** tend to perform better—consistency matters.
3. **Target High-Growth Markets**: Channels from the U.S. and India dominate—creators should consider **localized content strategies**.
4. **Use Predictive Modeling**: Businesses can **forecast channel success** and optimize marketing investments using **machine learning**.

## Limitations & Future Work

- **Dataset Bias**: Includes only **successful channels**, potentially overlooking emerging ones.
- **Feature Engineering**: Further refinement of categorical variables could enhance model performance.
- **Advanced Machine Learning**: Exploring **deep learning models** (e.g., neural networks) could improve predictions.

## How to Run the Code

1. Clone this repository:
   ```sh
   git clone https://github.com/your-username/your-repo-name.git
   cd your-repo-name
