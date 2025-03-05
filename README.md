<h1>Madrid Real Estate Price Prediction</h1>


<h2>Project Overview</h2>
This project focuses on analyzing the real estate market in Madrid to identify undervalued properties that may present high resale potential. By leveraging a Random Forest model, house prices are predicted using key features such as square footage, number of rooms, and neighborhood location. The predictions are then compared with actual prices to evaluate investment opportunities. This analysis will assist investors in making informed decisions regarding house purchases and resales.
<br />

<h2>Objective</h2>
<ul>
  <li>To analyze and predict house prices in Madrid based on a variety of factors (square footage, location, etc.).</li>
  <li>To identify investment opportunities by comparing predicted prices with actual buy prices.</li>
  <li>To provide insights for investors to identify undervalued properties with high resale potential.</li>
</ul>

<h2>Languages and Utilities Used</h2>
<ul>
  <li><b>R & Python</b></li>
  <li><b>Random Forest </b></li>
  <li><b>Data Manipulation & Analysis:</b> Pandas, NumPy, dplyr, tidyr</li>
  <li><b>Modeling & Evaluation:</b> glm, rpart, glmnet, ranger</li>
  <li><b>Visualization:</b> ggplot2</li>
</ul>

<h2>Data Sources</h2>
<ul>
  <li><b>Madrid Housing Market Data:</b> Includes house listings, pricing, and property features  (Kaggle Dataset) </li>
</ul>

<h2>Project Walk-Through</h2>
<ol>
  <li><b>Data Preparation:</b>
    <ul>
      <li>Read the housing data using <code>read_excel</code> and clean it by removing irrelevant features (e.g., title, ID).</li>
      <li>Perform Exploratory Data Analysis (EDA) to understand the variables and clean the dataset for further modeling.</li>
    </ul>
  </li>
  
  <li><b>Model Training:</b>
    <ul>
      <li>Multiple models are trained to predict house prices, including:
        <ul>
          <li><b>Null Model:</b> A baseline model that predicts the mean price.</li>
          <li><b>Linear Model:</b> To understand the relationship between variables and prices.</li>
          <li><b>Lasso Regression:</b> For feature selection and better generalization.</li>
          <li><b>Decision Tree & Random Forest:</b> To model complex relationships between features and predict house prices.</li>
          <li><b>Post-Lasso and Cross-Validation:</b> To optimize feature selection and reduce overfitting.</li>
        </ul>
      </li>
    </ul>
  </li>

  <li><b>Model Evaluation & Comparison:</b>
    <ul>
      <li><b>Performance Metrics:</b> MSE, RMSE, and R² are used to evaluate the models' performance.</li>
      <li><b>Cross-Validation:</b> Using multiple folds to assess model robustness and prevent overfitting.</li>
      <li><b>Random Forest:</b> Emerges as the top-performing model due to its ability to capture non-linear relationships and its importance in feature selection.</li>
    </ul>
  </li>

  <li><b>Insights:</b>
    <ul>
      <li><b>Investment Opportunities:</b> Insights into which properties are undervalued, based on the gap between predicted and actual prices.</li>
      <li><b>Feature Importance:</b> The Random Forest model helps to identify which features (like square meters, number of rooms, and neighborhood) most significantly impact the house price prediction.</li>
    </ul>
  </li>
</ol>
