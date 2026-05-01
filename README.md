# Advanced Data Science Final Project: Classifying New York City Neighborhoods

## Authors: Sunduss Hamdan and Kaela Girod

## **Objective:** 

To classify New York City Census Tracts by typology using dimension reduction and clustering methods.  

## Steps for Running the Project:

1.  Run data_cleaning.R to load the ACS data, NYC Open Data Portal data, and the Third Place Index data to clean and create the main dataset for the analysis  
2.  Run analysis.qmd to see the exploratory data analysis and results from the two methods of dimension reduction and clustering.  
3.  Run writeup.qmd to see the details on the project motivation, problem statement, research question, model interpretation, and conclusions.

## Files Directory

1. **data_cleaning.R:** This .R Script includes the data cleaning steps to create the dataset used in modeling  

2. **analysis.qmd:** [View Analysis](analysis.html) This quarto file includes the code for exploratory data analysis and each clustering model. The first model uses PCA + K-Means Clustering. The second model uses GLRM + Gaussian Mixture Modeling.  

3. **writeup.qmd:** [View Writeup](writeup.html) This quarto file includes our project motivation, problem statement, research question, model interpretations, and conclusions.  

4. **README.md:** project documentation

5. **.gitignore:** file ignoring the data and the census api key
