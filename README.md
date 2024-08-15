# Data_Analysis_R_Shiny_App
Interactive R Shiny web portal within R Studio to facilitate seamless data analysis and visualization, tailored to user-uploaded datasets

## Overview 
This application was created using R Shiny within R studio. The purpose of this app was to allow students to upload their own selected dataset and perform the Design of Experiments (DOE) 7-step analysis for thier data. Once the data is uploaded, users may select their response variable, independent variables/interactions, and blocking variable (optional). Once everything has been selected, the coded design matrix is created, as well as various summary statistics on the data. Users can then go through the 7 steps by moving through the tabs on the top. 

## 7-Steps
1. Determine Model/Factor SignificanceL: The application will output the ANOVA table and Pareto Chart of Effects to allow users to select their significant factors and interactions
2. Determine Model Adequacy: The application will display the R-squared and adjusted R-squared values
3. Formulate Model Equation: The application will display a table with the selected variables and their estimates as well as the full model equation with the coefficients
4. Analyze Residuals: The application will display an overview table of the data with columns for the standard residuals and Cook's Distance values. Users can also select specific tabs within this step to view the full Cook's Distance Plot or Standard Residuals Plot with lines to guide users on if the datapoint may be an outlier
5. Verify ANOVA Model Assumption: The application will display three tabs (Normality Plot, Constance Variance Plot, and Independence Plot) so that users may click through these tabs and verify that all three of the ANOVA model assumptions are met.
6. Visualiza Data: The application will display 4 tabs that show 4 different visualizations of the data. The first is the main effects plot and users can select the specific variables that they want to see the effects for. The second is the interaction plots and users can select the specific factors they want to view the interaction effect of. The third tab displays the boxplot of factors and users can select multiple factors to compare. Lastly, the fourth tab displays the contour plot and users may select 2 variables to view on this graph.
7. Analyse Results / Formulate Conclusions


<img width="897" alt="Screenshot 2024-07-31 at 10 04 00 AM" src="https://github.com/user-attachments/assets/c639a1ae-75ff-4e49-9a45-66f7eb76d74c">
