# 5243-Project2

Advanced Data Analysis Shiny Application Report
Team 13: Chang Xiong (cx2335), Zishun Shen (zs2695), Yutong Wei (yw4384), Xinyi Gui (xg2426)

1. Introduction & Deployment Link
Our team developed a versatile R Shiny application called the “Advanced Data Analysis Tool” to address the full data workflow, from data upload through cleaning, feature engineering, and exploratory data analysis. The app is designed to be intuitive, user‐friendly, and flexible.
App Link: https://xiongchang5243.shinyapps.io/project2app/ Github Link: https://github.com/yutongwei28/5243-Project2

2. Key Functionalities
(1). Loading Datasets (Lead: Chang Xiong)
○ Multiple File Formats: Users can upload CSV, Excel (XLS/XLSX), JSON, and RDS
files.

○ Built‐in Datasets: The app offers two built‐in data sets (mtcars and iris) so that users can
explore the tool’s capabilities even without their own data.
○ Preview: The first few rows of the selected dataset are displayed for quick inspection.

(2). Data Cleaning & Preprocessing (Lead: Zishun Shen)
○ Duplicate Removal: Users can optionally remove duplicate rows at the click of a button.
○ Missing‐Value Handling: The application allows removing missing‐value rows or
imputing them with mean, median, or mode.
○ Scaling: Numeric columns can be scaled/standardized/normalized if desired.
○ Categorical Encoding: One‐hot encoding is supported for all factor/character columns.
○ Cleaned Data Preview: Displays a cleaned version of the dataset, along with updated
summary statistics.
○ Outlier: Choose a Z-score and handle outliers according to this threshold.
○ Handling Inconsistencies: The application enables fixing inconsistencies in date or
numeric variables.
(3). Feature Engineering (Lead: Yutong Wei)
○ New Feature Creation: Users can create new features by typing in mathematical expressions (e.g., col1 + col2).
○ Column Transformations: Log, square root, or square transformations can be applied to any numeric column, automatically creating a new “transformed” column.
○ Interactive Plots: Histograms before and after transformations are displayed side by side to visualize the impact of feature engineering.
(4). Exploratory Data Analysis (EDA) (Lead: Xinyi Gui)
○ Data Summary: Shows descriptive statistics (min, median, mean, max) for all columns.
○ Interactive Visualizations: Users can select the variable to plot and choose among
scatter plot, histogram, or box plot, rendered via plotly for interactivity.
○ Filtering: A slider filters rows based on numeric column ranges.
 
○ Correlation Matrix: Computes and displays a correlation plot for numeric columns.
(5). User Interface (UI) and User Experience (UX)
○ The app is divided into clear tabs: 1.Upload Data; 2.Data Cleaning 3.Feature Engineering 4.Exploratory Data Analysis
○ Each tab provides relevant controls and instant feedback (tables, plots, or summaries).
○ Instructions and labels guide the user through each step.
(6). Web Application Functionality
○ Interactivity: All modules respond immediately to user choices (e.g., toggling data cleaning steps, changing plot types, etc.).
○ Responsiveness: Plots and tables update without delay, providing a smooth user experience.

3. Team Contributions
a. Data Upload, Merging and Deployment (Chang Xiong)
b. Preprocessing (Zishun Shen)
c. Feature Engineering (Yutong Wei)
d. Exploratory Data Analysis (Xinyi Gui)

5. How to Use the App
(1). Upload Data (or select a built‐in dataset).
(2). Data Cleaning: Choose whether to remove duplicates, how to handle missing values, apply
scaling, or encode categorical columns.
(3). Feature Engineering: Specify new feature expressions or transformation operations to enhance
your dataset.
(4). Explore:
○ Data Summary: Check summary statistics.
○ Visualization: Choose a plot type (scatter, histogram, or box), select variables, and
interact with the plots.
○ Correlation Matrix: Click “Show Correlation Matrix” to view numeric correlations.
○ Filtered Data: Narrow the range of a chosen numeric column to inspect data subsets.

5. Conclusion
Our Advanced Data Analysis Tool in R Shiny consolidates data uploading, cleaning, feature engineering, and EDA in one place. It supports multiple files formats, offers built‐in datasets, and provides a variety of user‐friendly cleaning and analysis options. Deployed on shinyapps.io, this application showcases how R Shiny can streamline data science workflows into a single interactive environment.
