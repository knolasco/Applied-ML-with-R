# load libraries ----
install.packages("imputeTS", dependencies = TRUE)
library(imputeTS)

# load dataset ----
data_with_na <- tsNH4
data_complete <- tsNH4Complete

# HYPOTHESIS ----
# According to the documentation for imputeTS, the tsNH4 dataset is a time
# series NH4 concentration in a waste-water system.
# since this is a time series dataset, I believe seasonality will play
# a big role on the way the dataset behaves. Therefore, a simple imputation method - such as
# using the mean, median, or mode will not be sufficient to capture the trend of the data.
# I believe the best method for imputing this dataset will be an interpolation model
# with spline or Stineman Interpolation.

# We will test three interpolation functions (na_interpolation, na_kalman, and na_seadec)
# and we will tweak each parameters for the best result.
# For example, we will impute using na_interpolation with linear, 
# spline, and stine interpolation.

# The results for the imputations will be verified using RMSE.

# DATA VISUALIZATIONS ----
# visualize missing values
ggplot_na_distribution(data_with_na)

# get a summary of the missing values
statsNA(data_with_na)
# from the stats output, we can see the dataset consists of 4,552 data points with
# 883 missing values (19.4% of values are missing).


