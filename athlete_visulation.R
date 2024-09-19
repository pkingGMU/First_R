require(athletemonitoring)
library(dplyr)
library(tidyr)
#> Loading required package: athletemonitoring
#> 
#> 


# Load Data
data <- read_csv('paper3data_original9.3.csv')

# Ensure what columns are character
summary(data)


# Convert char to double
data$PERC_EXP_EFF <- as.double(data$PERC_EXP_EFF)
data$FG_PERC <- as.double(data$FG_PERC)
data$ORB <- as.double(data$ORB)


# Reshape the data for the model
# Change cols to make variable column
data_shaped <- data %>%

  
  pivot_longer (
    
    # cols is finding all columns that not the ones specified
    cols = -any_of(c("FIRST", "LAST", "ATHLETE_ID", "ATHLETE_NUM", "POSITION", "GUARD", "DATE", "TIMEPOINT", "TP1")),  # Select columns to pivot
    names_to = "Variable",           # New column for variable names
    values_to = "Value"              # New column for values
  )

# Choose what variable to monitor
monitoring <- data_shaped[data_shaped$Variable == "PLAYERLOAD", ]

# Specify date format
monitoring$DATE <- as.Date(monitoring$DATE, "%m/%d/%y")

# Prepare is the athletemonitoring function
prepared_data <- prepare(
  data = monitoring,
  athlete = "LAST",
  date = "DATE",
  variable = "Variable",
  value = "Value",
  acute = 7,
  chronic = 28,
  
  NA_session = NA,
  

  NA_day = 0,
  
  # How should be multiple day entries summarised?
  # With "load", it is a "sum", with other metrics that
  # do not aggregate, it can me "mean"
  day_aggregate = function(x) {
    sum(x, na.rm = TRUE)
  },
  
  # Rolling estimators for Acute and Chronic windows
  rolling_estimators = function(x) {
    c(
      "mean" = mean(x, na.rm = TRUE),
      "sd" = sd(x, na.rm = TRUE),
      "cv" = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    )
  },
  
  # Additional estimator post-rolling
  posthoc_estimators = function(data) {
    data$ACD <- data$acute.mean - data$chronic.mean
    data$ACR <- data$acute.mean / data$chronic.mean
    data$ES <- data$ACD / data$chronic.sd
    
    # Make sure to return the data
    return(data)
  },
  
  # Group summary estimators
  group_summary_estimators = function(x) {
    c(
      "median" = median(x, na.rm = TRUE),
      "lower" = quantile(x, 0.25, na.rm = TRUE)[[1]],
      "upper" = quantile(x, 0.75, na.rm = TRUE)[[1]]
    )
  }
)


# Get summary
prepared_data


summary(prepared_data)


# Bar plot
# Trellis seperates by athlete
plot(
  prepared_data,
  type = "bar",
  trellis=TRUE
  
)

# Line plots
# These plots represent summary of the rollins estimators


plot(
  prepared_data,
  type = "line",
  
  # To filter out athletes
  athlete_name = "Mitchell",
  group_lower_name = "group.lower",
  group_central_name = "group.median",
  group_upper_name = "group.upper",
  trellis = TRUE
)


# Calendar heatmap plot
plot(
  prepared_data,
  type = "calendar",
  
  # To filter out athletes
  athlete_name = "Doster",
  
  # To filter out variables
  #variable_name = "PLAYERLOAD",
  
  # To print estimator
  estimator_name = "variable.value", # Or use "entries"
  
  # To filter out last days
  last_n = 365,
  
  # To setup colors
  low_color = "white",
  high_color = "red",
  na_color = "grey50",
  
  # Should the whole year be plotted?
  # Otherwise full months are plotted
  full_year = FALSE,
  
  # Should year label be plotted? 
  # in the case of multiple years involved
  # it is always plotted
  year_label = FALSE,
  
  # Short weekdays?
  short_weekday = TRUE,
  
  # Label size
  label_size = 2,
  
  # Aggregation function in the case multiple athletes/variables/levels are used
  aggregate_func = mean
)

