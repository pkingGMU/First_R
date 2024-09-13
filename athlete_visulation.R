require(athletemonitoring)
#> Loading required package: athletemonitoring

data("monitoring")


monitoring <- monitoring[monitoring$Variable == "Training Load", ]


monitoring$Date <- as.Date(monitoring$Date, "%Y-%m-%d")


prepared_data <- prepare(
  data = monitoring,
  athlete = "Full Name",
  date = "Date",
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
#> Preparing data...
#> Rolling...
#> Group summaries...
#> Missing data summaries...
#> Done!

# Get summary
prepared_data
#> Athlete monitoring numeric data with the following characteristics:
#> 
#> 10 athletes:
#> Alan McDonald, Ann Whitaker, Eve Black, Frank West, John Doe, Michael Peterson, Mike Smith, Peter Jackson, Stuart Rogan, Susan Kane 
#> 
#> 363 days:
#> From 18263 to 18625 
#> 
#> 5200 total entries
#> 
#> 0 missing entries
#> 510 missing days
#> 0 extended days
#> 
#> 1 variables:
#> Training Load 
#> 
#> 10 estimators:
#> variable.value, acute.mean, acute.sd, acute.cv, chronic.mean, chronic.sd, chronic.cv, ACD, ACR, ES

summary(prepared_data)
#> # A tibble: 10 × 16
#>    athlete          variable     `Total entries` `Day entries` `Missing entries`
#>    <chr>            <chr>                  <dbl>         <int>             <dbl>
#>  1 Alan McDonald    Training Lo…             520           363                 0
#>  2 Ann Whitaker     Training Lo…             520           363                 0
#>  3 Eve Black        Training Lo…             520           363                 0
#>  4 Frank West       Training Lo…             520           363                 0
#>  5 John Doe         Training Lo…             520           363                 0
#>  6 Michael Peterson Training Lo…             520           363                 0
#>  7 Mike Smith       Training Lo…             520           363                 0
#>  8 Peter Jackson    Training Lo…             520           363                 0
#>  9 Stuart Rogan     Training Lo…             520           363                 0
#> 10 Susan Kane       Training Lo…             520           363                 0
#> # ℹ 11 more variables: `Missing days` <int>, `Extended days` <int>,
#> #   `Start date` <dbl>, `Stop date` <dbl>, Mean <dbl>, SD <dbl>, Min <dbl>,
#> #   Max <dbl>, Median <dbl>, IQR <dbl>, MAD <dbl>


## Plots

# Table plot
# Produces formattable output with sparklines
# This will not work in the readme file, so just copy paste to your console
# plot(
#  prepared_data,
#  type = "table",
#
#  # Use to filter out estimators
#  estimator_name = c("acute.mean", "chronic.mean", "ES", "chronic.sd", "chronic.cv"),
#
#  # Use to filter out athlete
#  # athlete_name = NULL,
#
#  # Use to filter out variables
#  #variable_name = NULL,
#
#  # Show last entries
#  last_n = 42,
#
#  # Round numbers
#  digits = 2
# )

# Bar plot
# To plot group average
plot(
  prepared_data,
  type = "bar"
)
#> Plotting average across athletes. Please select athlete or use `trellis=TRUE`
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).

