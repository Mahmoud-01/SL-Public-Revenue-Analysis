# Install necessary packages if you haven't already
install.packages(c("tidyverse", "lubridate", "forecast"))


# Install the stringi package from the downloaded file
install.packages("C:\\Users\\User\\Downloads\\stringi_1.8.4.zip", repos = NULL, type = "win.binary")
# install.packages("stringi")

# Load the packages
library(tidyverse)
library(lubridate)
library(forecast)

# Load the CSV file
csv_file_path <- "C:\\Users\\User\\Desktop\\SL Public Revenue Analysis - R\\REVSLE_26052024073604118.csv"

revenue_data <- read.csv(csv_file_path)

# Print the first few rows of the data to ensure it is loaded correctly
print(head(revenue_data))

# Check the structure of the data
print(str(revenue_data))

# Verify the Year column and create a Date column
if ("Year" %in% names(revenue_data)) {
  revenue_data$Date <- as.Date(paste0(revenue_data$Year, "-01-01"))
} else {
  stop("The 'Year' column is missing in the CSV file.")
}

# Check for missing values
print(sum(is.na(revenue_data)))

# Fill or remove missing values if any
revenue_data <- revenue_data %>% drop_na(Value)

# Check the data structure again
print(str(revenue_data))

# Group by year and calculate total revenue for each year
annual_revenue <- revenue_data %>%
  group_by(Year) %>%
  summarize(AnnualRevenue = sum(Value, na.rm = TRUE))

# Print the annual revenue
print(annual_revenue)

# Calculate the average annual revenue
average_annual_revenue <- annual_revenue %>%
  summarize(AverageRevenue = mean(AnnualRevenue, na.rm = TRUE))

print(average_annual_revenue)

# Calculate annual growth rate
annual_revenue <- annual_revenue %>%
  mutate(
    PreviousRevenue = lag(AnnualRevenue),
    GrowthRate = (AnnualRevenue - PreviousRevenue) / PreviousRevenue * 100
  )

# View the annual revenue with growth rate
print(head(annual_revenue))

# Check if there are any observations
if (nrow(annual_revenue) > 0) {
  # Convert the data to a time series object
  revenue_ts <- ts(annual_revenue$AnnualRevenue, start = min(annual_revenue$Year), frequency = 1)

  # Fit an ARIMA model
  fit <- auto.arima(revenue_ts)

  # Forecast the next 5 years
  forecasted_revenue <- forecast(fit, h = 5)

  # View the forecasted revenue
  print(forecasted_revenue)

  # Plot the original revenue data
  ggplot(annual_revenue, aes(x = Year, y = AnnualRevenue)) +
    geom_line() +
    ggtitle("Annual Revenue Trend") +
    xlab("Year") +
    ylab("Revenue")

  # Plot the annual growth rate
  ggplot(annual_revenue, aes(x = Year, y = GrowthRate)) +
    geom_line() +
    ggtitle("Annual Growth Rate") +
    xlab("Year") +
    ylab("Growth Rate (%)")

  # Plot the forecasted revenue
  autoplot(forecasted_revenue) +
    ggtitle("Forecasted Annual Revenue") +
    xlab("Year") +
    ylab("Revenue")
} else {
  print("No data available for time series analysis.")
}
