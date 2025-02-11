library(dplyr)
library(ggplot)
df <- getSymbols('MSFT',src='yahoo',from="1990-01-01",to="2022-09-30",auto.assign=FALSE)

df$returns = na.locf(diff(df$MSFT.Adjusted) / df$MSFT.Adjusted[1:length(df$MSFT.Adjusted), 1])

df2 <- data.frame(date = index(df), coredata(df))

dates <- df2 %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month)  %>% summarise(first_dates = first(date))

# Get the first date of Oct-2021
initial_date = subset(dates, (dates$year=='2021') & (dates$month=='10'))$first_dates

# Set the initial iloc for the forecast data
initial_iloc_to_forecast <- which(df2$date==as.Date(initial_date))

# Subset the df2 dataframe to only our forecast data
df_forecasts <- subset(df2, df2$date>=as.Date(initial_date))

# Create the signal column
df_forecasts$signal <- 0

# The for loop to estimate the model each day
for (i in initial_iloc_to_forecast:nrow(df2)) {

  # Estimate the models
  arimaFit = auto.arima(
    df2[((i-1)-5000):(i-1),]$returns,
    max.p = 5,
    max.q = 5,
    stationary = TRUE,
    seasonal = FALSE,
    ic = c("aic"),
    stepwise = TRUE,
    allowdrift = FALSE,
    allowmean = FALSE,
  )
  # Forecast one step ahead
  forecast1 = forecast(arimaFit,1)

  # We go long if the forecast return is positive, otherwise, we make no position.
  df_forecasts[(i+1-initial_iloc_to_forecast),]$signal <- if (forecast1$mean[1]>=0) 1 else 0

  # Print date and signal
  print(paste0("Date is ",df_forecasts[(i+1-initial_iloc_to_forecast),]$date))
  print(paste0("Signal is ",df_forecasts[(i+1-initial_iloc_to_forecast),]$signal))
}
# Create the strategy returns
df_forecasts$stra_returns <- df_forecasts$returns*df_forecasts$signal

# Create the strategy cumulative returns
df_forecasts$stra_cum_returns <- cumprod(1+df_forecasts$stra_returns)

# Create the buy and hold strategy cumulative returns
df_forecasts$bnh_cum_returns <- cumprod(1+df_forecasts$returns)

# Convert the date column in date type
df_forecasts$date <- as.Date(df_forecasts$date)

# Plot the both buy-and-hold and the strategy cumulative returns
ggplot(data=df_forecasts, aes(x = df_forecasts$date))+
       geom_line(aes(y = df_forecasts$stra_cum_returns, color="Strategy Cumulative Returns"))
       +geom_line(aes(y = df_forecasts$bnh_cum_returns,color="B&H Cumulative Returns"))+ ggtitle("Buy and Hold and Strategy Cumulative Returns") + xlab("Date") + ylab("Returns") + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + scale_x_date(date_labels = "%b %y")
