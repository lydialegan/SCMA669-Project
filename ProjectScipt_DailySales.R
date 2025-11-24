library(fpp3)

#reading daily datasets
target_day_1 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_10302025_KW.txt", header = TRUE, sep = "\t")
target_day_2 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_10312025_KW.txt", header = TRUE, sep = "\t")
target_day_3 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11012025_KW.txt", header = TRUE, sep = "\t")
target_day_4 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11022025_KW.txt", header = TRUE, sep = "\t")
target_day_5 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11032025_KW.txt", header = TRUE, sep = "\t")
target_day_6 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11042025_KW.txt", header = TRUE, sep = "\t")
target_day_7 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11052025_KW.txt", header = TRUE, sep = "\t")
target_day_8 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11062025_KW.txt", header = TRUE, sep = "\t")
target_day_9 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11072025_KW.txt", header = TRUE, sep = "\t")
target_day_10 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_DAILY_SALES_TCIN_LOC_11082025_KW.txt", header = TRUE, sep = "\t")


#concatenating daily dataframes
All_days <- rbind(
  target_day_1, target_day_2, target_day_3, target_day_4, target_day_5,
  target_day_6, target_day_7, target_day_8, target_day_9, target_day_10
)

#aggregate to our forecasting series: total daily sales
daily_total <- All_days %>%
  mutate(SALES_DATE = as.Date(SALES_DATE)) %>%   # make sure it's a Date
  group_by(SALES_DATE) %>%
  summarise(total_sales = sum(SALE_AMOUNT, na.rm = TRUE), .groups = "drop")

daily_ts <- daily_total %>%
  as_tsibble(index = SALES_DATE)

#autoplot of Total Daily Sales for Target
autoplot(daily_ts, total_sales, size = 1.1) +
  geom_point(size = 2) +
  labs(
    title    = "Total Daily Sales for Target Product",
    subtitle = "Concatenated from 10 daily files (Oct 30 – Nov 8)",
    x = "Date",
    y = "Sales"
  )

#train/validation split of daily total sales
k <- 3
N <- nrow(daily_ts)

daily_train <- daily_ts %>% filter(row_number() <= N - k)
daily_valid <- daily_ts %>% filter(row_number() >  N - k)

#testing multiple ARIMA models: daily total sales
fits_arima <- daily_train %>%
  model(
    ARIMA_010 = ARIMA(total_sales ~ pdq(0,1,0)),
    ARIMA_110 = ARIMA(total_sales ~ pdq(1,1,0)),
    ARIMA_011 = ARIMA(total_sales ~ pdq(0,1,1)),
    ARIMA_111 = ARIMA(total_sales ~ pdq(1,1,1))
  )
report(fits_arima) #results show that 0,1,0 is the best ARIMA model due to lowest AIC, AICc, BIC

#ARIMA models: daily total sales
ARIMA(total_sales ~ pdq(0, 1, 0) + drift())

#main model block: daily total sales
fits <- daily_train %>%
  model(
    ETS   = ETS(total_sales),
    ARIMA = ARIMA(total_sales ~ pdq(0,1,0)),
    TSLM  = TSLM(total_sales ~ trend())
  )

report(fits)

fits %>%
  select(ARIMA) %>%
  report()

#using ARIMA block for final ARIMA model: daily total sales
final_arima <- daily_ts %>%
  model(
    ARIMA = ARIMA(total_sales ~ pdq(0,1,0))
  )

#forecasting ARIMA: daily total sales
final_arima_fc <- final_arima %>%
  forecast(h = "14 days")

autoplot(final_arima_fc, daily_ts) +
  labs(
    title = "ARIMA(0,1,0) Forecast for Daily Sales",
    subtitle = "Forecast horizon: 14 days with 80% and 95% prediction intervals",
    x = "Date",
    y = "Total Sales"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



glimpse(daily_ts)

#fit TSLM on training: total daily sales
fit_tslm <- daily_train %>%
  model(
    TSLM = TSLM(total_sales ~ trend())
  )

#TSLM: forecast on validation: total daily sales
fc_tslm <- fit_tslm %>%
  forecast(new_data = daily_valid)

#plot TSLM: total daily sales
autoplot(daily_ts, total_sales) +
  autolayer(fc_tslm, level = NULL) +
  labs(
    title    = "TSLM: Trend Regression for Daily Sales",
    subtitle = "Fit on first 7 days, forecast for last 3 days",
    x        = "Date",
    y        = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#ETS model: total daily sales
fit_ets <- daily_train %>%
  model(
    ETS = ETS(total_sales)
  )

#ETS mode: forecast on validation: total daily sales
fc_ets <- fit_ets %>%
  forecast(new_data = daily_valid)

#plot ETS: total daily sales
autoplot(daily_ts, total_sales) +
  autolayer(fc_ets, level = NULL) +
  labs(
    title    = "ETS: Exponential Smoothing for Daily Sales",
    subtitle = "Fit on first 7 days, forecast for last 3 days",
    x        = "Date",
    y        = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#ARIMA(0,1,0) model: total daily sales
fit_arima <- daily_train %>%
  model(
    ARIMA = ARIMA(total_sales ~ pdq(0, 1, 0))
  )

#ARIMA(0,1,0) model: forecast on validation: total daily sales
fc_arima <- fit_arima %>%
  forecast(new_data = daily_valid)

#plot ARIMA(0,1,0) model: total daily sales
autoplot(daily_ts, total_sales) +
  autolayer(fc_arima) +
  labs(
    title    = "ARIMA(0,1,0): Random Walk Model",
    subtitle = "Fit on first 7 days, 3-day ahead forecast with prediction intervals",
    x        = "Date",
    y        = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#ensemble model (ETS + ARIMA + TSLM): total daily sales
fit_ens <- daily_train %>%
  model(
    Ensemble = (ETS(total_sales) +
                  ARIMA(total_sales ~ pdq(0,1,0)) +
                  TSLM(total_sales ~ trend())) / 3
  )

#ensemble model: forecast on validation: total daily sales
fc_ens <- fit_ens %>%
  forecast(new_data = daily_valid)

#plot ensemble model: total daily sales
autoplot(daily_ts, total_sales) +
  autolayer(fc_ens) +
  labs(
    title    = "Ensemble: Average of ETS, ARIMA(0,1,0), and TSLM",
    subtitle = "Fit on first 7 days, forecast for last 3 days",
    x        = "Date",
    y        = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#data visualization
autoplot(daily_ts, total_sales) +
  geom_point(size = 2) +
  labs(
    title    = "Total Daily Sales for Target Ice Cream Sandwiches",
    subtitle = "Aggregated across all stores (10 days of data)",
    x        = "Date",
    y        = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#cross validation for all four models
cv <- daily_train %>%
  stretch_tsibble(.init = 5, .step = 1) %>%
  model(
    ETS      = ETS(total_sales),
    ARIMA    = ARIMA(total_sales ~ pdq(0,1,0)),
    TSLM     = TSLM(total_sales ~ trend()),
    Ensemble = (ETS(total_sales) +
                  ARIMA(total_sales ~ pdq(0,1,0)) +
                  TSLM(total_sales ~ trend())) / 3
  ) %>%
  forecast(h = 1) %>%
  accuracy(daily_train)

cv


#final operational forecast: ARIMA(0,1,0): total daily sales
final_fit <- daily_ts %>%
  model(
    ARIMA = ARIMA(total_sales ~ pdq(0,1,0))
  )

final_fc <- final_fit %>%
  forecast(h = "14 days")

autoplot(daily_ts, total_sales) +
  autolayer(final_fc, level = c(80, 95)) +
  labs(
    title = "Final Operational Forecast: ARIMA(0,1,0)",
    subtitle = "14-day forecast with 80% and 95% prediction intervals",
    x = "Date",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
