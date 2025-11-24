library(fpp3)

#reading weekly datasets
target_week_1 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_10112025_KW.txt", header = TRUE, sep = "\t")
target_week_2_1 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_10182025_KW.txt", header = TRUE, sep = "\t")
target_week_2_2 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_10182025_KW 2.txt", header = TRUE, sep = "\t")
target_week_3_1 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_10252025_KW.txt", header = TRUE, sep = "\t")
target_week_3_2 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_10252025_KW 2.txt", header = TRUE, sep = "\t")
target_week_4_1 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_11012025_KW.txt", header = TRUE, sep = "\t")
target_week_4_2 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_11012025_KW 2.txt", header = TRUE, sep = "\t")
target_week_4_3 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_11012025_KW 3.txt", header = TRUE, sep = "\t")
target_week_5 <- read.table("C:/Users/lydle/Downloads/Target Sales BPD/Target Sales BPD/BV_157656_WEEKLY_SALES_TCIN_LOC_11082025_KW.txt", header = TRUE, sep = "\t")


#concatenating weekly dataframes
All_weeks <- rbind(
  target_week_1, target_week_2_1, target_week_2_2, target_week_3_1, target_week_3_2,
  target_week_4_1, target_week_4_2, target_week_4_3, target_week_5
)


#aggregate to our forecasting series: total weekly sales
weekly_total <- All_weeks %>%
  mutate(SALES_DATE = as.Date(SALES_DATE)) %>%
  group_by(SALES_DATE) %>%
  summarise(
    weekly_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  )

weekly_ts <- weekly_total %>%
  as_tsibble(index = SALES_DATE)


#quick visualization of weekly sales
autoplot(weekly_ts, weekly_sales) +
  geom_point(size = 2) +
  labs(
    title    = "Weekly Total Sales for Target Ice Cream Sandwiches",
    subtitle = "Aggregated across all locations (5 weekly observations)",
    x        = "Week Ending Date",
    y        = "Weekly Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#ARIMA model(0,1,0): total weekly sales
weekly_fit_arima <- weekly_ts %>%
  model(
    ARIMA = ARIMA(weekly_sales ~ pdq(0,1,0))
  )

weekly_fc_arima <- weekly_fit_arima %>%
  forecast(h = 4)   # e.g., forecast 4 more weeks

#plot ARIMA model(0,1,0): total weekly sales
autoplot(weekly_ts, weekly_sales) +
  autolayer(weekly_fc_arima, level = c(80, 95)) +
  labs(
    title    = "Weekly ARIMA(0,1,0) Forecast",
    subtitle = "Top-level forecast of weekly sales",
    x        = "Week Ending Date",
    y        = "Weekly Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 60000))


names(final_fc)

#bottoms-up weekly forecast from daily ARIMA
daily_fc_weekly <- final_fc %>%
  index_by(week = yearweek(SALES_DATE)) %>%
  summarise(
    bottomup_weekly_sales = sum(.mean, na.rm = TRUE)
  ) %>%
  as_tsibble(index = week)


#prepare top-down forecast from weekly ARIMA
topdown_weekly <- weekly_fc_arima %>%
  as_tibble() %>%
  transmute(
    week = yearweek(SALES_DATE),
    topdown_weekly_sales = .mean
  ) %>%
  as_tsibble(index = week)

#Historical weekly actuals (convert date -> yearweek to match)
weekly_actual <- weekly_ts %>%
  index_by(week = yearweek(SALES_DATE)) %>%
  summarise(actual_weekly_sales = sum(weekly_sales)) %>%
  as_tsibble(index = week)

#join the bottoms-up and the top-town and historical
comparison_all <- weekly_actual %>%
  full_join(topdown_weekly, by = "week") %>%
  full_join(daily_fc_weekly, by = "week")

#Long format for plotting
comparison_long <- comparison_all %>%
  pivot_longer(
    cols = c(actual_weekly_sales,
             topdown_weekly_sales,
             bottomup_weekly_sales),
    names_to = "series",
    values_to = "sales"
  )

#plotting
ggplot(comparison_long, aes(x = week, y = sales, color = series, linetype = series)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title    = "Hierarchical View: Weekly Actuals and Forecasts",
    subtitle = "Top-down weekly ARIMA(0,1,0) vs bottom-up from daily ARIMA(0,1,0)",
    x        = "Week",
    y        = "Weekly Sales",
    color    = "Series",
    linetype = "Series"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#daily decomposition
daily_ts %>%
  model(STL = STL(total_sales ~ season(window = "periodic"))) %>%
  components() %>%
  autoplot() +
  labs(
    title = "STL Decomposition of Daily Sales",
    subtitle = "Short series — no stable trend or seasonality detected",
    x = "Date"
  ) +
  theme_minimal()




#model comparison validation plot
fc_compare <- daily_train %>%
  model(
    ETS   = ETS(total_sales),
    ARIMA = ARIMA(total_sales ~ pdq(0,1,0)),
    TSLM  = TSLM(total_sales ~ trend()),
    Ensemble = (ETS(total_sales) +
                  ARIMA(total_sales ~ pdq(0,1,0)) +
                  TSLM(total_sales ~ trend())) / 3
  ) %>%
  forecast(new_data = daily_valid)

autoplot(daily_ts, total_sales) +
  autolayer(fc_compare, aes(color = .model), level = NULL) +
  labs(
    title = "Validation Forecast Comparison Across Models",
    subtitle = "ARIMA(0,1,0) provides the closest alignment to actual values",
    x = "Date",
    y = "Total Sales",
    color = "Model"
  ) +
  theme_minimal()
