# --- Actual weekly totals (top level) ---
weekly_actual <- weekly_ts %>%
  index_by(week = yearweek(SALES_DATE)) %>%
  summarise(actual_weekly_sales = sum(weekly_sales)) %>%
  as_tsibble(index = week)

# --- Bottom-up weekly forecast from daily ARIMA ---
daily_fc_weekly <- final_fc %>%
  index_by(week = yearweek(SALES_DATE)) %>%
  summarise(bottomup_weekly_sales = sum(.mean, na.rm = TRUE)) %>%
  as_tsibble(index = week)

# --- Top-down weekly forecast from weekly ARIMA ---
topdown_weekly <- weekly_fc_arima %>%
  as_tibble() %>%
  transmute(
    week = yearweek(SALES_DATE),
    topdown_weekly_sales = .mean
  ) %>%
  as_tsibble(index = week)

# --- Combine everything ---
comparison_all <- weekly_actual %>%
  full_join(topdown_weekly,  by = "week") %>%
  full_join(daily_fc_weekly, by = "week")

library(ggplot2)

# Last actual week (for shading forecast region)
last_actual_week <- max(weekly_actual$week, na.rm = TRUE)

# Convert to long format for plotting
comparison_long <- comparison_all %>%
  pivot_longer(
    cols = c(actual_weekly_sales,
             topdown_weekly_sales,
             bottomup_weekly_sales),
    names_to = "series",
    values_to = "sales"
  ) %>%
  filter(!is.na(sales))

# Friendly labels
series_labels <- c(
  actual_weekly_sales   = "Actual weekly sales",
  topdown_weekly_sales  = "Top-down weekly ARIMA(0,1,0)",
  bottomup_weekly_sales = "Bottom-up from daily ARIMA(0,1,0)"
)

# Polished plot
ggplot() +
  # Shaded forecast region
  geom_rect(
    aes(
      xmin = last_actual_week,
      xmax = max(comparison_all$week, na.rm = TRUE),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey95",
    alpha = 0.8
  ) +
  # Actual weekly sales as bars
  geom_col(
    data = filter(comparison_long, series == "actual_weekly_sales"),
    aes(x = week, y = sales, fill = "Actual weekly sales"),
    width = 0.6,
    alpha = 0.9
  ) +
  # Forecast lines (top-down & bottom-up)
  geom_line(
    data = filter(comparison_long, series != "actual_weekly_sales"),
    aes(x = week, y = sales, color = series, linetype = series),
    size = 1.1
  ) +
  geom_point(
    data = filter(comparison_long, series != "actual_weekly_sales"),
    aes(x = week, y = sales, color = series),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      topdown_weekly_sales  = "#0073e6",   # blue
      bottomup_weekly_sales = "#FF6A00"    # Target-ish orange
    ),
    labels = series_labels[ c("topdown_weekly_sales", "bottomup_weekly_sales") ]
  ) +
  scale_linetype_manual(
    values = c(
      topdown_weekly_sales  = "solid",
      bottomup_weekly_sales = "dashed"
    ),
    labels = series_labels[ c("topdown_weekly_sales", "bottomup_weekly_sales") ]
  ) +
  scale_fill_manual(
    values = c("Actual weekly sales" = "#CC0000"),   # Target red
    guide = "legend"
  ) +
  labs(
    title    = "Hierarchical View: Weekly Actuals and Forecasts",
    subtitle = "Top-down weekly ARIMA(0,1,0) vs bottom-up from daily ARIMA(0,1,0)",
    x        = "Week",
    y        = "Weekly Sales",
    color    = "Forecast series",
    linetype = "Forecast series",
    fill     = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )