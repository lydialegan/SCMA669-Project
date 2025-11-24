library(fpp3)

#Build Daily Sales Decomposition (Regular vs Promo vs Circular)
daily_promo <- All_days %>%
  mutate(
    REGULAR   = REGULAR_SALE_AMOUNT,
    PROMO     = PROMO_SALE_AMOUNT,
    CIRCULAR  = CIRCULAR_SALE_AMOUNT,
    CIRCLE    = CIRCLE_SALE_AMOUNT,
    CLEARANCE = CLEARANCE_SALE_AMOUNT
  ) %>%
  select(SALES_DATE, REGULAR, PROMO, CIRCULAR, CIRCLE, CLEARANCE) %>%
  pivot_longer(-SALES_DATE, names_to = "Category", values_to = "Amount")

#Daily Promo Decomposition Visualization
daily_promo %>%
  ggplot(aes(x = SALES_DATE, y = Amount, fill = Category)) +
  geom_col() +
  labs(
    title = "Daily Sales Decomposition",
    subtitle = "Regular vs Promo vs Circular vs Clearance",
    x = "Date",
    y = "Sales Amount"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()




#build daily promo flag
promo_lift <- All_days %>%
  mutate(
    promo_flag = if_else(PROMO_SALE_AMOUNT > 0 | CIRCULAR_SALE_AMOUNT > 0 | CIRCLE_SALE_AMOUNT > 0, 1, 0)
  )

#compare promo vs. non-promo days
promo_summary <- promo_lift %>%
  group_by(promo_flag) %>%
  summarise(
    avg_sales = mean(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  )

promo_summary #results come to 7.58 and 8.01, respectively

#promo lift in units sold
promo_quantity_summary <- promo_lift %>%
  group_by(promo_flag) %>%
  summarise(
    avg_units = mean(SALE_QUANTITY, na.rm = TRUE),
    .groups = "drop"
  )

promo_quantity_summary #results come to 1.08 and 1.14, respectively







#channel mix summary
channel_mix <- All_days %>%
  group_by(REPORTING_CHANNEL) %>%
  summarise(
    total_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    total_units = sum(SALE_QUANTITY, na.rm = TRUE),
    .groups = "drop"
  )

channel_mix

#visualization for channel mix
ggplot(channel_mix, aes(x = REPORTING_CHANNEL, y = total_sales, fill = REPORTING_CHANNEL)) +
  geom_col() +
  labs(
    title = "Sales by Channel",
    x = "Channel",
    y = "Total Sales ($)"
  ) +
  theme_minimal()







#flavor mix summary
flavor_mix <- All_days %>%
  group_by(ITEM_DESCRIPTION) %>%
  summarise(
    total_units = sum(SALE_QUANTITY, na.rm = TRUE),
    total_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  )

flavor_mix

flavor_mix_clean <- flavor_mix %>%
  mutate(Flavor = case_when(
    grepl("Brookie Dough", ITEM_DESCRIPTION) ~ "Brookie Dough",
    grepl("Cookie Monster", ITEM_DESCRIPTION) ~ "Cookie Monster",
    grepl("Strawberry Shortcake", ITEM_DESCRIPTION) ~ "Strawberry Shortcake",
    grepl("Classic", ITEM_DESCRIPTION) ~ "Classic",
    grepl("Chocolate Blackout", ITEM_DESCRIPTION) ~ "Chocolate Blackout",
    TRUE ~ ITEM_DESCRIPTION
  ))

#flavor mix visualization
flavor_mix_clean %>%
  ggplot(aes(x = reorder(Flavor, total_units),
             y = total_units,
             fill = Flavor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Units Sold by Flavor",
    x = "Flavor",
    y = "Units Sold"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )