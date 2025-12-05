#-----------------REVISIONS POST-PRESENTATION--------------------------

names(All_weeks)

#aggregate to total weekly sales
weekly_total <- All_weeks %>%
  mutate(SALES_DATE = as.Date(SALES_DATE)) %>%
  group_by(SALES_DATE) %>%
  summarise(
    weekly_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  )

# Weekly sales by ITEM_DESCRIPTION (per product, per week)
weekly_items <- All_weeks %>%
  mutate(SALES_DATE = as.Date(SALES_DATE)) %>%
  group_by(SALES_DATE, ITEM_DESCRIPTION) %>%
  summarise(
    weekly_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as_tsibble(
    index = SALES_DATE,
    key   = ITEM_DESCRIPTION
  )

# Hierarchical weekly data: Total + each ITEM_DESCRIPTION
weekly_hier <- weekly_items %>%
  aggregate_key(
    ITEM_DESCRIPTION,
    weekly_sales = sum(weekly_sales)
  )

key_vars(weekly_hier)
distinct(weekly_hier, ITEM_DESCRIPTION)

#look at distinct weekly dates
date_order <- weekly_hier %>%
  distinct(SALES_DATE) %>%
  arrange(SALES_DATE)

date_order

#training set
train_end <- date_order %>%
  slice(4) %>%
  pull(SALES_DATE)

train_end

#split hierarchical data into train and test
weekly_hier_train <- weekly_hier %>%
  filter(SALES_DATE <= train_end)

weekly_hier_test <- weekly_hier %>%
  filter(SALES_DATE > train_end)

#fit auto ETS
weekly_hier_fit <- weekly_hier_train %>%
  model(
    ets = ETS(weekly_sales)
  )

#create new hierarchical forecast
weekly_hier_rec <- weekly_hier_fit %>%
  reconcile(
    bu = bottom_up(ets)
  )

#forecast one week ahead
weekly_hier_fc <- weekly_hier_rec %>%
  forecast(h = 1)

#check accuracy
accuracy(weekly_hier_fc, weekly_hier_test)


#fitting multiple ETS models on total series
weekly_total_only <- weekly_items %>%
  filter(ITEM_DESCRIPTION == "Total")

ets_compare <- weekly_total_only %>%
  model(
    ETS_auto = ETS(weekly_sales),
    ETS_ANN  = ETS(weekly_sales ~ error("A") + trend("N") + season("N")),
    ETS_AAN  = ETS(weekly_sales ~ error("A") + trend("A") + season("N"))
  )

# Compare their fit statistics
glance(ets_compare)


#comparing ETS models on total weekly sales
ets_compare <- weekly_ts %>%
  model(
    ETS_auto = ETS(weekly_sales),
    ETS_ANN  = ETS(weekly_sales ~ error("A") + trend("N") + season("N"))
    # We can try ETS_AAN later if this runs OK
  )

glance(ets_compare)



# -- producing final hierarchical forecast plot

# 4-week hierarchical forecast for all levels
weekly_hier_fc_4 <- weekly_hier_rec %>%
  forecast(h = 4)

# keeping only the aggregated (top-level) series
weekly_total_hist <- weekly_hier %>%
  filter(ITEM_DESCRIPTION == "<aggregated>")

weekly_total_fc_4 <- weekly_hier_fc_4 %>%
  filter(ITEM_DESCRIPTION == "<aggregated>")

# Plot: historical total weekly sales + hierarchical forecast
weekly_total_hist %>%
  autoplot(weekly_sales) +
  autolayer(weekly_total_fc_4, level = c(80, 95)) +
  labs(
    title    = "Hierarchical ETS Bottom-Up Forecast – Total Weekly Sales",
    subtitle = "4-week reconciled forecast from product-level ETS models",
    x        = "Week Ending Date",
    y        = "Total Weekly Sales"
  ) +
  theme_minimal()






# -- per-product forecast plot (bottom level)
# item-level bottom-up forecast option 1
library(fabletools)  # for is_aggregated()
weekly_items_fc_clean <- weekly_items_fc %>%
  mutate(
    Flavor = case_when(
      grepl("Brookie Dough", ITEM_DESCRIPTION) ~ "Brookie Dough",
      grepl("Cookie Monster", ITEM_DESCRIPTION) ~ "Cookie Monster",
      grepl("Strawberry Shortcake", ITEM_DESCRIPTION) ~ "Strawberry Shortcake",
      grepl("Classic", ITEM_DESCRIPTION) ~ "Classic",
      grepl("Chocolate Blackout", ITEM_DESCRIPTION) ~ "Chocolate Blackout",
      TRUE ~ ITEM_DESCRIPTION
    )
  ) %>%
  filter(!is_aggregated(ITEM_DESCRIPTION))   # drop the aggregated/top level

library(ggplot2)

ggplot(weekly_items_fc_clean,
       aes(x = SALES_DATE,
           y = .mean,
           colour = Flavor,
           group = Flavor)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Item-Level Bottom-Up Forecasts",
    subtitle = "Each flavor modeled individually with ETS, then reconciled upward",
    x = "Week Ending Date",
    y = "Forecasted Weekly Sales",
    colour = "Flavor"
  ) +
  theme_minimal()


#item-level bottom-up forecast option 2
library(fabletools)  # for is_aggregated()
weekly_items_fc_clean <- weekly_items_fc %>%
  mutate(
    Flavor = case_when(
      grepl("Brookie Dough", ITEM_DESCRIPTION) ~ "Brookie Dough",
      grepl("Cookie Monster", ITEM_DESCRIPTION) ~ "Cookie Monster",
      grepl("Strawberry Shortcake", ITEM_DESCRIPTION) ~ "Strawberry Shortcake",
      grepl("Classic", ITEM_DESCRIPTION) ~ "Classic",
      grepl("Chocolate Blackout", ITEM_DESCRIPTION) ~ "Chocolate Blackout",
      TRUE ~ ITEM_DESCRIPTION
    )
  ) %>%
  filter(!is_aggregated(ITEM_DESCRIPTION))   # drop the aggregated/top level

library(dplyr)
library(tsibble)    # for as_tibble if needed

weekly_items_fc_clean_tbl <- weekly_items_fc_clean %>%
  as_tibble() %>%                     # drop tsibble classes / agg_vec quirks
  mutate(
    Flavor = as.character(Flavor)     # ensure Flavor is a normal character vector
  )

library(ggplot2)

ggplot(weekly_items_fc_clean_tbl,
       aes(x = SALES_DATE,
           y = .mean,
           colour = Flavor,
           group = Flavor)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Item-Level Bottom-Up Forecasts",
    subtitle = "Each flavor modeled individually with ETS, then reconciled upward",
    x = "Week Ending Date",
    y = "Forecasted Weekly Sales",
    colour = "Flavor"
  ) +
  theme_minimal()

#We compared multiple ETS structures at the total level, including both the 
#automatic ETS model and a manual ETS(A,N,N) specification. With only five weekly 
#observations, both models produced nearly identical likelihoods and similar 
#accuracy metrics, with auto ETS slightly favored by AIC/BIC.

#For item-level hierarchical forecasting, automatic ETS modeling was used for 
#each flavor because the extremely short series did not support reliable 
#estimation of more complex or custom ETS structures. These item-level ETS 
#forecasts were then reconciled using a bottom-up hierarchy to produce coherent 

#forecasts for both flavor-level and total weekly sales.



#--------USED THE FOLLOWING CODE TO DISTINGUISH FLAVORS WITHIN WEEKLY MODEL GRAPHICS FOR COMPARISON ANALYSIS--------

# Weekly sales by flavor (ITEM_DESCRIPTION) across all locations
weekly_flavor_ts <- All_weeks %>%
  mutate(
    SALES_DATE = as.Date(SALES_DATE),
    Flavor = case_when(
      grepl("Brookie Dough", ITEM_DESCRIPTION) ~ "Brookie Dough",
      grepl("Cookie Monster", ITEM_DESCRIPTION) ~ "Cookie Monster",
      grepl("Strawberry Shortcake", ITEM_DESCRIPTION) ~ "Strawberry Shortcake",
      grepl("Classic", ITEM_DESCRIPTION) ~ "Classic",
      grepl("Chocolate Blackout", ITEM_DESCRIPTION) ~ "Chocolate Blackout",
      TRUE ~ ITEM_DESCRIPTION
    )) %>%
  group_by(SALES_DATE, Flavor) %>%
  summarise(
    weekly_sales = sum(SALE_AMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as_tsibble(
    key   = Flavor,
    index = SALES_DATE
  )
