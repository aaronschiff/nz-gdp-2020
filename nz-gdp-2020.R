# New Zealand 2020 GDP prediction

# *****************************************************************************
# Setup ----

library(conflicted)
library(tidyverse)
library(janitor)
library(here)
library(my.r.functions)
library(tsibble)
library(fable)
library(feasts)
library(RcppRoll)
library(lubridate)
library(distributional)
library(scales)

conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ----

dat_raw <- read_csv(file = here("data/gross-domestic-product-december-2020-quarter-csv.csv"), 
                    col_types = "ccncciccccccc") %>%
  clean_names()

dat_qtr_gdp <- dat_raw %>%
  filter(series_reference == "SNEQ.SG01RSC00B01") %>%
  select(period, value = data_value) %>%
  separate(col = "period", 
           into = c("year", "month"), 
           sep = "\\.", 
           convert = TRUE) %>%
  mutate(quarter = as.integer(month / 3)) %>%
  select(-month) %>%
  mutate(date = yearquarter(x = paste0(year, "Q", quarter))) %>%
  select(-year, -quarter) %>%
  rename(value_q = value) %>%
  mutate(value_a = roll_sum(x = value_q, n = 4, fill = NA_real_, align = "right")) %>%
  as_tsibble(regular = TRUE, index = date) %>%
  relocate(date, .before = value_q)

# *****************************************************************************


# *****************************************************************************
# Model and forecast for 2020 ----

# Training data for model
dat_qtr_gdp_model <- dat_qtr_gdp %>% filter(year(date) < 2020)

# Models
bc_lambda_qtr <- dat_qtr_gdp_model %>% 
  features(.var = value_q, features = guerrero) %>%
  pull(lambda_guerrero)

qtr_gdp_model <- dat_qtr_gdp_model %>%
  model(
    arima = ARIMA(formula = box_cox(x = value_q, lambda = bc_lambda_qtr), 
                  ic = "bic")
  ) 

bc_lambda_ann <- dat_qtr_gdp_model %>% 
  filter(!is.na(value_a)) %>%
  features(.var = value_a, features = guerrero) %>%
  pull(lambda_guerrero)

ann_gdp_model <- dat_qtr_gdp_model %>%
  model(
    arima = ARIMA(formula = box_cox(x = value_a, lambda = bc_lambda_ann), 
                  ic = "bic")
  )

# Forecast for 2020 from combined model
qtr_gdp_forecast_2020 <- qtr_gdp_model %>%
  forecast(h = 4) %>%
  hilo(level = 95) %>%
  rename(conf = paste0(95, "%")) %>%
  unpack_hilo(cols = conf) %>%
  as_tibble() %>%
  select(-value_q, -.model) 

ann_gdp_forecast_2020 <- ann_gdp_model %>%
  forecast(h = 4) %>%
  filter(year(date) == 2020, 
         quarter(date) == 4) %>%
  hilo(level = 95) %>%
  rename(conf = paste0(95, "%")) %>%
  unpack_hilo(cols = conf) %>%
  as_tibble() %>%
  select(-value_a, -.model) 

# *****************************************************************************


# *****************************************************************************
# Comparisons ----

# Quarterly
qtr_comp_2020 <- full_join(
  x = qtr_gdp_forecast_2020, 
  y = dat_qtr_gdp %>% filter(year(date) > 2017) %>% select(-value_a), 
  by = "date"
) %>%
  mutate(type = ifelse(year(date) < 2020, "actual", "forecast")) %>%
  arrange(date)

chart_qtr_comp_2020 <- qtr_comp_2020 %>%
  ggplot(mapping = aes(x = date)) + 
  my_geom_point(mapping = aes(y = value_q / 1000)) +
  geom_line(mapping = aes(y = value_q / 1000), size = 0.1) + 
  my_geom_pointrange(mapping = aes(y = .mean / 1000, 
                                   ymin = conf_lower / 1000, 
                                   ymax = conf_upper / 1000), 
                     colour = "firebrick", 
                     fatten = 0.1, 
                     size = 0.25) + 
  geom_segment(x = as_date(yearquarter("2019 Q4")), 
               xend = as_date(yearquarter("2020 Q1")), 
               y = qtr_comp_2020 %>% filter(date == yearquarter("2019 Q4")) %>% pull(value_q) / 1000, 
               yend = qtr_comp_2020 %>% filter(date == yearquarter("2020 Q1")) %>% pull(.mean) / 1000, 
               size = 0.1, 
               colour = "firebrick") + 
  geom_line(mapping = aes(y = .mean / 1000), size = 0.1, colour = "firebrick") + 
  my_geom_text(mapping = aes(y = value_q / 1000, 
                             label = format_decimal_label(x = value_q / 1000, dp = 1)), 
               nudge_x = 25) + 
  my_geom_text(mapping = aes(y = .mean / 1000, 
                             label = format_decimal_label(x = .mean / 1000, dp = 1)), 
               nudge_x = 25, 
               colour = "firebrick") + 
  my_geom_text(mapping = aes(y = conf_lower / 1000, 
                             label = format_decimal_label(x = conf_lower / 1000, dp = 1)), 
               nudge_y = -0.2, 
               rel_size = 0.75, 
               colour = "indianred") + 
  my_geom_text(mapping = aes(y = conf_upper / 1000, 
                             label = format_decimal_label(x = conf_upper / 1000, dp = 1)), 
               nudge_y = 0.2, 
               rel_size = 0.75, 
               colour = "indianred") + 
  scale_y_continuous(limits = c(56, 68), breaks = seq(56, 68, 2)) + 
  scale_x_yearquarter(breaks = date_breaks("3 months"))

output_chart(chart = chart_qtr_comp_2020, 
             path = here("outputs"), 
             orientation = "wide", 
             xlab = "Quarter", 
             ggtitle = "NZ real GDP (2009/10 $billion)")

# Annual -- uses prediction interval width from annual model applied to values
# from quarterly model for consistency
forecast_2020_qtr_year <- sum(qtr_gdp_forecast_2020$.mean)

annual_comp_2020 <- full_join(
  x = dat_qtr_gdp %>%
    filter(quarter(date) == 4) %>%
    mutate(year = as.integer(year(date))) %>%
    select(year, value_a) %>%
    as_tibble(), 
  y = ann_gdp_forecast_2020 %>%
    mutate(year = as.integer(year(date))) %>%
    select(-date), 
  by = "year"
) %>%
  filter(year > 2009) %>%
  mutate(conf_lower_ratio = conf_lower / .mean, 
         conf_upper_ratio = conf_upper / .mean) %>%
  mutate(.mean = ifelse(year == 2020, forecast_2020_qtr_year, .mean)) %>%
  mutate(conf_lower = .mean * conf_lower_ratio, 
         conf_upper = .mean * conf_upper_ratio) %>%
  print()

chart_annual_comp_2020 <- annual_comp_2020 %>%
  ggplot(mapping = aes(x = year)) + 
  my_geom_point(mapping = aes(y = value_a / 1000)) +
  geom_line(mapping = aes(y = value_a / 1000), size = 0.1) + 
  my_geom_pointrange(mapping = aes(y = .mean / 1000, 
                                   ymin = conf_lower / 1000, 
                                   ymax = conf_upper / 1000), 
                     colour = "firebrick", 
                     fatten = 0.1, 
                     size = 0.25) + 
  geom_segment(x = 2019L, 
               xend = 2020L, 
               y = annual_comp_2020 %>% filter(year == 2019) %>% pull(value_a) / 1000, 
               yend = annual_comp_2020 %>% filter(year == 2020) %>% pull(.mean) / 1000, 
               size = 0.1, 
               colour = "firebrick") + 
  my_geom_text(mapping = aes(y = value_a / 1000, 
                             label = format_decimal_label(x = value_a / 1000, dp = 0)), 
               nudge_x = 0.25) + 
  my_geom_text(mapping = aes(y = .mean / 1000, 
                             label = format_decimal_label(x = .mean / 1000, dp = 0)), 
               nudge_x = 0.25, 
               colour = "firebrick") + 
  my_geom_text(mapping = aes(y = conf_lower / 1000, 
                             label = format_decimal_label(x = conf_lower / 1000, dp = 0)), 
               nudge_y = -1, 
               rel_size = 0.75, 
               colour = "indianred") + 
  my_geom_text(mapping = aes(y = conf_upper / 1000, 
                             label = format_decimal_label(x = conf_upper / 1000, dp = 0)), 
               nudge_y = 1, 
               rel_size = 0.75, 
               colour = "indianred") +
  scale_y_continuous(limits = c(190, 270), breaks = seq(190, 270, 10)) + 
  scale_x_continuous(limits = c(2010, 2020.5), breaks = seq(2010, 2020, 1))

output_chart(chart = chart_annual_comp_2020, 
             path = here("outputs"), 
             orientation = "wide", 
             xlab = "Calendar year", 
             ggtitle = "NZ real GDP (2009/10 $billion)")

# *****************************************************************************

