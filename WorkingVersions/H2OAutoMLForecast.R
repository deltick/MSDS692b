library(tidyquant)
library(h2o)        # Awesome ML Library
library(timetk)
library(ggplot2)
library(tidyverse)
library(dplyr)
library (readr)



TestV1 <- read_csv("C:/Users/JJT/Desktop/USBPTS.csv")
t1 <- select(TestV1, xDate, "Southwest Border")

t1 <- dplyr::rename(t1, date=xDate)
t1 <- dplyr::rename(t1, value="Southwest Border")

t1TS <- ts(t1$value, start = c(2000, 1), frequency = 12)

t1Tib <- as_tibble((t1))

beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")

beer_sales_tbl <- t1Tib

# Plot Beer Sales with train, validation, and test sets shown
beer_sales_tbl %>%
  ggplot(aes(date, price)) +
  # Train Region
  annotate("text", x = ymd("2012-01-01"), y = 7000,
           color = palette_light()[[1]], label = "Train Region") +
  # Validation Region
  geom_rect(xmin = as.numeric(ymd("2016-01-01")), 
            xmax = as.numeric(ymd("2016-12-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[3]]) +
  annotate("text", x = ymd("2016-07-01"), y = 7000,
           color = palette_light()[[1]], label = "Validation\nRegion") +
  # Test Region
  geom_rect(xmin = as.numeric(ymd("2017-01-01")), 
            xmax = as.numeric(ymd("2017-08-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[4]]) +
  annotate("text", x = ymd("2017-05-01"), y = 7000,
           color = palette_light()[[1]], label = "Test\nRegion") +
  # Data
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  # Aesthetics
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Beer Sales: 2007 through 2017",
       subtitle = "Train, Validation, and Test Sets Shown") 


beer_sales_tbl_aug <- beer_sales_tbl %>%
  tk_augment_timeseries_signature()


beer_sales_tbl_clean <- beer_sales_tbl_aug %>%
  select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

beer_sales_tbl_clean

train_tbl <- beer_sales_tbl_clean %>% filter(year < 2018)
valid_tbl <- beer_sales_tbl_clean %>% filter(year == 2018)
test_tbl  <- beer_sales_tbl_clean %>% filter(year == 2019)


h2o.init()
train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)


y <- "value"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance")

automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

error_tbl <- beer_sales_tbl %>% 
  filter(lubridate::year(date) == 2019) %>%
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

error_tbl %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) 


beer_sales_tbl %>%
  ggplot(aes(x = date, y = value)) +
  # Data - Spooky Orange
  geom_point(size = 2, color = "gray", alpha = 0.5, shape = 21, fill = "orange") +
  geom_line(color = "orange", size = 0.5) +
  geom_ma(n = 12, color = "white") +
  # Predictions - Spooky Purple
  geom_point(aes(y = pred), size = 2, color = "gray", alpha = 1, shape = 21, fill = "purple", data = error_tbl) +
  geom_line(aes(y = pred), color = "purple", size = 0.5, data = error_tbl) +
  # Aesthetics
  #theme_spooky(base_size = 20) +
  labs(
    title = "Beer Sales Forecast: h2o + timetk",
    subtitle = "H2O had highest accuracy, MAPE = 3.9%",
    caption = "Thanks to @lenkiefer for theme_spooky!"
  )

