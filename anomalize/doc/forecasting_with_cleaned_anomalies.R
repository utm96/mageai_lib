## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = F,
  fig.align = "center"
)

devtools::load_all()

## ----setup, message=FALSE-----------------------------------------------------
library(tidyverse)
library(tidyquant)
library(anomalize)
library(timetk)

# NOTE: timetk now has anomaly detection built in, which 
#  will get the new functionality going forward.
#  Use this script to prevent overwriting legacy anomalize:

anomalize <- anomalize::anomalize
plot_anomalies <- anomalize::plot_anomalies

## -----------------------------------------------------------------------------
tidyverse_cran_downloads

## ----fig.height=8, fig.width=6------------------------------------------------
tidyverse_cran_downloads %>%
  ggplot(aes(date, count, color = package)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scales = "free_y") +
  scale_color_viridis_d() +
  theme_tq() 

## -----------------------------------------------------------------------------
lubridate_tbl <- tidyverse_cran_downloads %>%
  ungroup() %>%
  filter(package == "lubridate")

## -----------------------------------------------------------------------------
forecast_mae <- function(data, col_train, col_test, prop = 0.8) {
  
  predict_expr <- enquo(col_train)
  actual_expr <- enquo(col_test)
  
  idx_train <- 1:(floor(prop * nrow(data)))
  
  train_tbl <- data %>% filter(row_number() %in% idx_train)
  test_tbl  <- data %>% filter(!row_number() %in% idx_train)
  
  # Model using training data (training) 
  model_formula <- as.formula(paste0(quo_name(predict_expr), " ~ index.num + year + quarter + month.lbl + day + wday.lbl"))
  
  model_glm <- train_tbl %>%
    tk_augment_timeseries_signature() %>%
    glm(model_formula, data = .)
  
  # Make Prediction
  suppressWarnings({
    # Suppress rank-deficit warning
    prediction <- predict(model_glm, newdata = test_tbl %>% tk_augment_timeseries_signature()) 
    actual     <- test_tbl %>% pull(!! actual_expr)
  })
  
  # Calculate MAE
  mae <- mean(abs(prediction - actual))
  
  return(mae)
  
}

## -----------------------------------------------------------------------------
lubridate_anomalized_tbl <- lubridate_tbl %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  
  # Function to clean & repair anomalous data
  clean_anomalies()

lubridate_anomalized_tbl

## -----------------------------------------------------------------------------
lubridate_anomalized_tbl %>%
  forecast_mae(col_train = observed, col_test = observed, prop = 0.8)

## -----------------------------------------------------------------------------
lubridate_anomalized_tbl %>%
  forecast_mae(col_train = observed_cleaned, col_test = observed, prop = 0.8)

## -----------------------------------------------------------------------------
(2755 - 4054) / 4054 

