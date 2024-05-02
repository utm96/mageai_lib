## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  
  out.width='100%',
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  
  message = FALSE,
  warning = FALSE
)

# CRAN OMP THREAD LIMIT
Sys.setenv("OMP_THREAD_LIMIT" = 1)

## ---- echo=F,  out.width="100%", fig.align='center'---------------------------
knitr::include_graphics("forecast_plot.jpg")

## ---- echo=F,  out.width="100%", fig.align='center', fig.cap="The Modeltime Workflow"----
knitr::include_graphics("modeltime_workflow.jpg")

## -----------------------------------------------------------------------------
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

## -----------------------------------------------------------------------------
# Data
m750 <- m4_monthly %>% filter(id == "M750")

## -----------------------------------------------------------------------------
m750 %>%
  plot_time_series(date, value, .interactive = interactive)

## -----------------------------------------------------------------------------
# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

## ---- message=TRUE------------------------------------------------------------
# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
        data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(value ~ date, data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
        data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth") 

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_normalize(date_num) %>%
    step_rm(date)
  
wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(training(splits))

## ---- paged.print = FALSE-----------------------------------------------------
models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    wflw_fit_mars
)

models_tbl

## ---- paged.print = FALSE-----------------------------------------------------
calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl

## -----------------------------------------------------------------------------
calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive
    )

## -----------------------------------------------------------------------------
calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        .interactive = interactive
    )

## ---- paged.print = F, message=F----------------------------------------------
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = m750)

refit_tbl %>%
    modeltime_forecast(h = "3 years", actual_data = m750) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive
    )

