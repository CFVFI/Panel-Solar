library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)
library(glmnet)



View(Corriente)



A_2022 <- Corriente 

View(A_2022)

View(I1)
####Promedio####
###I1####
I1 <- Corriente %>%
  select(Fecha,I1) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I1$date <- as.Date(I1$date,format="%d/%m/%Y")

I1 %>% 
  plot_time_series(date,value, .interactive=TRUE)

splits <- I1 %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)
##Auto ARIMA####
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))

model_fit_arima

##Prophet####
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splits))

model_fit_prophet

### Machine Learning Models###
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))
###Prophet Boost####
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost



##Modeltime Table####
model_table <- modeltime_table(
   model_fit_arima, 
   model_fit_prophet,
  workflow_fit_glmnet,
   workflow_fit_prophet_boost
) 

model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table


calibration_table %>%
  modeltime_forecast(actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)



#####
h <- c("1 month","2 months","7 months","4 months","5 months",
       "6 months","7 months","8 months","9 months","10 months",
       "11 months","12 months")
calibration_table %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[1], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[1], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1 <- ojo1$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[2], actual_data = I1)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

  Pred2 <- ojo2$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[3], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3 <- ojo3$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[4], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4 <- ojo4$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[5], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5 <- ojo5$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[6], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6 <- ojo6$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[7], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7 <- ojo7$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[8], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8 <- ojo8$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[9], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9 <- ojo9$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[10], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10 <- ojo10$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[11], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11 <- ojo11$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1) %>%
  modeltime_forecast(h = h[12], actual_data = I1) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12 <- ojo12$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI1 <- rbind(Pred1,Pred2,Pred3,Pred4,Pred5,Pred6,
              Pred7,Pred8,Pred9,Pred10,Pred11,Pred12)

PredI1$Entrada <- 'I1'








###I2####
I2 <- Corriente %>%
  select(Fecha,I2) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I2$date <- as.Date(I2$date,format="%d/%m/%Y")

I2 %>% 
  plot_time_series(date,value, .interactive=TRUE)

splits <- I2 %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)



##Auto ARIMA####
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))

model_fit_arima

##Prophet####
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splits))

model_fit_prophet

### Machine Learning Models###
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))
###Prophet Boost####
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost



##Modeltime Table####
model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_prophet_boost
) 

model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table


calibration_table %>%
  modeltime_forecast(actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)



#####
h <- c("1 month","2 months","7 months","4 months","5 months",
       "6 months","7 months","8 months","9 months","10 months",
       "11 months","12 months")
calibration_table %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[1], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[1], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1 <- ojo1$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[2], actual_data = I2)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

Pred2 <- ojo2$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[3], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3 <- ojo3$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[4], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4 <- ojo4$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[5], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5 <- ojo5$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[6], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6 <- ojo6$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[7], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7 <- ojo7$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[8], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8 <- ojo8$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[9], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9 <- ojo9$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[10], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10 <- ojo10$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[11], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11 <- ojo11$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2) %>%
  modeltime_forecast(h = h[12], actual_data = I2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12 <- ojo12$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI2 <- rbind(Pred1,Pred2,Pred3,Pred4,Pred5,Pred6,
                Pred7,Pred8,Pred9,Pred10,Pred11,Pred12)

View(PredI2)

PredI2$Entrada <- 'I2'











###I3####
I3 <- Corriente %>%
  select(Fecha,I3) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I3$date <- as.Date(I3$date,format="%d/%m/%Y")

I3 %>% 
  plot_time_series(date,value, .interactive=TRUE)

splits <- I3 %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)



##Auto ARIMA####
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))

model_fit_arima

##Prophet####
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splits))

model_fit_prophet

### Machine Learning Models###
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))
###Prophet Boost####
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost



##Modeltime Table####
model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_prophet_boost
) 

model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table


calibration_table %>%
  modeltime_forecast(actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

#####
calibration_table %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[1], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[1], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1 <- ojo1$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[2], actual_data = I3)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

Pred2 <- ojo2$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[3], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3 <- ojo3$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[4], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4 <- ojo4$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[5], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5 <- ojo5$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[6], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6 <- ojo6$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[7], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7 <- ojo7$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[8], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8 <- ojo8$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[9], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9 <- ojo9$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[10], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10 <- ojo10$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[11], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11 <- ojo11$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12 <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3) %>%
  modeltime_forecast(h = h[12], actual_data = I3) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12 <- ojo12$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI3 <- rbind(Pred1,Pred2,Pred3,Pred4,Pred5,Pred6,
                Pred7,Pred8,Pred9,Pred10,Pred11,Pred12)

View(PredI3)

PredI3$Entrada <- 'I3'



#####

PredP <- rbind(PredI1,PredI2,PredI3)
PredP$Corriente <- 'Promedio'














####Maxima####
###I1MM####
I1M <- CorrienteM %>%
  select(Fecha,I1) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I1M$date <- as.Date(I1M$date,format="%d/%m/%Y")

I1M %>% 
  plot_time_series(date,value, .interactive=TRUE)

splitsM <- I1M %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splitsM %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)
##Auto ARIMA####
model_fit_arimaM <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splitsM))

model_fit_arimaM

##Prophet####
model_fit_prophetM <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splitsM))

model_fit_prophetM

### Machine Learning Models###
recipe_specM <- recipe(value ~ date, training(splitsM)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_specM %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnetM <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnetM <- workflow() %>%
  add_model(model_spec_glmnetM) %>%
  add_recipe(recipe_specM %>% step_rm(date)) %>%
  fit(training(splitsM))
###Prophet Boost####
model_spec_prophet_boostM <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boostM <- workflow() %>%
  add_model(model_spec_prophet_boostM) %>%
  add_recipe(recipe_specM) %>%
  fit(training(splitsM))

workflow_fit_prophet_boostM



##Modeltime Table####
model_tableM <- modeltime_table(
  model_fit_arimaM, 
  model_fit_prophetM,
  workflow_fit_glmnetM,
  workflow_fit_prophet_boostM
) 

model_tableM

calibration_tableM <- model_tableM %>%
  modeltime_calibrate(testing(splitsM))

calibration_tableM


calibration_tableM %>%
  modeltime_forecast(actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_tableM %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)



#####
calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[1], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1MM) %>%
  modeltime_forecast(h = h[1], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1M <- ojo1M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[2], actual_data = I1M)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

Pred2M <- ojo2M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[3], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3M <- ojo3M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[4], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4M <- ojo4M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[5], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5M <- ojo5M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[6], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6M <- ojo6M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[7], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7M <- ojo7M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[8], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8M <- ojo8M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[9], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9M <- ojo9M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[10], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10M <- ojo10M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[11], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11M <- ojo11M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I1M) %>%
  modeltime_forecast(h = h[12], actual_data = I1M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12M <- ojo12M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI1M <- rbind(Pred1M,Pred2M,Pred3M,Pred4M,Pred5M,Pred6M,
                Pred7M,Pred8M,Pred9M,Pred10M,Pred11M,Pred12M)

PredI1M$Entrada <- 'I1'













###I2M####
I2M <- CorrienteM %>%
  select(Fecha,I2) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I2M$date <- as.Date(I2M$date,format="%d/%m/%Y")

I2M %>% 
  plot_time_series(date,value, .interactive=TRUE)

splitsM <- I2M %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splitsM %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)



##Auto ARIMA####
model_fit_arimaM <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splitsM))

model_fit_arimaM

##Prophet####
model_fit_prophetM <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splitsM))

model_fit_prophetM

### Machine Learning Models###
recipe_specM <- recipe(value ~ date, training(splitsM)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_specM %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnetM <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnetM <- workflow() %>%
  add_model(model_spec_glmnetM) %>%
  add_recipe(recipe_specM %>% step_rm(date)) %>%
  fit(training(splitsM))
###Prophet Boost####
model_spec_prophet_boostM <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boostM <- workflow() %>%
  add_model(model_spec_prophet_boostM) %>%
  add_recipe(recipe_specM) %>%
  fit(training(splitsM))

workflow_fit_prophet_boostM



##Modeltime Table####
model_tableM <- modeltime_table(
  model_fit_arimaM, 
  model_fit_prophetM,
  workflow_fit_glmnetM,
  workflow_fit_prophet_boostM
) 

model_tableM

calibration_tableM <- model_tableM %>%
  modeltime_calibrate(testing(splitsM))

calibration_tableM


calibration_tableM %>%
  modeltime_forecast(actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_tableM %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)



#####

calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[1], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[1], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1M <- ojo1M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[2], actual_data = I2M)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

Pred2M <- ojo2M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[3], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3M <- ojo3M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[4], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4M <- ojo4M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[5], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5M <- ojo5M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[6], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6M <- ojo6M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[7], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7M <- ojo7M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[8], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8M <- ojo8M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[9], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9M <- ojo9M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[10], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10M <- ojo10M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[11], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11M <- ojo11M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I2M) %>%
  modeltime_forecast(h = h[12], actual_data = I2M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12M <- ojo12M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI2M <- rbind(Pred1M,Pred2M,Pred3M,Pred4M,Pred5M,Pred6M,
                Pred7M,Pred8M,Pred9M,Pred10M,Pred11M,Pred12M)

View(PredI2M)

PredI2M$Entrada <- 'I2'


















###I3M####
I3M <- CorrienteM %>%
  select(Fecha,I3) %>% 
  filter(Fecha>='2022-05-12' & Fecha<='2022-09-12') %>% 
  set_names(c("date", "value"))
I3M$date <- as.Date(I3M$date,format="%d/%m/%Y")

I3M %>% 
  plot_time_series(date,value, .interactive=TRUE)

splitsM <- I3M %>%
  time_series_split(assess = "2 months",cumulative = TRUE)


splitsM %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)



##Auto ARIMA####
model_fit_arimaM<- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splitsM))

model_fit_arimaM

##Prophet####
model_fit_prophetM <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splitsM))

model_fit_prophetM

### Machine Learning Models###
recipe_specM <- recipe(value ~ date, training(splitsM)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 765, K = 5) %>%
  step_dummy(all_nominal())

recipe_specM %>% prep() %>% juice()
##Elastic Net####
model_spec_glmnetM <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnetM <- workflow() %>%
  add_model(model_spec_glmnetM) %>%
  add_recipe(recipe_specM %>% step_rm(date)) %>%
  fit(training(splitsM))
###Prophet Boost####
model_spec_prophet_boostM <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boostM <- workflow() %>%
  add_model(model_spec_prophet_boostM) %>%
  add_recipe(recipe_specM) %>%
  fit(training(splitsM))

workflow_fit_prophet_boostM



##Modeltime Table####
model_tableM <- modeltime_table(
  model_fit_arimaM, 
  model_fit_prophetM,
  workflow_fit_glmnetM,
  workflow_fit_prophet_boostM
) 

model_tableM

calibration_tableM <- model_tableM %>%
  modeltime_calibrate(testing(splitsM))

calibration_tableM


calibration_tableM %>%
  modeltime_forecast(actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)








#####

calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[1], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


ojo1M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[1], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)


Pred1M <- ojo1M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="1 month")



ojo2M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[2], actual_data = I3M)%>%
  plot_modeltime_forecast(.interactive = FALSE) 

Pred2M <- ojo2M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="2 months")

ojo3M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[3], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred3M <- ojo3M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="3 months")



ojo4M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[4], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred4M <- ojo4M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="4 months")


ojo5M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[5], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred5M <- ojo5M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="5 months")


ojo6M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[6], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred6M <- ojo6M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="6 months")


ojo7M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[7], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred7M <- ojo7M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="7 months")


ojo8M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[8], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred8M <- ojo8M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="8 months")


ojo9M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[9], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred9M <- ojo9M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="9 months")


ojo10M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[10], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred10M <- ojo10M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="10 months")


ojo11M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[11], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred11M <- ojo11M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="11 months")


ojo12M <- calibration_tableM %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id ==3 ) %>%
  # Refit and Forecast Forward
  modeltime_refit(I3M) %>%
  modeltime_forecast(h = h[12], actual_data = I3M) %>%
  plot_modeltime_forecast(.interactive = FALSE)
Pred12M <- ojo12M$data %>% 
  select(.index,.value,.conf_lo,.conf_hi) %>% 
  mutate(Month="12 months")


PredI3M <- rbind(Pred1M,Pred2M,Pred3M,Pred4M,Pred5M,Pred6M,
                Pred7M,Pred8M,Pred9M,Pred10M,Pred11M,Pred12M)
PredI3M$Entrada <- 'I3'

#View(PredI3M)















PredP <- rbind(PredI3M,PredI2,PredI3)
Pred$Corriente <- 'Promedio'

#####
PredM <- rbind(PredI1M,PredI2M,PredI3M)
PredM$Corriente <- 'Máxima'

#####
View(PredP)
View(PredM)


