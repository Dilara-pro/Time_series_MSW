library(openxlsx)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(greyforecasting)
library(Metrics)
library(readxl)

time_series_municipal_waste <- read.xlsx("C:/Users/Huawei/Masaüstü/Waste_forecast/time_series_municipal_waste.xlsx", colNames = TRUE)
ts_province <- read.xlsx("C:/Users/Huawei/Masaüstü/Waste_forecast/ts_waste_province.xlsx")

# EDA
# remove unnecessary rows
time_series_municipal_waste <- time_series_municipal_waste[-c(2:6,10,14:43), ]
time_series_municipal_waste <- rename(time_series_municipal_waste, "Waste/Year" = "X1")
# rename rows
time_series_municipal_waste[1,1] <- "Turkey population"
time_series_municipal_waste[2,1] <- "Amount of municipal waste generated (Thousand tonnes/year)"
time_series_municipal_waste[3,1] <- "Amount of municipal waste collected (Thousand tonnes/year)"
time_series_municipal_waste[4,1] <- "Average amount of municipal waste per capita (Kg/capita-day)"
time_series_municipal_waste[5,1] <- "Waste treatment facilities"
time_series_municipal_waste[6,1] <- "Municipality's dumping sites"
time_series_municipal_waste[7,1] <- "Other disposal methods"
# reorder row names that is disordered
row.names(time_series_municipal_waste) <- NULL
# adjust necessary columns as numbers
time_series_municipal_waste <- time_series_municipal_waste %>%
  mutate(across(-`Waste/Year`, ~as.numeric(as.character(.))))
sapply(time_series_municipal_waste,class)

str(time_series_municipal_waste)

ts_province  <- select(ts_province , -BÖLGE.KODU)  
ts_province  <- rename(ts_province , "Year" = YIL)
ts_province  <- rename(ts_province , "Province" = BÖLGE.ADI)
ts_province  <- rename(ts_province , "Waste amount (1000 ton)" = `Belediye.atık.istatistikleri.:.Toplanan.atık.miktarı.(1000.ton)`)
ts_province$Year <- as.numeric(ts_province$Year)
ts_province$`Waste amount (1000 ton)` <- as.numeric(ts_province$`Waste amount (1000 ton)`)
sapply(ts_province,class)

str(ts_province)

ts_data <- time_series_municipal_waste %>%
  pivot_longer(cols = -`Waste/Year`, names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year))  

# Türkiye population ts
ggplot(ts_data %>% filter(`Waste/Year` == "Turkey population"), 
            aes(x = Year, y = Value / 10^6)) +
  geom_line(color = "blue") +
  geom_point(color = "magenta") +
  theme_bw() + coord_fixed() +
  labs(title = "Turkiye Population Over the Years", x = "Year", y = "Population (million)")

# Waste graphs
ts_data2 <- ts_data %>%
  filter(`Waste/Year` %in% c("Amount of municipal waste generated (Thousand tonnes/year)", 
                             "Amount of municipal waste collected (Thousand tonnes/year)")) %>%
  mutate(Type = case_when(
    `Waste/Year` == "Amount of municipal waste generated (Thousand tonnes/year)" ~ "Generated",
    `Waste/Year` == "Amount of municipal waste collected (Thousand tonnes/year)" ~ "Collected"
  ))

ggplot(ts_data2, aes(x = Year, y = Value, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Generated" = "green", "Collected" = "purple")) +
  labs(title = "Municipal Waste Generated and Collected Over the Years",
       x = "Year", 
       y = "Waste (Thousand tonnes)",
       color = "Type") + 
  theme_minimal()

disposal_methods_data <- ts_data %>%
  filter(`Waste/Year` %in% c("Waste treatment facilities", 
                             "Municipality's dumping sites", 
                             "Other disposal methods")) %>%
  mutate(Type = case_when(
    `Waste/Year` == "Waste treatment facilities" ~ "Waste Treatment",
    `Waste/Year` == "Municipality's dumping sites" ~ "Dumping Sites",
    `Waste/Year` == "Other disposal methods" ~ "Other Methods"
  ))

ggplot(disposal_methods_data, aes(x = Year, y = Value, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Waste Treatment" = "blue", "Dumping Sites" = "red", "Other Methods" = "green")) +
  labs(title = "Disposal Methods Over the Years",
       x = "Year", 
       y = "Amount (Thousand tonnes)",
       color = "Method") +
  theme_classic()


summary_stats <- ts_province %>%
  group_by(Province) %>%
  summarise(
    mean_waste = mean(`Waste amount (1000 ton)`, na.rm = TRUE),
    median_waste = median(`Waste amount (1000 ton)`, na.rm = TRUE),
    sd_waste = sd(`Waste amount (1000 ton)`, na.rm = TRUE),
    min_waste = min(`Waste amount (1000 ton)`, na.rm = TRUE),
    max_waste = max(`Waste amount (1000 ton)`, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_waste)) %>%
  top_n(10, mean_waste)
print(summary_stats)


mean_waste <- ts_province %>%
  group_by(Province) %>%
  summarise(mean_waste = mean(`Waste amount (1000 ton)`, na.rm = TRUE)) %>%
  arrange(desc(mean_waste))

top5_provinces <- mean_waste %>%
  top_n(5, mean_waste) %>%
  pull(Province)

top5_data <- ts_province %>%
  filter(Province %in% top5_provinces)

ggplot(top5_data, aes(x = Year, y = `Waste amount (1000 ton)`, color = Province)) +
  geom_line() +
  geom_point() +
  labs(title = "Waste Amount in Top 5 Provinces with Largest Mean Waste Amount",
       x = "Year",
       y = "Waste Amount (1000 ton)") +
  theme_pander()


top10_summary <- ts_province %>%
  group_by(Province) %>%
  summarise(mean_waste = mean(`Waste amount (1000 ton)`, na.rm = TRUE)) %>%
  arrange(desc(mean_waste)) %>%
  top_n(10, mean_waste)

top10_provinces <- top10_summary$Province
top10_data <- ts_province %>%
  filter(Province %in% top10_provinces)

top10_data <- top10_data %>%
  mutate(Province = factor(Province, levels = top10_summary$Province[order(top10_summary$mean_waste)]))

ggplot(top10_data, aes(x = Province, y = `Waste amount (1000 ton)`, fill = Province)) +
  geom_boxplot() +
  labs(title = "Waste Amounts by Province (Top 10 Provinces)",
       x = "Province",
       y = "Waste Amount (1000 ton)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ts_province_no_istanbul <- ts_province %>% filter(Province != "İstanbul")
top10_summary_no_istanbul <- ts_province_no_istanbul %>%
  group_by(Province) %>%
  summarise(mean_waste = mean(`Waste amount (1000 ton)`, na.rm = TRUE)) %>%
  arrange(desc(mean_waste)) %>%
  top_n(10, mean_waste)

top10_provinces_no_istanbul <- top10_summary_no_istanbul$Province
top10_data_no_istanbul <- ts_province_no_istanbul %>%
  filter(Province %in% top10_provinces_no_istanbul)

top10_data_no_istanbul <- top10_data_no_istanbul %>%
  mutate(Province = factor(Province, levels = top10_summary_no_istanbul$Province[order(top10_summary_no_istanbul$mean_waste)]))

ggplot(top10_data_no_istanbul, aes(x = Province, y = `Waste amount (1000 ton)`, fill = Province)) +
  geom_boxplot() +
  labs(title = "Waste Amounts by Province (Top 10 Provinces, Excluding İstanbul)",
       x = "Province",
       y = "Waste Amount (1000 ton)") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Grey Prediction- Total waste
waste_collected <- as.numeric(time_series_municipal_waste[3, -1]) 

# AutoML 
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  n <- length(data)
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

# Models to evaluate
models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

# Applying models and calculate accuracy
results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, waste_collected, forecast_steps = 5)
  accuracy <- rmse(waste_collected[(length(waste_collected)-4):length(waste_collected)], result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- as.numeric(substr(names(time_series_municipal_waste)[-1], 1, 4))  
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))
plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(waste_collected, best_model$forecast),
  Type = c(rep("Actual", length(waste_collected)), rep("Forecast", length(best_model$forecast)))
)

fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)
plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()




# Grey Prediction- waste per capita
waste_collected <- as.numeric(time_series_municipal_waste[4, -1]) 
# AutoML 
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  n <- length(data)
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

# Models to evaluate
models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

# Applying models and calculate accuracy
results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, waste_collected, forecast_steps = 5)
  accuracy <- rmse(waste_collected[(length(waste_collected)-4):length(waste_collected)], result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")

cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- as.numeric(substr(names(time_series_municipal_waste)[-1], 1, 4))  
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))
plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(waste_collected, best_model$forecast),
  Type = c(rep("Actual", length(waste_collected)), rep("Forecast", length(best_model$forecast)))
)

fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)
plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()


ankara <- ts_province %>% filter(Province == "Ankara") %>% arrange(Year)
ankara_waste <- ankara$`Waste amount (1000 ton)`

results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, ankara_waste, forecast_steps = 5)
  accuracy <- rmse(tail(ankara_waste, min(5, length(ankara_waste))), result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)


# Prepare data for plotting
years <- ankara$Year
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))

plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(ankara_waste, best_model$forecast),
  Type = c(rep("Actual", length(ankara_waste)), rep("Forecast", length(best_model$forecast)))
)
fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)

plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast for Ankara",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()


istanbul <- ts_province %>% filter(Province == "İstanbul") %>% arrange(Year)
istanbul_waste <- istanbul$`Waste amount (1000 ton)`

# AutoML greyforecasting function
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, istanbul_waste, forecast_steps = 5)
  accuracy <- rmse(tail(istanbul_waste, min(5, length(istanbul_waste))), result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- istanbul$Year
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))

plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(istanbul_waste, best_model$forecast),
  Type = c(rep("Actual", length(istanbul_waste)), rep("Forecast", length(best_model$forecast)))
)
fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)

plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast for İstanbul",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()



izmir <- ts_province %>% filter(Province == "İzmir") %>% arrange(Year)
izmir_waste <- izmir$`Waste amount (1000 ton)`

# AutoML greyforecasting function
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, izmir_waste, forecast_steps = 5)
  accuracy <- rmse(tail(izmir_waste, min(5, length(izmir_waste))), result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- izmir$Year
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))

plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(izmir_waste, best_model$forecast),
  Type = c(rep("Actual", length(izmir_waste)), rep("Forecast", length(best_model$forecast)))
)
fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)

plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast for izmir",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()




antalya <- ts_province %>% filter(Province == "Antalya") %>% arrange(Year)
antalya_waste <- antalya$`Waste amount (1000 ton)`

# AutoML greyforecasting function
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, antalya_waste, forecast_steps = 5)
  accuracy <- rmse(tail(antalya_waste, min(5, length(antalya_waste))), result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- antalya$Year
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))

plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(antalya_waste, best_model$forecast),
  Type = c(rep("Actual", length(antalya_waste)), rep("Forecast", length(best_model$forecast)))
)
fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)

plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast for Antalya",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()


bursa <- ts_province %>% filter(Province == "Bursa") %>% arrange(Year)
bursa_waste <- bursa$`Waste amount (1000 ton)`

# AutoML greyforecasting function
fit_and_forecast <- function(model_func, data, forecast_steps = 5) {
  model <- model_func(data)
  fitted_values <- model$fitted
  forecast_values <- numeric(forecast_steps)
  
  for (i in 1:forecast_steps) {
    extended_data <- c(data, forecast_values[1:(i-1)])
    forecast_model <- model_func(extended_data)
    forecast_values[i] <- tail(forecast_model$fitted, 1)
  }
  
  list(model = model, fitted = fitted_values, forecast = forecast_values)
}

models <- list(
  gm = gm,
  gm_1 = gm_1,
  gm_2 = gm_2,
  dgm = dgm,
  verhulst = verhulst,
  pgm = pgm
)

results <- lapply(models, function(model_func) {
  result <- fit_and_forecast(model_func, bursa_waste, forecast_steps = 5)
  accuracy <- rmse(tail(bursa_waste, min(5, length(bursa_waste))), result$forecast)
  list(model = result$model, fitted = result$fitted, forecast = result$forecast, accuracy = accuracy)
})

best_model_index <- which.min(sapply(results, function(x) x$accuracy))
best_model_name <- names(results)[best_model_index]
best_model <- results[[best_model_index]]

cat("The best model is:", best_model_name, "\n")
cat("Best model RMSE:", best_model$accuracy, "\n")
print(best_model$forecast)

years <- bursa$Year
forecast_years <- (max(years) + 1):(max(years) + length(best_model$forecast))

plot_data <- data.frame(
  Year = c(years, forecast_years),
  Value = c(bursa_waste, best_model$forecast),
  Type = c(rep("Actual", length(bursa_waste)), rep("Forecast", length(best_model$forecast)))
)
fitted_data <- data.frame(
  Year = years,
  Value = best_model$fitted,
  Type = "Fitted"
)

plot_data <- rbind(plot_data, fitted_data)

ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Grey Model Fitting and Forecast for Bursa",
       x = "Year",
       y = "Amount of Municipal Waste Collected (Thousand tonnes/year)") +
  scale_color_manual(values = c("Actual" = "green", "Fitted" = "red", "Forecast" = "blue")) +
  theme_classic()

