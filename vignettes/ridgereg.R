## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(bonuslab)
library(caret)
library(MASS)
library(leaps)
set.seed(1234)

## ----step2, message=FALSE-----------------------------------------------------
df <- Boston
index <- createDataPartition(df$crim, times = 1, p = 0.8, list = FALSE)
train <- df[index,]
test <- df[-index,]

## ----step3a, message=FALSE----------------------------------------------------
lm_mod <- train(medv ~ ., data = train, method = "lm")

## ----step3b, message=FALSE----------------------------------------------------
forward_mod <- train(medv ~ ., data = train, method = "leapForward")

## ----step3c, message=FALSE----------------------------------------------------
rdg_reg <- list(type = "Regression",
                library = "bonuslab",
                loop = NULL)

rdg_reg$parameters <- data.frame(parameter = c("lambda"),
                      class = c("numeric"),
                      label = c("Lambda"))

rdg_reg$grid <- function(x, y, len = NULL, search = "grid") {
  out <- expand.grid(lambda = seq(from = 0, to = 1, by = 0.1))
  out
}

rdg_reg$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  df <- as.data.frame(cbind(x, y))
  mod_formula <- as.formula(paste0("y~", paste(colnames(df)[1:(ncol(df)-1)], collapse = "+")))
  mod <- ridgereg(data = df, formula = mod_formula, lambda = param$lambda)
}

rdg_reg$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  modelFit$predict(modelFit, newdata)
}

rdg_reg$prob <- list(NULL)

ridge_mod <- train(medv ~ ., data = train, 
                   method = rdg_reg,
                   trControl = trainControl(method = "repeatedcv", repeats = 3))

## ----step4--------------------------------------------------------------------
lm_pred <- predict(lm_mod, test)
forward_pred <- predict(forward_mod, test)
ridge_pred <- predict(ridge_mod, test)

RMSE(lm_pred, test$medv)
RMSE(forward_pred, test$medv)
RMSE(ridge_pred, test$medv)

## ----step2_1, message=FALSE---------------------------------------------------
library(nycflights13)
library(tidyr)
library(caret)
library(bonuslab)

# Loading and merging the data sets flights and weather
df_flights <- nycflights13::flights
df_weather <- nycflights13::weather

## ----step2_2, message=FALSE---------------------------------------------------
df <- merge(df_flights, df_weather, 
            by = c("time_hour", "origin", "day", "hour", "month", "year"), 
            all.x = TRUE)

df <- df[,-c(1,2,6,10,13,14,15,16,17,19,25)]
df <- df[complete.cases(df),]

df$wind_dir_t_wind_speed <- df$wind_dir * df$wind_speed
df$temp_t_humid <- df$temp * df$humid
df$wind_speed_t_humid <- df$wind_speed * df$humid

## ----step2_3, message=FALSE---------------------------------------------------
ind1 <- createDataPartition(df$day, p = 0.05, list = FALSE)
test <- df[ind1,]
train <- df[-ind1,]
ind2 <- createDataPartition(train$day, p = (80/95), list = TRUE)

## ----step2_4, message=FALSE---------------------------------------------------
ridge_mod <- train(arr_delay ~ day + hour + month + dep_time + sched_dep_time + dep_delay + sched_arr_time + distance +
                     temp + dewp + humid + wind_dir + wind_speed + precip + pressure + visib +
                     wind_dir_t_wind_speed + temp_t_humid + wind_speed_t_humid, 
                   data = train, 
                   method = rdg_reg,
                   trControl = trainControl(method = "repeatedcv", repeats = 1,
                                            index = ind2,
                                            indexOut = list(c(1:nrow(train))[-unlist(ind2)]) ))

ridge_mod$bestTune
ridge_mod$results[,1:2]

## ----step2_5, message=FALSE---------------------------------------------------
ridge_pred <- predict(ridge_mod, test)
RMSE(ridge_pred, test$arr_delay)

