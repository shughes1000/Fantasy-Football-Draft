library(dplyr)
library(ggplot2)
library(caret)

twenty_all = read.csv("twenty_all.csv")
twenty_all = twenty_all[ , -1]
# a 12 team draft with 16 players on a team goes to 192 picks, so let's remove all data points above ADP 200
twenty_all = twenty_all[twenty_all$ADP_average <= 200, ]

rb = twenty_all[twenty_all$Position == "RB", ]

ggplot(data = rb,
       mapping = aes(x = ADP_average, y = points_average))+
  geom_point()+
  geom_smooth()

# find best spline
best_MAE = 1000
best_iter = 1
best_sp = NULL
best_sp_predict = NULL

for(j in 1:100) {
  set.seed(j)
  split = createDataPartition(rb$points_average, p = 0.8, list = FALSE)
  train = rb[split, ]
  test = rb[-split, ]
  for(i in 2:15) {
    sp = smooth.spline(x = train$ADP_average, y = train$points_average, df = i)
    test_sp = predict(sp, test$ADP_average)$y
    test_error_sp = test_sp - test$points_average
    new = mean(abs(test_error_sp))
    der = predict(sp, test$ADP_average, deriv = TRUE)$y
    if(max(der) <= 0) {
      if(new < best_MAE) {
        best_seed = j
        best_sp = sp
        best_sp_predict = test_sp
        best_MAE = new
        best_iter = i
      }
    }
  }
}

set.seed(best_seed)
split = createDataPartition(rb$points_average, p = 0.8, list = FALSE)
train = rb[split, ]
test = rb[-split, ]
best_sp
best_MAE
ggplot(data = test,
       mapping = aes(x = ADP_average, y = points_average))+
  geom_point()+
  geom_line(aes(y = best_sp_predict), size = 3, col = "blue", alpha = 0.5)


rb_predict = predict(best_sp, c(1:200))$y

rb_deriv = predict(best_sp, c(1:200), deriv = TRUE)$y