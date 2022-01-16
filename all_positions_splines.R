library(tidyverse)

# make sure other files are loaded before running this
ADP_average = c(1:200)
df = data.frame(ADP_average, qb_predict, rb_predict, wr_predict, te_predict, qb_deriv, rb_deriv, wr_deriv, te_deriv)

# This part is unnecessary because predict has a built in derivative parameter
# df = df %>% mutate(qb_deriv = qb_predict - lag(qb_predict, default = first(qb_predict)))
# df = df %>% mutate(rb_deriv = rb_predict - lag(rb_predict, default = first(rb_predict)))
# df = df %>% mutate(wr_deriv = wr_predict - lag(wr_predict, default = first(wr_predict)))
# df = df %>% mutate(te_deriv = te_predict - lag(te_predict, default = first(te_predict)))



ggplot(data = df,
       mapping = aes(x = ADP_average))+
  geom_line(aes(y = qb_predict, col = "QB"), size = 3)+
  geom_line(aes(y = rb_predict, col = "RB"), size = 3)+
  geom_line(aes(y = wr_predict, col = "WR"), size = 3)+
  geom_line(aes(y = te_predict, col = "TE"), size = 3)+
  labs(title = "Expected Points Per Game by Average Draft Position",
      y = "Expected Points Per Game",
      x = "Average Draft Position",
      col = "Position")

ggplot(data = df,
       mapping = aes(x = ADP_average))+
  geom_line(aes(y = qb_deriv, col = "QB"), size = 3)+
  geom_line(aes(y = rb_deriv, col = "RB"), size = 3)+
  geom_line(aes(y = wr_deriv, col = "WR"), size = 3)+
  geom_line(aes(y = te_deriv, col = "TE"), size = 3)+
  labs(title = "Derivative of Expected Points Per Game by Average Draft Position",
       y = "Expected Points Per Game Derivative",
       x = "Average Draft Position",
       col = "Position")

write.csv(df, "spline_predictions.csv", row.names = FALSE)
