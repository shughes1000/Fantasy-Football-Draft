library(tidyverse)


df = read.csv("spline_predictions.csv")

user_input = function() {
  valid_input = FALSE
  
  while(valid_input == FALSE) {
    now = as.integer(readline("When is your closest upcoming pick? "))
    later = as.integer(readline("When is your second closest upcoming pick? "))
    
    # Possible errors
    if(now >= later) {
      message("Your second closest pick should be a higher number than your closest pick. Try again.")
    } else if (later > 200) {
      message("Sorry, your picks have to be lower than pick 201. Try again.")
    } else if(now <= 0) {
      message("This is impossible. Try again.")
    } else {
      valid_input = TRUE
      return(c(now, later))
    }
  }
}

master = function(now, later) {
  qb_now = df[now, 2]
  qb_later = df[later, 2]
  qb_difference = qb_now - qb_later
  
  rb_now = df[now, 3]
  rb_later = df[later, 3]
  rb_difference = rb_now - rb_later
  
  wr_now = df[now, 4]
  wr_later = df[later, 4]
  wr_difference = wr_now - wr_later
  
  te_now = df[now, 5]
  te_later = df[later, 5]
  te_difference = te_now - te_later
  
  Diff = c(qb_difference, rb_difference, wr_difference, te_difference)
  Now = c(qb_now, rb_now, wr_now, te_now)
  Later = c(qb_later, rb_later, wr_later, te_later)
  
  mf = data.frame(Diff, Now, Later, row.names = c("QB", "RB", "WR", "TE"))
  mf = mf %>% arrange(desc(Diff)) %>% mutate(across(where(is.numeric), round, digits = 2))
  cat("\n")
  print(mf)
  cat("\nNow: Estimated points per game at pick ", now, "\n", sep = "")
  cat("Later: Estimated points per game at pick ", later, "\n", sep = "")
  cat("Diff: Difference in estimated points per game\n", sep = "")
}

run = function() {
  get = user_input()
  master(get[1], get[2])
}

run()