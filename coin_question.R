# Nathan's Coin Question

# Two different biased coins, one normal and the other with a hidden third side

# Case 1:
case_1 = function(flips, p_bias){
  results = c(rep(-100, flips))
  flip_probs = runif(flips)
  for(i in 1:flips){
    if(flip_probs[i] < 0.5 + p_bias){
      results[i] = rnorm(1, mean = 0, sd = 1)
    } else {
      results[i] = rnorm(1, mean = 1, sd = 1)
    }
  }

  return(results)
}

# Case 2:
case_2 = function(flips, p_bias, p_third_side){
  results = c(rep(-100, flips))
  flip_probs = runif(flips)
  for(i in 1:flips){
    if(flip_probs[i] < 0.5 - (p_third_side / 2) + p_bias){
      # heads
      results[i] = rnorm(1, mean = 0, sd = 1)
    } else if (flip_probs[i] > 0.5 + (p_third_side / 2) - p_bias){
      # tails
      results[i] = rnorm(1, mean = 1, sd = 1)
    } else {
      # side
      results[i] = rnorm(1, mean = 0.5, sd = 1)
    }

  }
  return(results)
}
flips = 5000
coinA = case_1(flips, 0.01)
coinB = case_2(flips, 0.01, 0.02)
coinC = case_2(flips, 0.05, 0.02)
coinD = case_1(flips, 0.05)
?data.frame
coin_df = data.frame(coinA, coinB, coinC, coinD)
summary(coin_df)
write.csv(coin_df, "coin_question.csv")
