y_1 = -16.6
y_2 = -14.7
y_3 = 6.3
y_4 = 8.4

pi = function(theta) {
  (1/(1+(y_1-theta)^2))*(1/(1+(y_2-theta)^2))*(1/(1+(y_3-theta)^2))*(1/(1+(y_4-theta)^2))
}

q = function(theta) {
  rnorm(1, theta, 1)
}

theta_MH_a = array (0,5000)
theta_MH_a[1] = -20
for (t in 2:5001) {
  theta_star = q(theta_MH_a[t-1])
  u = runif(1)
  if (u < min(1, (pi(theta_star))/(pi(theta_MH_a[t-1])))) {
    theta_MH_a[t] = theta_star
  } else {
    theta_MH_a[t] = theta_MH_a[t-1]
  }
}

theta_MH_b = array (0,5000)
theta_MH_b[1] = 0
for (t in 2:5001) {
  theta_star = q(theta_MH_b[t-1])
  u = runif(1)
  if (u < min(1, (pi(theta_star))/(pi(theta_MH_b[t-1])))) {
    theta_MH_b[t] = theta_star
  } else {
    theta_MH_b[t] = theta_MH_b[t-1]
  }
}

library(ggplot2)

df_MH = data.frame(seq(0,5000,1), theta_MH_a, theta_MH_b)
ggplot(df_MH,aes(seq(0,5000,1))) + geom_line(aes(y = theta_MH_a), color = "blue") + geom_line(aes(y = theta_MH_b), color = "red") + labs(x= "iteration") + labs(y= "theta")


G_s = function(theta) { 
  pi(theta) >= s 
}
H_s = function(theta) { 
  (theta >= (y_1 - sqrt((1-s^(1/4))/s^(1/4)))) & (theta <= (y_4 + sqrt((1-s^(1/4))/s^(1/4)))) 
}

rejection_sampler = function(G_s) {
  accept = 0
  while (accept == 0) {
    theta = runif(1, y_1-sqrt((1-s^(1/4))/s^(1/4)), y_4+sqrt((1-s^(1/4))/s^(1/4)))
    if (G_s(theta)) {
      accept = 1
    }
  }
  return(theta)
}

theta_SS_a = array (0,5000)
theta_SS_a[1] = -20
for (t in 2:5001) {
  s = runif(1, 0, pi(theta_SS_a[t-1]))
  G_s = function(theta) { 
    pi(theta) >= s 
  }
  theta_SS_a[t] = rejection_sampler(G_s)
}


theta_SS_b = array (0,5000)
theta_SS_b[1] = 0
for (t in 2:5001) {
  s = runif(1, 0, pi(theta_SS_b[t-1]))
  G_s = function(theta) { 
    pi(theta) >= s 
  }
  theta_SS_b[t] = rejection_sampler(G_s)
}

df_SS = data.frame(seq(0,5000,1), theta_SS_a, theta_SS_b)
ggplot(df_SS,aes(seq(0,5000,1))) + geom_line(aes(y = theta_SS_a), color = "blue")  + geom_line(aes(y = theta_SS_b), color = "red") + labs(x= "iteration") + labs(y= "theta")

num_larger_than_8 = sum(df_SS[-1, 2:3] > 8)
num_less_than_minus_15 = sum(df_SS[-1, 2:3] < -15)
total_values = sum(lengths(df_SS[-1, 2:3]))
prob_8 = num_larger_than_8/total_values
print("Estimate of probability that theta is more than 8 is:")
print(prob_8)
prob_minus_15 = num_less_than_minus_15/total_values
print("Estimate of probability that theta is less than -15 is:")
print(prob_minus_15)
