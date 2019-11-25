

plot_alpha_beta(1.95, 2.1, .2) 

t.test(iris$Sepal.Length)
conf_int(iris$Sepal.Length)


plot_alpha_beta = function(
  x1, x2 = NULL, 
  alternative = c("two.sided", "less", "greater"),
  mu = 0, 
  alpha = 0.05,
  xlim_adj = 3.0,
  n = 500
)
{
  # for testing
  {
    x1 = subset(iris, Species == "setosa")$Sepal.Width
    x2 = subset(iris, Species == "virginica")$Sepal.Width
    
    x2 = subset(iris, Species == "setosa")$Sepal.Width
    x1 = subset(iris, Species == "virginica")$Sepal.Width
    
    mu = 0
    alpha = 0.05
    alternative = c("two.sided", "less", "greater")
    alternative = "less"
    alternative = "greater"
    alternative = "t"
    xlim_adj = 3.0
    n = 500
  }
  
  alt = substr(alternative, 1, 1)
  
  alpha_crit = ifelse(
    alt == "t",
    alpha / 2.0,
    alpha)
  t2 = ifelse(
    is.null(x2),
    list(t.test(x1, mu = mu, conf.level = 1.0 - alpha, alternative = alt)),
    list(t.test(x1, x2, conf.level = 1.0 - alpha, alternative = alt))
    # list(t.test(signif(x1 - mean(x1), 6), signif(x2 - mean(x1), 6), conf.level = 1.0 - alpha, alternative = alternative))
  )[[1]]
  st_err = t2$stderr
  st_err_adj = abs(qnorm(alpha_crit) * st_err)
  deg_fr = t2$parameter
  true_diff = -diff(t2$estimate)
  critical_diff = abs(qt(alpha_crit, df = deg_fr) * st_err)

  pnorm(alpha_crit) * st_err_adj
  pnorm(alpha_crit, sd = st_err_adj)
  qnorm(alpha_crit, sd = st_err_adj)
  
  qt(alpha_crit, df = deg_fr)
  
  pnorm(critical_diff, sd = st_err_adj)
  pnorm(critical_diff, sd = st_err)
  
  
  critical_diff
  
  alpha_crit
  
  
  qnorm(alpha_crit, sd = st_err)
  pnorm(critical_diff, sd = st_err)  
  pnorm(alpha_crit, sd = st_err)
  
  
  
  
  # two-tailed confidence interval  
  conf_int = c(true_diff - critical_diff, true_diff + critical_diff)
  
  if (alt == "l")
    conf_int = c(-Inf, conf_int[2])
  if (alt == "g")
    conf_int = c(conf_int[1], Inf)
  
  t2$estimate
  
  xlim = sort(t2$estimate)
  xlim = sort(c(0, true_diff))
  # xlim = sort(-t2$estimate)
  # xlim = t2$estimate
  xlim - xlim[1]
  xlim_2 = c(xlim[1] - xlim_adj * st_err, xlim[2] + xlim_adj * st_err)
  
  x = seq(xlim_2[1], xlim_2[2], length.out = n)
  
  y1 = dnorm(x, mean = t2$estimate[1], sd = st_err)
  y2 = dnorm(x, mean = t2$estimate[2], sd = st_err)
  
  y1 = dnorm(x, mean = 0, sd = st_err)
  y2 = dnorm(x, mean = true_diff, sd = st_err)
  
  
  
  # if two-tailed, the alpha region is in both tails
  if (alt == "t")
  {
    
  }
  
  pnorm(critical_diff, mean = 0, sd = st_err)
  
  x_alpha = c(critical_x, x[x > critical_x], xmax) 
  y_alpha = dnorm(x_alpha, mean = H0, sd = critical_sd) 
  
  x_beta  = c(critical_x, x[x < critical_x], xmin) 
  y_beta  = dnorm(x_beta, mean = H1, sd = critical_sd) 
  
  
  
  plot(x, dnorm(x, mean = 0, sd = st_err), type = "n")
  # plot(x, dnorm(x, mean = t2$estimate[1], sd = st_err), type = "n")
  
  points(x, y1, type = "l")
  points(x, y2, type = "l")
  
  
  
  
  
  
  
  # choose a reasonable range for xlim
  
  
  
  
  true_diff - t2$conf.int[1]
  abs(qt(alpha_crit, df = t2$parameter) * st_err)
  pnorm(0.1397192, 0, sd = st_err)
  pt(0.1397192, df = 95)
  pnorm(critical_diff, 0, sd = st_err)
  
  true_diff - t2$conf.int[1]
  
  str(t2)
  mean(x1); mean(x2)
  
  mean(x2) - mean(x1)
  


  
  t2
  str(t2)
  (0.5 * (t2$conf.int[2] - t2$conf.int[1]))
  
    
  critical_z = abs(qnorm(1 - alpha_crit)) 
  critical_x = H0 + critical_diff 
  critical_sd = critical_diff / critical_z 
  
  
  
  str(t1)
  str(t2)
  
  t1
  t2
  
  t1$stderr
  t2$stderr
  
  se1 = t.test(x1)$stderr
  se2 = t.test(x2)$stderr
  
  
  (sd(x1) + sd(x2)) / sqrt(length(c(x1, x2)))
  
  c(se1, se2)  
  
}

t.test()

plot_alpha_beta = function( 
  H0, H1,  
  critical_diff, alpha = 0.05,  
  n = 1000,  
  hypothesis = "upper",  
  alpha_col = 2, beta_col = 3,  
  xmin = NA, xmax = NA, 
  title = "", xlab = "", ylab = "Prob. Density") 
{ 
  # for testing 
  { 
    # H0 = 0; H1 = 2.1  
    # critical_diff = 0.2; alpha = 0.05 
    # n = 1000 
    # hypothesis = "upper" 
    # alpha_col = 2 
    # beta_col = 3  
    # xmin = NA 
    # xmax = NA 
    # title = "" 
    # xlab = "" 
    # ylab = "Prob. Density" 
  } 
  
  xlim_scale = 2.8 * critical_diff 
  
  
  if (hypothesis == "both") alpha_crit = alpha / 2 
  if (hypothesis == "upper" | hypothesis == "lower") alpha_crit = alpha 
  
  critical_z = abs(qnorm(1 - alpha_crit)) 
  critical_x = H0 + critical_diff 
  critical_sd = critical_diff / critical_z 
  
  # Calculate a reasonable range for the x-axis if xmin/xmax are not provided 
  
  if (is.na(xmin)) 
    if (H0 <= H1) 
    { 
      xmin = H0 - xlim_scale 
      xmax = H1 + xlim_scale 
    } else 
      
    { 
      xmin = H0 + xlim_scale 
      xmax = H1 - xlim_scale 
    } 
  
  x = seq(xmin, xmax, length.out = n) 
  y_0 = dnorm(x, mean = H0, sd = critical_sd) 
  y_1 = dnorm(x, mean = H1, sd = critical_sd) 
  
  x_alpha = c(critical_x, x[x > critical_x], xmax) 
  y_alpha = dnorm(x_alpha, mean = H0, sd = critical_sd) 
  
  x_beta  = c(critical_x, x[x < critical_x], xmin) 
  y_beta  = dnorm(x_beta, mean = H1, sd = critical_sd) 
  
  plot( 
    x, y_0, type = "n", 
    main = title, xlab = xlab, ylab = ylab) 
  polygon( 
    x = c(x_alpha, xmax, critical_x), 
    y = c(y_alpha, 0, 0), 
    border = rgb(0, 0, 0, 0), 
    col = alpha_col 
  ) 
  
  polygon( 
    x = c(x_beta, xmin, critical_x), 
    y = c(y_beta, 0, 0), 
    border = rgb(0, 0, 0, 0), 
    col = beta_col 
  ) 
  
  mtext(text = bquote(paste('x'[crit], " = ", .(signif(critical_x, 3)))), line = 2.2, side = 1, at = critical_x) 
  axis(side = 1, at = critical_x, labels = "") 
  
  # Plot the density curves over the top of the polygons:  
  points(x, y_0, type = "l") 
  points(x, y_1, type = "l") 
} 





# Q8 ---- 

plot_alpha_beta(3.1, 3.4, 0.1) 



# Question 7 ---- 

{ 
  
  
  
  plot_alpha_beta(0, 3.1, .1) 
  
  
  
  
  
  n = 1000 
  
  xlim = c(-.5, 3.6) 
  
  sd_h0 = 0.05 
  
  h0 = 0 
  
  h1 = 3.1 
  
  
  
  x = seq(xlim[1], xlim[2], length.out = n) 
  
  x_crit = qnorm(0.975, mean = h0, sd = sd_h0) 
  
  
  
  y_h0 = dnorm(x, mean = 0, sd = 0.05) 
  
  y_h1 = dnorm(x, mean = 3.1, sd = 0.05) 
  
  
  
  plot( 
    
    x, y_h0, type = "l", 
    
    xlab = "Sepal Width", 
    
    ylab = "Pr. Density", 
    
    main = "Question Set 3 - Q7") 
  
  points(x, y_h1, type = "l") 
  
  
  
  text(x = 0.5, y = y_frac(0.5), labels = expression("H"[0])) 
  
  text(x = 2.6, y = y_frac(0.5), labels = expression("H"[1])) 
  
  
  
  x_alpha = c(x_crit, x[x > x_crit], xlim[2]) 
  
  y_alpha = dnorm(x_alpha, mean = h0, sd = sd_h0) 
  
  
  
  
  
  
  
  polygon( 
    
    x = c(x_alpha, xlim[2], x_crit), 
    
    y = c(y_alpha, 0, 0), 
    
    col = 2, border = rgb(0, 0, 0, 0) 
    
  ) 
  
  
  
} 



# Question 8 

{ 
  
  h0 = 3.1 
  
  h1 = 3.4 
  
  xlim = c(2.9, 3.6) 
  
  sd_h0 = 0.1 
  
  
  
  x = seq(xlim[1], xlim[2], length.out = n) 
  
  x_crit = qnorm(0.975, mean = h0, sd = sd_h0) 
  
  
  
  y_h0 = dnorm(x, mean = h0, sd = sd_h0) 
  
  y_h1 = dnorm(x, mean = h1, sd = sd_h0) 
  
  
  
  plot( 
    
    x, y_h0, type = "n", 
    
    xlab = "Sepal Width", 
    
    ylab = "Pr. Density", 
    
    main = "Question Set 3 - Q8") 
  
  points(x, y_h1, type = "l") 
  
  x_alpha = c(x_crit, x[x > x_crit], xlim[2]) 
  
  y_alpha = dnorm(x_alpha, mean = h0, sd = sd_h0) 
  
  
  
  x_beta = c(xlim[1], x[x < x_crit], x_crit) 
  
  y_beta = dnorm(x_beta, mean = h1, sd = sd_h0) 
  
  
  
  polygon( 
    
    x = c(x_beta, x_crit, xlim[1]), 
    
    y = c(y_beta, 0, 0), 
    
    col = 3, border = rgb(0, 0, 0, 0) 
    
  ) 
  
  
  
  polygon( 
    
    x = c(x_alpha, xlim[2], x_crit), 
    
    y = c(y_alpha, 0, 0), 
    
    col = 2, border = rgb(0, 0, 0, 0) 
    
  ) 
  
  points(x, y_h1, type = "l") 
  
  points(x, y_h0, type = "l") 
  
  
  
  text(x = 2.9, y = y_frac(0.6), labels = expression("H"[0])) 
  
  text(x = 3.3, y = y_frac(0.6), labels = expression("H"[1])) 
  
  
  
  
  
} 







y_frac(0.5) 

