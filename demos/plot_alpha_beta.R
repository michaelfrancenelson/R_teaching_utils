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
    H0 = 1.95; H1 = 2.1
    critical_diff = .2; alpha = 0.05
    n = 1000
    hypothesis = "upper"
    alpha_col = 2
    beta_col = 3
    xmin = NA
    xmax = NA
    title = ""
    xlab = ""
    ylab = "Prob. Density"
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
  
  x_beta  = c(critical_x, x[x < critical_x], critical_x) 
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