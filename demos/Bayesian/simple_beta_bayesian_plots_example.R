
# Simple example plot of prior, current data, and posterior
# density/likelihood curves for beta prior and posterior distributions.
plot_beta_bayes = function(prior, new_data)
{
  legend_pos = "top"
  posterior = prior + new_data
  
  # Beta distribution domain is 0 - 1
  x = seq(0.0001, 0.9999, len = 300)
  

  
  # density curves for prior, posterior, and data
  y_prior = dbeta(x, prior[1], prior[2])
  y_data = dbeta(x, new_data[1], new_data[2])
  y_posterior = dbeta(x, posterior[1], posterior[2])
  
  ymax = max(c(y_prior, y_data, y_posterior))
  ylim = c(0, ymax * 1.1)
  
  lwd = 2
  
  col_prior = "orange"
  col_data  = "red"
  col_post  = "blue"
  
  plot_title = ""
  ylab = "likelihood or density"
  xlab = "p"
  
  plot(
    x, y_prior, 
    ylim = ylim, col = col_prior, 
    type = "l", 
    xlab = xlab, ylab = ylab,
    lwd = lwd,
    yaxt = "n")
  
  points(x, y_data, col = col_data, type = "l", lwd = lwd)
  points(x, y_posterior, col = col_post, type = "l", lwd = lwd)
  # points(x, y_post, ylim = ylim, col = "blue", type = "l", xlab = "p", ylab = "density", lwd = lwd)
  legend(
    x = "top", legend = c("prior", "data", "posterior"), 
    lty = 1, lwd = lwd, 
    col = c(col_prior, col_data, col_post), 
    bty = "n", bg = rgb(0, 0, 0, 0), horiz = TRUE)
  
}


# A weak prior:
plot_beta_bayes(prior = c(1, 1), new_data = c(18, 3))

# A weird weak prior
plot_beta_bayes(prior = c(.6, .6), new_data = c(18, 3))

# A less weak prior:
plot_beta_bayes(prior = c(2, 3), new_data = c(18, 3))

# A stronger prior for a low p, new data disagrees with prior
plot_beta_bayes(prior = c(10, 60), new_data = c(18, 3))

# A strong prior with strong data
plot_beta_bayes(prior = c(10, 300), new_data = c(300, 30))

