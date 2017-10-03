library(ggplot2)
library(grid)
library(gridExtra)

make_plots = function(data, lpsolve_out, name) {
  ## Makes three plots: plot of the c functions, positive probability masses, and positive Lagrange multipliers
  
  len = length(data$x)
  
  ### c plot
  
  temp = lpsolve_out$allvar$opt
  temp2 = cumsum(temp[1:len])
  
  # CHECK - hacky - multiplying c(0, diff(x)) approximates an integral
  data[, c_opt := cumsum(temp2 * c(0, diff(x)))]
  data[, c_0 := cumsum(cumsum(p0) * c(0, diff(x)))]
  
  # CHECK - also hacky...
  data[, c_mean := cumsum( (x > mean(x)) * c(0, diff(x)) )]
  
  uplot = ggplot(data, aes(x = x, y = Uvec)) + 
    geom_line(size = 1.3)+
    scale_x_continuous(name = "X") + 
    scale_y_continuous(name = "Utility") + 
    labs(title = "Utility function") + 
    theme(text = element_text(size = 40))
  
  CairoPNG(filename = paste("plots/", name, "_uplot.png", sep = ""), width = 1600, height = 1200)
  print(
    uplot
  )
  dev.off()
  
  cplot = ggplot(data) + 
    geom_line(aes(x = x, y = c_0), color = "red", size = 1.3) + 
    geom_line(aes(x = x, y = c_mean), color = "red", size = 1.3) + 
    geom_line(aes(x = x, y = c_opt), color = "blue", size = 1.3) +
    scale_x_continuous(name = "X") + 
    scale_y_continuous(name = "C") + 
    labs(title = "C bounds (red) vs. optimal C (blue)") + 
    theme(text = element_text(size = 40))
  
  CairoPNG(filename = paste("plots/", name, "_cplot.png", sep = ""), width = 1600, height = 1200)
  print(
    cplot
  )
  dev.off()
  
  ### Probability Mass Function
  data[, opt_p := lpsolve_out$solution]
  
  pplot = ggplot(data, aes(x = x, y = opt_p)) + 
    geom_point(size = 5)+
    scale_x_continuous(name = "X") + 
    scale_y_continuous(name = "Probability Mass") + 
    labs(title = "Optimal PMF") + 
    theme(text = element_text(size = 40))
  
  CairoPNG(filename = paste("plots/", name, "_pmfplot.png", sep = ""), width = 1600, height = 1200)
  print(
    pplot
  )
  dev.off()
  
  ### Lagrange Multipliers
  data[, lagrange_mults := c(out$con$dual[2:len], 0)]
  
  lplot = ggplot(data, aes(x = x, y = lagrange_mults)) + 
    geom_point(size = 5)+
    scale_x_continuous(name = "a") + 
    scale_y_continuous(name = "Lagrange Multipliers") + 
    labs(title = "Lagrange Multipliers") + 
    theme(text = element_text(size = 40))
  
  CairoPNG(filename = paste("plots/", name, "_lmplot.png", sep = ""), width = 1600, height = 1200)
  print(
    lplot
  )
  dev.off()
  
  ## Grid of plots
  g = arrangeGrob(
    uplot, cplot, pplot, lplot,
    ncol = 2)
  
  CairoPNG(filename = paste("plots/", name, "_plotgrid.png", sep = ""), width = 1080*2, height = 1080*2)
  grid.draw(g)
  dev.off()
}