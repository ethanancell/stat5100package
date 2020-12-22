fit_plot <- function(lmobject, data) {

  # Get names in the linear model
  yname <- lmobject$terms[[2]]
  xname <- lmobject$terms[[3]]
  ggplot(data = data) +
    geom_point(mapping = aes(x = toString(xname), y = toString(yname)))
}
