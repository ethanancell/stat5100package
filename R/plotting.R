#' (Stat 5100 function) Plot a scatterplot of the original data with a
#' regression line overlaid on top. This can help you decide on how well a
#' regression line fits the data.
#'
#' @param lmobject An object of type 'lm' created from the lm() function.
#' @return A plot object of the regression model and the original data.
fit_plot <- function(lmobject, ...) {

  # Error check
  if (missing(lmobject)) {
    stop("Function fit_plot() is missing a linear model object.")
  }

  data = lmobject$model

  # Make sure there is only one predictor
  term_length <- length(lmobject$terms) - 1
  if (term_length > 2) {
    stop("Function fit_plot() requires that the linear model has only
         one reponse variable and one predictor variable.")
  }

  # Get names in the linear model
  yname <- toString(lmobject$terms[[2]])
  xname <- toString(lmobject$terms[[3]])

  # Get coefficient and intercept from linear model
  intercept <- lmobject$coefficients[1]
  coeff <- lmobject$coefficients[2]

  # Plot
  plot(data[[xname]], data[[yname]], ...)
  abline(a = intercept, b = coeff)
}

#' (Stat 5100 function) Obtain a residual plot for a linear model. This helps you decide on the
#' appropriateness of linear regression model assumptions.
#'
#' @param lmobject An object of type 'lm' created from the lm() function.
#' @return A plot object of the residuals plotted against the fitted values.
residual_plot <- function(lmobject, ...) {
  plot(lmobject$fitted.values, lmobject$resid, ...)
  abline(a = 0, b = 0)
}

#' (Stat 5100 function) Obtain a sequence plot of residuals. This is a plot
#' that plots the order of the residuals in the data agains the residuals
#' themselves. This can help you identify a possible violation of
#' independence in the residuals.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A plot object with the residuals plotted in order.
seq_plot <- function(lmobject, ...) {

  plotdata <- data.frame(resid = lmobject$resid,
                         order.in.data = 1:length(lmobject$resid))
  plot(plotdata$order.in.data, plotdata$resid, ...)
  lines(plotdata$order.in.data, plotdata$resid)
}

#' (Stat 5100 function) A combination of the 4 main graphical checks to
#' assess whether model assumptions are satisfied or not. This is a way
#' to clean up the code and make it a bit easier.
#'
#' @param lmobject The linear model object from the lm() function
#' @return A 2x2 plot with some main graphical checks we use.
visual_assumptions <- function(lmobject, ...) {
  par(mfrow = c(2, 2))
  seq_plot(lmobject, main = "Sequence plot", ...)
  qq_plot(lmobject, main = "QQ Plot", ...)
  residual_hist(lmobject, main = "Residual histogram", ...)
  residual_plot(lmobject, main = "Residual plot", ...)

  par(mfrow = c(1,1))
}

#' (Stat 5100 function) Obtain a histogram of residuals for a linear model.
#' This function also plots a normal curve on top of the histogram with the
#' same mean and standard deviation. This helps you assess the validity of the
#' normality assumption of residuals in linear regression.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A histogram of the residuals along with a theoretical normal curve
#' with the same mean and standard deviation.
residual_hist <- function(lmobject, ...) {
  hist(lmobject$resid, breaks = sqrt(length(lmobject$resid)), freq = FALSE, ...)

  # Draw the normal curve with mean 0 and same standard deviation as residuals
  x <- seq(min(lmobject$resid), max(lmobject$resid), length = 400)
  y <- dnorm(x, mean = 0, sd = sd(lmobject$resid))
  lines(x, y)
}

#' (Stat 5100 function) Obtain a QQ plot for a linear model. This plot can
#' help you assess the normality assumption of a linear regression model. This
#' plot will plot theoretical quantiles of a normal distribution against the
#' residuals.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A QQ plot object.
qq_plot <- function(lmobject, ...) {
  qqnorm(lmobject$resid, ...)
  qqline(lmobject$resid)
}
