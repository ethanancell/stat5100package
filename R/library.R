#' (Stat 5100 function) Plot a scatterplot of the original data with a
#' regression line overlaid on top. This can help you decide on how well a
#' regression line fits the data.
#'
#' @param lmobject An object of type 'lm' created from the lm() function.
#' @return A plot object of the regression model and the original data.
fit_plot <- function(lmobject) {

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
  ggplot2::ggplot(data = data) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(xname, yname)) +
    ggplot2::geom_abline(intercept = intercept, slope = coeff, color = "red") +
    ggplot2::ggtitle("Fit plot for linear model")
}

#' (Stat 5100 function) Obtain a residual plot for a linear model. This helps you decide on the
#' appropriateness of linear regression model assumptions.
#'
#' @param lmobject An object of type 'lm' created from the lm() function.
#' @return A plot object of the residuals plotted against the fitted values.
residual_plot <- function(lmobject) {

  plotdata <- data.frame(resid = lmobject$resid,
                         actual = lmobject$fitted.values)

  ggplot2::ggplot(data = plotdata) +
    ggplot2::geom_point(ggplot2::aes(x = actual, y = resid)) +
    ggplot2::geom_abline(intercept = 0, slope = 0, color = "black") +
    ggplot2::ggtitle("Residuals vs predicted values")
}

seq_plot <- function(lmobject) {
  plotdata <- data.frame(resid = lmobject$resid,
                         fitted.values = lmobject$fitted.values)
  ggplot2::ggplot(data = plotdata) +
    ggplot2::geom_point(ggplot2::aes(x = fitted.values, y = resid)) +
    ggplot2::geom_abline(intercept = 0, slope = 0) +
    ggplot2::ggtitle("Sequence plot of residuals and predicted values")
}

#' (Stat 5100 function) Obtain a histogram of residuals for a linear model.
#' This function also plots a normal curve on top of the histogram with the
#' same mean and standard deviation. This helps you assess the validity of the
#' normality assumption of residuals in linear regression.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A histogram of the residuals along with a theoretical normal curve
#' with the same mean and standard deviation.
residual_hist <- function(lmobject) {
  ggplot2::ggplot(data = data.frame(resid = lmobject$resid)) +
    ggplot2::geom_histogram(ggplot2::aes(x = resid, y = ..density..),
                            bins = 20) +
    ggplot2::stat_function(fun = dnorm, args = list(mean = 0,
                                                    sd = sd(lmobject$resid))) +
    ggplot2::ggtitle("Residual histogram with theoretical normal distribution")
}

#' (Stat 5100 function) Obtain a QQ plot for a linear model. This plot can
#' help you assess the normality assumption of a linear regression model. This
#' plot will plot theoretical quantiles of a normal distribution against the
#' residuals.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A QQ plot object.
qq_plot <- function(lmobject) {
  ggplot2::ggplot(data = data.frame(resid = lmobject$resid)) +
    ggplot2::geom_qq(ggplot2::aes(sample = resid)) +
    ggplot2::geom_qq_line(ggplot2::aes(sample = resid))
}

#' Obtain ANOVA statistics for a linear model.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return A dataframe with various labelled ANOVA statistics.
anova_lm <- function(lmobject) {

  # By default gives scientific numbers which might be hard to read,
  # this code fixes that
  options(digits = 4)
  options(scipen = 999)

  model_df <- lmobject$rank - 1
  error_df <- lmobject$df.residual
  total_df <- model_df + error_df

  ss_model <- sum((lmobject$fitted.values - mean(lmobject$model[[lmobject$terms[[2]]]]))^2)
  ss_error <- sum(lmobject$residuals^2)
  ss_total <- sum((lmobject$model[[lmobject$terms[[2]]]] - mean(lmobject$model[[lmobject$terms[[2]]]]))^2)

  ms_model <- ss_model / model_df
  ms_error <- ss_error / error_df

  list(data.frame(source = c("model", "error", "total"),
             df = c(model_df, error_df, total_df),
             sum_squares = c(ss_model, ss_error, ss_total),
             mean_squares = c(ms_model, ms_error, NA)),
       c(fvalue = ms_model / ms_error,
         p = pf(ms_model / ms_error, model_df, error_df, lower.tail = FALSE)))
}
