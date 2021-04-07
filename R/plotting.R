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

  data <- lmobject$model
  pred_names <- names(lmobject$coefficients)

  # Plot the first
  # If there is more than one predictor,
  num_predictors <- length(pred_names)
  if (num_predictors > 2) {
    stop("Function fit_plot() requires that the linear model has only
         one response variable and one predictor variable.")
  }

  # Get names in the linear model
  yname <- toString(lmobject$terms[[2]])
  xname <- variable.names(lmobject)

  intercept <- 0
  coeff <- 0
  # Give an exception for if there is no intercept in the model
  if (length(lmobject$coefficients) == 1) {
    coeff <- lmobject$coefficients[1]

    # Plot through origin
    plot(data[[xname[1]]], data[[yname]], ...)
    abline(a = 0, b = coeff)
  } else {
    intercept <- lmobject$coefficients[1]
    coeff <- lmobject$coefficients[2]

    # Plot normally
    plot(data[[xname[2]]], data[[yname]], ...)
    abline(a = intercept, b = coeff)
  }
}

fit_plot_quadratic_model <- function(lmobject, ...) {

  # Error check
  if (missing(lmobject)) {
    stop("Function fit_plot() is missing a linear model object.")
  }
  data <- lmobject$model
  pred_names <- names(lmobject$coefficients)
  num_predictors <- length(pred_names)
  if (num_predictors > 3) {
    stop("Function fit_plot() requires that the linear model has only
         two response variables.")
  }

  # Get names in the linear model
  yname <- toString(lmobject$terms[[2]])

  # Create a sequence from minimum x to maximum x, and then predict at all
  # the locations to get our quadratic curve

  # Plot
  plot(data[[pred_names[1]]], data[[yname]], ...)
  abline(a = intercept, b = coeff)
}

#' (Stat 5100 function) Plot influence diagnostics for a logistic regression
#' model.
#'
#' @param glmobject An object from the glm() function that has a binomial family
#' (logistic regression)
#' @return A grid of four plots that shows various influence diagnostics.
logistic_influence_diagnostics <- function(glmobject) {

  N <- length(residuals(glmobject))
  fitted_prob <- fitted(glmobject)
  
  # Store residual labels (defaults to row numbers)
  labels <- names(shuttle_logreg$residuals)
  
  # Get hat matrix (useful for some of these plots)
  H <- hatvalues(glmobject)

  # ====================================

  # Fit models without each of the observations (leave one out)
  # (these are all clumped together for computation purposes)

  dev_i <- vector("numeric", length = N)
  chisq_i <- vector("numeric", length = N)
  for (i in 1:N) {
    new_logreg_df <- glmobject$model[-i, ]
    new_glmmodel <- glm(glmobject$formula, data = new_logreg_df,
                        family = "binomial")
    dev_i[i] <- new_glmmodel$deviance
    chisq_i[i] <- sum(residuals(new_glmmodel, type = "pearson")^2)
  }

  # =========================

  par(mfrow = c(2, 2))

  # Chi-squared deletion difference
  # chisq <- sum(residuals(glmobject, type = "pearson")^2)
  # plot(fitted_prob, chisq - chisq_i, type = "n",
  #      ylab = "Chi-square Deletion Difference", xlab = "Predicted Probability")
  # text(fitted_prob, chisq - chisq_i, labels = labels )

  # Pearson Residual
  plot(1:N, residuals(glmobject, type = "pearson"), type = "n",
       xlab = "Observation #", ylab = "Pearson Residual",
       main = "Influence diagnostic #1")
  abline(a = 0, b = 0)
  text(1:N, residuals(glmobject, type = "pearson"), labels = labels )

  # Deviance Residual
  plot(1:N, residuals(glmobject, type = "deviance"), type = "n",
       xlab = "Observation #", ylab = "Deviance Residual",
       main = "Influence diagnostic #2")
  abline(a = 0, b = 0)
  text(1:N, residuals(glmobject, type = "deviance"), labels = labels )

  # Deviance deletion difference
  plot(fitted_prob, glmobject$deviance - dev_i, type = "n",
       ylab = "Deviance Deletion Difference", xlab = "Predicted Probability")
  text(fitted_prob, glmobject$deviance - dev_i, labels = labels )

  # CI Displacements
  # Pending....

  # Leverage
  plot(fitted_prob, H, xlab = "Predicted Probability", ylab = "Leverage",
       type = "n")
  text(fitted_prob, H, labels = labels )

  par(mfrow = c(1, 1))
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

#' Create simultaneous plots of the ridge constant plotted against both
#' the variance inflation factor and the coefficient estimates. This can help
#' you decide on an optimal ridge constant.
#'
#' @param ridgeobject An object of type "ridge" from the genridge package.
ridge_vif_trace_plot <- function(ridgeobject) {

  # Check to make sure is ridge object
  if (class(ridgeobject) != "ridge") {
    stop("ridgeobject parameter must be of type \"ridge\" from genridge package.")
  }

  vridge <- genridge::vif.ridge(ridgeobject)
  pch <- c(15:18, 7, 9)

  par(mfrow = c(2, 1))
  matplot(rownames(vridge), vridge, type = 'b', xlab = "Ridge constant",
          ylab = "Variance inflation", main = "Ridge Regression Analysis",
          cex = 1.2, pch = pch)
  text(0.0, vridge[1,], colnames(vridge), pos = 4)
  genridge::traceplot(ridgeobject)
  par(mfrow = c(1, 1))
}

#' (Stat 5100 function) Simulated envelope plot for logistic regression models
#'
#' @param glmobject An object from the glm() function. Ensure that the family of
#' the glm object is binomial.
#' @return A simulated envelope plot to check for outliers.
simulated_envelope_logreg <- function(glmobject) {
  # Extract useful information
  fitted_prob <- fitted(glmobject)
  N <- length(glmobject$residuals)
  X <- glmobject$model

  # Repeated 19 times:
  ordered_abs_dev_all <- vector()
  for (simulation_num in 1:19) {
    # Step 1: for each of the n cases, generate Bernoulli outcome where
    # the Bernoulli parameter is \hat{\pi}_i
    bernoulli_outcome <- vector(mode = "numeric", length = N)
    for (i in 1:N) {
      bernoulli_outcome[i] <- rbinom(1, 1, prob = fitted_prob[i])
    }

    # Step 2: Fit a logistic regression model with the new vector of
    # responses (keeping original predictors) and obtain ordered
    # absolute deviance residuals.
    new_logreg_df <- X
    new_logreg_df[[as.character(attributes(glmobject$terms)$variables[[2]])]] <-
      bernoulli_outcome
    new_logreg <- glm(glmobject$formula, data = new_logreg_df,
                      family = "binomial")
    ordered_abs_dev <- sort(abs(residuals(new_logreg, type = "deviance")))
    ordered_abs_dev_all <- rbind(ordered_abs_dev_all, ordered_abs_dev)
  }

  # Step 3:
  # Get minimum, mean, and maximum values across all 19 groups
  min_abs_dev_all <- apply(ordered_abs_dev_all, 2, min)
  max_abs_dev_all <- apply(ordered_abs_dev_all, 2, max)
  mean_abs_dev_all <- apply(ordered_abs_dev_all, 2, mean)

  # Get expected values for ordered residuals
  expected_values <- vector("numeric", length = N)
  for (i in 1:N) {
    expected_values[i] <- qnorm((i + N - (1/8)) / (2*N + (1/2)))
  }

  plot(expected_values, sort(abs(residuals(glmobject, type = "deviance"))),
       xlab = "Expected value", ylab = "Absolute Deviance Residuals",
       main = "Half-Normal Probability Plot with Simulated Envelope",
       pch = 16)
  # Min, mean, and maximum lines
  lines(expected_values, min_abs_dev_all, type = "l")
  lines(expected_values, max_abs_dev_all, type = "l")
  lines(expected_values, mean_abs_dev_all, type = "l")
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
