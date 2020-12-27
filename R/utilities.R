#' (Stat 5100) F-test for lack of fit. A test for determining if too much
#' of the error in prediction is due to the lack of model fit.
#'
#' @param lmobject A linear model object from the lm() function.
#' @return An ANOVA result that gives an F-statistic and p-value for this
#' test.
ftest_lackfit_lm <- function(lmobject) {

  # get name of response
  response <- lmobject$terms[[2]]

  # Get a list of every single model term
  var_names <- variable.names(lmobject)
  var_names <- var_names[2:length(var_names)]

  # Turn all of the variables into factors
  mod_dataframe <- lmobject$model
  for (i in 1:length(var_names)) {
    mod_dataframe[[var_names[i]]] <- as.factor(mod_dataframe[[var_names[i]]])
  }

  # Loop through all the variable names and add them on as crossed factors
  # into the string
  current_formula <- paste(response, " ~ ", var_names[1])
  if (length(var_names) > 1) {
    for (i in 2:length(var_names)) {
      current_formula <- paste(current_formula, "*", var_names[i], sep = "")
    }
  }

  # Create a linear model with the newly crossed factors, and then we ANOVA
  # that with our original linear model for our final result
  factor_cross_lm <- lm(as.formula(current_formula), data = mod_dataframe)
  anova(lmobject, factor_cross_lm)
}

#' (Stat 5100) Brown-Forsythe test for constant variance in the residuals.
#'
#' This function splits the data into two halves based upon the median
#' fitted value, and then within each the median absolute deviations
#' of the residuals are calculated. A pooled variance t-test is
#' subsequently applied to the two groups of median absolute deviations.
#' This test is a way to evaluate whether there is constant variance in
#' the residuals or not.
#' @param lmobject An object obtained from the lm() function.
#' @return Printed output with the results of the t-test.
brown_forsythe_lm <- function(lmobject) {

  response_name <- lmobject$terms[[2]]
  n <- length(lmobject$fitted.values)

  # Split the data into two groups based on median predicted value
  median_pred <- median(lmobject$fitted.values)
  group_B <- lmobject$fitted.values > median_pred

  bf_test_df <- data.frame(resid = lmobject$residuals,
                           group = rep("A", length(lmobject$residuals)))
  bf_test_df$group[group_B] <- "B"
  bf_test_df$group <- as.factor(bf_test_df$group)

  group_A <- bf_test_df$resid[bf_test_df$group == "A"]
  group_B <- bf_test_df$resid[bf_test_df$group == "B"]

  # Get median of residuals in each group
  median_A <- median(group_A)
  median_B <- median(group_B)

  # Get deviations within each group from the median
  zA <- abs(group_A - median_A)
  zB <- abs(group_B - median_B)

  # Do t-test of zA and zB
  result <- t.test(zA, zB, var.equal = TRUE)
  print("Brown-forsythe test for constant variance in the residuals:")
  print(paste("T-statistic: ", round(result$statistic, 4), ", p-value: ",
              round(result$p.value, 4), sep = ""))
}

#' (Stat 5100) Calculate confidence intervals for the coefficients of a linear
#' model. This function pulls info from the lm object and constructs a
#' confidence interval of the form "coefficient est +- critical value * std error"
#'
#' @param lmobject A linear model object from the lm() function
#' @param confidence A level of confidence for the interval expressed as a proportion
#' @return A coefficient matrix for the model that contains lower and upper
#' confidence interval values for the coefficient estimate.
coefficient_confidence_lm <- function(lmobject, confidence = 0.95, simul = FALSE) {
  # Error check
  if (confidence < 0 | confidence > 1) {
    stop("confidence parameter must be a number between 0 and 1.")
  }

  # To accomplish this, we simply grab the standard error from the
  # summary function as well as the critical value that gives the given
  # confidence level
  lm_sum <- summary(lmobject)

  out_mtx <- lm_sum$coefficients
  var_names <- row.names(out_mtx)

  g <- 1
  if (simul == TRUE) {
    g <- length(var_names)
  }

  lower.est <- vector("numeric", length = length(var_names))
  upper.est <- vector("numeric", length = length(var_names))

  alpha <- (1 - confidence) / g

  # Calculate the coefficient estimates based upon the multiple hypothesis
  # testing method
  crit_value <- abs(qt(alpha/2, df = lmobject$df.residual))

  for (i in 1:length(var_names)) {
    # Estimate +- std.error * crit_value
    lower.est[i] <- out_mtx[i, 1] - out_mtx[i, 2] * crit_value
    upper.est[i] <- out_mtx[i, 1] + out_mtx[i, 2] * crit_value
  }

  out_mtx <- cbind(out_mtx, lower.est, upper.est)

  cat("lower.est and upper.est are the ", 100 - round(alpha*100, digits = 4),
      "% confidence limits.\n", sep = "")
  if (simul == TRUE) {
    cat("The Bonferroni adjustment for simultaneous confidence levels was made.\n")
  } else {
    cat("There were no adjustments for simultaneous confidence levels.\n")
  }
  out_mtx
}

#' (Stat 5100) confidence interval for the prediction of a single new observation
#' given some known X-profile.
#'
#' @param lmobject The linear model object from the lm() function
#' @param newdata A dataframe with columns named the same thing as the predictor variables
#' used to create the model.
#' @param confidence A number between 0 and 1 that indicates the confidence level
simul_prediction_limits <- function(lmobject, newdata, confidence = 0.95) {
  # Much the same as the mean prediction limits, except here I have to
  # calculate the standard errors for the prediction manually
  pred_frame <- predict(lmobject, newdata, se.fit = TRUE)
  n <- nrow(lmobject$model) # Sample size

  # Do this first:
  # Get variable names and then include the X-profiles in the output dataset
  var_names <- variable.names(lmobject)
  var_names <- var_names[2:length(var_names)]

  mydf <- data.frame(newdata[[var_names[1]]])
  if (length(var_names) > 1) {
    for (i in 2:length(var_names)) {
      mydf <- cbind(mydf, newdata[[var_names[i]]])
    }
  }
  names(mydf) <- var_names

  # Now follow formula on page 5 of handout 2.3
  SST <- sum(anova(lmobject)$`Sum Sq`)
  MSE <- anova(lmobject)$`Mean Sq`[2]
  # I'm pretty sure that this only works for a single predictor, because in
  # the case of multiple parameters this formula doesn't quite work. I may
  # have to adjust this later.
  num <- (mydf[[var_names[1]]] - mean(lmobject$model[[var_names[1]]]))^2
  se_yhat_pred <- sqrt(MSE) * (1 + 1/n + num/SST)

  alpha <- 1 - confidence # Simultaneous confidence level
  p <- lmobject$rank # Number of parameters
  g <- nrow(newdata) # Number of simultaneous intervals
  S <- sqrt(p * qf(1-alpha, g, n-p)) # Scheffe critical value
  t <- qt(1 - alpha/(2*g), n-p) # Bonferroni critical value

  yhat <- pred_frame$fit

  S_upper <- yhat + S*se_yhat_pred
  S_lower <- yhat - S*se_yhat_pred
  B_upper <- yhat + t*se_yhat_pred
  B_lower <- yhat - t*se_yhat_pred

  cbind(mydf, yhat, se_yhat_pred, S_lower, S_upper, B_lower, B_upper)
}

#' (Stat 5100) confidence interval for the mean response variable at a given
#' X-level.
#'
#' @param lmobject The linear model object from the lm() function
#' @param newdata A dataframe with columns named the same thing as the predictor variables
#' used to create the model.
#' @param confidence A number between 0 and 1 that indicates the confidence level
simul_mean_prediction_limits <- function(lmobject, newdata, confidence = 0.95) {
  # Let predict.lm function do most the heavy lifting here
  pred_frame <- predict(lmobject, newdata, se.fit = TRUE)

  alpha <- 1 - confidence # Simultaneous confidence level
  p <- lmobject$rank # Number of parameters
  n <- nrow(lmobject$model) # Sample size
  g <- nrow(newdata) # Number of simultaneous intervals
  W <- sqrt(p * qf(1-alpha, p, n-p)) # WH critical value
  t <- qt(1 - alpha/(2*g), n-p) # Bonferroni critical value

  yhat <- pred_frame$fit
  se_yhat <- pred_frame$se.fit

  WH_upper <- yhat + W*se_yhat
  WH_lower <- yhat - W*se_yhat
  B_upper <- yhat + t*se_yhat
  B_lower <- yhat - t*se_yhat

  # Get variable names and then include the X-profiles in the output dataset
  var_names <- variable.names(lmobject)
  var_names <- var_names[2:length(var_names)]

  mydf <- data.frame(newdata[[var_names[1]]])
  if (length(var_names) > 1) {
    for (i in 2:length(var_names)) {
      mydf <- cbind(mydf, newdata[[var_names[i]]])
    }
  }
  names(mydf) <- var_names

  cbind(mydf, yhat, se_yhat, WH_lower, WH_upper, B_lower, B_upper)
}

#' (Stat 5100) Correlation test of normality function. This function will give
#' output on the correlation between the residuals and the expected residuals
#' under a normal distribution. Table B.6 from the notes will give you what
#' correlation numeric values would reject/fail to reject a null hypothesis
#' that the residuals have a normal distribution.
#'
#' @param lmobject A linear model object from the lm() function
#' @return Console output with a correlation matrix of the residuals and
#' expected residuals
cor_normality_lm <- function(lmobject) {
  # Sort the data in order of the residuals
  residuals <- sort(lmobject$residuals)

  n <- length(residuals)
  resid_order <- 1:n

  # I have no idea why we use these constants here of 0.375 and 0.25,
  # I just go this from Dr. Steven's SAS macro and this gives me the same
  # output as SAS so we'll go with it I guess
  expected_norm <- qnorm((resid_order - 0.375)/(n + 0.25))
  result <- cor.test(residuals, expected_norm)

  result_df <- data.frame(resid = c(1.0, result$estimate),
                          expected_norm = c(result$estimate, 1.0))
  row.names(result_df) <- c("resid", "expected_norm")

  cat("Correlation test of normality:\n")
  print(result_df)
  cat("\n")
  cat("Total observations: ", n, "\n", sep = "")
  cat("Make sure to consult with table B.6 for your final result.\n")
}
