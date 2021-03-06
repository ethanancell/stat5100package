% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{brown_forsythe_lm}
\alias{brown_forsythe_lm}
\title{(Stat 5100) Brown-Forsythe test for constant variance in the residuals.}
\usage{
brown_forsythe_lm(lmobject)
}
\arguments{
\item{lmobject}{An object obtained from the lm() function.}
}
\value{
Printed output with the results of the t-test.
}
\description{
This function splits the data into two halves based upon the median
fitted value, and then within each the median absolute deviations
of the residuals are calculated. A pooled variance t-test is
subsequently applied to the two groups of median absolute deviations.
This test is a way to evaluate whether there is constant variance in
the residuals or not.
}
