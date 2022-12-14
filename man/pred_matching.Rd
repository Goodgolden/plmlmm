% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_plm.R
\name{pred_matching}
\alias{pred_matching}
\title{Title prediction matching}
\usage{
pred_matching(
  lb_data = lb_data,
  obs_data = train,
  match_methods = c("mahalanobis", "euclidean", "single"),
  match_num = NULL,
  match_alpha = NULL,
  match_time = NULL,
  gamlss_formula = "ht ~ cs(time, df = 3)",
  gamsigma_formula = "~ cs(time, df = 1)",
  match_plot = FALSE,
  predict_plot = FALSE,
  sbj
)
}
\arguments{
\item{sbj}{}
}
\description{
Title prediction matching
}
