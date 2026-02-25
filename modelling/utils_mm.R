library(lme4)


# overdispersion function - useful for GLMMs
over <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1) / 2
  }
  model_df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model_df
  rp <- residuals(model, type = "pearson")
  pearson_chisq <- sum(rp ^ 2)
  prat <- pearson_chisq/rdf
  pval <- pchisq(pearson_chisq, df = rdf, lower.tail = FALSE)
  c(chisq = pearson_chisq, ratio = prat, rdf = rdf, p = pval)
}


# Variance inflation factor function for mixed models
vif <- function(fit) {
  ## adapted from rms::vif

  v <- vcov(fit)
  nam <- names(fixef(fit))

  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }

  d <- diag(v) ^ 0.5
  v <- diag(solve(v / (d %o% d)))
  names(v) <- nam
  v
}