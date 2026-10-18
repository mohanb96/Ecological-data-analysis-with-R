# Optional demonstration: HC3 covariance with approximate t-based intervals.
# For a full-rank, unweighted ordinary lm fit with residual degrees of freedom.
# This addresses heteroskedasticity, not dependence or an incorrect mean model.
robust_lm_ci <- function(model, level = 0.95) {
  stopifnot(inherits(model, "lm"), is.null(model$weights),
            is.numeric(level), length(level) == 1L, level > 0, level < 1)
  X <- model.matrix(model)
  stopifnot(model$rank == ncol(X), df.residual(model) > 0)
  h <- hatvalues(model)
  stopifnot(all(h < 1))
  bread <- solve(crossprod(X))
  weighted_X <- X * as.numeric(residuals(model) / (1 - h))
  covariance <- bread %*% crossprod(weighted_X) %*% bread
  standard_error <- sqrt(diag(covariance))
  estimate <- coef(model)
  critical <- qt(1 - (1 - level) / 2, df = df.residual(model))
  data.frame(term = names(estimate), estimate = as.numeric(estimate),
             HC3_SE = as.numeric(standard_error),
             lower = as.numeric(estimate - critical * standard_error),
             upper = as.numeric(estimate + critical * standard_error),
             p_approx = as.numeric(2 * pt(-abs(estimate / standard_error),
                                        df = df.residual(model))),
             row.names = NULL)
}
