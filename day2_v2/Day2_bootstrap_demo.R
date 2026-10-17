# Optional instructor demonstration; not required for the Day 2 core.
# Inputs are two independent groups. Direction is drought minus control.
# This percentile bootstrap does not impose a null hypothesis for a test.

bootstrap_difference <- function(drought, control, n_resamples = 4000,
                                 seed = 3) {
  drought <- drought[is.finite(drought)]
  control <- control[is.finite(control)]
  stopifnot(length(drought) >= 2L, length(control) >= 2L,
            length(n_resamples) == 1L, is.finite(n_resamples),
            n_resamples >= 1000, n_resamples == as.integer(n_resamples))
  set.seed(seed)
  differences <- replicate(n_resamples, {
    drought_sample <- sample(drought, length(drought), replace = TRUE)
    control_sample <- sample(control, length(control), replace = TRUE)
    mean(drought_sample) - mean(control_sample)
  })
  list(estimate = mean(drought) - mean(control),
       conf.int = unname(quantile(differences, c(0.025, 0.975))),
       differences = differences)
}
