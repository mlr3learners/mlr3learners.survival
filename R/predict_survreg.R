predict_survreg = function(object, task, type = "aft") {

  # Extracts baseline distribution and the model fit, performs assertions
  basedist = object$basedist
  fit = object$fit
  distr6::assertDistribution(basedist)
  checkmate::assertClass(fit, "survreg")

  # define newdata from the supplied task and convert to model matrix
  newdata = task$data(cols = task$feature_names)
  x = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata,
    xlev = task$levels())[, -1]

  # linear predictor defined by the fitted cofficients multiplied by the model matrix
  # (i.e. covariates)
  lp = matrix(fit$coefficients[-1], nrow = 1) %*% t(x)

  # checks and parameterises the chosen model type: proportional hazard (ph), accelerated failure
  # time (aft), odds.
  # PH: h(t) = h0(t)exp(lp)
  # AFT: h(t) = exp(-lp)h0(t/exp(lp))
  # PO: h(t)/h0(t) = {1 + (exp(lp)-1)S0(t)}^-1

  dist = toproper(fit$dist)

  if (type == "ph") {
    name = paste(dist, "Proportional Hazards Model")
    short_name = paste0(dist, "PH")
    description = paste(dist, "Proportional Hazards Model with negative log-likelihood",
      -fit$loglik[2])
  } else if (type == "aft") {
    name = paste(dist, "Accelerated Failure Time Model")
    short_name = paste0(dist, "AFT")
    description = paste(dist, "Accelerated Failure Time Model with negative log-likelihood",
      -fit$loglik[2])
  } else if (type == "po") {
    name = paste(dist, "Proportional Odds Model")
    short_name = paste0(dist, "PO")
    description = paste(dist, "Proportional Odds Model with negative log-likelihood",
      -fit$loglik[2])
  }

  params = list(list(name = name,
    short_name = short_name,
    type = set6::PosReals$new(),
    support = set6::PosReals$new(),
    valueSupport = "continuous",
    variateForm = "univariate",
    description = description,
    .suppressChecks = TRUE,
    pdf = function() {
    },
    cdf = function() {
    },
    parameters = ParameterSet$new()
  ))

  params = rep(params, length(lp))

  pdf = function(x) {} # nolint
  cdf = function(x) {} # nolint
  quantile = function(p) {} # nolint

  if (type == "ph") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((exp(y) * basedist$hazard(x)) * (1 - self$cdf(x)), list(y = lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x)^exp(y)), list(y = lp[i]))
      body(quantile) = substitute(basedist$quantile(1 - exp(exp(-y) * log(1 - p))), list(y = lp[i])) # nolint
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  } else if (type == "aft") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((exp(-y) * basedist$hazard(x / exp(y))) * (1 - self$cdf(x)),
                             list(y = lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x / exp(y))), list(y = lp[i]))
      body(quantile) = substitute(exp(y) * basedist$quantile(p), list(y = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  } else if (type == "po") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((basedist$hazard(x) *
                                (1 - (basedist$survival(x) /
                                        (((exp(y) - 1)^-1) + basedist$survival(x))))) *
                               (1 - self$cdf(x)), list(y = lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x) *
        (exp(-y) + (1 - exp(-y)) * basedist$survival(x))^-1),
      list(y = lp[i]))
      body(quantile) = substitute(basedist$quantile(-p / ((exp(-y) * (p - 1)) - p)), # nolint
                                  list(y = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  }

  distlist = lapply(params, function(.x) do.call(Distribution$new, .x))
  names(distlist) = paste0("WeibullAFT", seq_along(distlist))


  distr = distr6::VectorDistribution$new(distlist,
                                         decorators = c("CoreStatistics", "ExoticStatistics"))

  lp = lp + fit$coefficients[1]

  return(list(lp = as.numeric(lp), distr = distr))
}
