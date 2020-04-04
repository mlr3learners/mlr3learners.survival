#' @title Survival Nelson-Aalen Estimator Learner
#' @name mlr_learners_surv.nelson
#'
#' @description
#' A [mlr3proba::LearnerSurv] implementing `survfit` from package
#'   \CRANpkg{survival}.
#' Calls [survival::survfit()].
#'
#' @details
#' * `distr` is predicted by estimating the cumulative hazard function from `survival::survfit()`.
#' * `crank` is predicted as the expectation of `distr`
#'
#' @templateVar id surv.nelson
#' @template section_dictionary_learner
#'
#' @references
#' \cite{mlr3proba}{nelson_1969}
#'
#' \cite{mlr3proba}{nelson_1972}
#'
#' \cite{mlr3proba}{aalen_1978}
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerSurvNelson = R6Class("LearnerSurvNelson", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.nelson",
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = "survival",
        man = "mlr3learners.survival::mlr_learners_surv.nelson"
      )
    }
  ),

  private = list(
    .train = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
    },

    .predict = function(task) {

      # Ensures that at all times before the first observed time the cumulative hazard is 0,
      # as expected.
      # cumhaz = c(0, self$model$cumhaz)
      # time = c(0, self$model$time)

      # Define WeightedDiscrete distr6 distribution from the cumulative hazard
      x = rep(list(data = data.frame(x = self$model$time, cdf = 1 - exp(-self$model$cumhaz))),
              task$nrow)
      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
        decorators = c("CoreStatistics", "ExoticStatistics"))

      # Define crank as the mean of the survival distribution
      crank = as.numeric(sum(x[[1]][, 1] * c(x[[1]][, 2][1], diff(x[[1]][, 2]))))

      PredictionSurv$new(task = task, crank = rep(crank, task$nrow), distr = distr)
    }
  )
)
