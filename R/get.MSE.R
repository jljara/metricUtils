#' Gets the MSE between two vectors.
#'
#' Calculates mean squared error between two vectors.
#'
#' @param sim A vector with the predicted values
#' @param obs A vector with the observed values
#' @param ... Variables passed to method implementations.
#'
#' @return A measure of the mean squared error between
#'     the two specified vectors.
#'
#' @export
#'
get.MSE <-function(sim, obs, ...) UseMethod("get.MSE")

#' @describeIn get.MSE Default implementation
#'
#' @details
#' This is the default implementation that works with
#' two vectors of classes "integer", "numeric" or "ts".
#'
#' @param ... Variables passed to an internal call to the
#'     \code{mean()} function
#'
#' @export
#'
get.MSE.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)

  mse <- mean((sim - obs)^2, ...)

  return(mse)
}

#' @describeIn get.MSE Implementation for matrices
#'
#' @details
#' In this implementation, the arguments are numeric
#' matrices of the same dimensions.
#'
#' @param sim A matrix with the predicted values
#' @param obs A matrix with the observed values
#' @param ... Variables passed to an internal call to the
#'     \code{colMeans()} function
#'
#' @return A vector with column-wise measures of the
#'     mean squared error between the specified matrices.
#'
#' @export
#'
get.MSE.matrix <- function(sim, obs, ...)
{
  # Check that 'sim' and 'obs' have the same dimensions
  if(!all.equal(dim(sim), dim(obs)))
    stop(paste0("Invalid argument: dim(sim) != dim(obs) ",
                "(", "[", paste(dim(sim), collapse = " "), "]", " != ",
                "[", paste(dim(obs), collapse = " "), "]", ")"))

  mse <- colMeans((sim - obs)^2, ...)

  return(mse)
}

#' @describeIn get.MSE Implementation for data frames
#'
#' @details
#' In this implementation, the arguments are numeric
#' data frames of the same dimensions.
#'
#' @param sim A data frame with the predicted values
#' @param obs A data frame with the observed values
#' @param ... Variables passed to an internal call to the
#'     \code{colMeans()} function
#'
#' @return A vector with column-wise measures of the
#'     mean squared error between the specified data frames
#'
#' @export
#'
get.MSE.data.frame <- function(sim, obs, ...)
{
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  get.MSE.matrix(sim = sim, obs = obs, ...)

}
