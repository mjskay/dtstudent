# [dpqr]dtstudentSD() functions
#
# Author: mjskay
###############################################################################


#' Discrete Truncated Student-t Distribution, Standard Deviation Parameterization
#'
#' Discrete truncated Student-t distribution parameterized by standard deviation.
#'
#' @name dtstudentSD
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n	number of observations.
#' @param nuprime \code{degrees of freedom - 2} of underlying Student-t distribution. \code{nuprime > 0},
#'           and as \code{nuprime} goes to \code{Inf}, the underlying distribution becomes Normal.
#' @param mu mode / mean of underlying Student-t distribution.
#' @param sigma standard deviation of underlying Student-t distribution.
#' @param width width of the intervals used for discretization.
#' @param lower lower truncation point.
#' @param upper upper truncation point.
#' @param log,log.p	logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \emph{P[X ≤ x]} otherwise,
#'                   \emph{P[X > x]}.
#' @rdname dtstudentSD
#' @author Matthew Kay
NULL

sigma_to_scale = function(nuprime, sigma) list(
    nu = nuprime + 2,
    #lim(nuprime->Inf) sqrt(nuprime/(nuprime+2))*sigma = sigma
    scale = ifelse(nuprime == Inf, sigma, sqrt(nuprime/(nuprime+2))*sigma)
)

#' @rdname dtstudentSD
#' @export
#' @importFrom magrittr %$%
ddtstudentSD = function(x, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log = FALSE)
    sigma_to_scale(nuprime, sigma) %$% ddtstudent(x, nu, mu, scale, width, lower, upper, log)

#' @rdname dtstudentSD
#' @export
pdtstudentSD = function(q, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE)
    sigma_to_scale(nuprime, sigma) %$% pdtstudent(q, nu, mu, scale, width, lower, upper, log.p, lower.tail)

#' @rdname dtstudentSD
#' @export
qdtstudentSD = function(p, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE)
    sigma_to_scale(nuprime, sigma) %$% qdtstudent(p, nu, mu, scale, width, lower, upper, log.p, lower.tail)

#' @rdname dtstudentSD
#' @export
rdtstudentSD = function(n, nuprime, mu, sigma, width = 1, lower = 0, upper = 100)
    sigma_to_scale(nuprime, sigma) %$% rdtstudent(n, nu, mu, scale, width, lower, upper)
