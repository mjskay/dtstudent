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
#'           and as \code{nu} goes to \code{Inf}, the underlying distribution becomes Normal.
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

#' @rdname dtstudentSD
#' @export
#' @import gamlss.dist
ddtstudentSD = function(x, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log = FALSE) {
    stopifnot(all(lower <= upper))
    nu = nuprime + 2

    tolerance = sqrt(.Machine$double.eps)
    `width/2` = width/2
    p = ifelse(
        lower <= x & x <= upper & abs(x - round(x / width) * width) < tolerance,
        (pTF2(x + `width/2`, mu, sigma, nu) - pTF2(x - `width/2`, mu, sigma, nu)) /
            (pTF2(upper + `width/2`, mu, sigma, nu) - pTF2(lower - `width/2`, mu, sigma, nu)),
        0)

    if (log) log(p) else p
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
pdtstudentSD = function(q, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE) {
    stopifnot(all(lower <= upper))
    nu = nuprime + 2

    q = floor(q / width) * width
    `width/2` = width/2
    p = (pTF2(q, mu, sigma, nu) - (pTF2(lower - `width/2`, mu, sigma, nu))) /
        (pTF2(upper + `width/2`, mu, sigma, nu) - pTF2(lower - `width/2`, mu, sigma, nu))

    if (!lower.tail) p = 1 - p
    if (log.p) log(p) else p
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
#' @import gamlss.tr
#' @importFrom magrittr %$%
qdtstudentSD = function(p, nuprime, mu, sigma, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE) {
    nu = nuprime + 2

    data.frame(p, nu, mu, sigma, width, lower, upper) %$% {
        stopifnot(all(lower <= upper))

        qt = trun.q(cbind(lower - width/2, upper + width/2), family = "TF2", type="both", varying=TRUE)
        x = qt(p, mu = mu, sigma = sigma, nu = nu, log.p = log.p, lower.tail = lower.tail)
        round(x / width) * width
    }
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
#' @import gamlss.tr
#' @importFrom magrittr %$%
rdtstudentSD = function(n, nuprime, mu, sigma, width = 1, lower = 0, upper = 100) {
    nu = nuprime + 2

    data.frame(1:n, nu, mu, sigma, width, lower, upper) %$% {
        stopifnot(all(lower <= upper))

        rt = trun.r(cbind(lower - width/2, upper + width/2), family = "TF2", type="both", varying=TRUE)
        round(rt(n, mu, sigma, nu) / width) * width
    }
}
