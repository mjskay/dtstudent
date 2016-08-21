# [dpqr]dtstudent() functions
#
# Author: mjskay
###############################################################################


#' Discrete Truncated Student-t Distribution, Scale Parameterization
#'
#' Discrete truncated Student-t distribution.
#'
#' @name dtstudent
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n	number of observations.
#' @param nu Degrees of freedom parameter of underlying Student-t distribution. \code{nu > 0},
#'           and as \code{nu} goes to \code{Inf}, the underlying distribution becomes Normal.
#' @param mu mode / mean of underlying Student-t distribution.
#' @param scale scale of underlying Student-t distribution.
#' @param width width of the intervals used for discretization.
#' @param lower lower truncation point.
#' @param upper upper truncation point.
#' @param log,log.p	logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are \emph{P[X ≤ x]} otherwise,
#'                   \emph{P[X > x]}.
#' @rdname dtstudent
#' @author Matthew Kay
NULL

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
ddtstudent = function(x, nu, mu, scale, width = 1, lower = 0, upper = 100, log = FALSE) {
    stopifnot(all(lower <= upper))

    tolerance = sqrt(.Machine$double.eps)
    `width/2` = width/2
    p = ifelse(
        lower <= x & x <= upper & abs(x - round(x / width) * width) < tolerance,
        (pTF(x + `width/2`, mu, scale, nu) - pTF(x - `width/2`, mu, scale, nu)) /
            (pTF(upper + `width/2`, mu, scale, nu) - pTF(lower - `width/2`, mu, scale, nu)),
        0)

    if (log) log(p) else p
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
pdtstudent = function(q, nu, mu, scale, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE) {
    stopifnot(all(lower <= upper))

    q = floor(q / width) * width
    `width/2` = width/2
    p = (pTF(q, mu, scale, nu) - (pTF(lower - `width/2`, mu, scale, nu))) /
        (pTF(upper + `width/2`, mu, scale, nu) - pTF(lower - `width/2`, mu, scale, nu))

    if (!lower.tail) p = 1 - p
    if (log.p) log(p) else p
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
#' @import gamlss.tr
#' @importFrom magrittr %$%
qdtstudent = function(p, nu, mu, scale, width = 1, lower = 0, upper = 100, log.p = FALSE, lower.tail = TRUE) {
    data.frame(p, nu, mu, scale, width, lower, upper) %$% {
        stopifnot(all(lower <= upper))

        qt = trun.q(cbind(lower - width/2, upper + width/2), family = "TF", type="both", varying=TRUE)
        x = qt(p, mu = mu, sigma = scale, nu = nu, log.p = log.p, lower.tail = lower.tail)
        round(x / width) * width
    }
}

#' @rdname dtstudent
#' @export
#' @import gamlss.dist
#' @import gamlss.tr
#' @importFrom magrittr %$%
rdtstudent = function(n, nu, mu, scale, width = 1, lower = 0, upper = 100) {
    data.frame(1:n, nu, mu, scale, width, lower, upper) %$% {
        stopifnot(all(lower <= upper))

        rt = trun.r(cbind(lower - width/2, upper + width/2), family = "TF", type="both", varying=TRUE)
        round(rt(n, mu, scale, nu) / width) * width
    }
}
