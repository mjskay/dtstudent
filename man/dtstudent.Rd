% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtstudent.R
\name{dtstudent}
\alias{ddtstudent}
\alias{dtstudent}
\alias{pdtstudent}
\alias{qdtstudent}
\alias{rdtstudent}
\title{Discrete Truncated Student-t Distribution, Scale Parameterization}
\usage{
ddtstudent(x, nu, mu, scale, width = 1, lower = 0, upper = 100,
  log = FALSE)

pdtstudent(q, nu, mu, scale, width = 1, lower = 0, upper = 100,
  log.p = FALSE, lower.tail = TRUE)

qdtstudent(p, nu, mu, scale, width = 1, lower = 0, upper = 100,
  log.p = FALSE, lower.tail = TRUE)

rdtstudent(n, nu, mu, scale, width = 1, lower = 0, upper = 100)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{nu}{Degrees of freedom parameter of underlying Student-t distribution. \code{nu > 0},
and as \code{nu} goes to \code{Inf}, the underlying distribution becomes Normal.}

\item{mu}{mode / mean of underlying Student-t distribution.}

\item{scale}{scale of underlying Student-t distribution.}

\item{width}{width of the intervals used for discretization.}

\item{lower}{lower truncation point.}

\item{upper}{upper truncation point.}

\item{log, log.p}{logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.}

\item{lower.tail}{logical; if \code{TRUE} (default), probabilities are \emph{P[X ≤ x]} otherwise,
\emph{P[X > x]}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations.}
}
\description{
Discrete truncated Student-t distribution.
}
\author{
Matthew Kay
}

