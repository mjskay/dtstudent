#gamlss is broken and trun.r / trun.q don't work if TF/etc aren't in the global environment
#(and I don't want to burden people with the equivalent of calling library(gamlss) since that
#pollutes the global environment with a whole bunch of garbage)
#' @export
TF <- gamlss.dist::TF
#' @export
qTF <- gamlss.dist::qTF
#' @export
pTF <- gamlss.dist::pTF
#' @export
dTF <- gamlss.dist::dTF
#' @export
rTF <- gamlss.dist::rTF
#' @export
TF2 <- gamlss.dist::TF2
#' @export
qTF2 <- gamlss.dist::qTF2
#' @export
pTF2 <- gamlss.dist::pTF2
#' @export
dTF2 <- gamlss.dist::dTF2
#' @export
rTF2 <- gamlss.dist::rTF2
