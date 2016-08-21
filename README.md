# dtstudent: Discrete Truncated Student-t Distribution for R

[![GPL ≥ 3](https://img.shields.io/badge/GPL-%E2%89%A5%203-brightgreen.svg)](https://cran.r-project.org/web/licenses/GPL-3)

_Matthew Kay, University of Michigan <mjskay@umich.edu>_<br>

This is a package implementing a discrete, truncated Student-t distribution, which
I have found useful in robustly modeling people's responses on rating scales, or
even as an alternative to a Beta distribution in modeling people's estimates of
probabilities in visualizations.

It includes the customary `[dpqr]dtstudent()` functions, and also (somewhat hackishly)
defines support for the discrete truncated Student-t distribution for use with
the `map2stan` function in Richard McElreath's [rethinking](https://github.com/rmcelreath/rethinking)
package.

## Installation

You can install the latest development version from GitHub with these R commands:

```r
install.packages("devtools")
devtools::install_github("mjskay/dtstudent")
```

## Example

TBD

## Example with `rethinking` package

TBD

