# Avoid the devtools::check() NOTE that the . used in piping is a non-declared
# variable
utils::globalVariables(":=")
utils::globalVariables(".data")

# devtools::check() does not realize that package withr is used in examples
# Strategy suggested in R packages (2e), 11.4.1.1 How to not use a package in
# Imports
ignore_unused_imports <- function() {
  dirmult::simPop(n = 2, theat = 0.03)
  ordinal::plgamma(1:5, lambda = .5)
  simstudy::genCatFormula(n = 2)
}
