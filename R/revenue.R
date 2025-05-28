#' Gross maple revenue
#'
#' @param syrup_volume vector of syrup volumes (gallons) produced per tree
#'
#' @returns vector of gross revenues (USD present value) attributable to each
#'   tree
#' @export
gross_maple_revenue <- function(syrup_volume){
  return(syrup_volume * 32)
  # $32/gallon is typically cited average, accounting for bulk sales and
  # direct to consumer sales
}
