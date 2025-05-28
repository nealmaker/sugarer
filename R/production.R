#' Maple sap yield
#'
#' gallons sap produced per season for individual trees
#'
#' @param dbh numeric vector of trees' diameters at breast height (inches)
#' @param spp character vector of trees' species
#'
#' @returns vector of sap volumes (gallons) for each tree
#' @references after Rademacher et al 2023, but simplified as a linear relationship
#' @export
sap_yield <- function(dbh, spp) {
  maple <- spp %in% c("hard maple", "red maple")
  y <- rep(0, length(dbh))
  y[maple] <- .7247 * dbh[maple] - 1.6907
  y[spp == "hard maple"] <- y[spp == "hard maple"] + 0.43324
  y[spp == "red maple"] <- y[spp == "red maple"] - 1.7330
  return(y)
}

# Avg. sugar content (Brix) for a tree
# after Rademacher et al 2023; Larochelle et al 1998; Wild and Yana 2015:
#' Maple sugar content
#'
#' sap sugar content for individual trees
#'
#' @param dbh numeric vector of trees' diameters at breast height (inches)
#' @param spp character vector of trees' species
#'
#' @returns vector of sap sugar contents (brix) for each tree
#' @references after Rademacher et al 2023; Larochelle et al 1998; Wild and Yana 2015
#' @export
sugar_content <- function(dbh, spp) {
  maple <- spp %in% c("hard maple", "red maple")
  c <- rep(0, length(dbh))
  c[maple] <- .0254 * dbh[maple] + 2.52
  c[spp == "hard maple"] <- c[spp == "hard maple"] + 0.1
  c[spp == "red maple"] <- c[spp == "red maple"] - 0.41
  return(c)
}


#' Maple syrup yield
#'
#' gallons syrup produced per season for individual trees
#'
#' @param dbh numeric vector of trees' diameters at breast height (inches)
#' @param spp character vector of trees' species
#'
#' @returns vector of syrup volumes (gallons) for each tree
#' @references after Rademacher et al 2023, but simplified as a linear relationship
#' @export
syrup_yield <- function(dbh, spp) {
  (sap_yield(dbh, spp) * sugar_content(dbh, spp)) / 66.9
  # 66.9 is legal brix of syrup in VT, it's only 66 elsewhere
}

# # Alternate syrup Yield model
# # from Isselhardt et al 2018 (Tree Size Matters, Maple Syrup Digest)
# # based on 2 years' data at Proctor
# s_iss <- function(dbh) .056 * dbh - .059
