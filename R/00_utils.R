.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n Welcome to my package; this is a package developed for Randy Jin's MS thesis")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Randy",
    devtools.desc.author = "Randy Jin <xin.2.jin@cuanschutz.edu> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}



## 0.1 norm L2 {{{------------------
#' Title L2 or other norms
#'
#' @param v
#'
#' @return
#' @export
#'
norm2 <- function(v) {
  sqrt(sum(v^2))
}

## Add other type of norms???




## 0.2 soft {{{-----------------------------------------------------------------
#' Title soft activation function
#' @details this is the equation
#'
#' @param M
#' @param v
#'
#' @return
#' @export
#'
soft_fun <- function(M, v) {
  soft <- M
  ## number of cols
  p <- dim(M)[2]
  for (j in 1 : p) {
    M.j <- M[, j]
    temp.j <- abs(M.j) - v
    temp.j[temp.j < 0] <- 0
    soft[, j] <- sign(M.j) * temp.j
  }
  return(soft)
}





## 0.3 not all na {{{-----------------------------------------------------------
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
not_all_na <- function(x) {
  any(!is.na(x))
}



## 0.4 not any na {{{-----------------------------------------------------------
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
not_any_na <- function(x) {
  all(!is.na(x))
}



## 0.5 not in {{{---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)


# Distance ---------------------------------------------------------------------

## 0.6 euclidean_df ----------------------------------------------------------
euclidean_df <- function(Dmatrix,
                         center) {
  matching <<- as.data.frame(Dmatrix - center) %>%
    apply(2, norm, type = "2") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff)

  return(matching)
}


## 0.7 mahalanobis_df --------------------------------------------------------
mahalanobis_df <- function(Dmatrix,
                           center) {

  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    as.matrix() %>%
    t()

  x <- sweep(df, 2L, center)
  invcov <- MASS::ginv(cov(df))

  value <- rowSums(x %*% invcov * x)
  pvalue <- pchisq(value, df = def, lower.tail = FALSE)
  matching <<- data.frame(diff = value,
                          pvalue = pvalue) %>%
    arrange(desc(pvalue)) %>%
    rownames_to_column("id")

  return(matching)
}



## 0.9 singletime_n {{{---------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param match_time
#'
#' @return
#' @export
#'
#' @examples
single_df <- function(Dmatrix,
                        match_time) {
  matching <<- Dmatrix %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff))

  return(matching)
}


