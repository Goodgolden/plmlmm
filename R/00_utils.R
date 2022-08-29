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
# }}}---------------------------------------------------------------------------



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

# }}}---------------------------------------------------------------------------



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
# }}}---------------------------------------------------------------------------


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
# }}}---------------------------------------------------------------------------


## 0.5 not in {{{---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)
# }}}---------------------------------------------------------------------------



## 0.6 mahalanobis p {{{--------------------------------------------------------

#' Title
#'
#' @param Dmatrix
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
mahalanobis_p <- function(Dmatrix,
                         alpha) {

  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    ## Mahalanobis distance using the chisq pvalues
    as.matrix() %>%
    t()

  matching <<- mahalanobis(df, colMeans(df), cov(df)) %>%
    # mahalanobis(colMeans(.), cov(.)) %>%
    as.data.frame() %>%
    mutate(pvalue = pchisq(., df = def, lower.tail = FALSE)) %>%
    filter(pvalue >= alpha) %>%
    dplyr::select(malahanobis = 1, pvalue = 2) %>%
    rownames_to_column("id")
  # arrange(diff) %>%

  # slice(1:match_num) %>%
  # inner_join(obs_data, by = "id")

  return(matching)
}
## }}}--------------------------------------------------------------------------


## 0.7 mahalanobis_n {{{--------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param match_num
#'
#' @return
#' @export
#'
#' @examples
mahalanobis_n <- function(Dmatrix,
                          match_num) {
  matching <<- Dmatrix %>%
    as.matrix() %>%
    t() %>%
    mahalanobis(colMeans(.), cov(.)) %>%
    # mahalanobis(colMeans(.), cov(.)) %>%
    ## Now it is a vector of Mahalanobis distance
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%

    slice(1:match_num)
    # inner_join(dataset, by = "id")

  return(matching)
}
## }}}--------------------------------------------------------------------------


## 0.8 euclidean_n {{{----------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param match_num
#'
#' @return
#' @export
#'
#' @examples
euclidean_n <- function(Dmatrix,
                        match_num) {
  matching <<- Dmatrix %>%
    apply(2, norm, type = "2") %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%
    slice(1:match_num)

  return(matching)
}
## }}}--------------------------------------------------------------------------


## 0.9 singletime_n {{{---------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param match_time
#' @param match_num
#'
#' @return
#' @export
#'
#' @examples
singletime_n <- function(Dmatrix,
                        match_time,
                        match_num) {
  matching <<- Dmatrix %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff)) %>%
    slice(1:match_num)

  return(matching)
}
## }}}--------------------------------------------------------------------------

