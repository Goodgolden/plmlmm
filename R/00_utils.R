# Discription ------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n Welcome to my package; this is a package
                        developed for Randy Jin's MS thesis")
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

# Miscellious ------------------------------------------------------------------

## 0.1 norm L2 {{{--------------------------------------------------------------
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


## 0.2 not all na {{{-----------------------------------------------------------
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



## 0.3 not any na {{{-----------------------------------------------------------
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



## 0.4 not in {{{---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)


# Distance ---------------------------------------------------------------------

## 0.5 euclidean_df ----------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param center
#'
#' @return
#' @export
#'
#' @examples
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


## 0.6 mahalanobis_df --------------------------------------------------------
#' Title
#'
#' @param Dmatrix
#' @param center
#'
#' @return
#' @export
#'
#' @examples
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



## 0.7 singletime_n {{{---------------------------------------------------------
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
                      match_time,
                      center) {
  matching <<- as.data.frame(Dmatrix) %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    mutate(diff = diff - as.numeric(center)) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff))

  return(matching)
}



## 0.8 mahalanobis p ------------------
mahalanobis_p <- function(Dmatrix,
                          alpha) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    ## Mahalanobis distance using the chisq pvalues
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)
  pvalue <- pchisq(value, df = def, lower.tail = FALSE)
  matching <<- data.frame(diff = value,
                          pvalue = as.numeric(pvalue)) %>%
    arrange(desc(pvalue)) %>%
    rownames_to_column("id") %>%
    as.data.frame() %>%
    dplyr::filter(pvalue >= alpha)

  # slice(1:match_num) %>%
  # inner_join(obs_data, by = "id")
  return(matching)
}

## 0.9 mahalanobis_n ---------------
mahalanobis_n <- function(Dmatrix,
                          match_num) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)

  matching <<- Dmatrix %>%
    t() %>%
    as.data.frame() %>%
    mutate(value = value) %>%
    arrange(value) %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    slice(1:match_num)

  return(matching)
}
