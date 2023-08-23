# Functions in For-loop --------------------------------------------------------

## 2.1 distance_df -------------------------------------------------------------
#' Title Distance calculation
#'
#' @param lb_train
#' @param lb_test_ind
#' @param match_methods
#' @param match_time
#' @param id_var
#' @param outcome_var
#' @param time_var
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
distance_df <- function(lb_train,
                        lb_test_ind,
                        match_methods = c("euclidean", "mahalanobis", "single"),
                        match_time = NULL,
                        id_var,
                        outcome_var,
                        time_var,
                        ...) {
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## the matching subset
  lb_sub1 <- lb_train %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = {{ outcome_var }}) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)

  center = as.numeric(unlist(lb_test_ind[, 3]))

  if (match_methods == "euclidean") {
    dist_df <<- euclidean_df(Dmatrix = lb_sub1,
                             center = center)
    cat("\n using euclidean distance\n")
  }

  if (match_methods == "mahalanobis") {
      dist_df <<- mahalanobis_df(Dmatrix = lb_sub1,
                                 center = center)
      cat("\n using mahalanobis distance\n")
  }

  # if (match_methods == "single") {
  #   if (is.null(match_time)) {
  #     stop("provide matching time points for single-time PLM methods")
  #   }
  #   dist_df <<- single_df(Dmatrix = lb_sub2,
  #                              match_time = match_time)
  #   cat("\n using single critical time point matching \n")
  # }

  return(distance = dist_df)
}


## 2.2 matching ----------------------------------------------------------------

#' Title Finding the matches subset
#'
#' @param distance_df
#' @param train
#' @param test_one
#' @param id_var
#' @param outcome_var
#' @param time_var
#' @param match_alpha
#' @param match_number
#' @param match_plot
#'
#' @return
#' @export
#'
#' @examples
match <- function(distance_df = ddd,
                  train = train,
                  test_one,
                  id_var,
                  outcome_var,
                  time_var,
                  match_alpha = NULL,
                  match_number = NULL,
                  match_plot = FALSE) {

  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)


  if (is.null(match_alpha)) {
    data <- distance_df %>%
      slice(1:match_num) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (is.null(match_number)) {
    data <- distance_df %>%
      filter(pvalue >= match_alpha) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (match_plot == TRUE) {

    matching_plot <- ggplot() +
      geom_line(data = data, aes(x = {{ time_var }}, y = {{ outcome_var }},
                    group = {{ id_var }}),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = test_one,
                aes(x = {{time_var}}, y = {{outcome_var}}),
                color = "darkblue",
                linewidth = 1) +
      theme_bw()

    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }

  return(list(subset = data,
              plot = matching_plot,
              id = unique(test_one[[as_label(enquo(id_var))]]),
              alpha = match_alpha,
              number = match_number))
}

