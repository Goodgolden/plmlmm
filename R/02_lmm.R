## 2.1 lmm dynamic prediction {{{-----------------------------------------------
#' Title
#'
#' @param fitting
#' @param baseline
#'
#' @return
#' @export
#'
#' @examples
lmm_pred <- function(fitting,
                     test_baseline) {

  lmmpred_09 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    # times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.9,
    interval = "prediction",
    seed = 555)



  lmmpred_05 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    # times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.5,
    interval = "prediction",
    seed = 555)

  lmm_results <-
    dplyr::select(lmmpred_05,
                  id, time,
                  # observed = ht,
                  pred,
                  centile25 = low,
                  centile75 = upp) %>%
    full_join(dplyr::select(lmmpred_09,
                            id, time,
                            # observed = ht,
                            pred,
                            centile05 = low,
                            centile95 = upp),
              by = c("id", "time", "pred")) %>%
    na.omit()

  return(lmm_results)
}
## }}}--------------------------------------------------------------------------


## 2.2 plot lmm_pred results {{{------------------------------------------------



### }}}-------------------------------------------------------------------------


