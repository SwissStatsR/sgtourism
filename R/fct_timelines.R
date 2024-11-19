#' Get start of timelines (min length: 10 yrs)
#'
#' @param yr1 year 1
#' @param yr2 year 2
#'
#' @return start yr for timeline plots
#' @keywords internal
#' @export
get_start_tl <- function(yr1, yr2){
  min(as.numeric(yr1), as.numeric(yr2), max(as.numeric(yr1), as.numeric(yr2)) - 10)
}

#' Get end of timelines (ending at last complete year )
#'
#' @param yr1 year 1
#' @param yr2 year 2
#'
#' @return end yr for timeline plots
#' @keywords internal
#' @export
get_end_tl <- function(yr1, yr2){
  today <- Sys.Date()
  if (as.numeric(format(today, "%m")) > 2 ||
      (as.numeric(format(today, "%m")) == 2
       && as.numeric(format(today, "%d")) > 20)){
    min(as.numeric(format(Sys.Date(), "%Y")) - 1, max(yr1, yr2))
  } else {
    min(as.numeric(format(Sys.Date(), "%Y")) - 2, max(yr1, yr2))
    }
  }
