#' Night Night function
#' The function create random time gaps bewtween scraping requests to site to simulate normal traffic behavior and not saturate the site.
#'
#' @param periods default waiting periods interval that will be randomly chosen
#'
#' @return message
#' @export
#'
#' @examples
#' nytnyt()
#'
nytnyt <- function(periods = c(1,1.5)) {

          tictoc <- runif(1, periods[1], periods[2])
          Sys.sleep(tictoc)
          message(paste0(Sys.time()), "- Sleeping for ", round(tictoc, 2), " seconds \n")

}

