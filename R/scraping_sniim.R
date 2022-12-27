#' Scraping price data website
#' The function loops into all urls from fishery prices with the dates indicated and gets a list of data.frames
#'
#' @param weblist a url from the sniim market page to be scraped
#'
#' @return a data.frame
#' @export
#' @importFrom rlang .data
#'
#' @examples
#'\dontrun{
#' scraping_sniim(url)
#'}
#'
scraping_sniim <- function(weblist) {
  . <- NULL
      scrap <-  rvest::read_html(curl::curl(
        weblist$url,
        handle = curl::new_handle("useragent" = "Mozilla/5.0")
      )) %>%
        rvest::html_nodes(xpath = '/html/body/div[1]/center/table[2]') %>%
        rvest::html_table() %>%
        .[[1]] %>%
        as.data.frame() %>%
        dplyr::mutate(category = c("category", rep(weblist$name, length(.$X1) -
                                                     1)))
      message(paste0("\n Webscraping ", weblist$name, "\n"))

      nytnyt()
      return(scrap)
  }



