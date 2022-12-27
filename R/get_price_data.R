
#' Scraping price data from SNIIM
#' The function webscrape the tables containing fish price data from the SNIIM using
#' year, month, starting day (day1), and last day (day2) that is wanted. The structure of the website for now
#' only allows single year and month queries, therefore it needs to be looped in some whay to be obtained.
#' Sometimes the URL returns an error depending on the dates interted. This is caused by the somewhat inestable structure
#' of the website. Using different dates might solve the issue. By default it scrapes today's prices.
#'
#' @param year the year input to scrape, must be numeric, defaults to current year
#' @param month the month input to scrape, must be numeric, defaults to current month
#' @param day1 the starting day input to scrape, must be numeric, defaults to current day
#' @param day2 the final day input to scrape, must be numeric, defaults to current day, can be different than day1
#'
#' @return a data.frame object
#' @export
#' @importFrom rlang .data
#' @importFrom stats runif
#' @importFrom utils data
#'
#' @examples
#' # Scraping prices from a custom date
#'\dontrun{
#' get_price_data(year = 2022, month = 02, day1 = 01, day2 = 22)
#'}
get_price_data <- function(year = as.numeric(lubridate::year(Sys.Date())), month = as.numeric(lubridate::month(Sys.Date())), day1 = as.numeric(lubridate::day(Sys.Date()-1)), day2 = as.numeric(lubridate::day(Sys.Date()))) {
  suppressWarnings({
    fecha <- origen <- grp <- fuente <- scrp_id <- type <- category <- producto <- pfrec <- sp_dictionary <- short_names <- ID <- product_short_name <- NULL
    product <- IDproduct <-  . <- NULL

  data("sp_dictionary")

  pescado_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=32&y=14")
  crustaceos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=37&y=17")
  moluscos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=42&y=15")
  filete_otros_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=41&y=20")

  #create list of websites
  weblist <- list(
    pescado = data.frame(
      url = pescado_origin,
      name = "pescado"),
    crustaceos = data.frame(
      url = crustaceos_origin,
      name = "crustaceos"
    ),
    moluscos = data.frame(
      url = moluscos_origin,
      name = "moluscos"
    ),
    filetes = data.frame(
      url = filete_otros_origin,
      name = "filetes"
    ))
  ## Scraping the website as a single query
  tryCatch({
    scrap <-  lapply(weblist, scraping_sniim)
  },
  error = function(e) {
    message('Could not scrape date, possibly URL error')
    message(e)
    NULL
  })

  # Scraping the website as a single query category

  results_origin <- do.call(rbind, scrap)

  results_origin <- janitor::clean_names(janitor::row_to_names(results_origin, row_number = 1))
  results_origin <- results_origin %>%
    dplyr::filter(
    !stringr::str_detect(.$fecha, c("Fuente")),
    !stringr::str_detect(.$fecha, "Fecha")) %>%
    dplyr::mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>%
    dplyr::mutate(scrp_id = 1:length(.$fecha), .before = fecha) %>%
    dplyr::mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>%
    dplyr::mutate(type = "origin", .before = fecha) %>%
    dplyr::mutate(fuente = origen)


  pescado_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=36&y=24")
  crustaceos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=25&y=17")
  moluscos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=50&y=9")
  filete_otros_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=28&y=17")

  #create list of websites
  weblist_destiny <- list(
    pescado = data.frame(
      url = pescado_destiny,
      name = "pescado"),
    crustaceos = data.frame(
      url = crustaceos_destiny,
      name = "crustaceos"
    ),
    moluscos = data.frame(
      url = moluscos_destiny,
      name = "moluscos"
    ),
    filetes = data.frame(
      url = filete_otros_destiny,
      name = "filetes"
    ))

  # Scraping the website as a single query category -------------------------------------------------------

  tryCatch({
    scrap_destiny <-  lapply(weblist, scraping_sniim)
  },
  error = function(e) {
    message('Could not scrape date, possibly URL error')
    message(e)
    NULL
  })

  results_destiny <- do.call(rbind, scrap_destiny)

  results_destiny <- janitor::clean_names(janitor::row_to_names(results_destiny, row_number = 1))

  results_destiny <- results_destiny %>%
    dplyr::mutate(grp = stringr::str_detect(string = fecha, pattern = ":"),
           fuente = ifelse(grp, sub('.*:', '', fecha), NA_real_)) %>%
    tidyr::fill(fuente) %>%
    dplyr::filter(!grp) %>%
    dplyr::select(-grp) %>%
    dplyr::mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>%
    dplyr::mutate(scrp_id = 1:length(.$fecha), .before = fecha) %>%
    dplyr::mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>%
    dplyr::mutate(type = "destiny", .before = fecha)



  results <- rbind(results_origin, results_destiny) %>%
    dplyr::select(scrp_id, date = fecha, type, category,
           product = producto,
           origin_name = origen, market = fuente,
           pmin, pmax, price_mxn = pfrec) %>%
    dplyr::left_join(., sp_dictionary, by = "product") %>%
    dplyr::rename(product_short_name = short_names, IDproduct = ID) %>%
    dplyr::relocate(product_short_name, .after = product) %>%
    dplyr::relocate(IDproduct, .before = product)

  return(results)
  })
  message("You succesfully webscraped SNIIM! \n\n\n Dates scraped: ", paste0(day1, "-", month,"-", year), "\n to:", paste0(day2, "-", month, "-", year), "\n")

}



