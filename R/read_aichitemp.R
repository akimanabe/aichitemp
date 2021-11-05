read_aichitemp <- function(fname) {
  readr::read_csv(fname, col_names = FALSE) %>%
    dplyr::slice(-c(1:4)) %>%
    magrittr::set_colnames(c("Day", c(1:12))) %>%
    tidyr::pivot_longer(-c(Day), names_to = "Month", values_to = "Temp") %>%
    dplyr::mutate(Day = stringi::stri_trans_nfkc(Day),
                  Day = stringr::str_remove(Day, "日"),
                  Year = extract_year(fname),
                  Location = extract_location(fname)) %>%
    dplyr::mutate_at(., .vars = c("Day", "Month", "Temp"),
                     .funs = function(x) as.numeric(x)) %>%
    dplyr::mutate(Date =
                    lubridate::as_date(
                      paste(Year, Month, Day, sep = "/"))) %>%
    dplyr::arrange(Date) %>%
    dplyr::select(Date, Year, Month, Day, Location, Temp)
}

extract_location <-
  function(fname) {
    dat <- readr::read_csv(fname) %>%
      suppressWarnings() %>%
      suppressMessages()

    dat %>%
      dplyr::select(1) %>%
      dplyr::slice(1) %>%
      dplyr::pull()
  }

extract_year <-
  function(fname) {
    dat <- readr::read_csv(fname) %>%
      suppressWarnings() %>%
      suppressMessages()

    dat %>%
      dplyr::select(6) %>%
      dplyr::slice(1) %>%
      dplyr::pull() %>%
      stringr::str_extract(., "[0-9]{4}") %>%
      as.numeric()
  }
