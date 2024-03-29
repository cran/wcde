#' Pull multiple vectors for a given indicator, scenarios and .Rdata file names
#'
#' @description Requires a working internet connection. Intended for internal use.
#' @param indicator One character string based on the `name` column in the `wic_indicators` data frame, representing the variable to be interested.
#' @param scenario Vector with a numbers corresponding the scenarios. See details in `wcde` for more information.
#' @param country_code Vector of length one or more of country numeric codes based on ISO 3 digit numeric values.
#' @param server Character string for server to download from. Defaults to `iiasa`, but can use `github` if IIASA server is down.
#' @param version Character string for version of projections to obtain. Defaults to `wcde-v3`, but can use `wcde-v2` and `wcde-v1`. Scenario and indicator availability vary between versions.
#'
#' @return A tibble with multiple columns.
#' @keywords internal
#' @export
get_wcde_single <- function(indicator = NULL, scenario = 2, country_code = NULL,
                            server = NULL, version = NULL){
  # scenario = c(1, 3); indicator = "etfr"; country_code = c(40, 100)
  # scenario = 2; indicator = "e0"; country_code = "900"
  # server = "github"; version = "wcde-v1"
  if(length(indicator) > 1){
    message("can only get data on one indicator at a time, taking first indicator given")
    indicator <- indicator[1]
  }
  if(!wcde_location(country_code = country_code, version = version)){
    stop("data for one of the country codes not available")
  }
  # if(length(scenario) > 1){
  #   message("can only get data on one scenario at a time, taking first scenario given")
  #   scenario <- scenario[1]
  # }
  wic_scenarios_v <- wcde::wic_scenarios %>%
    tidyr::pivot_longer(dplyr::contains("wcde"), names_to = "v", values_to = "avail") %>%
    dplyr::filter(v == version,
                  avail) %>%
    dplyr::select(1:3)

  if(!all(scenario %in% wic_scenarios_v$scenario)){
    stop("scenario must be an integer in wic_scenarios, and available for version specified (default will be wcde-v3)")
  }
  v0 <- wcde::wic_indicators %>%
    dplyr::filter(indicator == {{indicator}}) %>%
    dplyr::select_if(is.logical) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "v", values_to = "avail") %>%
    dplyr::filter(avail == 1) %>%
    dplyr::pull(v)

  if(length(v0) == 0 & stringr::str_detect(string = indicator, pattern = "pop-")){
    v0 <- stringr::str_split(string = indicator, pattern = "-")[[1]]
    v0 <- v0[-1]
    v0 <- stringr::str_replace(string = v0, pattern = "edattain", replacement = "edu")
    v0 <- stringr::str_replace(string = v0, pattern = "total", replacement = "")
    v0 <- v0[!nchar(v0)==0]
  }

  if(!any(v0 == "period"))
    v0 <- c(v0, "year")
  if(any(stringr::str_detect(string = v0, pattern = "bage")))
    v0 <- stringr::str_replace(string = v0, pattern = "bage", "age")
  if(any(stringr::str_detect(string = v0, pattern = "sage")))
    v0 <- stringr::str_replace(string = v0, pattern = "sage", "age")

  v1 <- c(v0, country_code)

  d0 <- tidyr::expand_grid(scenario, indicator, v1) %>%
    dplyr::rename(country_code = 3)

  read_with_progress <- function(f){
    pb$tick()
    # message(f)
    f %>%
      url() %>%
      readRDS()
    # readr::read_csv(f, col_types = readr::cols(), guess_max = 1e5, progress = FALSE)
  }

  # server <- match.arg(server)
  # version <- match.arg(version)
  if(server == "search-available" | is.null(server)){
    server <- dplyr::case_when(
      RCurl::url.exists("https://wicshiny2023.iiasa.ac.at/wcde-data/") ~ "iiasa",
      # RCurl::url.exists("https://wicshiny.iiasa.ac.at/wcde-data/") ~ "iiasa",
      RCurl::url.exists("https://github.com/guyabel/wcde-data/") ~ "github",
      RCurl::url.exists("https://shiny.wittgensteincentre.info/wcde-data/") ~ "1&1",
      TRUE ~ "none-available"
    )
  }

  server_url <- dplyr::case_when(
    server == "iiasa" ~ "https://wicshiny2023.iiasa.ac.at/wcde-data/",
    server == "iiasa-local" ~ "../wcde-data/",
    server == "github" ~ "https://github.com/guyabel/wcde-data/raw/master/",
    server == "1&1" ~ "https://shiny.wittgensteincentre.info/wcde-data/",
    TRUE ~ server)

  pb <- progress::progress_bar$new(total = nrow(d0))
  pb$tick(0)
  d0 <- d0 %>%
    dplyr::mutate(u = paste0(server_url, version, "-single/", scenario, "/",
                             indicator, "/", country_code, ".rds")) %>%
    dplyr::mutate(d = purrr::map(.x = u, .f = ~read_with_progress(f = .x))) %>%
    dplyr::group_by(scenario) %>%
    dplyr::reframe(dplyr::bind_cols(d)) %>%
    dplyr::ungroup()
  pb$terminate()

  cc <- which(names(d0) %in% country_code)
  d1 <- tidyr::pivot_longer(data = d0, cols = dplyr::all_of(cc),
                            names_to = "isono", values_to = {{indicator}})
  return(d1)
}
