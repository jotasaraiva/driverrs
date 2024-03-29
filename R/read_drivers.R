#' Scrape licensed drivers data from the Brazilian Ministry of Transport.
#'
#' @return A `tibble` dataframe.
#' @export
#'
#' @examples
#' x <- read_drivers()
#' @importFrom rlang .data
read_drivers <- function() {
  urls <- get_drivers_links()

  dfs <-
    lapply(urls, read_drivers_url) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::arrange("ano") |>
    dplyr::rename_with(~ paste0("categoria_", .),
                       -c("uf", "sexo", "faixa_etaria", "total", "ano")) |>
    dplyr::mutate(regiao = get_region(.data$uf), .before = 1)

  return(dfs)
}
