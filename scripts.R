get_drivers_links <- function() {
  page <- "https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/estatisticas-quantidade-de-habilitados-denatran"

  xl_links <- rvest::read_html(page) |> 
    rvest::html_elements(".internal-link") |> 
    rvest::html_attr("href") |> 
    Filter(
      f = function(l) {
        grepl("condutores.*12",l) & tools::file_ext(l) %in% c("xls","xlsx")
      }
    )
  
  return(xl_links)
}

brazil_states_acronym <- function(df) {
  brazil_states_df <- data.frame(
    State = c(
      "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal",
      "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
      "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí",
      "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima",
      "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
    ),
    Acronym = c(
      "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG",
      "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
    )
  )
  
  for(i in 1:length(df$uf)) {
    for(j in 1:length(brazil_states_df$State)) {
      if(df$uf[i] == brazil_states_df$State[j] & !is.na(df$uf[i])) {
        df$uf[i] <- brazil_states_df$Acronym[j]
      }
    }
  }
  
  return(df)
}

read_drivers_url <- function(url) {
  temp <- tempfile(fileext = ".xlsx")
  utils::download.file(url, temp, mode = "wb", quiet = T)
  
  suppressMessages({
    df <- 
      readxl::read_excel(
        temp, sheet = 1, range = readxl::cell_cols(c("B",NA)),
        .name_repair = "universal"
      ) |> 
      dplyr::slice(-1) |> 
      dplyr::rename_with(~ tolower(stringr::word(., 1, sep = "\\."))) |> 
      dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !grepl("Total", .))) |> 
      dplyr::rename("faixa_etaria" = "categoria") |> 
      dplyr::mutate(dplyr::across("a":"total", ~ as.numeric( .)),
             ano = as.integer(stringr::str_extract(url, "[12][0-9]{3}"))) |> 
      tidyr::fill("uf", "sexo") |> 
      tidyr::drop_na(faixa_etaria) |> 
      dplyr::mutate(faixa_etaria = forcats::as_factor(faixa_etaria)) |> 
      brazil_states_acronym()
  })
  
  unlink(temp)
  
  return(df)
}

read_drivers <- function() {
  urls <- get_drivers_links()
  
  dfs <- 
    lapply(urls, read_drivers_url) |>
    purrr::reduce(dplyr::bind_rows) |> 
    dplyr::arrange(ano)
  
  return(dfs)
}