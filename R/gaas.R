#' Estados a partir do site do GAA
#'
#' @return vetor de estados
#' @export
gaa_estados <- function() {
  u_gaas <- "https://www.angaad.org.br/portal/gaas/"
  r <- httr::GET(u_gaas)
  estados <- r |>
    xml2::read_html() |>
    xml2::xml_find_all("//select/option") |>
    xml2::xml_attr("value") |>
    purrr::discard(\(x) x == "")
  estados
}

# Instituicoes a partir do site do GAA
#
# Faz o parse de um item extraido da pagina inicial do GAA
gaa_parse_item <- function(item) {
  link <- item |>
    xml2::xml_find_first(".//a") |>
    xml2::xml_attr("href")
  img_link <- item |>
    xml2::xml_find_first(".//img") |>
    xml2::xml_attr("src")
  title <- item |>
    xml2::xml_find_first(".//h4") |>
    xml2::xml_text()
  city <- item |>
    xml2::xml_find_first(".//div[@class='pt-cv-ctf-value']") |>
    xml2::xml_text()
  tibble::tibble(
    link = link,
    img_link = img_link,
    title = title,
    city = city
  )
}

#' Instituicoes a partir do site do GAA
#'
#' @param estado sigla do estado
#' @return tibble com as instituicoes
#' @export
gaa_instituicoes <- function(estado) {
  u <- paste0("https://www.angaad.org.br/portal/gaas/?tx_category=", estado)
  r <- httr::GET(u)
  items <- r |>
    xml2::read_html() |>
    xml2::xml_find_all("//div[contains(@class,'pt-cv-content-item')]") |>
    purrr::map(parse_item, .progress = TRUE) |>
    purrr::list_rbind(names_to = "id") |>
    dplyr::mutate(estado = estado)
  items
}

#' Faz o download de um GAA a partir do link
#'
#' @param link link do GAA
#' @param path pasta onde salvar o arquivo
#' @return caminho do arquivo baixado
#' @export
gaa_download <- function(link, path) {
  fs::dir_create(path)
  f <- paste0(path, "/", basename(link), ".html")
  if (!file.exists(f)) {
    r <- httr::GET(link, httr::write_disk(f, TRUE))
  }
  f
}

#' Faz o parse de um GAA
#'
#' @param f caminho do arquivo
#' @return tibble com os dados do GAA
#' @export
gaa_parse <- function(f) {
  xml <- xml2::read_html(f)
  views <- xml |>
    xml2::xml_find_all("//span[@class='post-views-count']") |>
    xml2::xml_text() |>
    stringr::str_squish()
  txt_completo <- xml |>
    xml2::xml_find_first("//div[@class='the_content']") |>
    xml2::xml_text()
  xml |>
    xml2::xml_find_all("//div[@class='the_content']/p") |>
    xml2::xml_text() |>
    tibble::as_tibble() |>
    tidyr::separate(
      value,
      into = c("key", "value"),
      sep = ":",
      extra = "merge",
      fill = "right"
    ) |>
    dplyr::mutate(
      key = stringr::str_squish(key),
      value = stringr::str_squish(value)
    ) |>
    tibble::add_row(key = "views", value = views) |>
    tibble::add_row(key = "txt_completo", value = txt_completo)
}