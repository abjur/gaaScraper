devtools::load_all()

# baixando lista de GAAs ----

estados <- gaa_estados()

items_parsed <- purrr::map(estados, gaa_instituicoes, .progress = TRUE) |>
  purrr::list_rbind() |>
  dplyr::filter(!is.na(city))

items_parsed_ibge <- items_parsed |>
  dplyr::mutate(uf = dplyr::case_when(
    estado == "acre" ~ "AC",
    estado == "alagoas" ~ "AL",
    estado == "amapa" ~ "AP",
    estado == "amazonas" ~ "AM",
    estado == "bahia" ~ "BA",
    estado == "ceara" ~ "CE",
    estado == "distrito-federal" ~ "DF",
    estado == "espirito-santo" ~ "ES",
    estado == "goias" ~ "GO",
    estado == "maranhao" ~ "MA",
    estado == "mato-grosso" ~ "MT",
    estado == "mato-grosso-do-sul" ~ "MS",
    estado == "minas-gerais" ~ "MG",
    estado == "para" ~ "PA",
    estado == "paraiba" ~ "PB",
    estado == "parana" ~ "PR",
    estado == "pernambuco" ~ "PE",
    estado == "piaui" ~ "PI",
    estado == "rio-de-janeiro" ~ "RJ",
    estado == "rio-grande-do-norte" ~ "RN",
    estado == "rio-grande-do-sul" ~ "RS",
    estado == "rondonia" ~ "RO",
    estado == "roraima" ~ "RR",
    estado == "santa-catarina" ~ "SC",
    estado == "sao-paulo" ~ "SP",
    estado == "sergipe" ~ "SE",
    estado == "tocantins" ~ "TO"
  )) |>
  dplyr::mutate(
    city = stringr::str_remove(city, "-.*"),
    city = stringr::str_remove(city, "/.*"),
    city = stringr::str_remove(city, "GAA ")
  ) |>
  munifacil::limpar_colunas(city, uf) |>
  dplyr::mutate(
    muni_join = dplyr::case_when(
      muni_join == "sao luiz" ~ "sao luis",
      muni_join == "garanhus" ~ "garanhuns",
      muni_join == "jaboatao" ~ "jaboatao dos guararapes",
      muni_join == "pernambuco" ~ "recife",
      .default = muni_join
    )
  ) |>
  munifacil::incluir_codigo_ibge() |>
  dplyr::select(
    id:uf, ibge = id_municipio,
    -estado
  ) |>
  dplyr::mutate(slug = basename(link))

# baixando todos os arquivos ----
purrr::walk(
  items_parsed$link,
  gaa_download,
  path = "data-raw/gaas",
  .progress = TRUE
)

files <- fs::dir_ls("data-raw/gaas")

aux_gaas <- purrr::map(files, gaa_parse, .progress = TRUE) |>
  purrr::list_rbind(names_to = "file") |>
  dplyr::filter(key != "") |>
  dplyr::group_by(file, key) |>
  dplyr::summarise(
    value = paste(unique(value), collapse = " | "),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(names_from = key, values_from = value) |>
  janitor::clean_names()

readr::write_rds(aux_gaas, "data-raw/aux_gaas.rds")

# Parse ----
# Essa parte pode ser melhorada posteriormente

da_gaas <- aux_gaas |>
  dplyr::mutate(
    data_de_fundacao = dplyr::coalesce(
      data_de_fundacao,
      dada_de_fundacao,
      ano_de_fundacao,
      fundado_em,
      data_fundacao,
      ano_de_fundacao_2
    )
  ) |>
  dplyr::select(
    file, cep:txt_completo,
    -nos_acompanhe_em_nossas_redes_sociais
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), \(x) dplyr::na_if(x, "")),
    dplyr::across(dplyr::everything(), \(x) dplyr::na_if(x, "NA"))
  ) |>
  dplyr::mutate(slug = basename(tools::file_path_sans_ext(file))) |>
  dplyr::inner_join(items_parsed_ibge, "slug") |>
  dplyr::select(id:ibge, file:txt_completo)

usethis::use_data(da_gaas, overwrite = TRUE)

## upload to releases

# piggyback::pb_new_release(tag = "raw-data")
# piggyback::pb_upload("data-raw/gaas.zip", tag = "raw-data")
# piggyback::pb_upload("data-raw/aux_gaas.rds", tag = "raw-data")
