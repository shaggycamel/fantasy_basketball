

db_info <- ini::read.ini(here::here("database.ini"))$postgresql

postgre_nba_con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  user = db_info$user,
  host = db_info$host,
  port = db_info$port,
  password = db_info$password,
  dbname = db_info$database,
  options="-c search_path=nba"
)

postgre_fty_con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  user = db_info$user,
  host = db_info$host,
  port = db_info$port,
  password = db_info$password,
  dbname = db_info$database,
  options="-c search_path=fty"
)


dh_getQuery <- function(connection, file){
  connection |> 
    DBI::dbGetQuery(readr::read_file(here::here("analysis", file))) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(dplyr::across(where(~ class(.x) == "integer64"), ~ as.integer(.x)))
}