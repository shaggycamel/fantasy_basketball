
library(DBI)

db_info <- ini::read.ini(here::here("database.ini"))$postgresql

postgre_con <- dbConnect(
  drv = RPostgres::Postgres(),
  user = db_info$user,
  host = db_info$host,
  port = db_info$port,
  password = db_info$password,
  dbname = db_info$database
)

dh_getQuery <- function(file){
  postgre_con |> 
    dbGetQuery(read_file(here("analysis", file))) |> 
    as_tibble() |> 
    mutate(dplyr::across(where(~ class(.x) == "integer64"), ~ as.integer(.x)))
}