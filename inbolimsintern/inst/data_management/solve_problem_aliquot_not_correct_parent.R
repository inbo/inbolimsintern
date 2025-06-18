#'Beschrijving:
#'Probleem geformuleerd op 13/12/2024
#'  Het aliquot van het laatste staal hangt aan het eerste staal
#'  Al de rest is een staal verschoven
#'
#'Oplossing
#'  Haal de lijst op met correcte koppelingen uit excel
#'  zoek ieder aliquot staal op
#'  wijzig hiervoor ieder parent_aliquot, original_sample en c_orig_dup_number
#'
library(tidyverse)
library(readxl)
library(DBI)

creds <- inbolimsintern::read_db_credentials()
conn <- inbolimsintern::limsdb_connect(uid = creds$uid, pwd = creds$pwd)
data <- read_excel("Q:\\_PIETER\\V-24V057-20_VAL.xlsx",
                   sheet = "Projectdata",
                   skip = 2) |>
  select("ID", "Gecrushed", "Gemalen") |>
  filter(ID %in% c(1:52, 101:105))

gecrusht <- data |> pull(Gecrushed) |> paste(collapse = "','")
gecrusht <- paste0("('", gecrusht, "')")
q = paste0("select sample_number, text_id, original_sample, c_orig_dup_number",
           " from sample where text_id in ", gecrusht)
samp_orig <- dbGetQuery(conn, q)

gemalen <- data |> pull(Gemalen) |> paste(collapse = "','")
gemalen <- paste0("('", gemalen, "')")
q2 <- paste0("select sample_sub = sample_number,
              text_id_sub = text_id,
              original_sub = original_sample,
              parent_sub = parent_aliquot,
              dup_sub = c_orig_dup_number
              from sample where text_id in ", gemalen)
samp_sub <- dbGetQuery(conn, q2)

dataj <- data |>
  left_join(samp_orig, join_by(Gecrushed == text_id)) |>
  left_join(samp_sub, join_by(Gemalen == text_id_sub))

queries <- NULL
for (i in seq_along(1:nrow(dataj))) {
  orig <- dataj |> slice(i) |> pull(original_sample)
  duporig <- dataj |> slice(i) |> pull(c_orig_dup_number)
  text_id_sub <- dataj |> slice(i) |> pull(Gemalen)
  queries[[i]] <- paste0("update sample",
                         " set original_sample = ", orig,
                         ",parent_aliquot = ", orig,
                         ",c_orig_dup_number = ", duporig,
                         " where text_id = '", text_id_sub, "'"
                        )
}

#execute: sapply(queries, function(x) dbGetQuery(conn = conn, x))


