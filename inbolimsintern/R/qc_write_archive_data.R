#' Schrijf archiefkaartdata zeg naar de database
#'
#' @param conn DBI connectieobject naar database
#' @param data  de weg te schrijven data
#'
#' @return written records in database table C_CTR_ARCHIVE
#' @export
#'
write_archive_data <- function(conn, data) {
  colnames(data) <- toupper(colnames(data))

  cond_str <- paste0(" LABEL in ('",
                     paste(unique(data$LABEL), collapse = "','"), "')",
                     " and ANALYSIS in ('",
                     paste(unique(data$ANALYSIS), collapse = "','"), "')",
                     " and NAME in ('",
                     paste(unique(data$NAME), collapse = "','"), "')",
                     " and SAMPLE_NAME in ('",
                     paste(unique(data$PRODUCT_GRADE), collapse = "','"), "')"
  )


  qry_check <- paste0("select LABEL, ANALYSIS, NAME, SAMPLE_NAME ",
                      " from C_CTR_ARCHIVE ",
                      " where ", cond_str)
  in_db <- dbGetQuery(conn, qry_check)
  if (nrow(in_db)) {
    qry_dis <- paste0(" update C_CTR_ARCHIVE set ACTIVE = 'F' where ", cond_str)
    dbSendQuery(conn, qry_dis)
  }
  print("here")
  data <- data %>%
    select(-which(duplicated(colnames(data)))) %>% #kolom waarde is dubbel
    mutate(ACTIVE = 'T')

  print(colnames(data))
  dbAppendTable(conn, "C_CTR_ARCHIVE",
                data %>% select(-ROWNR, -FIRST_BATCH_ENTRY))
}
