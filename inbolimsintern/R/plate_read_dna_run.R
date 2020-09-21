


#'Lees data van een DNA run in om daar later een plaat layout van te maken
#'
#' @param conn de connectie met de databank
#' @param run de DNA_RUN_ID vanuit het lims systeem
#'
#' @importFrom DBI dbGetQuery
#'
#'
#' @return dataset containig PLATE, ROW_NUMBER, COLUMN_NUMBER, SAMPLE_NUMBER, PARENT_SAMPLE, DNA, MilliQ
#' @export
#'
plate_read_dna_run <- function(conn, run){

  qry1 <- paste0("
  select
  pp.PLATE
  , pp.POSITION_ID
  , pp.LABEL
  , pp.ROW_NUMBER
  , pp.COLUMN_NUMBER
  , s.SAMPLE_TYPE
  , pp.SAMPLE_NUMBER
  , s.PARENT_SAMPLE
  , s.PARENT_ALIQUOT
  from PLATE_POSITION pp
  left join PLATE plt on plt.NAME = pp.PLATE
  left join SAMPLE s ON pp.SAMPLE_NUMBER = s.SAMPLE_NUMBER
  ",
  " where plt.BATCH_NAME = '", run, "'",
  " order by pp.PLATE, pp.COLUMN_NUMBER, pp.Row_number")

  qry2 <- paste0("
  select
  s.SAMPLE_NUMBER
  , r.NAME
  , r.ENTRY
  from PLATE_POSITION pp
  left join PLATE plt on plt.NAME = pp.PLATE
  left join SAMPLE s ON pp.SAMPLE_NUMBER = s.SAMPLE_NUMBER
  left join TEST t on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
  left join ANALYSIS a on  a.NAME = t.ANALYSIS and a.VERSION = t.VERSION
  left join RESULT r on r.TEST_NUMBER = t.TEST_NUMBER
  where plt.BATCH_NAME = '", run, "'",
  "and a.ANALYSIS_TYPE = 'DNA_TEMPLATE'
   and r.NAME in ('MILLIQ', 'DNA')
   and r.STATUS not in ('X')
  order by pp.PLATE, pp.COLUMN_NUMBER, pp.Row_number,r.NAME")

  dfMD <-
    DBI::dbGetQuery(conn, qry2) %>%
    spread(key = .data$NAME, value = .data$ENTRY)

  if (nrow(dfMD) > 0) {
    print("DNA of MILLIQ gevonden")
    dfSamps <-
      DBI::dbGetQuery(conn, qry1) %>%
      left_join(dfMD, by = "SAMPLE_NUMBER") %>%
      left_join(select(dfMD, .data$SAMPLE_NUMBER, DNApar = .data$DNA, MilliQpar = .data$MilliQ),
                by = c("PARENT_SAMPLE" = "SAMPLE_NUMBER")) %>%
      mutate(MilliQ = as.numeric(ifelse(is.na(.data$MilliQ), .data$MilliQpar, .data$MilliQ)),
             DNA = as.numeric(ifelse(is.na(.data$DNA), .data$DNApar, .data$DNA)),
             SAMPLE_TYPE = ifelse(is.na(.data$SAMPLE_TYPE), "SAMPLE", .data$SAMPLE_TYPE)) %>%
      select (-.data$MilliQpar, -.data$DNApar)
  } else {
    print("Geen DNA of MILLIQ gevonden")
    dfSamps <- DBI::dbGetQuery(conn, qry1) %>%
      mutate(DNA = NA,
             MilliQ = NA,
             SAMPLE_TYPE = ifelse(is.na(.data$SAMPLE_TYPE), "SAMPLE", .data$SAMPLE_TYPE))
  }


  dna_milliq <- dfSamps %>% filter(.data$SAMPLE_TYPE == "QC_METHOD",  !is.na(.data$DNA),  !is.na(.data$MilliQ))
  if (nrow(dna_milliq) == 0) {
    qcm_dna <- 0
    qcm_milliq <- 0
  } else {
    qcm_dna <- round(as.numeric(dna_milliq$DNA[1]), 2)
    qcm_milliq <- round(as.numeric(dna_milliq$MilliQ[1]), 2)
  }

  dfDesign <-
    dfSamps %>%
    mutate(MilliQ = ifelse(.data$SAMPLE_TYPE == "QC_METHOD", qcm_milliq, .data$MilliQ),
           MilliQ = ifelse(.data$SAMPLE_TYPE == "BLANK", 0, .data$MilliQ),
           DNA = ifelse(.data$SAMPLE_TYPE == "QC_METHOD", qcm_dna, .data$DNA),
           DNA = ifelse(.data$SAMPLE_TYPE == "BLANK", 0, .data$DNA))

  dfDesign
}
