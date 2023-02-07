






#' Bereken de ELC basisstatistieken van een numerieke kolom (OUDE MANIER)
#'
#' @param x numerieke vector met waarden
#'
#' @return data.frame met basisstatistieken
#'
calc_elc_stats <- function(x){
  Norig   <- length(x)
  avgorig <- mean(x, na.rm = TRUE)
  sdorig  <- sd(x, na.rm = TRUE)
  not_all_in_3s <- TRUE
  n <- Norig
  avg = avgorig
  sd = sdorig
  x1 <- x
  #print(x1)

  while(not_all_in_3s & n > 3) {
    n = length(x1)
    avg <- mean(x1, na.rm = TRUE)
    sd  <- sd(x1, na.rm = TRUE)
    min3s <- avg - 3*sd
    max3s <- avg + 3*sd
    if (all(na.omit(x1) <= max3s) & all(na.omit(x1) >= min3s)) {
      not_all_in_3s <- FALSE
    } else {
      del <- which.max(abs(x1 - avg))
      x1 <- x1[-del]
    }
  }
  data.frame(N_all = Norig,
             Avg_all = avgorig,
             Sd_all = sdorig,
             N_used = n,
             Avg = avg,
             Sd = sd,
             min3s = avg - 3*sd,
             max3s = avg + 3*sd)
}


#############################################################
#' Voer een t en F test uit (OUDE MANIER)
#'
#' @param x waarden
#' @param years verschillende periodes
#'
#' @return dataset met t en F test resultaten
#'
compare_with_tf <- function(x, years){
  x0 <- x$value[x$jaar == years[4]]
  xmin1 <- x$value[x$jaar == years[3]]
  xmin2 <- x$value[x$jaar == years[2]]
  xmin3 <- x$value[x$jaar == years[1]]
  print(c(length(x0), length(xmin1), length(xmin2), length(xmin3)))
  #de vorige jaren worden verder gecombineerd zodat de vergelijkingen zijn:
  #x0 vs xmin1
  #x0 vs c(xmin1, xmin2)
  #x0 vs c(xmin1, xmin2, xmin3)
  #op voorwaarde dat beide kanten minstens 10 observaties hebben

  if (length(x0) >= 10 & length(xmin1) >= 10 ) {
    print(x0)
    print(xmin1)
    ttest1 <- try(t.test(x0, xmin1))
    ftest1 <- try(var.test(x0, xmin1))
    t1 <- ifelse(class(ttest1) == "try-error", NA, ttest1$p.value)
    f1 <- ifelse(class(ftest1) == "try-error", NA, ftest1$p.value)
  } else {
    t1 <- f1 <- NA
  }
  if (length(x0) >= 10 & length(c(xmin1,xmin2)) >= 10 ) {
    ttest2 <- try(t.test(x0, c(xmin1,xmin2)))
    ftest2 <- try(var.test(x0, c(xmin1,xmin2)))
    t2 <- ifelse(class(ttest2) == "try-error", NA, ttest2$p.value)
    f2 <- ifelse(class(ftest2) == "try-error", NA, ftest2$p.value)
  } else {
    t2 <- f2 <- NA
  }
  if (length(x0) >= 10 & length(c(xmin1, xmin2, xmin3)) >= 10 ) {
    ttest3 <- try(t.test(x0, c(xmin1, xmin2, xmin3)))
    ftest3 <- try(var.test(x0, c(xmin1, xmin2, xmin3)))
    t3 <- ifelse(class(ttest3) == "try-error", NA, ttest3$p.value)
    f3 <- ifelse(class(ftest3) == "try-error", NA, ftest3$p.value)
  } else {
    t3 <- f3 <- NA
  }

  rv <- data.frame(t1, f1, t2, f2, t3, f3)
  colnames(rv) <- c(paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[3])),
                    paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[2])),
                    paste0(c("pval_t_", "pval_F_"), paste0(years[4], "-", years[1])))
  rv
}





#' Lijst alle controlestalen waarvoor limieten bestaan op binnen de gekozen periode
#'
#' @param conn connectie naar db object
#' @param min_date vroegste beschouwde datum yyyy-mm-dd
#' @param max_date laatst beschouwde datum yyyy-mm-dd
#'
#' @return dataset met alle resultaatinformatie voor de geselecteerde controlestalen
#' @export
#'
list_all_qc_data <- function(conn, min_date = "2020-01-01", max_date = "2021-01-01") {
  sql <- paste0(
    "select
  s.SAMPLE_NUMBER, s.TEXT_ID, s.SAMPLE_TYPE, s.SAMPLE_NAME,
  s.PRODUCT, s.PRODUCT_VERSION,
  bo.BATCH, bo.ORDER_NUMBER,
  r.ANALYSIS, r.NAME, r.ENTRY, r.ENTERED_ON, r.UNITS,
  ps.C_CERTIFIED_VALUE, ps.C_CERTIFIED_SD,
  ps.C_CTR_X, ps.C_CTR_SD, ps.MIN_VALUE, ps.MAX_VALUE,
  c.C_QC,
  qcs.C_QC_CTR_CHART,
  v.VERSION as ANALYSIS_VERSION
  from test t
  inner join result r on r.test_number = t.test_number
  inner join sample s on s.sample_number = t.sample_number
  inner join PRODUCT_SPEC ps on ps.PRODUCT = s.PRODUCT
  and ps.VERSION = s.PRODUCT_VERSION and s.PRODUCT_GRADE = ps.GRADE and ps.ANALYSIS = t.ANALYSIS and ps.COMPONENT = r.NAME
  inner join component c on c.NAME = ps.COMPONENT
  and c.ANALYSIS = ps.ANALYSIS
  inner join versions v on v.TABLE_NAME = 'ANALYSIS'
  and v.NAME = c.ANALYSIS
  and v.VERSION = c.VERSION
  inner join batch_objects bo on bo.OBJECT_ID = t.TEST_NUMBER
  inner join QC_SAMPLES qcs on qcs.NAME = s.PRODUCT_GRADE
  where r.ENTRY is not null
  and s.SAMPLE_TYPE is not null
  and s.SAMPLE_TYPE <> 'DUP'
  and r.ENTERED_ON > '", min_date, "'",
    " and ENTERED_ON <= '", max_date, "'",
    " and r.STATUS in ('E', 'M', 'A')
  and c.C_QC = 'T'
  and qcs.C_QC_CTR_CHART = 'T'")
  cat(sql)
  DBI::dbGetQuery(conn, sql)
}

#' Verkrijg de ELC data
#'
#' @param dbcon dbconnection object (DBI)
#' @param sqlfile path naar de file die de sql code bevat
#' @param keep aantal batches te behouden voor de figuur
#'
#' @return dataset met alle te verwerken gegevens
#' @export
get_ELC_data <- function(dbcon, sqlfile, keep = 30) {
  sqlcode <- readLines(sqlfile)
  sqlcode <- paste(sqlcode, collapse = "\n")

  qry = "select NAME, MaxVersion = max(VERSION) from PRODUCT group by NAME"
  productVersions = DBI::dbGetQuery(dbcon, qry)
  cat(sqlcode)

  plotdata <- DBI::dbGetQuery(dbcon, sqlcode)
  batchvolgorde <- plotdata %>%
    group_by(BATCH) %>%
    summarize(FIRST_ENTRY = min(ENTERED_ON)) %>%
    arrange(FIRST_ENTRY)
  n_batch <- nrow(batchvolgorde)
  batches_to_keep <- batchvolgorde[max(1, n_batch - keep + 1):n_batch, , drop = FALSE]
  batches_to_keep <- batches_to_keep %>% mutate(BATCHNR = 1:nrow(batches_to_keep))

  plotdata <- plotdata %>%
    inner_join(batches_to_keep) %>%
    mutate(combi = paste(ANALYSIS, SAMPLE_NAME, NAME, sep = "---")) %>%
    arrange(FIRST_ENTRY, BATCH, ORDER_NUMBER)

  attr(plotdata, "sqlcode") <- sqlcode
  plotdata
}
