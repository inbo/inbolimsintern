#' Haal de batchnaam uit een importfile
#'
#' @param file de naam van een bestand met importresultaten, de batch wordt gevonden door te zoeken naar ---, alles daarvoor wordt als batchnaam beschouwd
#'
#' @return character string met de batch naam
#' @export
get_batchname_from_file <-  function(file) {
  if (!is.character(file))
    stop("bestandsnaam niet geldig")
  finddash <- regexpr("---", file)
  if (!is.na(finddash) && finddash > 0) {
    batch_name <- substring(file, 1, finddash-1)
  } else {
    warning("Batchnaam niet gevonden in de file, zorg dat de naam gevolgd wordt door ---")
    stop(paste("batch name niet gevonden in file", file))
  }
  batch_name
}


#' Verkrijg de batchinformatie uit de LIMS DB
#'
#' @param conn dbi connectie naar de lims databank
#' @param batchname string met de naam van de batch
#'
#' @return dataset met 1 rij en volgende kolommen: name, template,  template_version, owner, c_import_routine, c_import_sheet
#' @export
get_batch_info <-  function(conn, batchname) {
  #lange kolommen zoals c_root_dir moeten op het einde, anders invalid descriptor index
  qry = paste0("select b.name, b.template, b.template_version, b.owner, b.group_name, bt.c_import_routine, bt.c_import_sheet, bt.c_import_cell_start, b.c_root_dir\n",
               " from batch b inner join batch_hdr_template bt on b.template = bt.name and b.template_version = bt.version\n",
               " where b.name = '", batchname, "'")
  cat(qry)
  as.data.frame(DBI::dbGetQuery(conn, qry))
}



#' Verkrijg de bestandsextensie
#'
#' @param file de naam van de file waarvoor de extensie gezocht wordt. De extensie is alles wat na het laatste '.' komt
#' @return character string met de extensie
get_file_extension <- function(file) {
  occ <- max(gregexpr(pattern = "\\.", file)[[1]])
  if (occ > 0) {
    return(substring(file, occ+1))
  } else {
    return('')
  }
}



#' Krijg de informatie uit het excel tabblad of txt of csv bestand
#'
#' @param path naam van de bestandsnaam inclusief het pad ernaar
#' @param batch_info dataset met de batchinformatie die minstens de kolommen template, name, c_import_routine, c_import_sheet, c_root_dir, en group_name, bevat
#' @importFrom readr read_tsv read_csv2
#' @return dataset et alle importgegevens
#' @export
get_data_from_importfile <- function(path, batch_info) {
  batch_template <- batch_info[1, "template"]
  batch_name <- batch_info[1, "name"]
  import_routine <- batch_info[1, "c_import_routine"]
  sheet <- batch_info[1, "c_import_sheet"]
  cell <- batch_info[1, "c_import_cell_start"]
  filename <- path
  extension <- get_file_extension(filename)
  if (extension %in% c("xls", "xlsx")) {
    tabblad <- readxl::read_excel(filename, sheet = sheet, guess_max = 5000, col_names = FALSE)
    aantalNA <- apply(tabblad, 2, function(x) sum(!is.na(x)))
    first_empty <- which(aantalNA <= 1)
    first_col = which(toupper(substring(cell, 1, 1)) == LETTERS)
    print(first_empty)
    print(head(tabblad))
    if (length(first_empty)) {
      tabblad <- tabblad[, first_col:(first_empty - 1), drop = FALSE]
    }
  } else if (extension %in% c("txt")) {
    tabblad <- readr::read_tsv(filename, guess_max = 5000, col_names = FALSE)
  } else if (extension %in% c("csv")) {
    tabblad <- readr::read_csv2(filename, guess_max = 5000, col_names = FALSE)
  } else {
    tabblad = NULL
  }
  tabblad
}


#' Verplaats file naar lims importlocatie
#'
#' @param data dataset die als textbestand naar de juiste locatie wordt verplaatst
#' @param batch_info batch_info dataset met de batchinformatie die minstens de kolommen template, name, c_import_routine, c_import_sheet, c_root_dir en group_name bevat
#' @param scheduler_base_dir directory waaronder de structuur staat waar de file naartoe geschreven moet worden
#' @param source_path locatie van het bronbestand
#' @param source_file bronbestand inclusief padnaam
#' @importFrom readr write_tsv
#' @return geen return, wel verplaatste bestanden
#' @export
move_batch_importfile <- function(data, source_path, source_file, batch_info, scheduler_base_dir) {
  print(dim(data))
  if  (is.null(data)) {
    stop(paste("Geen data om te verplaatsen uit file", source))
  }
  source <- file.path(source_path, source_file)
  labo = batch_info[1, 'group_name']
  template = batch_info[1, 'template']
  batchname = batch_info[1, "name"]
  file = paste0(batchname, "---", Sys.Date(), '.tsv')
  movepath = paste(scheduler_base_dir, labo, template, file, sep = "\\")
  print(source)
  print(movepath)
  res <- try(readr::write_tsv(data, path = movepath, col_names = FALSE))
  print(res)
  if (class(res)[1] != "try-error") {
    file.rename(source, paste0(source_path, "\\_FINISHED\\", source_file))
    print('file verplaatst')
  } else {
    print("file kon niet in juiste directory geschreven worden")
  }
}

#' Kopieer de data naar een tsv en verplaats de originele file
#'
#' @param data dataset die ingelezen is
#' @param source_file het volledig path met bestandsnaam (nodig om te verplaatsen en de file naamgeving)
#' @param target_location het volledig path naar de doeldirectory, ofwel "." voor op dezelfde plaats te blijven
#' @param move_location het volledig  path naar de verplaatsdirectory, ofwel "_FINISHED" om relatief van de file in de _FINISHED directory te schrijven
#' @importFrom readr write_tsv
#' @return succes status
#' @export
#'
process_file_generic <- function(data, source_file, target_location = ".", move_location = "_FINISHED") {
  #scheid pad en file
  end_slash <- max(c(gregexpr("\\\\", text = source_file)[[1]], gregexpr("/", text = source_file)[[1]]))
  if (end_slash > 0) {
    path <- substring(source_file, 1, end_slash)
    file <- substring(source_file, end_slash + 1)
  } else {
    stop("source_file moet de volledige padverwijzing bevatten, geen / of \\ gevonden")
  }

  #schrijf data weg als tsv
  target_file <- paste0(file, ".tsv")
  if (target_location == ".") {
    target_file <- file.path(path, paste0(file, ".csv"))
  } else {
    target_file <- file.path(target_location, paste0(file, ".csv"))
  }
  readr::write_tsv(data, path = target_file, col_names = TRUE)

  #verplaats de originele file
  if (move_location == "_FINISHED") {
    move_path <- file.path(path, '_FINISHED')
  } else {
    move_path <- move_location
  }
  try(file.remove(file.path(move_location, file)))
  file.rename(from = source_file, to = file.path(move_location, file))
}

#################################################################################################################
#################################################################################################################
#################################################################################################################


# batch_importfile_parse <- function(path, file, batch_info) {
#   batch_template = batch_info[1, "template"]
#   batch_name = batch_info[1, "name"]
#   import_routine = batch_info[1, "c_import_routine"]
#   sheet = batch_info[1, "c_import_sheet"]
#   filename = file.path(path, file)
#   print(filename)
#   FUN = eval(parse(text=import_routine))
#   data <- FUN(filename = filename, sheet = sheet, template = batch_template, batch = batch_name)
#   data
# }

# batch_importfile_parse(path = "Q:\\_SCHEDULER\\PARSER",
#                        file = "N_MIN-200629-1---test.xlsx",
#                        batch_info = get_batchinfo(conn, "N_MIN-200629-1"))


# FN_IMPORT_HG_MA3000_V <-  function(filename, sheet, template, batch) {
#   return("haha")
# }
#
#
# FN_IMPORT_XLS_DEFAULT <- function(filename, sheet, template, batch) {
#   print("in FN_IMPORT_XLS_DEFAULT")
#   print(match.call())
#   errormessage = " "
#   print(filename)
#   print(sheet)
#
#   data <-
#     readxl::read_excel(path = filename,
#                        sheet = sheet) %>%
#     select(2:starts_with("...")[1]) %>%
#     select(-ncol(.))
#
#   data <- data[which(!is.na(as.numeric(data[[1]]))), , drop = FALSE]
#   data <- data %>% arrange_at(1)
#   print("before gathering")
#   gathered_data <- data %>%
#     tidyr::gather(key = "CName", value = "Value", -(1:3)) %>%
#     arrange_at(1)
#   print("finished batch_parse_xls_default")
#   gathered_data
# }


# batch_importfile_parse <- function(path, file) {
#   extension = substring(file, max(gregexpr("\\.", file)[[1]]))
#   print(extension)
#   if (extension %in% c('.xls', '.xlsx')) {
#      sheets <- toupper(excel_sheets(file.path(path, file)))
#      if ('LIMSIMPORT' %in% sheets) {
#        sheet <- "LIMSIMPORT"
#        imported <- read_excel(file.path(path, file), guess_max = 5000, n_max = 10)
#        importtype <- xl_importfile_type(imported)
#      }
#
#   } else if (extension %in% c('txt')) {
#     imported <- read_delim(file.path(path, file), delim = "\t", col_names = FALSE)
#     importtype <- txt_importfile_type(imported)
#
#   } else {
#     warning("File type not supported")
#   }
#   imported
# }
#

# xl_importfile_type <- function(data) {
#   if (all(names(data)[1:3] == c("ID","Methode","Serial no")))
#     return("CN") # C_N_ANALYSER_V C_TIC_PRIMACS_V
#   if (all(names(data)[1:3] == c('AnalysisName',	'Method',	'AnalysisDate')))
#     return("TCA") #C_TC_ANALYSER C_TIC_MULTI_EA_V
#   if (all(names(data)[1:3] == c('Lijnnummer',	'Text_ID',	'R1')))
#     return("CEC") #CACO3_TIT_V CEC_ACID_EXCH_V CEC_H_EXCH_TIT_V PH_EC_TIT_V PHCACL2_TIT_V PHH2O_TIT_V PHKCL_TIT_V
#   if (all(names(data)[1:3] == c('ID',	'Name',	'Gewicht')))
#     return("HG") #HG_TOT_ANAL_V HG_TOT_MA3000_V
#   if (all(names(data)[1:3] == c('ID',	'Sample number',	'Test number')))
#     return("XLS_DEFAULT") #ICP_CEC_AGTU_V ICP_CEC_BACL2_V ICP_CEC_COHEX_V ICP_MET_OXAL ICP_MINMET_HNO3_12 ICP_MINMET_PE_V
#     # N_MIN_V P_OLSEN_V SO4_IC_V CFA_ANIONEN_W CFA_HCO3_W IC_ANIONEN_W IC_KATIONEN_W ICP_MET_HNO3_1 ICP_MINMET_HNO3_1 P_TOT_L_ICP_W
#   if (all(names(data)[1:3] == c('Methode',	'Staal ID',	'Temp')))
#     return("ALK") #ALK_OEP_PH_EC_W ALK_TEP_PH_EC_W
#   if (all(names(data)[1:3] == c('ID',	'LABO_ID+TESTNUMER',	'ABS665')))
#     return("CHLA") #CHLA_SPECTRO
#   if (names(data)[1] == "Date" & data[1,1] == "Time" & data[4,1] == "Template Settings")
#     return("NPOC1") #C_HWC_V,CN_NPOC_TN_ANAL_W
#   if (names(data)[1] == "General Info" & data[1,1] == "Analysis Name" & data[2,1] == "Template Name")
#     return("NPOC2") #C_HWC_V, CN_NPOC_TN_ANAL_W (2 manieren zelfde methode)
# }
#
# txt_importfile_type <- function(data) {
#   if(data[1,1] = 'LS')
#     return("TEXTUUR") #TEXTUUR_LD_LS13320_V
# }
