get_batchname_from_file <-  function(file) {
  finddash <- regexpr("---", file)
  if (finddash > 0) {
    batch_name <- substring(file, 1, finddash-1)
  } else {
    batch_name = NULL
  }
  batch_name
}

get_batchinfo <-  function(conn, batchname) {
  qry = paste0("select b.name, b.template, b.template_version, b.owner, bt.c_import_routine, bt.c_import_sheet\n",
               " from batch b inner join batch_hdr_template bt on b.template = bt.name and b.template_version = bt.version\n",
               " where b.name = '", batchname, "'")
  DBI::dbGetQuery(conn, qry)
}

batch_importfile_parse <- function(path, file, batch_info) {
  batch_template = batch_info[1, "template"]
  batch_name = batch_info[1, "name"]
  import_routine = batch_info[1, "c_import_routine"]
  sheet = batch_info[1, "c_import_sheet"]
  filename = file.path(path, file)
  print(filename)
  FUN = eval(parse(text=import_routine))
  data <- FUN(filename = filename, sheet = sheet, template = batch_template, batch = batch_name)
  data
}

# batch_importfile_parse(path = "Q:\\_SCHEDULER\\PARSER",
#                        file = "N_MIN-200629-1---test.xlsx",
#                        batch_info = get_batchinfo(conn, "N_MIN-200629-1"))

FN_IMPORT_HG_MA3000_V <-  function(filename, sheet, template, batch) {
  return("haha")
}


FN_IMPORT_XLS_DEFAULT <- function(filename, sheet, template, batch) {
  print("in FN_IMPORT_XLS_DEFAULT")
  print(match.call())
  errormessage = " "
  print(filename)
  print(sheet)

  data <-
    readxl::read_excel(path = filename,
                       sheet = sheet) %>%
    select(2:starts_with("...")[1]) %>%
    select(-ncol(.))

  data <- data[which(!is.na(as.numeric(data[[1]]))), , drop = FALSE]
  data <- data %>% arrange_at(1)
  print("before gathering")
  gathered_data <- data %>%
    tidyr::gather(key = "CName", value = "Value", -(1:3)) %>%
    arrange_at(1)
  print("finished batch_parse_xls_default")
  gathered_data
}


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
