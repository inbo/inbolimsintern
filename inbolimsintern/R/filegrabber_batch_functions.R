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
    incrementdash <- gregexpr("-", file)[[1]][2]
    underscores <- gregexpr("_", file)[[1]]
    underscores <- underscores[underscores > incrementdash]
    if(length(underscores)) {
      underscore <- min(underscores)
    } else {
      underscore <- Inf
    }
    dashes <- gregexpr("-", file)[[1]]
    dashes <- dashes[dashes > incrementdash]
    if (length(dashes)) {
      dash <- min(dashes)
    } else {
      dash <- Inf
    }
    points <- gregexpr("\\.", file)[[1]]
    point <- min(points[points > incrementdash])
    afterbatch <- min(c(underscore, dash, point))
    if (afterbatch == Inf | is.na(afterbatch)) {
      warning("Batchnaam niet gevonden in de file, zorg dat de naam gevolgd wordt door ---")
      stop(paste("batch name niet gevonden in file", file))
    } else {
      batch_name <- substring(file, 1, afterbatch - 1)
    }
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


#' Krijg de informatie uit het excel tabblad of txt of csv bestand
#'
#' @param path naam van de bestandsnaam inclusief het pad ernaar
#' @param batch_info dataset met de batchinformatie die minstens de kolommen template, name, c_import_routine, c_import_sheet, c_root_dir, en group_name, bevat
#' @param interpret_types voorlopig niet gebruikt, zou de guess_max naar character veranderen
#' @param digits maximaal digits voor numerieke waarde
#' @importFrom readr read_tsv read_csv2
#' @return dataset et alle importgegevens
#' @export
get_data_from_importfile <- function(path, batch_info, interpret_types = TRUE, digits = 12) {
  batch_template <- batch_info[1, "template"]
  batch_name <- batch_info[1, "name"]
  import_routine <- batch_info[1, "c_import_routine"]
  sheet <- batch_info[1, "c_import_sheet"]
  cell <- batch_info[1, "c_import_cell_start"]
  cell = "A1" #fixeren op A1, want de Lims routines zijn zo aangepast
  filename <- path
  extension <- get_file_extension(filename)
  if (extension %in% c("xls", "xlsx")) {
    tabblad <- readxl::read_excel(filename, sheet = sheet, guess_max = 5000, col_names = FALSE)
    aantalNA <- apply(tabblad, 2, function(x) sum(!is.na(x)))
    first_empty <- which(aantalNA <= 1)
    first_col = which(toupper(substring(cell, 1, 1)) == LETTERS)
    print(first_empty)
    print(head(tabblad))
    tabblad <- as.data.frame(tabblad, stringsAsFactors = FALSE)
    if (length(first_empty)) {
      tabblad <- tabblad[, first_col:(first_empty - 1), drop = FALSE]
    }
    for (i  in 1:ncol(tabblad)) {
      whi_num <- which(!is.na(as.numeric(unlist(tabblad[,i]))))
      if (length(whi_num)) {
        tabblad[whi_num,i] <- round(as.numeric(unlist(tabblad[whi_num,i])), digits)
      }
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
  if (template == "C_N_ANALYSER_V") {
    methode = data[2,2]
    print(methode)
    methode = ifelse(methode == '10', 'TC', "TN")
    file = paste0(batchname, "---", methode, "_", datetime_text(), '.tsv')
  } else {
    file = paste0(batchname, "---", datetime_text(), '.tsv')
  }

  targetfile = paste(scheduler_base_dir, labo, template, file, sep = "\\")
  print(source)
  print(targetfile)
  print(batch_info[1,"template"])
  if ((as.character(batch_info[1,"template"]) %in% c("TEXTUUR_LD_LS13320_V"))) {
    print("gewoon de originele file kopieren met extensie tsv")
    res <- try(file.copy(source, targetfile))
  } else {
    res <- try(readr::write_tsv(data, path = targetfile, col_names = FALSE, na = ''))

  }
  print(res)
  if (class(res)[1] != "try-error") {
    datetxt <- datetime_text()
    xtpos = max(gregexpr("\\.", source_file)[[1]])
    file.rename(source, paste0(source_path, "\\_FINISHED\\",
                               substring(source_file, 1, xtpos-1), "_", datetxt, "_",
                               substring(source_file, xtpos, )))
    print('file verplaatst')
  } else {
    print("file kon niet in juiste directory geschreven worden")
  }
}
