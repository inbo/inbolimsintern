


#' Generate a quick and simple html page with the qc plots
#'
#' @param env environment PRD, UAT or DEV
#' @param call_id (int) ID for the call
#' @param user_name lims user name
#' @param sqlfile where the sql code is housed
#' @param htmlfile the resulting html file
#' @param maxpoints the maximum amount of points to go back to per chart
#'
#' @returns location of the html file
#' @export
plumber_qc_chart_simple <- function(env, call_id, user_name, sqlfile, htmlfile, maxpoints) {
  rv <- list(env = env, call_id = call_id, user_name = user_name,
             sqlfile = sqlfile, htmlfile = htmlfile, maxpoints = maxpoints)
  
  library(htmltools)
  require(inbolimsintern)
  if (exists("global_conn")) {
    conn <- global_conn #needed for write_db_log'    
  }

  write_db_log("Entered QC_CHART_SIMPLE", "P")
  
  
  logfile = "D:\\LOGS\\QC_SIMP.txt"
  cat("START\n", append = FALSE, file = logfile)
  lgf <- function(...) {cat(paste(..., "\n"), append = TRUE, file = logfile)}
  lgf(paste(rv, collapse = "\n"))
  
  
  # Check arguments
  if (sqlfile == "" || htmlfile == "") {
    stop("Missing required file paths (sqlfile or htmlfile)")
  }
  if (!exists("global_conn")) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)    
    conn <- global_conn
  }
  if (!DBI::dbIsValid(global_conn)) {
    global_conn <<- inbolimsintern::limsdb_connect(env = env)
    conn <- global_conn
  }
  
  
  e <- try(alldata <- DBI::dbGetQuery(global_conn, paste(readLines(sqlfile), collapse = "\n")))
  if (inherits(e, "try-error")) {
    #log_message(paste("ERROR retrieving parameters:", as.character(e)))
    write_db_log("data could not be read", "E")
    stop(e)
  } else {
    write_db_log(paste0("data read: ", nrow(alldata), " rows"), "P")
  }
  
  
  rv$dim <- paste(rows = nrow(alldata), cols = ncol(alldata))
  lgf("alldata dim:", rv$dim)
  
  combis <- alldata |>
    dplyr::select(combi) |>
    dplyr::distinct() |>
    tidyr::separate(col = "combi", into = c("ana", "qc", "comp"), sep = "---", remove = FALSE) |>
    dplyr::arrange(ana, comp, qc)
  
  rv$combis <- paste(combis$combi, collapse = "\n")
  lgf(rv$combis)
  lgf("first 3 rows:\n", paste(knitr::kable(head(alldata,3), format = "html"), collapse = "\n"))

  
  
  #return(paste(rv, collapse = "\n"))
  
  fig_height <- 6 # Inches for ggsave/base64 device
  fig_width  <- 9 # Inches
  
  htmlobjects <- replicate(nrow(combis), list(), simplify = FALSE)
  
  #return(paste(rv, collapse = "\n"))
  write_db_log("start processing each plot", "P")

  for (i in seq_len(nrow(combis))) {
    write_db_log(paste("start", combis$combi[i]), "P")
    lgf("starting", i)
    combdata <- combis |> dplyr::slice(i)
    comb <- combis$combi[i]
    my_id <- paste(combdata$comp, combdata$qc, sep = "---")
    lgf("combinatie:", comb, " with id: ", my_id)
    
    lgf("retrieving data")
    plotdata <- alldata |> dplyr::filter(combi == comb)
    lgf("aantal rijen plotdata:", nrow(plotdata))
    htmldata <- elc_htmldata(plotdata)
    lgf("aantal rijen htmldata:", nrow(htmldata$plot))
    
    
    lgf("start creating graph")
    
    # 1. Force static ggplot (interactive = FALSE)
    fig <- ELC_shewhart_plot(
      subdata = htmldata[["plot"]],
      interactive = FALSE, 
      title = "",
      fig_height = fig_height * 100 # Adjust if your internal function expects pixels
    )
    lgf("fig made")
    write_db_log("fig made", "P")
    
    
    # 2. Convert ggplot directly to Base64 in-memory (No disk I/O = Blazing Fast)
    tf <- tempfile(fileext = ".png")
    ggplot2::ggsave(tf, plot = fig, width = fig_width, height = fig_height, dpi = 100)
    img_base64 <- knitr::image_uri(tf)
    file.remove(tf) # instant temp file cleanup
    
    lgf("start saving in htmlobjects list\n")
    write_db_log("saving in html object list", "P")
    # 3. Cache structural properties
    htmlobjects[[i]]$id <- my_id
    htmlobjects[[i]]$comp <- combdata$comp
    htmlobjects[[i]]$qc <- combdata$qc
    htmlobjects[[i]]$ana <- combdata$ana
    htmlobjects[[i]]$img_base64 <- img_base64
    lgf("fihished loop", i, "\n")
    write_db_log(paste("Finished processing", my_id), "P")
  }
  
  write_db_log("Finished creating graphs, starting generating html", "P")
  
  rv$num_objects <- length(htmlobjects)
  lgf("loop finished: ", rv$num_objects, " plots made")
  write_db_log(paste("loop finished: ", rv$num_objects, " plots made"), "P")
  
  # 4. Generate the pure-CSS sidebar navigation links
  
  lgf("start creating nav_links")
  write_db_log("start creating nav_links", "P")
  
  nav_links <- lapply(htmlobjects, function(obj) {
    tags$a(
      href = paste0("#", obj$id), 
      paste(obj$ana, obj$comp, obj$qc, sep = " | ")
    )
  })
  
  lgf("start creating sidebar")
  sidebar <- tags$div(
    class = "sidebar",
    tags$h3("Navigation"),
    nav_links
  )
  
  # 5. Build content sections using your exact H1, H2, H3 hierarchy
  lgf("start creating content_sections")
  write_db_log("start creating content_sections", "P")
  content_sections <- lapply(htmlobjects, function(obj) {
    tags$div(
      id = obj$id,
      class = "section-card",
      tags$h1(paste("Analysis:", obj$ana)),
      tags$h2(paste("Component:", obj$comp)),
      tags$h3(paste("QC Sample:", obj$qc)),
      
      tags$div(
        class = "plot-container", 
        # Embed the static image string directly
        tags$img(src = obj$img_base64, alt = obj$id, style = "max-width:100%; height:auto;")
      ),
      tags$hr()
    )
  })
  
  lgf("start creating main content")
  write_db_log("start creating main content", "P")
  main_content <- tags$div(
    class = "main-content",
    content_sections
  )
  
  # 6. Apply pure-CSS styling layout
  lgf("start creating css styles")
  write_db_log("start creating css styles", "P")
  css_styles <- tags$style(HTML("
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      margin: 0;
      display: flex;
      background-color: #f8f9fa;
    }
    .sidebar {
      width: 300px;
      height: 100vh;
      position: fixed;
      top: 0; left: 0;
      background-color: #2c3e50;
      color: white;
      padding: 20px;
      box-sizing: border-box;
      overflow-y: auto;
    }
    .sidebar h3 { border-bottom: 2px solid #34495e; padding-bottom: 10px; margin-top: 0; }
    .sidebar a {
      display: block;
      color: #ecf0f1;
      text-decoration: none;
      padding: 8px 10px;
      border-radius: 4px;
      margin-bottom: 5px;
      font-size: 13px;
      word-wrap: break-word;
    }
    .sidebar a:hover { background-color: #34495e; color: #3498db; }
    .main-content {
      margin-left: 300px;
      padding: 40px;
      width: calc(100% - 300px);
      box-sizing: border-box;
    }
    .section-card {
      background: white;
      padding: 30px;
      margin-bottom: 40px;
      border-radius: 8px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      scroll-margin-top: 30px;
    }
    h1 { color: #2c3e50; margin-top: 0; font-size: 28px; }
    h2 { color: #7f8c8d; font-size: 22px; margin-top: 5px; }
    h3 { color: #95a5a6; font-size: 18px; font-weight: normal; margin-top: 5px; }
    html { scroll-behavior: smooth; }
  "))
  
  # 7. Assemble and Save
  lgf("start assembling html page")
  write_db_log("start assembling html page", "P")
  html_page <- tags$html(
    tags$head(
      tags$title("QC Shewhart Dashboard"),
      css_styles
    ),
    tags$body(sidebar, main_content)
  )
  
  lgf("saving html file")
  e <- (try(htmltools::save_html(html_page, file = htmlfile)))
  if (inherits(e, "try-error")) {
    write_db_log(paste0("Error creating HTML: ", e), "E")
  } else  {
    write_db_log(paste0("HTML created on ", htmlfile,
                        "\n R routine finished "), "C")
  }
  lgf("END")
  return(paste(rv, collapse = "\n"))
}


testit <- FALSE
if (testit) {
  require(knitr)
  require(httr2)
  plumber_qc_chart_simple(env = "PRD",
                          call_id = 12405,
                          user_name = "PIETERVS",
                          sqlfile = "\\\\Inbo-limsbg-prd-labware8.inbo.be\\LABO_FS_PRD\\ANA\\LIMS\\CTR_REALTIME\\2026\\HG_TOT_MA3000_V\\HG_TOT_MA3000_V_260713_153057_cid_12405_PIETERVS.SQL",
                          htmlfile = "\\\\Inbo-limsbg-prd-labware8.inbo.be\\LABO_FS_PRD\\ANA\\LIMS\\CTR_REALTIME\\2026\\HG_TOT_MA3000_V\\HG_TOT_MA3000_V_260713_153057_cid_12405_PIETERVS.HTML", 
                          maxpoints = 30) 
  

  url <- r"(http://127.31.11.24:8000/qc_chart_simple?env=PRD&call_id=12428&username=PIETERVS&sqlfile=\\Inbo-limsbg-prd-labware8.inbo.be\LABO_FS_PRD\ANA\LIMS\CTR_REALTIME\2026\HG_TOT_MA3000_V\HG_TOT_MA3000_V_260714_100626_cid_12428_PIETERVS.SQL&htmlfile=\\Inbo-limsbg-prd-labware8.inbo.be\LABO_FS_PRD\ANA\LIMS\CTR_REALTIME\2026\HG_TOT_MA3000_V\HG_TOT_MA3000_V_260714_100626_cid_12428_PIETERVS.HTML&maxpoints=30)"
  parsed_url <- httr2::url_parse(url)
  function_name <- gsub("^/", "", parsed_url$path)
  params <- parsed_url$query
  result <- c(list(function_name = function_name), params)
  plumber_qc_chart_simple(env = result$env,
                          call_id = result$call_id,
                          user_name = result$user_name,
                          sqlfile = result$sqlfile,
                          htmlfile = result$htmlfile,
                          maxpoints = result$maxpoints)
  
}


