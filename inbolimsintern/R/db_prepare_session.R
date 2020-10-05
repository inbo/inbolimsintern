


###############################################################

#' Ophalen commando-argumenten
#'
#' @param args de argumenten die meekomen bij het oproepen van het script (meestal commandArgs)
#' @param min_args minimum aantal argumenten om te checken of het een effectieve call is, of een test (bij een test werk je interactief en zijn er veel minder commandoargumenten)
#' @param first_arg de positie van het eerste argument (dit zal 5 of 6 zijn afh. van je R). Het eerste argument is het opgeroepen R script, daarna volgt normaal gezien de data-source, de user-name, het passwoord, en de call_id, die de extra benodigde argumenten voor het script ophaalt in de lims databank (tabel C_RSCRIPT_ARGS)
#' @param cred_file de bestandsnaam (al dan niet met padverwijzing) van het 3 regels lange tekstbestand, waar de data-source, user-name en paswoord kunnen opgehaald worden in die volgorde. Dit is enkel relevant in testmodus, dus als er minder dan min_args argumenten zijn
#' @param call_id het nummer waarmee de argumenten uit de lims databank kunnen opgehaald worden (tabel C_RSCRIPT_ARGS)
#'
#' @return character vector waarbij de elementen met de naam dsn (data-source), uid (gebruikersnaam), pwd (paswoord), call_id (call identificatie) teruggegeven worden
#' @export
#'
#' @examples
#' \dontrun{
#' cred_file <- system.file("extdata", "dbcredentials.txt", package = "inbolimsintern")
#' args <- prepare_session(call_id = 5, cred_file = cred_file)
#' }

prepare_session <- function(call_id = NULL, args = commandArgs(), min_args = 5, first_arg = min_args,
                            cred_file = "dbcredentials.txt") {
  #indien minder dan 5 argumenten, verwacht een call_id die opgegeven is, anders haal die uit de argumenten
  is_test <- ifelse(length(args) < min_args, TRUE, FALSE)
  print(call_id)
  if (is_test) {
    creds <- try(inbolimsintern::read_db_credentials(cred_file))
    if (class(creds)[1] == "try-error") {
      stop("databasse info niet gevonden, zorg dat cred_file verwijst naar een bestaand bestand")
    }
    if (is.null(call_id)) {
      stop("call_id moet een waarde hebben (bij het testen)")
    }
    argvec <- c(dsn = creds$dsn, uid = creds$uid,
                pwd = creds$pwd, call_id = call_id)
  } else {
    argvec <- c(dsn = args[min_args + 1], uid = args[min_args + 2],
                pwd = args[min_args + 3], call_id = args[min_args + 4])
  }
  argvec
}

