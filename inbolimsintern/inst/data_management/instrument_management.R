
library(inbolimsintern)
library(googlesheets4)

creds <- read_db_credentials(file = "../dbcredentials.txt")
conn <- limsdb_connect(uid = creds$uid, pwd = creds$pwd)

### Definieer nieuwe instrumentenm op basis van google sheet
key <-  "1gKRWiHnZFRb8mtF-ddyeCpQN4Q151_HRDfdoD-mDCTg"
gs4_auth("pieter.verschelde@inbo.be")

qry <- "select naam = NAME, groep = INST_GROUP, sip = C_ALIAS_NAME, gerapporteerd = C_REPORTED_NAME, DESCRIPTION from instruments"
df_inst <- dbGetQuery(conn, qry)

df_instdef <- read_sheet(key, sheet = "QRY_LIMS")
colnames(df_instdef)

qd <- list()
for (i in 1:nrow(df_instdef)) {
  inst_name <- df_instdef$`Instrument naam - LIMS KEY`[i]
  existing <- inst_name %in% df_inst$naam
  if (!existing) {
    qi <- paste0("insert into instruments(NAME, INST_GROUP, GROUP_NAME, DESCRIPTION, ON_LINE, SERIAL_NO, C_ALIAS_NAME, C_REPORTED_NAME, CHANGED_ON, CHANGED_BY, REMOVED) VALUES (",
                 "'", inst_name, "', " ,
                 "'", df_instdef$`Inst Group`[i] , "', " ,
                 "'ANALYTISCH', " ,
                 "'", df_instdef$`Description Inst`[i] , "', " ,
                 "'T', " ,
                 "'", df_instdef$`Serial No`[i], "', " ,
                 "'", df_instdef$`SIP code`[i], "', " ,
                 "'", df_instdef$`Reported Name`[i], "', " ,
                 "'2022-04-06', " ,
                 "'PIETERVS', " ,
                 "'F');\n ")

  } else {
    qi <- paste0("update instruments set ",
                 "INST_GROUP = '", df_instdef$`Inst Group`[i], "'",
                 ", GROUP_NAME = 'ANALYTISCH'",
                 ", DESCRIPTION = '", df_instdef$`Description Inst`[i], "'",
                 ", ON_LINE = 'T'",
                 ", SERIAL_NO = '", df_instdef$`Serial No`[i], "'",
                 ", C_ALIAS_NAME = '", df_instdef$`SIP code`[i], "'",
                 ", C_REPORTED_NAME = '", df_instdef$`Reported Name`[i], "'",
                 " where name = '", inst_name, "'")
  }
  qd[[i]] <- qi
}

execute <- FALSE
if (execute) {
  for (i in 1: length(qd)) {
    a <- try(dbGetQuery(conn, qd[[i]]))
    if (class(a) == "try-error") {
      print("PROBLEEM")
      print(qd[i])
    }
  }
}




### Maak de lijsten per instrumentgroep
qry <- "select naam = NAME, groep = INST_GROUP, sip = C_ALIAS_NAME, gerapporteerd = C_REPORTED_NAME,  beschrijving = DESCRIPTION from instruments where group_name in ('ANALYTISCH') and removed = 'F'"
df_inst <- dbGetQuery(conn, qry)

qry <- "select name from list where name like 'IG%' and charindex('_', name) = 3"
ig_lists <- dbGetQuery(conn, qry)
q <- list()
for (grp in unique(df_inst$groep)){
  list_name = paste0("IG_", grp)
  print(list_name)

  #kijk of de lijst bestaat
  lijst_bestaat <-  list_name %in% ig_lists$name
  df_groep <- df_inst %>% filter(groep == grp)
  n <- nrow(df_groep)

  if (!lijst_bestaat) {
    print("nieuwe lijst")
    q1 <- paste0("insert into LIST (NAME, GROUP_NAME, REMOVED) VALUES (",
                "'", list_name, "',",
                 "'INBO', 'F');\n")

  } else {
    print('delete list')
    q1 <- paste0("delete from LIST_ENTRY where LIST = '", list_name, "';\n")
  }

  q2 <- q1
  for (i in 1:n) {
    qe <- paste0("insert into LIST_ENTRY(LIST, NAME, VALUE) VALUES (",
                 "'", list_name, "',",
                 "'", df_groep[i, "naam"], "',",
                 "'", paste(df_groep[i, "beschrijving"]), "');\n")
    q2 <- paste0(q2, qe)
  }
  q[[grp]] <- q2
}

execute <- FALSE
if (execute) {
  for (nm in names(q)) {
    e <- try(dbGetQuery(conn, q[[nm]]))
    if (class(e) == "try-error") {
      print('PROBLEEM')
      print(q[nm])
    }
  }
}
