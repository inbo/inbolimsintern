#Let op! Dit script werkt enkel in een 32-bit R omgeving indien 32-bit Access
# wat het geval is op het INBO (10 mei 2019)
#Nu zou dit geen probleem meer mogen vormen omdat iedereen office 365 heeft

#!VOER ONDERSTAAND COMMANDO UIT (zonder het hekje vooraan als google je niet kan inloggen)
#googlesheets4::gs4_auth()

logfile <- "C:\\ZEBRA\\_Automatisation\\R_log.txt"
sink(logfile)
print(Sys.time())
argumenten <- commandArgs()
print(argumenten)
sink()
sink(logfile, append = TRUE)
google_url <- argumenten[[6]]
db_location <- 'C:\\ZEBRA\\_Automatisation\\BodemLabels02.accdb'
library(googlesheets4)
library(tidyverse)
library(RODBC) #werkt enkel in 32-bit R
print("libraries loaded")
sink()

#google_url <- googlesheets4::as_sheets_id('https://docs.google.com/spreadsheets/d/1USbWnsXtxbkmAOS890rgPhaIBbBDv0OPvW4wiiNYPlY/edit#gid=1514754140')

sink(logfile, append = TRUE)
dfLabels <- googlesheets4::read_sheet(google_url, sheet = 1)
colnames(dfLabels) <- toupper(colnames(dfLabels))
if ("LB72X" %in% colnames(dfLabels)) {
  dfLabels <- dfLabels %>%
    select( StaalType = STAALTYPE, YEAR, AID, SID, LB72X, LB72Y, DEPTH,
            LabProjectCode = LABPROJECT, LabSampleCode = LABCODE)
} else if ("WGS84LONG" %in% colnames(dfLabels)) {
  dfLabels <- dfLabels %>%
    select( StaalType = STAALTYPE, YEAR, AID, SID, WGS84LONG, WGS84LAT, DEPTH,
            LabProjectCode = LABPROJECT, LabSampleCode = LABCODE)
} else {
  cat("Geen geldige coördinaten gevonden")
}

sink()

sink(logfile, append = TRUE)
titel <- googlesheets4::gs4_get(google_url)$name
sink()

#dus enkel in 32-bit R
conn <- odbcConnectAccess2007(db_location)

sink(logfile, append = TRUE)
print(conn)
sink()
sink(logfile, append = TRUE)
sqlQuery(conn, paste0("drop table [" , titel, "]"))
sqlSave(conn, dfLabels, tablename = titel, colnames = FALSE)
sink()
sink(logfile, append = TRUE)
sqlQuery(conn, "drop table tblTemp")
sqlSave(conn, dfLabels, tablename = "tblTemp", colnames = FALSE)
sink()

print("OK. Nu kan je in Access het rapport openen")

odbcClose(conn)
