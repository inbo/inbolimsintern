
#Let op! Dit script werkt enkel in een 32-bit R omgeving indien 32-bit Access
# wat het geval is op het INBO (10 mei 2019)
argumenten <- commandArgs()
google_url <- argumenten[[6]]
db_location <- argumenten[[7]]

library(googlesheets)
library(tidyverse)
library(RODBC) #werkt enkel in 32-bit R

gs_auth() #Eenmalig per werkdirectory moet je google authoriseren

#Haal de info op uit de google sheet
key <- googlesheets::extract_key_from_url(google_url)
ss <- gs_key(key)
titel <- ss$sheet_title

dfLabels <- 
  googlesheets::gs_read_csv(ss, ws = "Labeldata") %>%
  select( StaalType = STAALTYPE, YEAR, AID, SID, LB72X, LB72Y, DEPTH, 
         LabProjectCode = LABProject, LabSampleCode = LabCode)

#dus enkel in 32-bit R
conn <- odbcConnectAccess2007(db_location)
print(conn)
sqlQuery(conn, paste0("drop table [" , titel, "]"))
sqlSave(conn, dfLabels, tablename = titel, colnames = FALSE)
sqlQuery(conn, "drop table tblTemp")
sqlSave(conn, dfLabels, tablename = "tblTemp", colnames = FALSE)

print("OK. Nu kan je in Access het rapport openen")

odbcClose(conn)
    