TIMEOUT /T 1
Set /p Input=Plak de URL:

"C:/Program Files/R/R-35~1.2/bin/i386/Rscript.exe" "C:/ZEBRA/_AUTOMATISATIE/ArchiefLabels.R" %Input% "C:/ZEBRA/_AUTOMATISATIE/ArchiefLabelsDB.accdb"
TIMEOUT /T 5