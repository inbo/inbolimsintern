TIMEOUT /T 1
set /p Input=Plak de URL:
 
"C:\Zebra\R\R-4.0.4\bin\i386\Rscript.exe" "C:\Zebra\_AUTOMATISATION\R_Archief_Labels.R" %Input%
TIMEOUT /T 60