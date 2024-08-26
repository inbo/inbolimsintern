
library(tidyverse)
library(glue)
library(readxl)

df <- read_excel("c:/Users/pieter_verschelde/Desktop/V-23W090-01_Bad_ExternSampleID.xlsx")
qries <- ""
for (i in 1:nrow(df)) {
  qries <- paste(qries,
                 glue("update sample set SAMPLE_ID = '{df$ExternSampleID_NEW[i]}' where TEXT_ID = '{df$LaboCode[i]}' and SAMPLE_ID = '{df$ExternSampleID_OLD[i]}'"),
                 sep = ";\n")
}

