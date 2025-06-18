library(inbolimsintern)
library(tidyverse)


qry_tc <- "
select s.project, s.sample_number, s.text_id, s.original_sample
, s.sample_type, s.sample_name
,t.instrument, t.batch
,r.analysis, r.name, r.entry, r.units, r.entered_on, r.status
from result r
inner join test t on r.test_number = t.test_number
inner join sample s on t.sample_number = s.sample_number
where r.entered_on > '2022-01-01'
and r.analysis = 'C_N_ANAL_V' and r.name = 'T.C.DS'
and t.status in ('C', 'A') and r.status in ('A')
"

#includeert ook de X
qry_tic <- "
select s.project, s.sample_number, s.text_id, s.original_sample
,s.sample_type, s.sample_name, t.batch
,r.analysis, r.name, r.entry, r.units, r.entered_on, r.status
from result r
inner join test t on r.test_number = t.test_number
inner join sample s on t.sample_number = s.sample_number
where r.entered_on > '2022-01-01'
and r.analysis = 'C_TIC_ANALYSER' and r.name = 'T.IC.DS'
and t.status  in ('A', 'C', 'X') and r.status in ('A', 'X')
"

extra_info <- read_tsv(file = "inbolimsintern/inst/get_information/meetonzekerheid_ref.txt", locale = locale(decimal_mark = ","))

creds <- read_csv2("dbcredentials.txt")
conn <- limsdb_connect(uid = creds[[1]][1], pwd = creds[[1]][2])

df_tc <- dbGetQuery(conn, qry_tc)
df_tic <- dbGetQuery(conn, qry_tic) |>
  filter(!(analysis == "C_TIC_ANALYSER" & status == "X" & !is.na(entry)))

df_all <- bind_rows(df_tc, df_tic)
df_all <- df_all |>
  left_join(extra_info,
            join_by(analysis == analysis,
                    name == component,
                    sample_name == reference)) |>
  left_join(extra_info |>
              rename(lod = certificate_value) |>
              filter(reference == "Bepaalbaarheidsgrens") |>
              select(-reference),
            join_by(analysis == analysis,
                    name == component)) |>
  mutate(is_reference = is.na(project),
         entry = as.numeric(entry)) |>
  group_by(original_sample) |>
  mutate(common_sample = n_distinct(analysis) > 1) |>
  select(-sample_type, -instrument)

write_excel_csv2(df_all, "data_meetonzekerheid.csv")


#Meest relevante kolommen:
#--------------------------
# - original_sample: staalidentificatie
# - name: componentnaam T.C.DS (total carbon) T.IC.DS (inorganic carbon)
# - entry: gemeten waarde
# - entered_on: tijdstip van meting
# - certificate_value: certificaat- of verwachte waarde van controlestalen
# - sample_name: type controlestaal
# - lod: detectielimiet
# - is_reference: TRUE indien een controlestaal
# - common_sample: TRUE indien het staal zowel TIC als TC meting heeft

#Opmerkingen:
#------------
# - data vanaf 2022-01-01 (vroeger was niet relevant)
# - batch, sample_number, text_id, analysis, units, status heb je zelf niet nodig, zijn voor probleemoplossing
# - deze data bevat voor TIC nog enkele NA waarden (die er nog instaan op vraag van Gerrit, maar je zelf niet moet gebruiken)
# - Soms kan eenzelfde analyse verschillende metingen hebben (original_sample)
# - instrument staat er niet in, want altijd hetzelfde sinds 2022




