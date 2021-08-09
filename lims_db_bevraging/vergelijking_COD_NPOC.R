sqlquery <- "
select
s.PROJECT, s.TEXT_ID, s.C_SAMPLE_MATRIX, S_STATUS = s.status
, r.NAME, r.ENTRY, r.STATUS
from result r
inner join sample s on s.SAMPLE_NUMBER = r.SAMPLE_NUMBER
where
r.STATUS = 'A'
AND
((ANALYSIS = 'CN_NPOC_TN_ANAL_W' and NAME = 'NPOC')
  OR (ANALYSIS = 'COD_HACH' and NAME = 'COD')
  OR (ANALYSIS = 'COD_FOTO_PF-3_W' and NAME = 'COD')
)"

connectlist <- read_db_credentials()
conn <- limsdb_connect(uid = connectlist$uid, pwd = connectlist$pwd)
dfOrig <- dbGetQuery(conn, sqlquery)
str(dfOrig)

check <-
  dfOrig %>%
    group_by(TEXT_ID) %>%
    summarise(aantal = n()) %>%
    arrange(desc(aantal))
table(check$aantal)

View(check %>% filter(aantal == 3))

dfOrig %>% filter(TEXT_ID == '15-001890')

#deze die 3 keer voorkomen zijn telkens 3x NPOC en zijn kalibratieprojecten en hebben geen COD, deze die maar 1 keer voorkomen zijn per definitie uitgesloten
dfVgl <-
  dfOrig %>%
  filter(!(TEXT_ID %in% (check %>% filter(aantal %in% c(1,3)) %>%
                                  pull(TEXT_ID))))
check2 <-
  dfVgl %>% group_by(TEXT_ID) %>%
    summarise(COD = sum(NAME == 'COD'),
              NPOC = sum(NAME == 'NPOC'),
              valid = COD - NPOC == 0)
View(check2 %>% filter(!valid ))


dfVgl <- dfVgl %>%
  filter(!(TEXT_ID %in% (check2 %>% filter(!valid) %>% pull(TEXT_ID)))) %>%
  mutate(WAARDE = as.numeric(ENTRY))

dfPivot <- dfVgl %>%
  pivot_wider(id_cols = c(PROJECT, TEXT_ID, C_SAMPLE_MATRIX),
              names_from = NAME,
              values_from = WAARDE)

write_excel_csv2(dfPivot, path = "lims_db_bevraging/output/vergelijking_COD_NPOC_1.csv")

ggplot(dfPivot, aes(x = NPOC, y = COD)) + geom_point()
ggsave("lims_db_bevraging/output/vergelijking_COD_NPOC_1.png",
       dpi = 300, width = 7, height = 5)

ggplot(dfPivot, aes(x = PROJECT, y = COD/NPOC)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("lims_db_bevraging/output/vergelijking_COD_NPOC_2.png",
       dpi = 300, width = 7, height = 5)




