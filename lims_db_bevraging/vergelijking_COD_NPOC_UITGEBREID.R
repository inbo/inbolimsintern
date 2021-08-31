sqlquery <- "
select
s.PROJECT, s.TEXT_ID, s.C_SAMPLE_MATRIX, S_STATUS = s.status
, r.ANALYSIS, r.NAME, r.ENTRY, r.STATUS
from result r
inner join sample s on s.SAMPLE_NUMBER = r.SAMPLE_NUMBER
where
r.STATUS = 'A'
AND
((ANALYSIS = 'CN_NPOC_TN_ANAL_W' and NAME = 'NPOC')
  OR (ANALYSIS = 'CN_NPOC_TN_ANAL_W' and NAME = 'T.N')
  OR (ANALYSIS = 'COD_HACH' and NAME = 'COD')
  OR (ANALYSIS = 'COD_FOTO_PF-3_W' and NAME = 'COD')
  OR (ANALYSIS in ('ALK_OEP_PH_EC_W', 'ALK_TEP_PH_EC_W') and name = 'pH')
  OR (ANALYSIS in ('ALK_OEP_PH_EC_W', 'ALK_TEP_PH_EC_W') and name = 'EC')
  OR (ANALYSIS in ('ALK_OEP_PH_EC_W', 'ALK_TEP_PH_EC_W') and name = 'ALK')
  OR (ANALYSIS = 'ICP_MINMET_HNO3_1' and name = 'S')
  OR (ANALYSIS in ('P_TOT_CFA_W','P_TOT_L_CFA_W ','P_TOT_L_ICP_W') and name = 'T.P')
  OR (ANALYSIS = 'CHLA_Spectro' and name = 'CHL.A')
)"

connectlist <- read_db_credentials()
conn <- limsdb_connect(uid = connectlist$uid, pwd = connectlist$pwd)
dfOrig <- dbGetQuery(conn, sqlquery)
str(dfOrig)

### checks

#ruwe inschatting van het aantal records per analysecomponent
dfOrig %>%
  filter(!is.na(C_SAMPLE_MATRIX)) %>%
  group_by(ANALYSIS, NAME) %>%
  summarize(Aantal = n()) %>%
  arrange(NAME, desc(Aantal))

#Check aantal records per text_id
check <-
  dfOrig %>%
    group_by(PROJECT, C_SAMPLE_ID, TEXT_ID) %>%
    summarise(aantal = n()) %>%
    arrange(desc(aantal))
table(check$aantal)


#Filter de records die op voorbaat niet kunnen (slehts 1 resultqqt, geen matrix, geen numerieke entry)
dfVgl <-
  dfOrig %>%
  mutate(NUM_ENTRY = as.numeric(ENTRY),
         C_SAMPLE_MATRIX = toupper(C_SAMPLE_MATRIX)) %>%
  filter(!(TEXT_ID %in% (check %>% filter(aantal %in% c(1)) %>%
                                  pull(TEXT_ID))),
         !is.na(NUM_ENTRY),
         !is.na(C_SAMPLE_MATRIX))

#Check of COD en NPOC aanwezig zijn
check2 <-
  dfVgl %>% group_by(PROJECT, C_SAMPLE_MATRIX, TEXT_ID) %>%
    summarise(n_COD = sum(NAME == 'COD'),
              n_NPOC = sum(NAME == 'NPOC'),
              EQ_COD = n_COD - n_NPOC == 0,
              valid = EQ_COD  & n_COD > 0)
#View(check2 %>% filter(!valid ))

#Filter de records die geen COD of NPOC bevatten
dfVgl <- dfVgl %>%
  filter(!(TEXT_ID %in% (check2 %>% filter(!valid) %>% pull(TEXT_ID)))) %>%
  mutate(WAARDE = as.numeric(ENTRY))

#Converteer naar breed formaat
dfPivot <- dfVgl %>%
  pivot_wider(id_cols = c(PROJECT, TEXT_ID, C_SAMPLE_MATRIX),
              names_from = NAME,
              values_from = WAARDE)

write_excel_csv2(dfPivot, path = "lims_db_bevraging/output/vergelijking_COD_NPOC_2.csv")

p <- ggplot(dfPivot, aes(x = NPOC, y = COD)) +
  geom_point() + geom_smooth() +
  facet_wrap(~C_SAMPLE_MATRIX)
ggsave(p, filename = "lims_db_bevraging/output/vergelijking_COD_NPOC_1.png",
       dpi = 300, width = 7, height = 5)
p <- p + coord_cartesian(ylim = c(0,150))
ggsave(p, filename = "lims_db_bevraging/output/vergelijking_COD_NPOC_1b.png",
       dpi = 300, width = 7, height = 5)

ggplot(dfPivot, aes(x = PROJECT, y = COD/NPOC)) + geom_point() +
  facet_wrap(~C_SAMPLE_MATRIX) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("lims_db_bevraging/output/vergelijking_COD_NPOC_2.png",
       dpi = 300, width = 7, height = 5)

cor(dfPivot[,4:11])






