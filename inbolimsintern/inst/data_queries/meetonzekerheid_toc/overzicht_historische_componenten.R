library(tidyverse)
library(DBI)

qry <- "
WITH ComponentData AS (
    SELECT
        a.group_name,
        r.analysis  COLLATE SQL_Latin1_General_CP1_CS_AS AS analysis,
        r.name  COLLATE SQL_Latin1_General_CP1_CS_AS  AS component,
        r.units COLLATE SQL_Latin1_General_CP1_CS_AS  AS units,
        t.version,
        r.ENTERED_ON,
		r.reportable
    FROM result r
    INNER JOIN test t ON r.test_number = t.test_number
    INNER JOIN analysis a ON t.analysis = a.name AND a.group_name NOT IN ('GENETICA', 'OBSOLETE')
    WHERE r.reportable = 'T' AND r.status IN ('C', 'A')
)
SELECT
    analysis,
    component,
    units,
    first_entry = MIN(ENTERED_ON),
    last_entry = MAX(ENTERED_ON),
    min_analysis_version = MIN(version),
    max_analysis_version = MAX(version),
	reportable
FROM ComponentData
GROUP BY analysis, component, units, reportable
ORDER BY analysis, component
"
all_reportable_results <- dbGetQuery(conn, qry)
write_excel_csv2(all_reportable_results,
                 file = "overzicht_reportable_results.csv")
