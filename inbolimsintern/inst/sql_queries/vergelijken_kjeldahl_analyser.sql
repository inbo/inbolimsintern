/* Purpose: Retrieves nitrogen analysis results for samples that have both Kjeldahl and analyzer measurements.

Key components:
1. paired_samples CTE: Identifies samples with both measurement types
2. Main query: Gets results for these samples plus their mineral N measurements */

WITH paired_samples AS (
   -- Find samples with both Kjeldahl and analyzer measurements
   SELECT s.ORIGINAL_SAMPLE
   FROM result r
   INNER JOIN sample s ON r.sample_number = s.sample_number
   WHERE r.status = 'A' AND s.status = 'A' AND r.ENTRY IS NOT NULL
   AND ((r.analysis IN ('C_N_ANAL_V', 'N_TOT_ANAL_V', 'N_TOT_TIT_V') AND r.NAME = 'T.N.DS')
       OR (r.analysis = 'N_KJEL_TIT' AND r.name = 'N.DS'))
   GROUP BY s.ORIGINAL_SAMPLE
   HAVING COUNT(DISTINCT CASE
       WHEN r.ANALYSIS IN ('C_N_ANAL_V', 'N_TOT_ANAL_V') THEN 'analyser'
       WHEN r.ANALYSIS IN ('N_KJEL_TIT', 'N_TOT_TIT_V') THEN 'Kjeldahl'
   END) = 2
)
SELECT DISTINCT
   s.PROJECT,
   s.ORIGINAL_SAMPLE,
   (SELECT TOP 1 TEXT_ID FROM sample WHERE ORIGINAL_SAMPLE = s.ORIGINAL_SAMPLE AND status = 'A') as SAMPLE_TEXT_ID,
   r.ANALYSIS,
   r.NAME,
   r.ENTRY,
   r.ENTERED_ON,
   CASE
       WHEN r.ANALYSIS IN ('C_N_ANAL_V', 'N_TOT_ANAL_V') THEN 'analyser'
       WHEN r.ANALYSIS IN ('N_KJEL_TIT', 'N_TOT_TIT_V') THEN 'Kjeldahl'
   END as METHOD_TYPE
FROM result r
INNER JOIN sample s ON r.sample_number = s.sample_number
WHERE r.status = 'A' AND s.status = 'A' AND r.ENTRY IS NOT NULL
AND (
   -- Get total N measurements for samples with both methods
   (s.ORIGINAL_SAMPLE IN (SELECT ORIGINAL_SAMPLE FROM paired_samples)
   AND ((r.analysis IN ('C_N_ANAL_V', 'N_TOT_ANAL_V', 'N_TOT_TIT_V') AND r.NAME = 'T.N.DS')
   OR (r.analysis = 'N_KJEL_TIT' AND r.name = 'N.DS')))
   OR
   -- Include mineral N measurements for these samples
   (s.ORIGINAL_SAMPLE IN (SELECT ORIGINAL_SAMPLE FROM paired_samples)
   AND r.analysis = 'N_MIN_V'
   AND r.name IN ('NH4.N.DS', 'NO2.N.DS', 'NO3.N.DS', 'T.MIN.N.DS'))
)
ORDER BY s.ORIGINAL_SAMPLE, r.ANALYSIS, r.NAME
