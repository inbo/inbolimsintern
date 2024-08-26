with kept as (
SELECT tc_txtid = tc.text_id
, tic_txtid = tic.text_id
,  tc.original_sample
, tn_tc = tc.test_number
, tn_tic = tic.test_number
FROM (
    SELECT text_id, original_sample, test_number
    FROM sample s
    INNER JOIN test t ON t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
    WHERE t.analysis = 'C_N_ANAL_V' AND t.status in ('A')
) AS tc
INNER JOIN (
    SELECT text_id, original_sample, test_number
    FROM sample s
    INNER JOIN test t ON t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
    WHERE t.analysis = 'C_TIC_ANALYSER' AND t.status in ('A')
) AS tic ON tc.original_sample = tic.original_sample
) 
select project = s.PROJECT
, batchName = b.NAME
, batchOwner = b.OWNER
, sampleType = s.SAMPLE_TYPE
, testlinksample = s.ORIGINAL_SAMPLE
, duplolinksample = s.C_ORIG_DUP_NUMBER
, sampleStatus = s.STATUS
, entryDate = r.ENTERED_ON
, enteredBy = r.ENTERED_BY
, analysis = r.ANALYSIS
, component = r.NAME
, result = r.[ENTRY]
, testStatus = r.[STATUS] 
, testInstrument = t.[INSTRUMENT]
, batchInstrument = b.[INSTRUMENT]
from RESULT r inner join test t on r.TEST_NUMBER = t.TEST_NUMBER and t.ANALYSIS in ('C_N_ANAL_V','C_TIC_ANALYSER')
inner join SAMPLE s on t.SAMPLE_NUMBER = s.sample_number and s.ORIGINAL_SAMPLE in (select original_sample from kept)
inner join batch b on b.NAME = t.BATCH
and r.STATUS in ('A')
and s.STATUS in ('A')
and r.NAME in ('T.C.DS', 'T.IC.DS')
