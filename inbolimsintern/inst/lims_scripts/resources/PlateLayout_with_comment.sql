-- =====================================================================================
-- CTE 1: ResultData
-- Purpose: Extract DNA and MILLIQ result entries for samples in the project
-- =====================================================================================
WITH ResultData AS (
    SELECT 
        s.SAMPLE_NUMBER,
        -- Pivot DNA and MILLIQ results into separate columns
        MAX(CASE WHEN r.NAME = 'DNA' THEN r.ENTRY END) AS DNA,
        MAX(CASE WHEN r.NAME = 'MILLIQ' THEN r.ENTRY END) AS MilliQ
    FROM SAMPLE s
    -- Join from SAMPLE (filtered by PROJECT) to reduce initial dataset
    JOIN TEST t ON t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
    JOIN ANALYSIS a ON a.NAME = t.ANALYSIS AND a.VERSION = t.VERSION
    JOIN RESULT r ON r.TEST_NUMBER = t.TEST_NUMBER
                 AND r.NAME IN ('MILLIQ', 'DNA') 
                 AND r.STATUS <> 'X'              
    WHERE s.PROJECT = <<ProjectName>>              
      AND a.ANALYSIS_TYPE = 'DNA_TEMPLATE'        
    GROUP BY s.SAMPLE_NUMBER
),

-- =====================================================================================
-- CTE 2: QCValues
-- Purpose: Get the DNA and MILLIQ values from the QC_METHOD sample
--          These values will be used for QC_METHOD sample types in final output
-- =====================================================================================
QCValues AS (
    SELECT TOP 1 
        TRY_CAST(rd.DNA AS FLOAT) as DNA,         
        TRY_CAST(rd.MilliQ AS FLOAT) as MilliQ    
    FROM SAMPLE s
    JOIN ResultData rd ON rd.SAMPLE_NUMBER = s.SAMPLE_NUMBER
    WHERE s.SAMPLE_TYPE = 'QC_METHOD'            
      AND s.PROJECT = <<ProjectName>>
),

-- =====================================================================================
-- CTE 3: BaseData
-- Purpose: Main dataset containing sample information, plate positions, and results
--          Links samples with their parent samples and respective DNA/MilliQ values
-- =====================================================================================
BaseData AS (
    SELECT 
        pp.PLATE,
        CHAR(pp.ROW_NUMBER + 64) AS CAPILAR_LETTER,  
        pp.COLUMN_NUMBER AS LANE,
        s.SAMPLE_TYPE,                                
        s.TEXT_ID,
        s.SAMPLE_NUMBER,
        s.PARENT_SAMPLE,                             
        ps.TEXT_ID AS PARENT_TEXT_ID,                 
        rd.DNA,                                        
        rd.MilliQ,                                    
        prd.DNA AS DNApar,                            
        prd.MilliQ AS MilliQpar                       
    FROM PLATE plt
    JOIN PLATE_POSITION pp ON plt.NAME = pp.PLATE
    JOIN SAMPLE s ON pp.SAMPLE_NUMBER = s.SAMPLE_NUMBER
                 AND s.PROJECT = <<ProjectName>>       
    LEFT JOIN SAMPLE ps ON s.PARENT_SAMPLE = ps.SAMPLE_NUMBER  
    LEFT JOIN ResultData rd ON s.SAMPLE_NUMBER = rd.SAMPLE_NUMBER  
    LEFT JOIN ResultData prd ON s.PARENT_SAMPLE = prd.SAMPLE_NUMBER  
    WHERE plt.BATCH_NAME = <<BatchName>>                       
),

-- =====================================================================================
-- CTE 4: LocationData
-- Purpose: Build location reference strings for related samples
--          Format: "PLATE_SHORTROW##" (e.g., "P1A01 P1B02")
-- =====================================================================================
LocationData AS (
    -- ---------------------------------------------------------------------------------
    -- Part A: For SUBSAMPLE records
    -- Show: Parent position + all sibling subsample positions (excluding self)
    -- ---------------------------------------------------------------------------------
    SELECT 
        b.SAMPLE_NUMBER,
        STUFF((
            SELECT ' ' + CAST(s2.C_PLATE_SHORT AS VARCHAR(10)) +  
                   CHAR(pp2.ROW_NUMBER + 64) +                      
                   RIGHT('0' + CAST(pp2.COLUMN_NUMBER AS VARCHAR(2)), 2)  
            FROM SAMPLE s2
            JOIN PLATE_POSITION pp2 ON pp2.SAMPLE_NUMBER = s2.SAMPLE_NUMBER
            WHERE (s2.SAMPLE_NUMBER = b.PARENT_SAMPLE          
                   OR s2.PARENT_SAMPLE = b.PARENT_SAMPLE)      
              AND s2.PROJECT = <<ProjectName>>  
              AND s2.SAMPLE_NUMBER <> b.SAMPLE_NUMBER          
            FOR XML PATH(''), TYPE).value('.', 'NVARCHAR(MAX)'), 
        1, 1, '') AS AllLocs  
    FROM BaseData b
    WHERE b.SAMPLE_TYPE = 'SUBSAMPLE'
    
    UNION ALL
    
    -- ---------------------------------------------------------------------------------
    -- Part B: For PARENT samples (non-subsample records that have children)
    -- Show: All subsample positions that belong to this parent
    -- ---------------------------------------------------------------------------------
    SELECT 
        b.SAMPLE_NUMBER,
        STUFF((
            SELECT ' ' + CAST(s2.C_PLATE_SHORT AS VARCHAR(10)) +   
                   CHAR(pp2.ROW_NUMBER + 64) +                      
                   RIGHT('0' + CAST(pp2.COLUMN_NUMBER AS VARCHAR(2)), 2)  
            FROM SAMPLE s2
            JOIN PLATE_POSITION pp2 ON pp2.SAMPLE_NUMBER = s2.SAMPLE_NUMBER
            WHERE s2.PARENT_SAMPLE = b.SAMPLE_NUMBER           
              AND s2.PROJECT = <<ProjectName>>  
              AND s2.SAMPLE_TYPE = 'SUBSAMPLE'                 
            FOR XML PATH(''), TYPE).value('.', 'NVARCHAR(MAX)'), 
        1, 1, '') AS AllLocs  -- Remove leading space with STUFF
    FROM BaseData b
    WHERE b.SAMPLE_TYPE <> 'SUBSAMPLE'                        
      AND b.PARENT_SAMPLE = 0                           
)

-- =====================================================================================
-- Final SELECT: Generate 5 rows per sample using CROSS APPLY
-- Row 1: Sample Text ID (with 'S' suffix for subsamples)
-- Row 2: Parent Text ID in parentheses (subsamples only)
-- Row 3: First ~12 characters of location references
-- Row 4: Next ~12 characters of location references  
-- Row 5: Calculated values (M = MilliQ, D = DNA)
-- =====================================================================================
SELECT 
    b.PLATE, 
    b.CAPILAR_LETTER AS CAPILAR, 
    b.LANE,
    d.RowSequence,
    d.Content
FROM BaseData b
LEFT JOIN LocationData ld ON b.SAMPLE_NUMBER = ld.SAMPLE_NUMBER
CROSS APPLY (
    -- Row 1: Display sample text ID with 'S' suffix for subsamples
    SELECT 1, CAST(b.TEXT_ID AS VARCHAR(50)) + 
              CASE WHEN b.SAMPLE_TYPE = 'SUBSAMPLE' THEN 'S' ELSE '' END
    
    UNION ALL
    
    -- Row 2: Display parent text ID in parentheses (subsamples only)
    SELECT 2, '(' + CAST(b.PARENT_TEXT_ID AS VARCHAR(50)) + ')' 
    WHERE b.SAMPLE_TYPE = 'SUBSAMPLE'
    
    UNION ALL
    
    -- Row 3: First portion of location string (up to 12 chars, ~2 locations)
    SELECT 3, LEFT(ISNULL(ld.AllLocs, ''), 12) 
    WHERE ld.AllLocs IS NOT NULL AND LEN(ld.AllLocs) > 0
    
    UNION ALL
    
    -- Row 4: Second portion of location string (chars 14-25, ~2 more locations)
    SELECT 4, SUBSTRING(ISNULL(ld.AllLocs, ''), 14, 12) 
    WHERE ld.AllLocs IS NOT NULL AND LEN(ld.AllLocs) >= 14
    
    UNION ALL
    
    -- Row 5: Calculated MilliQ and DNA values
    -- Format: "M[value] D[value]"
    -- Logic: BLANK=0, QC_METHOD=use QC values, others=use sample or parent values
    SELECT 5, 'M' + CAST(ROUND(COALESCE(
        CASE 
            WHEN b.SAMPLE_TYPE = 'BLANK' THEN 0 
            WHEN b.SAMPLE_TYPE = 'QC_METHOD' THEN (SELECT MilliQ FROM QCValues)
            ELSE COALESCE(TRY_CAST(b.MilliQ AS FLOAT), TRY_CAST(b.MilliQpar AS FLOAT))
        END, 0), 2) AS VARCHAR(10)) + 
              ' D' + CAST(ROUND(COALESCE(
        CASE 
            WHEN b.SAMPLE_TYPE = 'BLANK' THEN 0 
            WHEN b.SAMPLE_TYPE = 'QC_METHOD' THEN (SELECT DNA FROM QCValues)
            ELSE COALESCE(TRY_CAST(b.DNA AS FLOAT), TRY_CAST(b.DNApar AS FLOAT))
        END, 0), 2) AS VARCHAR(10))
) AS d(RowSequence, Content)
-- Order by plate, then lane (column), then capillary (row), then row sequence
ORDER BY b.PLATE, b.LANE, b.CAPILAR_LETTER, d.RowSequence;
