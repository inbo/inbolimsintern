CREATE OR ALTER PROCEDURE dbo.ProcPlateLayout
    @ProjectName NVARCHAR(100),
    @BatchName NVARCHAR(100)
AS
BEGIN
    SET NOCOUNT ON;
    BEGIN TRY
        -- 1. Optimized: Capture the Master QC values into a tiny CTE or Variable
        -- This avoids running a Window Function over the whole set
        DECLARE @MasterQCMilliQ NVARCHAR(50) = '0';
        DECLARE @MasterQCDNA NVARCHAR(50) = '0';

        SELECT TOP 1 
            @MasterQCMilliQ = ISNULL(r.ENTRY, '0'),
            @MasterQCDNA = ISNULL(r2.ENTRY, '0')
        FROM sample s
        INNER JOIN plate p ON s.project = p.project 
            AND s.project = @ProjectName 
            AND p.BATCH_NAME = @BatchName
        INNER JOIN plate_position pp ON s.SAMPLE_NUMBER = pp.SAMPLE_NUMBER AND pp.PLATE = p.NAME
        LEFT JOIN RESULT r ON s.SAMPLE_NUMBER = r.SAMPLE_NUMBER AND r.NAME = 'MILLIQ' AND r.STATUS <> 'X'
        LEFT JOIN RESULT r2 ON s.SAMPLE_NUMBER = r2.SAMPLE_NUMBER AND r2.NAME = 'DNA' AND r2.STATUS <> 'X'
        WHERE s.SAMPLE_TYPE = 'QC_METHOD'
        ORDER BY p.NAME, pp.ROW_NUMBER, pp.COLUMN_NUMBER;

        -- 2. Your Original Base Query (Now using the Variables)
        WITH PlateBase AS (
            SELECT 
                s.SAMPLE_NUMBER, 
                s.TEXT_ID, 
                s.SAMPLE_TYPE, 
                s.STATUS, 
                s.PARENT_SAMPLE, 
                p.NAME AS plate, 
                s2.TEXT_ID AS parent_text_id,
                pp.ROW_NUMBER,
                pp.COLUMN_NUMBER AS LANE,
                CASE WHEN s.PARENT_SAMPLE > 0 THEN s.PARENT_SAMPLE ELSE s.SAMPLE_NUMBER END AS FamilyID,
                CAST(s.TEXT_ID AS VARCHAR(50)) + 
                    CASE WHEN s.SAMPLE_TYPE IN ('S', 'SUBSAMPLE') THEN 'S' ELSE '' END AS DisplayID
            FROM sample s
            INNER JOIN plate p ON s.project = p.project  
                AND s.project = @ProjectName 
                AND p.BATCH_NAME = @BatchName
                AND s.status <> 'X'
            INNER JOIN plate_position pp ON s.SAMPLE_NUMBER = pp.SAMPLE_NUMBER AND pp.PLATE = p.NAME
            LEFT JOIN sample s2 ON s.PARENT_SAMPLE = s2.SAMPLE_NUMBER
        ),

        FamilyLocations AS (
            SELECT 
                pb.SAMPLE_NUMBER,
                STUFF((
                    SELECT ' ' + CAST(ISNULL(s_rel.C_PLATE_SHORT, '??') AS VARCHAR(10)) + 
                           CHAR(pp_rel.ROW_NUMBER + 64) +                                 
                           RIGHT('0' + CAST(pp_rel.COLUMN_NUMBER AS VARCHAR(2)), 2)       
                    FROM sample s_rel
                    JOIN plate_position pp_rel ON s_rel.SAMPLE_NUMBER = pp_rel.SAMPLE_NUMBER
                    WHERE (s_rel.SAMPLE_NUMBER = pb.FamilyID OR s_rel.PARENT_SAMPLE = pb.FamilyID)
                      AND s_rel.SAMPLE_NUMBER <> pb.SAMPLE_NUMBER
                      AND s_rel.PROJECT = @ProjectName
                    FOR XML PATH(''), TYPE).value('.', 'NVARCHAR(MAX)'), 1, 1, '') AS AllLocs
            FROM PlateBase pb
        ),

        ResultData AS (
            SELECT 
                r.SAMPLE_NUMBER,
                MAX(CASE WHEN r.NAME = 'MILLIQ' THEN r.ENTRY END) AS MilliQVal,
                MAX(CASE WHEN r.NAME = 'DNA' THEN r.ENTRY END) AS DNAVal
            FROM RESULT r
            WHERE r.NAME IN ('MILLIQ', 'DNA') AND r.STATUS <> 'X'
            GROUP BY r.SAMPLE_NUMBER
        ),

        FullDataset AS (
            SELECT 
                pb.*,
                fl.AllLocs,
                -- Logic: QC_METHOD gets the pinned variables, others get normal results
                CASE 
                    WHEN pb.SAMPLE_TYPE = 'QC_METHOD' THEN @MasterQCMilliQ
                    ELSE COALESCE(rd_self.MilliQVal, rd_parent.MilliQVal, '0') 
                END AS FinalMilliQ,
                CASE 
                    WHEN pb.SAMPLE_TYPE = 'QC_METHOD' THEN @MasterQCDNA
                    ELSE COALESCE(rd_self.DNAVal, rd_parent.DNAVal, '0') 
                END AS FinalDNA
            FROM PlateBase pb
            LEFT JOIN FamilyLocations fl ON pb.SAMPLE_NUMBER = fl.SAMPLE_NUMBER
            LEFT JOIN ResultData rd_self ON pb.SAMPLE_NUMBER = rd_self.SAMPLE_NUMBER
            LEFT JOIN ResultData rd_parent ON pb.PARENT_SAMPLE = rd_parent.SAMPLE_NUMBER
        )

        SELECT 
            fd.plate,
            CHAR(fd.ROW_NUMBER + 64) AS CAPILAR,
            fd.LANE,
            d.RowSequence,
            d.Content
        FROM FullDataset fd
        CROSS APPLY (
            SELECT 1, fd.DisplayID
            UNION ALL
            SELECT 2, '(' + fd.parent_text_id + ')'
            WHERE fd.SAMPLE_TYPE IN ('S', 'SUBSAMPLE') AND fd.parent_text_id IS NOT NULL
            UNION ALL
            SELECT 3, LEFT(fd.AllLocs, 11) WHERE LEN(fd.AllLocs) > 0
            UNION ALL
            SELECT 4, SUBSTRING(fd.AllLocs, 13, 11) WHERE LEN(fd.AllLocs) >= 13
            UNION ALL
            SELECT 5, 'M ' + LEFT(fd.FinalMilliQ, 4) + ' D ' + LEFT(fd.FinalDNA, 4)
        ) AS d(RowSequence, Content)
        ORDER BY fd.plate, fd.ROW_NUMBER, fd.LANE, d.RowSequence;

    END TRY
    BEGIN CATCH
        DECLARE @ErrorMessage NVARCHAR(4000) = ERROR_MESSAGE();
        RAISERROR(@ErrorMessage, 16, 1);
    END CATCH
END
/*
-- Example Execution:
EXEC dbo.ProcPlateLayout 
    @ProjectName = 'V-26G043-01', 
    @BatchName = '517';
*/

