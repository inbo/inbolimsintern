CREATE OR ALTER PROCEDURE dbo.ProcBatchWorksheet
    @BatchName NVARCHAR(100),
    @JsonOutput NVARCHAR(MAX) = NULL OUTPUT
AS
BEGIN
    SET NOCOUNT ON;
    BEGIN TRY
        DECLARE @ColumnNames NVARCHAR(MAX) = '';
        DECLARE @DynamicQuery NVARCHAR(MAX) = '';
        DECLARE @LocalJson NVARCHAR(MAX); 

        -- 1. Build the Pivot column list using the original names
        SELECT @ColumnNames += QUOTENAME(NAME) + ','
        FROM (
            SELECT DISTINCT r.NAME, c.ORDER_NUMBER
            FROM RESULT r
            INNER JOIN TEST t ON r.TEST_NUMBER = t.TEST_NUMBER
            INNER JOIN COMPONENT c ON r.ANALYSIS = c.ANALYSIS 
                                  AND r.NAME = c.NAME 
                                  AND t.VERSION = c.VERSION
            WHERE t.BATCH = @BatchName
        ) AS OrderedColumns
        ORDER BY ORDER_NUMBER;

        -- Exit if no data found
        IF LEN(@ColumnNames) = 0 
        BEGIN
            SET @JsonOutput = '[]';
            SELECT @JsonOutput AS JsonResult;
            RETURN;
        END

        -- Trim the trailing comma
        SET @ColumnNames = LEFT(@ColumnNames, LEN(@ColumnNames) - 1);

        -- 2. Build the Dynamic SQL
        -- We select ID, Pos, and TEXT_ID as the grouping columns
        SET @DynamicQuery = '
        SELECT @JsonResult = (
            SELECT 
				        BATCH,
                ID, 
                Pos, 
                TEXT_ID, 
                ' + @ColumnNames + '
            FROM (
                SELECT 
					          t.BATCH,
                    CAST(s.SAMPLE_NUMBER AS VARCHAR) + ''-'' + CAST(t.TEST_NUMBER AS VARCHAR) AS ID, 
                    bo.ORDER_NUMBER AS Pos, 
                    s.TEXT_ID, 
                    r.NAME, 
                    r.ENTRY
                FROM RESULT r
                INNER JOIN TEST t ON r.TEST_NUMBER = t.TEST_NUMBER
                INNER JOIN SAMPLE s ON s.SAMPLE_NUMBER = t.SAMPLE_NUMBER
                INNER JOIN BATCH_OBJECTS bo ON bo.BATCH = t.BATCH AND bo.SAMPLE_NUMBER = t.SAMPLE_NUMBER
                WHERE t.BATCH = @B
            ) AS Source
            PIVOT (
                MAX(ENTRY) 
                FOR NAME IN (' + @ColumnNames + ')
            ) AS PivotTable
            ORDER BY Pos
            FOR JSON AUTO
        );'; 

        -- 3. Execute and capture the output
        EXEC sp_executesql @DynamicQuery, 
             N'@B NVARCHAR(100), @JsonResult NVARCHAR(MAX) OUTPUT', 
             @B = @BatchName, 
             @JsonResult = @LocalJson OUTPUT;

        -- Map local result to output parameter and return the result set
        SET @JsonOutput = @LocalJson;
        SELECT @JsonOutput AS JsonResult;

    END TRY
    BEGIN CATCH
        -- Return the error message to LIMS for debugging
        DECLARE @ErrorMessage NVARCHAR(4000) = ERROR_MESSAGE();
        RAISERROR(@ErrorMessage, 16, 1);
    END CATCH
END
GO

/*
DECLARE @json NVARCHAR(MAX);
EXEC dbo.ProcBatchWorksheet @BatchName = 'ICP_W-260105-1', @JsonOutput = @json OUTPUT;
*/