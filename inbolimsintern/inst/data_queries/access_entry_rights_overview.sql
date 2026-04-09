SELECT 
    FUNCTION_ID, 
    ISNULL([LABORANT], 0) AS LABORANT, 
    ISNULL([BEHEERDER], 0) AS BEHEERDER, 
    ISNULL([SYSTEM], 0) AS [SYSTEM]
FROM (
    SELECT USER_NAME, FUNCTION_ID
    FROM access_entry
    WHERE USER_NAME IN ('LABORANT', 'BEHEERDER', 'SYSTEM')
) AS SourceTable
PIVOT (
    COUNT(USER_NAME) 
    FOR USER_NAME IN ([LABORANT], [BEHEERDER], [SYSTEM])
) AS PivotTable
ORDER BY 
    LABORANT DESC,    -- Functions the Laborant HAS come first
    BEHEERDER DESC,   -- Then functions the Beheerder HAS
    [SYSTEM] DESC,    -- Then functions the System HAS
    FUNCTION_ID ASC;  -- Finally, sort alphabetically by ID