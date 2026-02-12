USE [D0015_08_Lims]
GO

/****** Object:  StoredProcedure [dbo].[ProcPivotOpenPerProjPerAna]    Script Date: 2/12/2026 2:30:46 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE OR ALTER PROCEDURE [dbo].[ProcPivotOpenPerProjPerAna]
--argumentes
@groupNames nvarchar(MAX)

AS
--variables
Declare @anaquery nvarchar(MAX)
Declare @ViewName nvarchar(MAX)
Declare @ViewString nvarchar(MAX)
Declare @Query nvarchar(MAX)
Declare @Nanalyses int
Declare @Counter int
Declare @Analyses nvarchar(MAX)

set @anaquery = 
'
SELECT analysis, 1 as rank 
INTO #tmpAnalyse
FROM (
select analysis from
test t 
inner join sample s on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
inner join project p on s.PROJECT = p.NAME
where p.status in (''C'', ''P'', ''I'', ''U'')
and s.status in (''P'', ''I'')
and t.status in (''P'', ''I'')
and t.group_name in (' + @groupNames + ')
group by t.analysis
) S1
'

exec sp_executeSQL @anaquery
select * from #tmpAnalyse
set @Counter = 1
set @Analyses = ''
/*while @Counter <= @Nanalyses
	BEGIN
	IF @Counter = 1
		BEGIN
		set @Analyses = @Analyses + (Select Analyse from #tmpAnalyse where ROWID = @Counter)
		END
    ELSE
		BEGIN
        set @Analyses = @Analyses + ',' +  (Select Analyse from #tmpAnalyse where ROWID = @Counter)
		END
	SET @Counter=@Counter+1
	END
print @Analyses
*/




RETURN

/*
@query = '
select * from
(
select p.NAME, t.analysis, Openstaand = count(t.test_number) from 
test t 
inner join sample s on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER
inner join project p on s.PROJECT = p.NAME
where p.status in (''C'', ''P'', ''I'', ''U'')
and s.status in (''P'', ''I'')
and t.status in (''P'', ''I'')
group by t.analysis,p.NAME
) as src
pivot (
sum(Openstaand)
for ANALYSIS in (' +  @analysen + ')
) as pivo'

*/

RETURN
GO


