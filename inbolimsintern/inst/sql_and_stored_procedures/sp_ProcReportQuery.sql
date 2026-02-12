USE [D0015_08_Lims]
GO

/****** Object:  StoredProcedure [dbo].[ProcReportQuery]    Script Date: 2/12/2026 2:27:26 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE OR ALTER PROCEDURE [dbo].[ProcReportQuery]

-- Declaraties --

@XtabReport varchar(1),
@ProjectName varchar(50),
@ProjectTemplate varchar(50),
@SampleTypeNull nchar(1),
@SampleType varchar(254),
@SampleStatus varchar (50),
@TestStatus varchar (50),
@Reportable nchar(1),
@FORMATTED_ENTRY nchar(1),
@USERNAME varchar(50)
As
Declare @Sqlstring nvarchar(MAX)
Declare @Sqlstring2 nvarchar(MAX)
Declare @ValueColumn varchar(50)
Declare @xtabsql nvarchar(MAX)
Declare @DescVars varchar(MAX)
Declare @FinalString nvarchar(MAX)
Declare @xtabstring varchar(MAX)
declare @viewName varchar(50)
Declare @ViewString nvarchar(max)


-- Maak de basisquery aan --
--,s.SAMPLED_DATE  as Monsternamedatum
--,s.C_SAMPLED_BY as Monsternemer'

set @SqlString = 'SELECT   TOP (10000000)                
 proj.NAME As LaboProject
,cust.NAME as KlantNaam
,contract.CONTRACT_QUOTE_NO As Contractnummer
,ltrim(rtrim(s.SAMPLE_ID)) As ExternSampleID
,s.[TEXT_ID] As LaboCode
,s.SAMPLE_NUMBER as Staalnummer
,s.PARENT_SAMPLE as OuderStaal
,s.ORIGINAL_SAMPLE as OrigineelStaal
,s.PRODUCT_GRADE as ProductGrade
,s.C_SAMPLE_MATRIX as Matrix
,s.SAMPLING_POINT as SamplingPoint
,s.C_MONSTER_DATE  as Monsternamedatum
,s.C_MONSTER_BY as Monsternemer'



set @Sqlstring = @Sqlstring + '
,s.C_PREP_METHODE  as VoorbehandelingExtern
,s.C_CONDITION as Toestand'

set @Sqlstring = @Sqlstring + '
,s.C_REMARK as Staalopmerking
,t.ANALYSIS As LimsAnalyseNaam
,ana.ALIAS_NAME As SAPcode
,ana.REPORTED_NAME As AnalyseNaam
,res.REPORTED_NAME As Component
,(convert(varchar,t.REPLICATE_COUNT) + ''__'' + convert(varchar,res.REPLICATE_COUNT)) As Rep
,inst.C_REPORTED_NAME As Instrument
,res.ENTRY As WaardeRuw
,res.FORMATTED_ENTRY As WaardeGeformatteerd
,units.DISPLAY_STRING As Eenheid
,res.IN_SPEC As BinnenSpecificatie
,(res.ANALYSIS + ''__'' + res.NAME  + ''__'' +  (convert(varchar,t.REPLICATE_COUNT) + ''__'' + convert(varchar,res.REPLICATE_COUNT))) As UniqueCompReference
,res.RESULT_TYPE Resultaattype
,res.ENTRY_QUALIFIER Limietsymbool
,res.NUMERIC_ENTRY Numeriek
,''' + @USERNAME + '''' + ' as RapportGegenereerdDoor'
set @Sqlstring = @Sqlstring + '
FROM SAMPLE s
INNER JOIN PROJECT As Proj ON s.PROJECT=Proj.NAME
INNER JOIN CONTRACT_QUOTE As Contract ON Contract.CONTRACT_QUOTE_NO = proj.CONTRACT_NUMBER
INNER JOIN CUSTOMER As Cust ON Contract.CUSTOMER = cust.NAME
INNER JOIN TEST As t ON s.SAMPLE_NUMBER = t.SAMPLE_NUMBER
INNER JOIN ANALYSIS As ana ON t.ANALYSIS = ana.NAME and t.VERSION = ana.VERSION
INNER JOIN RESULT As res ON (s.SAMPLE_NUMBER = res.SAMPLE_NUMBER) and (t.TEST_NUMBER = res.TEST_NUMBER)
LEFT JOIN UNITS as units ON res.UNITS = units.UNIT_CODE
LEFT JOIN INSTRUMENTS inst on t.INSTRUMENT = inst.NAME'

set @Sqlstring = @Sqlstring + ' WHERE s.PROJECT = ''' +  @ProjectName + '''' 
 + ' AND res.REPORTABLE = ''' + @Reportable + '''' 
if @SampleTypeNull = 'T'
BEGIN
 set @SqlString = @SqlString + 'AND (s.SAMPLE_TYPE is Null OR s.SAMPLE_TYPE in ' + @SampleType + ')'
END
else
BEGIN
 set @SqlString = @SqlString + ' OR s.SAMPLE_TYPE in ' + @SampleType
END
set @Sqlstring = @Sqlstring + 
' AND s.STATUS in ' + @SampleStatus +
' AND t.STATUS in ' + @TestStatus

set @Sqlstring = @Sqlstring + '
order by proj.Name, s.ORIGINAL_SAMPLE, s.SAMPLE_NUMBER, ana.T_REPORT_HEADER
, t.TEST_NUMBER, res.RESULT_NUMBER, res.STATUS'

-- Nu zetten we deze query in een view om daarop verder te werken (aangezien LIMS niet overweg kan met gedclareerde tabellen als variabele --

set @viewName = 'dbo.C_REPORT_VW'
DROP VIEW dbo.C_REPORT_VW

set @ViewString = 'CREATE VIEW ' + @viewName + ' AS ' + @Sqlstring  
exec sp_executeSQL @viewString

-- Nu de output zelf --

Declare @ShowViewString nvarchar(255)
set @ShowViewString = 'select * from ' + @viewName


-- Voorbereidingen voor de crosstab --

SELECT @xtabstring = COALESCE(@xtabstring + ', ', '') + ('[' + UniqueCompReference + ']')
FROM (SELECT TOP(100000) UniqueCompReference FROM dbo.C_REPORT_VW GROUP BY UniqueCompReference ORDER BY UniqueCompReference) Subq1

set @DescVars = 'LaboProject,KlantNaam,Contractnummer, ExternSampleID,LaboCode,Staalnummer,Ouderstaal,OrigineelStaal,ProductGrade,Matrix,SamplingPoint,MonsternameDatum,Monsternemer, VoorbehandelingExtern,Toestand,Staalopmerking'

if @FORMATTED_ENTRY = 'F'
	set @ValueColumn = 'WaardeRuw'
ELSE
	set @ValueColumn = 'WaardeGeformatteerd'

-- Creeer de Pivot Table --
		
set @xtabsql = 'SELECT  * FROM '
set @xtabsql = @xtabsql + ' (SELECT ' + @DescVars + ',' + @ValueColumn + ',UniqueCompReference FROM (' +  @Sqlstring + ')As Subq ) as SourceTable '
set @xtabsql = @xtabsql + ' PIVOT ( MAX(' + @ValueColumn + ') FOR UniqueCompReference IN (' + @xtabstring + ')) As Pivotttable ORDER BY Staalnummer'



IF @XtabReport <> 'T'
BEGIN
	set  @FinalString = @showViewString
exec sp_executeSQL @FinalString
END	
else
BEGIN
	set @FinalString = @xtabsql
	exec sp_executeSQL @FinalString
END


RETURN

/*
EXEC ProcReportQuery
 @xtabReport = 'F',
@ProjectName = 'I-21V021-01',
@ProjectTemplate = 'EMPTY',
@SampleTypeNULL = 'T',
@SampleType =  '(''QC_METHOD'', ''BLANK'')', 
@SampleStatus = '(''A'')', 
@TestStatus = '(''A'')', 
@Reportable = 'T', 
@FORMATTED_ENTRY = 'T',
@USERNAME = 'PIETERVS'
*/

/*
EXEC ProcReportQuery
 @xtabReport = 'T',
@ProjectName = 'I-21V021-01',
@ProjectTemplate = 'EMPTY',
@SampleTypeNULL = 'T',
@SampleType =  '(''QC_METHOD'', ''BLANK'')', 
@SampleStatus = '(''A'')', 
@TestStatus = '(''A'')', 
@Reportable = 'T', 
@FORMATTED_ENTRY = 'T',
@USERNAME = 'PIETERVS'
*/


GO


