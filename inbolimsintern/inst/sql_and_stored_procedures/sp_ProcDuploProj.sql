USE [D0015_08_Lims]
GO

/****** Object:  StoredProcedure [dbo].[ProcDuploProj]    Script Date: 2/12/2026 2:29:09 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE OR ALTER PROCEDURE [dbo].[ProcDuploProj]

-- Declaraties --

@projectName varchar(20)

AS

 DECLARE @tblDup TABLE(PROJECT varchar(20), SAMPLE_NUMBER Integer, TEXT_ID varchar(20), C_ORIG_DUP_NUMBER Integer,
 ANALYSIS varchar(20),NAME varchar(40),trep integer,rrep integer,NUMERIC_ENTRY float)
 DECLARE @tblOrig TABLE(SAMPLE_NUMBER Integer, TEXT_ID varchar(20), ANALYSIS varchar(20),NAME varchar(40),trep integer,rrep integer,NUMERIC_ENTRY float)
 insert into @tblDup
 select s.PROJECT, s.SAMPLE_NUMBER,  s.TEXT_ID, s.C_ORIG_DUP_NUMBER
 ,r.ANALYSIS, r.NAME, t.REPLICATE_COUNT trep, r.REPLICATE_COUNT rrep, r.NUMERIC_ENTRY 
 FROM SAMPLE s INNER JOIN TEST t on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER INNER JOIN RESULT r on t.TEST_NUMBER = r.TEST_NUMBER
 WHERE s.PROJECT = '' + @projectName + ''
 AND s.STATUS <> 'X' and t.STATUS <> 'X' AND r.STATUS in ('A','M','E')
 AND (s.SAMPLE_TYPE = 'DUP')
 AND r.RESULT_TYPE in ('N', 'K')
 AND r.REPORTABLE = 'T'
 insert into @tblOrig	(SAMPLE_NUMBER, TEXT_ID, ANALYSIS, NAME, trep, rrep, NUMERIC_ENTRY) 
 select s.SAMPLE_NUMBER, s.TEXT_ID, r.ANALYSIS, r.NAME, t.REPLICATE_COUNT trep, r.REPLICATE_COUNT rrep, r.NUMERIC_ENTRY
 FROM SAMPLE s INNER JOIN TEST t on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER INNER JOIN RESULT r on t.TEST_NUMBER = r.TEST_NUMBER
 WHERE s.SAMPLE_NUMBER in (SELECT C_ORIG_DUP_NUMBER from @tblDup)
 AND s.STATUS <> 'X' and t.STATUS <> 'X' AND r.STATUS in ('A','M','E')
 AND ((s.SAMPLE_TYPE <> 'DUP') OR s.SAMPLE_TYPE is Null)
 AND r.RESULT_TYPE in ('N', 'K')
 AND r.REPORTABLE = 'T'
 

 select d.TEXT_ID textidDUP, o.TEXT_ID textidOrig, d.ANALYSIS, d.NAME, d.trep, d.rrep, round(d.NUMERIC_ENTRY,4) dupENTRY, round(o.NUMERIC_ENTRY,4) origENTRY,
 round(d.NUMERIC_ENTRY / (o.NUMERIC_ENTRY + 0.000001) * 100, 2) Ratio
 ,(d.ANALYSIS + ' ' +  d.NAME +  convert(varchar, d.trep) + ' ' +  CONVERT(varchar,d.rrep)) As REFCOL
 ,d.SAMPLE_NUMBER snumDUP
 ,o.SAMPLE_NUMBER snumORIG
 from @tblDup d inner join @tblOrig o on d.C_ORIG_DUP_NUMBER = o.SAMPLE_NUMBER and d.ANALYSIS = o.ANALYSIS and d.NAME = o.NAME and d.trep = o.trep and d.rrep = o.rrep
 commit


 /*
 EXEC dbo.ProcDuploProj
 @ProjectName = 'I-16046-03'
*/

 --create table C_TMP_PROJ_DUP (textidDUP varchar(20), textidOrig varchar(20), ANALYSIS varchar(20), NAME varchar(40), trep integer, rrep integer, 
 --dupEntry float, origEntry float, Ratio float, REFCOL varchar(255), snumDUP integer, snumORIG integer)
GO


