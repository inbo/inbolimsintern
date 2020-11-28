
qry_candidates <-
"
select
pg.PRODUCT
, pg.VERSION
, pg.SAMPLING_POINT
, pg.GRADE
, qcs.QC_TYPE
, ps.STAGE
, ps.ANALYSIS
, ps.COMPONENT
, ps.MIN_VALUE
, ps.MAX_VALUE
, ps.NOMINAL_VALUE
, ps.LO_CONTROL_1
, ps.HI_CONTROL_1
, cp.REPORTABLE
from PRODUCT_GRADE pg
inner join QC_SAMPLES qcs on pg.GRADE = qcs.NAME
inner join VERSIONS v on v.NAME = pg.PRODUCT and v.VERSION = pg.VERSION
inner join PRODUCT_SPEC ps on ps.GRADE = pg.GRADE
and ps.VERSION = pg.VERSION
and ps.PRODUCT = pg.PRODUCT
inner join
(	select component.analysis, component.name, component.version, component.reportable
  from component  inner join versions on component.VERSION = versions.VERSION
  and component.ANALYSIS = versions.NAME
  and versions.TABLE_NAME = 'ANALYSIS'
) cp  on cp.ANALYSIS = ps.ANALYSIS
and cp.NAME = ps.COMPONENT
where pg.product in ('QC_WATER', 'QC_VAST')
and v.TABLE_NAME = 'PRODUCT'
and cp.REPORTABLE = 'T'
"

