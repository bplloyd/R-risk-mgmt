getFundID = function(name)
{
  name = "FrontFour"
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  qry = paste0("SELECT s.Fund_UID FROM HAMF.SubAdvisors AS s WHERE s.Abbreviation = '", name, "'")
  id = sqlQuery(cn,qry)
  return(id[1,1])
}