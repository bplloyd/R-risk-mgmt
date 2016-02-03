executeSP <- function(procname, paramstring, db = "Hatteras_Sandbox_Tools"){
  require("RODBC");
  database = paste("database", db, sep = "=")
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01",database, "trusted_connection=true", sep = "; ")
  ch = odbcDriverConnect(ch)
  query <- paste("exec dbo.", procname, " ", paramstring);
  res = sqlQuery(ch, query);
  return(res);
}
