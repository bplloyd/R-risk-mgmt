executeSP <- function(procname, paramstring, db = "Hatteras_Sandbox_Tools", schema = "dbo"){
  require("RODBC");
  database = paste("database", db, sep = "=")
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01",database, "trusted_connection=true", sep = "; ")
  ch = odbcDriverConnect(ch)
  procString = paste(db, schema, procname, sep = ".")
  query <- paste("exec", procString, paramstring, sep = " ");
  res = sqlQuery(ch, query);
  #return(query)
  closeAllConnections();
  return(res);
}
