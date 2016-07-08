overlappingPositions = function(date)
{
  date = as.character(date)
  dateStr = paste0("'", date, "'")
  parStr = paste("@date", dateStr, sep = " = ")
  return(executeSP(procname = "usp_Overlapping_Positions", paramstring = parStr))
}