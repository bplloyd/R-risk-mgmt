exposureSummary = function(id, start, end=NULL)
{
#   id= 43
#   start = as.Date("2016-3-31")
#   end = as.Date("2016-3-31")
  proc = "usp_Summary_Exposure_WAREHOUSE_multdate"
  p.id = paste("@id", id, sep = "=")
  start = paste0("'", start, "'")
  p.start = paste("@startDate", start, sep = "=")
  if(!is.null(end))
  {
    end = paste0("'", end, "'")
    p.end = paste("@endDate", end, sep = "=")
  }
  else
  {
    p.end = paste("@endDate", start, sep = "=") 
  }
  params = paste(p.id, p.start, p.end, sep = ",")
  # executeSP(proc, params)
  return(executeSP(proc, params))
}