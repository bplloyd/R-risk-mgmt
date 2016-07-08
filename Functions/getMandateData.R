getMandateData = function(id)
{
  dd   executeSP(procname = "usp_Get_Mandate_Data", paramstring = paste0("@id = ",id))
}