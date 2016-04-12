exportXTS = function(data, filename, sheet)
{
    require(xts)
    require(XLConnect)
    df = data.frame(Date = as.Date(index(data)), data, row.names = NULL)
    writeWorksheetToFile(file = filename, data = df, sheet = sheet)
}