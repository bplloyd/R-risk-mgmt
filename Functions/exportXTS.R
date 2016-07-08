exportXTS = function(data, filename, sheet)
{
    require(xts)
    require(XLConnect)
    df = data.frame(Date = (as.numeric(index(data)) + 25569), data, row.names = NULL)
    writeWorksheetToFile(file = filename, data = df, sheet = sheet)
}