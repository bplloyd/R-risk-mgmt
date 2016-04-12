charts.AlphaBeta = function (Ra, Rb, width = 63, Rf = 0, main = NULL, legend.loc = NULL, 
          event.labels = NULL, alphaRng = c(-1,1), betaRng = c(-1,1), colorset = rich10equal, ...) 
{
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  op <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2)), widths = 1, heights = c(1,1))
  par(mar = c(1, 4, 2, 2))
  if (is.null(main)) {
    freq = periodicity(Ra)
    switch(freq$scale, minute = {
      freq.lab = "minute"
    }, hourly = {
      freq.lab = "hour"
    }, daily = {
      freq.lab = "day"
    }, weekly = {
      freq.lab = "week"
    }, monthly = {
      freq.lab = "month"
    }, quarterly = {
      freq.lab = "quarter"
    }, yearly = {
      freq.lab = "year"
    })
    main = paste("Rolling ", width, "-", freq.lab, " Regressions", 
                 sep = "")
  }
  chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Alpha", 
                          xaxis = FALSE, main = main, ylab = "Alpha", legend.loc = legend.loc, 
                          event.labels = event.labels, ylim = alphaRng, colorset = colorset, ...)
  par(mar = c(2, 4, 2, 2))
  chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Beta", 
                          main = "", ylab = "Beta", xaxis = T, event.labels = NULL,ylim = betaRng, colorset =colorset,
                          ...)
#   par(mar = c(5, 4, 0, 2))
#   chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "R-Squared", 
#                           main = "", ylab = "R-Squared", event.labels = NULL, ...)
  par(op)
}