timeseries = scan()

'''-0.31	0.41	0.51	-0.20	0.61	0.20	0.61	-0.30	-0.10	-0.20	-0.51	0.41
-0.51	0.61	-0.20	0.10	-0.10	0.30	0.00	0.20	-0.30	0.00	-0.10	0.81
-0.60	0.40	0.50	0.10	-0.10	0.00	0.20	0.10	-0.10	0.10	0.10	0.60
-0.20	0.60	0.59	0.00	0.00	0.10	0.20	0.10	0.20	0.00	0.20	0.19
-0.10	0.68	0.58	-0.19	0.00	-0.19	0.39	0.38	0.10	0.00	0.10	0.29
-0.48	0.57	0.48	-0.47	0.38	0.09	0.47	0.00	0.00	-0.19	0.19	0.38
-0.56	0.47	0.28	-0.19	-0.09	0.28	0.28	0.00	0.00	-0.28	0.00	0.00
-1.03	0.85	0.47	0.00	0.09	-0.09	0.19	0.00	-0.19	0.00	0.09	-0.09
-0.84	0.38	0.75	-0.37	0.28	0.09	0.28	0.00	0.09	0.19	0.09	0.74
-0.64	0.65	0.18	0.00	-0.18	0.18	0.37	0.09	0.09	0.00		
'''

plot.ts(timeseries)
View(timeseries)
head(timeseries)
germaninflation = ts(timeseries, start = 2008, frequency = 12)
head(germaninflation,5)

germaninflation = ts(timeseries, start = 2008, frequency = 12)
head(germaninflation)
plot(germaninflation,main="Overview of the dataset")

# Seasonal Decomposition
decompose(germaninflation)

plot(decompose(germaninflation))

#-------------------------------------------------------------------
#newer methods for Seasonal decomposition if needed :
# Using the stl method
plot(stl(germaninflation, s.window = 7))
# stl forecasting
plot(stlf(germaninflation, method = "ets"))
# comparison with a standard ets forecast
plot(forecast(ets(germaninflation), h = 12))
#----------------------------------------------------------------------


## Seasonal Arima (package forecast)
auto.arima(germaninflation, stepwise = T, 
           approximation = F, trace = T)

# Getting an object
germaninflationrima = auto.arima(germaninflation, 
                                 stepwise = T, 
                                 approximation = F, 
                                 trace = T)


# Forecast
View(timeseries)
typeof(timeseries)

forecastingarima = forecast(germaninflationrima)
typeof(forecastingarima)
View(forecastingarima)
plot(forecastingarima)


## Exponential Smoothing with ets

# Auto gemerated
ets(germaninflation)
# Forecast plot
germaninflationets = ets(germaninflation)

plot(forecast(germaninflets, h = 12))
# Comparison with seasonal Holt Winters model
plot(hw(germaninflation, h = 12))

## Cross Validation of 2 models
germaninflets = ets(germaninfl)

#functions for calcutaing error rate
forecastets = function(x, h) {
  forecast(ets(x), h = h)
}


forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(germaninflation, forecastets, h=1)
a = tsCV(germaninfl, forecastets, h=12)
plot(a)
View(a)
arimaerror = tsCV(germaninflation, forecastarima, h=1)


mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)

#----------------------------------------------------------------
#making it possible for graph

lower <- forecastingarima$lower[,2]
upper <- forecastingarima$upper[,2]
View(lower)
View(upper)
pforecasting <- forecastingarima$mean
View(pforecasting)
View(forecastingarima$x)
graph <- cbind(forecastingarima$x, lower, upper,
               pforecasting)
View(graph)
typeof(graph)

dygraph(graph)

dygraph(graph, main = "Inflation rate of Germany") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "forecastingarima$x", label = "Inflation Data") %>%
  dySeries(c("lower","pforecasting","upper"), label = "Predicted Inflation Data") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Monthly Inflation Rate") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
  
  #--------------------------------------------------------------------------

