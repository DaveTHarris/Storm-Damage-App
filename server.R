library(base64enc)
library(shiny)
library(reshape2)
library(rMaps)
library(RColorBrewer)
library(dplyr)
library(plyr)
finalData <- readRDS("data/finalData.rds")

#This function is a variation of the ichoropleth() function within rCharts.
#I wanted to change how the data were discretized.
ichoroplethdh<-function (x, data, pal = "YlOrRd", ncuts = 5, animate = NULL, 
                         play = F, map = "usa", legend = TRUE, labels = TRUE, ...) 
{
        d <- Datamaps$new()
        fml = lattice::latticeParseFormula(x, data = data)
        data = transform(data, fillKey = cut(fml$left, ncuts, ordered_result = TRUE, include.lowest = TRUE))
        fillColors = colorRampPalette(brewer.pal(9, pal))(ncuts)
        d$set(scope = map, fills = as.list(setNames(fillColors, 
                                                    levels(data$fillKey))), legend = legend, labels = labels,...)
        if (!is.null(animate)) {
                range_ = summary(data[[animate]])
                data = dlply(data, animate, function(x) {
                        y = toJSONArray2(x, json = F)
                        names(y) = lapply(y, "[[", fml$right.name)
                        return(y)
                })
                d$set(bodyattrs = "ng-app ng-controller='rChartsCtrl'")
                d$addAssets(jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js")
                if (play == T) {
                        d$setTemplate(chartDiv = sprintf("\n        <div class='container'>\n         <button ng-click='animateMap()'>Play</button>\n         <span ng-bind='year'></span>\n         <div id='{{chartId}}' class='rChart datamaps'></div>\n        </div>\n        <script>\n          function rChartsCtrl($scope, $timeout){\n            $scope.year = %s;\n              $scope.animateMap = function(){\n              if ($scope.year > %s){\n                return;\n              }\n              map{{chartId}}.updateChoropleth(chartParams.newData[$scope.year]);\n              $scope.year += 1\n              $timeout($scope.animateMap, 1000)\n            }\n          }\n       </script>", 
                                                         range_[1], range_[6]))
                }
                else {
                        d$setTemplate(chartDiv = sprintf("\n        <div class='container'>\n          <input id='slider' type='range' min=%s max=%s ng-model='year' width=200>\n          <span ng-bind='year'></span>\n          <div id='{{chartId}}' class='rChart datamaps'></div>          \n        </div>\n        <script>\n          function rChartsCtrl($scope){\n            $scope.year = %s;\n            $scope.$watch('year', function(newYear){\n              map{{chartId}}.updateChoropleth(chartParams.newData[newYear]);\n            })\n          }\n       </script>", 
                                                         range_[1], range_[6], range_[1]))
                }
                d$set(newData = data, data = data[[1]])
        }
        else {
                d$set(data = dlply(data, fml$right.name))
        }
        return(d)
}


#This function accepts a dataframe, a "multiplier" value that aggregates the lowest valued events into the "Other" category, the column name that corresponds to the specific variable of interest, and a starting date.
#This function takes all of the data for the variable of interest from the starting date to the latest date available and determines the average value per year. It outputs a dataframe that can then be plotted.
get_plot_data<-function(df,column,type,year){
        dfb<-df[df$Year==year & df$adjType == type,]
        sumbyyear<-melt(tapply(dfb[,column], list(dfb$STATE, dfb$Year), sum, na.rm=TRUE))
        sumbyyear[is.na(sumbyyear)]<-0
        
        names(sumbyyear)<-c("State","Year","Value")
        if(dim(sumbyyear)[1]==0){
                sumbyyear<-data.frame(State = unique(df$STATE), Year = year, Value = 0 )
        }       
        sumbyyear[sumbyyear$State=="XX",3]=max(sumbyyear$Value) + 10
        
        sumbyyear
        
}


# Define server logic required to draw a histogram
shinyServer(function(input, output){
        
        plot_data<-reactive({get_plot_data(finalData, input$column, input$type, input$year)})
        output$myplot = rCharts::renderChart2({ichoroplethdh(
                Value ~ State, 
                data = plot_data(),
                legend = TRUE,
                labels = TRUE,
                #type = input$type,
                ncuts = input$ncuts,
                geographyConfig = list(
                        popupTemplate = "#! function(geography, data){
                        return '<div class=hoverinfo><strong>' + geography.properties.name + 
                        ': ' + data.Value + '</strong></div>';
        } !#" 
                )
        )})
})