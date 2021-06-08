library('renv')
if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")
renv::init()
source('~/GitHub/ValueLine/ValueLine.r')
install.packages("remotes") #install remotes
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T)
renv::status()
devtools::install_github("talgalili/d3heatmap")
library('devtools')
install.packages("devtools")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("talgalili/d3heatmap")
install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
install.packages('curl', repos = 'http://cran.r-project.org')
install.packages('curl', repos = 'http://cran.r-project.org')
github.com/systematicinvestor/SIT
devtools::install_github('systematicinvestor/SIT.date')
library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')
install.packages("C:/Users/gfalk/Downloads/SIT-20210514T173311Z-001.zip", repos = NULL, type = "win.binary")
devtools::install_github('R-Finance/blotter')
renv::status()
devtools::install_github("R-Finance/FinancialInstrument") #if not installed
devtools::install_github("braverock/PerformanceAnalytics") #if not installed
devtools::install_github("braverock/quantstrat")
renv::status()
remotes::install_github('rstudio/DT')
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
remotes::install_github('rstudio/flexdashboard')
renv::status()
install_github('Swechhya/excelR')
library(devtools)
install_github('Swechhya/excelR')
devtools::install_github("renkun-ken/formattable")
devtools::install_github("rstudio/gt")
devtools::install_github("jennybc/here_here")
yes
install.packages("here")
remotes::install_github("jbkunst/highcharter")
devtools::install_github("haozhu233/kableExtra")
renv::status()
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hzambran/hydroTSM")
renv::status()
devtools::install_github("ropensci/plotly")
renv::status()
install.packages("shinydashboard")
devtools::install_github("dreamRs/shinyWidgets")
install.packages("shinydashboard")
renv::status()
devtools::install_github("business-science/tidyquant")
renv::status()
devtools::install_github("tidyverse/tidyverse")
devtools::install_github("r-lib/httr")
install_github("cvitolo/r_treemap", subdir = "treeMap")
library(treeMap)
install.packages("treemap")
devtools::install_github("statisticsNZ/simplevis")
devtools::install_github("timelyportfolio/sunburstR")
renv::status()
renv::snapshot()