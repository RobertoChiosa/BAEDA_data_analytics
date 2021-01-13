#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

library('units')                  # units of measurements
library('plotly')                 # interactive graphs
library('scales')                 # date breaks and plot scales
library('shiny')                  # construct shiny apps
library('shinydashboard')         # construct shiny dashboards
library('markdown')               # create markdown reports
library('rmarkdown')              # create R markdown reports
library('readtext')
library('RColorBrewer')           # Pacchetto per creare palette di colori più accattivanti
library('imputeTS')               # NA interpolations
library('shinycssloaders')        # enable cool loaders when loading graphs
library('dashboardthemes')        # extend classic themed with cool themes
library('tidyverse')              # data manipulation 
# library('ggplot2')              # (loaded with tidyverse'
# library('purrr')                # (loaded with tidyverse'
# library('tibble')               # (loaded with tidyverse'
# library('dplyr')                # (loaded with tidyverse'
# library('tidyr')                # (loaded with tidyverse'
# library('stringr')              # (loaded with tidyverse'
# library('readr')                # (loaded with tidyverse'
# library('forcats')              # (loaded with tidyverse'
library('lubridate')              # easy to work with dates and times
library('readxl')                 # read and import excel files
library('DT')                     # to use output datatable
library('skimr')                  # for statistical summary
library('plyr')                   # per usare pdply
library('rlang') # to parse and evaluate string expressions in mutate
library('amap') # Kmean slustering function
library('LICORS') # Kmean++ slustering function

# library('ggExtra')              # distributions on border
# library('RColorBrewer')         # Pacchetto per creare palette di colori più accattivanti
# library('rpart')                # make classification tree
# library('rpart.plot')           # plot classification tree
# library('partykit')             # regression and classification tree plot and tools
# library('MLmetrics')            # measure regression) classification and ranking performance.
# library('imputeTS')             # manage missing values
# library('jmotif')               # sax manipulations
# library('ggpubr')               # multiple plots in line or column
# library('stringr')              # multiple plots in line or column
library(caret)
library(dendextend) # color clusters
library(stringi)
# library('networkD3')            # draw sankey plots
# library('NbClust')              # cluster validation measures
# library('knitr')
# library('magrittr')
# library('dendextend')
# library('ggsci')
# library('zoo')
# library('qcc')
# library('Hmisc')                # permette di riportare info in lag
# library('evtree')               # labelling without overlap
# library('moments')              # para uso das kewness e kurtosis
# library('partykit')             # to draw and display trees
# library('party')                # to draw and display trees
# library('arules')               # to make association rules
# library('treemap')              # to make tree plot
# library('arulesViz')            # to draw association rules graphs
# library('rjson')                # read json files
library('rsconnect')              # deploy on shinyapp.io
library('shinyWidgets')           # add htms widgets
library('shinyhelper')            # add help feature
library('reactlog')               # construct a reactive graph
library('highcharter')            # time series plot
library('quantmod')               # xts series
library('stats')               # clustering and dissimily matrix
library('shinyalert')             # required to create alarms popups and welcome page
library('shinyBS')                # required to create modal dialogs in user interface
# 
# 
# # Package names
# packages <- c(
#   'units', # units of measurements
#   'plotly', # interactive graphs
#   'shiny',                  # construct shiny apps
#   'shinydashboard',         # construct shiny dashboards
#   'markdown',               # create markdown reports
#   'rmarkdown',              # create R markdown reports
#   'readtext',
#   'shinycssloaders',        # enable cool loaders when loading graphs
#   'dashboardthemes',        # extend classic themed with cool themes
#   'tidyverse',              # data manipulation 
#   # 'ggplot2',              # (loaded with tidyverse'
#   # 'purrr',                # (loaded with tidyverse'
#   # 'tibble',               # (loaded with tidyverse'
#   # 'dplyr',                # (loaded with tidyverse'
#   # 'tidyr',                # (loaded with tidyverse'
#   # 'stringr',              # (loaded with tidyverse'
#   # 'readr',                # (loaded with tidyverse'
#   # 'forcats',              # (loaded with tidyverse'
#   'lubridate',              # easy to work with dates and times
#   'readxl',                 # read and import excel files
#   'DT',                     # to use output datatable
#   'skimr',                  # for statistical summary
#   'plyr',                   # per usare pdply
#   # 'ggExtra',              # distributions on border
#   # 'RColorBrewer',         # Pacchetto per creare palette di colori più accattivanti
#   # 'rpart',                # make classification tree
#   # 'rpart.plot',           # plot classification tree
#   # 'partykit',             # regression and classification tree plot and tools
#   # 'scales',               # Graphical scales map data to aesthetics
#   # 'MLmetrics',            # measure regression, classification and ranking performance.
#   # 'imputeTS',             # manage missing values
#   # 'jmotif',               # sax manipulations
#   # 'ggpubr',               # multiple plots in line or column
#   # 'stringr',              # multiple plots in line or column
#   # 'networkD3',            # draw sankey plots
#   # 'NbClust',              # cluster validation measures
#   # 'knitr'
#   # 'magrittr'
#   # 'dendextend'
#   # 'ggsci'
#   # 'zoo'
#   # 'qcc'
#   # 'Hmisc',                # permette di riportare info in lag
#   # 'evtree',               # labelling without overlap
#   # 'moments',              # para uso das kewness e kurtosis
#   # 'partykit',             # to draw and display trees
#   # 'party',                # to draw and display trees
#   # 'arules',               # to make association rules
#   # 'treemap',              # to make tree plot
#   # 'arulesViz',            # to draw association rules graphs
#   # 'rjson',                # read json files
#   'rsconnect',              # deploy on shinyapp.io
#   'shinyWidgets',           # add htms widgets
#   'shinyhelper',            # add help feature
#   'reactlog',               # construct a reactive graph
#   'highcharter',            # time series plot
#   'quantmod',               # xts series
#   'shinyalert',              # required to create alarms popups and welcome page
#   'shinyBS'              # required to create modal dialogs in user interface
# )
# 
# 
# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))
# 
