#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

# Package names
packages <- c(
  'units',
  'shiny',                  # construct shiny apps
  'shinydashboard',         # construct shiny dashboards
  'markdown',               # create markdown reports
  'rmarkdown',              # create R markdown reports
  'readtext',
  'shinycssloaders',        # enable cool loaders when loading graphs
  'dashboardthemes',        # extend classic themed with cool themes
  'tidyverse',              # data manipulation 
  # 'ggplot2',              # (loaded with tidyverse'
  # 'purrr',                # (loaded with tidyverse'
  # 'tibble',               # (loaded with tidyverse'
  # 'dplyr',                # (loaded with tidyverse'
  # 'tidyr',                # (loaded with tidyverse'
  # 'stringr',              # (loaded with tidyverse'
  # 'readr',                # (loaded with tidyverse'
  # 'forcats',              # (loaded with tidyverse'
  'lubridate',              # easy to work with dates and times
  'readxl',                 # read and import excel files
  'DT',                     # to use output datatable
  'skimr',                  # for statistical summary
  'plyr',                   # per usare pdply
  # 'ggExtra',              # distributions on border
  # 'RColorBrewer',         # Pacchetto per creare palette di colori più accattivanti
  # 'rpart',                # make classification tree
  # 'rpart.plot',           # plot classification tree
  # 'partykit',             # regression and classification tree plot and tools
  # 'scales',               # Graphical scales map data to aesthetics
  # 'MLmetrics',            # measure regression, classification and ranking performance.
  # 'imputeTS',             # manage missing values
  # 'jmotif',               # sax manipulations
  # 'ggpubr',               # multiple plots in line or column
  # 'stringr',              # multiple plots in line or column
  # 'networkD3',            # draw sankey plots
  # 'NbClust',              # cluster validation measures
  # 'knitr'
  # 'magrittr'
  # 'dendextend'
  # 'ggsci'
  # 'zoo'
  # 'qcc'
  # 'Hmisc',                # permette di riportare info in lag
  # 'evtree',               # labelling without overlap
  # 'moments',              # para uso das kewness e kurtosis
  # 'partykit',             # to draw and display trees
  # 'party',                # to draw and display trees
  # 'arules',               # to make association rules
  # 'treemap',              # to make tree plot
  # 'arulesViz',            # to draw association rules graphs
  # 'rjson',                # read json files
  'rsconnect',              # deploy on shinyapp.io
  'shinyWidgets',           # add htms widgets
  'shinyhelper',            # add help feature
  'reactlog',               # construct a reactive graph
  'highcharter',            # time series plot
  'quantmod',               # xts series
  'shinyalert',              # required to create alarms popups and welcome page
  'shinyBS'              # required to create modal dialogs in user interface
)


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

