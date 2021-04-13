#' clustering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyFeedback hideFeedback feedbackWarning feedbackDanger
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select mutate distinct
#' @importFrom stats na.omit cutree hclust
#' @importFrom LICORS kmeanspp
#' @importFrom amap Kmeans
#' @importFrom RColorBrewer brewer.pal
#' @importFrom NbClust NbClust
#' @importFrom lubridate date round_date
#' @importFrom plyr ddply summarise
#' @importFrom scales date_breaks
#' @import  ggplot2
mod_clustering_ui_input <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 6,
      style = "padding-left:0px; padding-right:0px;",
      selectInput(
        ns('cluster_variable'),
        'Variable:',
        choices = NULL # updated according to the input
      ),
      selectInput(
        ns('cluster_distance'),
        'Distance:',
        choices = list(
          General = c(
            "euclidean",
            "maximum",
            "manhattan",
            "canberra",
            "binary",
            "minkowski"
          ),
          Partitive = c(
            "pearson" ,
            "abspearson" ,
            "abscorrelation",
            "correlation",
            "spearman",
            "kendall"
          )
        )
      ),
      numericInput(
        ns('cluster_number'),
        'Number of clusters',
        value = 2,
        min = 1
      )
    ),
    column(
      width = 6,
      style = "padding-left:10px; padding-right:0px;",
      selectInput(
        ns('cluster_normalization'),
        'Normalization:',
        choices = c("none", "maxmin", "max", "min", "zscore")
      ),
      selectInput(
        ns('cluster_method'),
        'Clustering method:',
        choices = list(
          Hierarchical = c(
            "ward.D2",
            "ward.D",
            "single",
            "complete",
            "average",
            "mcquitty",
            "median",
            "centroid"
          ),
          Partitive = c("kmeans", "kmeans++", "FDL")
        ),
        selected = 'ward.D2'
      ),
      # NBCLUST
      tags$div(title = "By selecting yes the NBclust package will evaluate the optimal number of clusters",
               radioGroupButtons(
                 inputId = ns("radio_nbclust"),
                 label = "Search optimal number",
                 choices = c("Yes", "No"),
                 selected = "No",
                 justified = TRUE
                 
               ))
      
    ),
    uiOutput(ns("clustering_inbox_nbclust")),
    actionButton(
      ns("cluster_button"),
      "Perform cluster",
      class = "btn-success",
      icon = icon("chart-bar"),
      width = "100%"
    ),
    hr(),
    column(
      width = 8,
      style = "padding-left:0px; padding-right:0px;",
      selectizeInput(
        ns('cluster_merge'),
        label = NULL,
        choices = c("Select cluster to merge..." = ''),
        multiple = TRUE
      )
    ),
    column(width = 4,
           style = "padding-left:10px; padding-right:0px;",
           actionButton(ns('cluster_merge_button'), 'Merge!',  width = '100%')),
    column(width = 8,
           style = "padding-left:0px; padding-right:0px;",
           selectizeInput(
        ns('cluster_discard'),
        label = NULL,
        choices = c('Select cluster to discard...' = ''),
        multiple = TRUE
      )
    ),
    column(width = 4,
           style = "padding-left:10px; padding-right:0px;",
           actionButton(ns('cluster_discard_button'), 'Discard!',  width = '100%')),
    searchInput(
      inputId = ns("clustering_dataframe_name"),
      label = "Save clustering results dataframe",
      placeholder = "New name..",
      value = NULL,
      # initial value
      btnSearch = icon("plus"),
      btnReset = icon("backspace"),
      # icons
      width = "100%"
    )
    
  )
  
}

mod_clustering_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    dropdownButton( size = "sm",
                    tags$h3("Graphical parameters"),
                    numericInput(inputId = ns('out_clustering_fontsize'), label = 'Fontsize:', value = 15),
                    numericInput(inputId = ns('out_clustering_linesize'), label = 'Line size:', value = 0.5),
                    numericInput(inputId = ns('out_clustering_alpha'), label = 'Line alpha:', value = 0.7),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                    tooltip = tooltipOptions(title = "Click to modify plot inputs")
    ),
    plotOutput(ns("out_clustering_preview"), height = "600px")
  )
}

#' clustering Server Functions
#'
#' @noRd
mod_clustering_server <- function(id, rvs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_results <- reactiveValues()
    
    # updates ui do for all
    observe({
      updateSelectInput(session, inputId = 'cluster_variable', choices = colnames(rvs))
    })
    
    observeEvent(input$cluster_button, {
      updateSelectInput(session,
                        inputId = 'cluster_merge',
                        choices = unique(data_results[["Clustering_df"]]$Cluster))
      updateSelectInput(session,
                        inputId = 'cluster_discard',
                        choices = unique(data_results[["Clustering_df"]]$Cluster))
    })
    
    # # 4.2) NBClust input parameters ----------------------------------------------------------------------
    output$clustering_inbox_nbclust <- renderUI({
      valid1 <- input$radio_nbclust == "Yes"
      req(valid1)# if we want to evaluate the optimal number of clusters
      # validation of nbclust function
      valid_dist <- input$cluster_distance %in% c("euclidean","maximum", "manhattan", "canberra", "binary", "minkowski")
      valid_meth <- !(input$cluster_method %in% c("kmeans++", "FDL") )
      
      shiny::validate(
        need(valid_dist == TRUE,  "Sorry, the selected distance is not supported for the optimal number of clusters evaluation"),
        need(valid_meth == TRUE, "Sorry, the selected clustering method is not supported for the optimal number of clusters evaluation")
      )
      
      # the user interface buttons
      tagList(
        column(width = 6,  style = "padding-left:0px; padding-right:5px;",
               selectInput('index_nbclust', 'Select index:',
                           choices = c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all", "alllong"),
                           selected = 'silhouette'
               )
        ),
        column(width = 6,  style = "padding-left:5px; padding-right:0px;",
               sliderInput(inputId = "cluster_number_nbclust", label = "Number of clusters:", min = 1, max = 10, value = c(1, 10) ),
        )
      )
    })
    
    
    # 4.4) Clustering process ----------------------------------------------------------------------
    observeEvent(input$cluster_button,{
      # validation
      #req(input$file)                                                   # require a uploaded file
      validate_df <- "Date_Time" %in% colnames(rvs) # require this column to be created
      
      # # give feedback about the absence of a correct dataframe
      # shinyFeedback::hideFeedback("cluster_variable")
      # if ( validate_df == FALSE ) { # incompatible condition
      #   shinyFeedback::feedbackWarning("cluster_variable", TRUE, "Please add calendar variables to the current dataframe")
      # }
      req(validate_df)                                                  # stops execution if no datetime found
      
      # notification of process
      id <- showNotification("Performing clustering...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      # process and create clustering dataframe
      df1 <- rvs  %>%
        dplyr::mutate(Date = lubridate::date(rvs$Date_Time),
                      Time = format(lubridate::round_date(rvs$Date_Time, "5 mins") , "%H:%M:%S"),
                      X = rvs[,input$cluster_variable]) %>%
        dplyr::select(Date, Time, X)
      
      # normalize data given input command
      switch(input$cluster_normalization,
             none =    df1 <- df1,
             zscore =  df1 <- df1 %>% dplyr::mutate(X = (X-mean(X))/sd(X) ) ,
             max =     df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = X/max(X)) %>% dplyr::ungroup(),
             min =     df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = X/min(X)) %>% dplyr::ungroup(),
             maxmin =  df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = (X-min(X))/(max(X)-min(X)) )  %>% dplyr::ungroup()
      )
      
      # create M N matrix
      df1 <- dplyr::distinct(df1)  # checks that the keys are unique, some problems arise when timezone
      df2 <- tidyr::pivot_wider(df1, names_from = Time, values_from = X) %>% # use pivot_wider instead of spread
        stats::na.omit() # omits na when coercing
      df3 <- df2[ 2:dim(df2)[2] ]  # keep only times
      
      # check consistency of method and distance
      shinyFeedback::hideFeedback("cluster_method")
      shinyFeedback::hideFeedback("cluster_distance")
      
      # if TRUE incompatible condition partitive distance but hierarchical method
      inconsistent = input$cluster_distance %in% c("pearson" , "abspearson" , "abscorrelation", "correlation", "spearman", "kendall") &
        input$cluster_method %in%  c("ward.D2", "ward.D", "single","complete", "average", "mcquitty", "median", "centroid")
      
      if ( inconsistent == TRUE ) { # incompatible condition
        shinyFeedback::feedbackDanger("cluster_method", TRUE, "Inconsistent method")
        shinyFeedback::feedbackDanger("cluster_distance", TRUE, "Inconsistent distance")
      }
      
      # continues the execution if there is consistence between method and distance
      # general+partitive/hierarchical OR partitive+partitive
      req(!inconsistent)
      
      if (input$cluster_method == "kmeans") {
        clust_res <- amap::Kmeans(df3, input$cluster_number, method = input$cluster_distance) # perform clustering
        df2$Cluster <- clust_res$cluster                                                # add labels to dataframe
      } else if (input$cluster_method == "kmeans++") {
        clust_res <- LICORS::kmeanspp(df3, input$cluster_number)                                # perform clustering
        df2$Cluster <- clust_res$cluster                                                # add labels to dataframe
      } else{ # hierarchical
        diss_matrix <- dist(df3, input$cluster_distance)                                # calculate distance matrix
        hcl <- hclust(diss_matrix, method = input$cluster_method)                       # perform clustering
        df2$Cluster <- cutree(hcl, input$cluster_number)                                # add labels to dataframe
      }
      
      
      # reconstruct the variable column name
      colnames(df1)[3] <- input$cluster_variable
      
      # create color palette
      color_palette <- RColorBrewer::brewer.pal(12, "Paired")
      
      data_results[["Clustering_df_color"]] <- data.frame(Cluster = unique(df2$Cluster),
                                                          Color = color_palette[c( 1:length(unique(df2$Cluster)))]
      )
      
      # save small dataframe
      data_results[["Clustering_df_date"]] <- df2 %>%
        dplyr::select(Date, Cluster) %>%
        dplyr::mutate(Cluster = as.factor(Cluster))
      
      # merge cluster information with the original dataframe
      data_results[["Clustering_df"]] <- merge.data.frame(df1, df2[,c("Date", "Cluster") ]) %>%
        dplyr::mutate(Cluster = as.factor(Cluster))
      
      # validation of nbclust function
      validate_nbclust <- input$radio_nbclust == "Yes" # if TRUE the user has checkes yes
      valid_dist <- input$cluster_distance %in% c("euclidean","maximum", "manhattan", "canberra", "binary", "minkowski") # if TRUE acceptable
      valid_meth <- !(input$cluster_method %in% c("kmeans++", "FDL") )  # if TRUE acceptable
      req(validate_nbclust, valid_dist, valid_meth)
      
      # notification of process
      id2 <- showNotification("Searching optimal number of clusters..", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id2), add = TRUE)
      
      Nb_res <- NbClust(df3, distance = input$cluster_distance,
                        min.nc = input$cluster_number_nbclust[1], max.nc = input$cluster_number_nbclust[2],
                        method = input$cluster_method,
                        index = input$index_nbclust)
      
      optimal_N_clusters <- length(unique(Nb_res$Best.partition))
      
      # give feedback about the optimal number
      shinyFeedback::hideFeedback("cluster_number_nbclust")
      shinyFeedback::feedbackWarning("cluster_number_nbclust", TRUE, paste("The suggested number of clusters is", optimal_N_clusters) )
      
    })
    
    # 4.5) Merge clusters ----------------------------------------------------------------------
    observeEvent(input$cluster_merge_button, {
      df_tmp <- data_results[["Clustering_df"]]                         # gets resulting dataframe with labels
      tmp <- min(input$cluster_merge)                                   # gets the minimum value of cluster to be merged
      df_tmp$Cluster[ df_tmp$Cluster %in% input$cluster_merge] <- tmp   # assign to all the cluster labels the minimum cluster label
      data_results[["Clustering_df"]] <- df_tmp                         # save resulting dataframe in results memory
    })
    
    
    # 4.6) Discard clusters ----------------------------------------------------------------------
    observeEvent(input$cluster_discard_button, {
      df_tmp <- data_results[["Clustering_df"]]                         # gets resulting dataframe with labels
      df_tmp <- df_tmp[! df_tmp$Cluster %in% input$cluster_discard,]    # keeps only those without the selected label
      data_results[["Clustering_df"]] <- df_tmp                         # save resulting dataframe in results memory
    })
    
    # 4.7) Plot daily profiles clusters ----------------------------------------------------------------------
    output$out_clustering_preview <- renderPlot({
      
      # requires a performed clustering and a result dataframe in memory
      validate_df <- "Date_Time" %in% colnames(rvs) # require this column to be created
      req(input$cluster_button, data_results[["Clustering_df"]], validate_df)
      
      # load actual cluster dataframe and add color info
      df1 <-  merge(data_results[["Clustering_df"]], data_results[["Clustering_df_color"]])
      
      # go back to the original notation
      colnames(df1)[colnames(df1) == input$cluster_variable] <- "X"
      # manipulates the dataframe to add cluster label with count
      conteggio <- df1 %>%
        tidyr::pivot_wider(names_from = Time, values_from = X)  %>%
        dplyr::group_by(Cluster) %>%
        dplyr::count(Cluster)
      
      # creates label with number of profiles
      conteggio$Cluster_lab <- paste("Cluster", conteggio$Cluster, "( n =", conteggio$n, ")")
      
      # ricreated the original dataframe but with labels and count
      df1 <- merge.data.frame(df1, conteggio[c("Cluster", "Cluster_lab")])
      
      # calculates the centroid for each cluster
      centr <- plyr::ddply(df1, c("Cluster_lab","Time", "Color"), plyr::summarise, X = mean(X))
      
      # graphical parameters
      line_color <- "gray"
      line_size <- input$out_clustering_linesize
      line_alpha <- input$out_clustering_alpha
      timezone <- gsub(" ", "", paste("timezone_", input$type))
      
      plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data = df1,
                           aes(x = as.POSIXct(Time, format="%H:%M:%S" , tz = "Europe/Rome") ,
                               y = X,
                               group = Date,
                           ), na.rm = T,
                           color = line_color,
                           alpha = line_alpha,
                           size = line_size) +
        ggplot2::geom_line(data = centr,
                           aes(x = as.POSIXct(Time, format="%H:%M:%S" , tz = "Europe/Rome") ,
                               y = X,
                               color = Color),
                           size = line_size*2, na.rm = T) +
        ggplot2::scale_x_datetime(
          breaks = scales::date_breaks("4 hour"),                     # specify breaks every 4 hours
          labels = scales::date_format(("%H:%M") , tz = "Europe/Rome"),  # specify format of labels
          expand = c(0,0)                                     # expands x axis
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0,ceiling(max(df1$X)) ),       # set limits from 0 to higher power consumption
          expand = c(0,0)                                       # expands x axis
        ) +
        ggplot2::theme_bw() +                                           # white bakground with lines
        ggplot2::theme(
          legend.position = "none",                     # legend position on the top of the graph
          strip.text = element_text(size = eval(input$out_clustering_fontsize-3)), # facet wrap title fontsize
          axis.title.x = element_text(size = input$out_clustering_fontsize, margin = margin(t = 20, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(size = input$out_clustering_fontsize, margin = margin(t = 20, r = 20, b = 0, l = 0)),
          axis.text.x = element_text(size = eval(input$out_clustering_fontsize-3), angle=45, vjust = .5),
          axis.text.y = element_text(size = eval(input$out_clustering_fontsize-3) , vjust=.3),
        ) +
        ggplot2::labs(x = "Time", y = isolate(input$cluster_variable), color = "Cluster") +
        ggplot2::facet_wrap(~Cluster_lab)
      
      plot
      
    })
    
    
  })
}

## To be copied in the UI
# mod_clustering_ui("clustering_ui_1")

## To be copied in the UI
# mod_clustering_ui_output("clustering_ui_1")

## To be copied in the server
# mod_clustering_server("clustering_ui_1", rvs)
