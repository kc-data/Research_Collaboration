# Author: Rory Pulvino
# Date:   Aug. 1, 2017
# Info:   A First Shiny app. The app visualizes network data using ggnet2
#         and allows for some interactivity. I'd like to move the network
#         data into a ggnetwork for better visualization in a ggplot that 
#         would allow other interactivity (hover and click) and/or flip 
#         to a d3 visualization object.
#         Recently found this tutorial on network viz: 
#         http://curleylab.psych.columbia.edu/netviz/netviz1.html#/33

library(shiny)
pkgs <- c("tidyverse", "lubridate", "readxl", 'stringr', 
          "reshape2", 'RColorBrewer', 'network', 'sna', 'igraph',
          'GGally', 'intergraph', 'RecordLinkage')
lapply(pkgs, require, character.only = TRUE)
#library(datasets) # Need to load in data

# Likely need to load in bipartite and unipartite graphs
dffull <- read.csv('sample.csv', stringsAsFactors = FALSE) 

dfnet <- select(dffull, name, code) # Creating the edgelist to create igraph object

sample1_network <- simplify(graph_from_data_frame(dfnet, directed = FALSE)) # bipartite network of codes and names

full_network <- graph.edgelist(as.matrix(dfnet)) # Create a graph object to break down in unipartite graph
V(full_network)$type <- bipartite.mapping(full_network)$type # Creating the bipartite labels to be able to isolate the name network
sample2_network <- bipartite.projection(full_network)$proj1 # Creating the unipartite graph of just names


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  central_node <- reactive({
    # Ensuring that the input$UID is upper case
    test_UID <- toupper(input$UID)
    
    # Calculate nearest strings
    distance = levenshteinSim(test_UID, dfsuspect_attr$UID)
    
    if (test_UID %in% dfsuspect_attr$UID){
      name_used <- test_UID 
    }
    else if (max(distance) > 0.7){
      # Taking the best matched string given the user entry
      name_used <- dfsuspect_attr$UID[distance == max(distance)][1]
    }
    else {
      name_used <- test_UID
    }
    return(name_used)
  })
  
  # The output$Network_Title is computed based on a reactive expression that
  # returns input$UID. When the user changes the "Name" field:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  output$Network_Title <- renderText({
    paste0(central_node(), "'s network.")
  })
  
  # The output$Network_Test tests whether the name appears in the data
  #
  output$Network_Test <- renderText({
    if (central_node() %in% dffull$name){
      paste0(central_node(), " appears in the data. This is the closest name to your
             entry.")
    }
    else{
      paste0(input$UID, " does not appear in the data. Be sure that the name entered
             is entered correctly.")
    }
  })
  
  label_nodes <- reactive({
    if (input$labels == 'No'){
      labeling <- FALSE
    }
    else {labeling <- TRUE}
    labeling
  })
  #########################################################
  ##       GRAPHING Names  
  #########################################################
  # Reactive induced subgraph that is based on input$UID & input$degree_separation

  # Grabbing the subnetwork for the named person
  G_sample1 <- reactive({
    induced.subgraph(sample2_network, 
                     vids = unlist(igraph::neighborhood(sample2_network, 
                                                        order = input$degree_separation, 
                                                        nodes = central_node())))
  })

  # Reactive induced dataframe that is based on input$UID & input$degree_separation

  # Grabbing the dataframe for the name
  # example: john
  df_name <- reactive({
    
    name_list <- V(G_sample1())$name
    
    df <- dffull[dffull$UID %in% name_list,] 
    
    df <- data.frame(distinct(df, name, .keep_all = TRUE))
    
    df
    
  })
  
  # Dataframe of node information
  output$person_node_dataframe <- renderDataTable({
    df <- select(df_name(), UID, drove, tot_drove)
    df
  })
  
  # Setting the node color
  node_name_color <- reactive({
    
    if (input$variable == 'drove'){
      node_color <- df_name()$drove
    }
    else {
      node_color <- 'dodgerblue'
    }
    
    node_color
  })
  
  # Setting the node size
  node_name_size <- reactive({
    
    if (input$centrality == 'Betweenness'){
      node_size <- igraph::betweenness(G_sample1(), 
                                       v = V(G_sample1()),
                                       directed = FALSE,
                                       normalized = TRUE)
    }
    else if (input$centrality == 'Closeness'){
      node_size <- igraph::closeness(G_sample1(),
                                     vids = V(G_sample1()),
                                     normalized = TRUE)
    }
    else if (input$centrality == 'Degree'){
      node_size <- igraph::degree(G_sample1(),
                                  v = V(G_sample1()),
                                  loops = FALSE,
                                  normalized = TRUE)
    }
    else if (input$centrality == 'Eigen'){
      node_size <- igraph::eigen_centrality(G_sample1(),
                                            directed = FALSE)
    }
    else {
      node_size <- 6
    }
    
    node_size
  })
  
  # The output$network_graph_people visualizes subgraphs with the following aesthetics:
  #   1) centrality = size of node
  #   2) variable = color of node
  #   3)  = shape of node
  # 
  output$network_graph_people <- renderPlot({
    
    # Graphing the network
    if (input$variable != 'None'){
      graph_network <- ggnet2(G_sample1(),
                              label = label_nodes(),
                              color = node_name_color(), 
                              color.legend = "Name in variable?",
                              palette = c('0' = 'dodgerblue', '1' = 'darkorange'),
                              size = node_name_size()
                              )+
        guides(size = FALSE)
    }
    else {
      graph_network <- ggnet2(G_sample1(), 
                              label = label_nodes(),
                              color = node_name_color(), 
                              color.legend = "Name in variable?",
                              size = node_name_size()
                              )+
        guides(size = FALSE)
    }
    
    
    # Printing the network
    print(graph_network)
    
  })
  
  
  
  #########################################################
  ##       GRAPHING Names + Codes 
  #########################################################
  
  # Grabbing the subnetwork for the name + codes
  G_sample2 <- reactive({
    induced.subgraph(sample1_network,
                     vids = unlist(igraph::neighborhood(sample1_network,
                                                        order = input$degree_separation,
                                                        nodes = central_node())))
  })
  
  # Grabbing the dataframe for the name + codes
  df_name_code <- reactive({
    name_code_list <- V(G_sample2())$name
    
    dfname <- dffull[dffull$name %in% name_code_list,] 
    dfcode <- dffull[dffull$code %in% name_code_list,]
    
    dfname <- distinct(dfname, name, .keep_all = TRUE)
    dfcode <- distinct(dfcode, code, .keep_all = TRUE)
    
    df <- data.frame(bind_rows(dfname, dfcode))
    
    df
    
  })
  
  # Dataframe of node information
  output$full_node_dataframe <- renderDataTable({
    df <- select(df_name_code(), name, code, drove, tot_code)
  })
  
  node_name_code_size <- reactive({
    
    if (input$centrality == 'Betweenness'){
      node_size <- igraph::betweenness(G_sample2(), 
                                       v = V(G_sample2()),
                                       directed = FALSE,
                                       normalized = TRUE)
    }
    else if (input$centrality == 'Closeness'){
      node_size <- igraph::closeness(G_sample2(),
                                     vids = V(G_sample2()),
                                     normalized = TRUE)
    }
    else if (input$centrality == 'Degree'){
      node_size <- igraph::degree(G_sample2(),
                                  v = V(G_sample2()),
                                  loops = FALSE,
                                  normalized = TRUE)
    }
    else if (input$centrality == 'Eigen'){
      node_size <- igraph::eigen_centrality(G_sample2(),
                                            directed = FALSE)
    }
    else {
      node_size <- 6
    }
    
    node_size
  })
  
  # The output$network_graph_people_and_codes visualizes subgraphs with the following aesthetics:
  #   1) centrality = size of node
  #   2) name or code = color of node
  #   3) 
  # 
  output$network_graph_people_and_incidents <- renderPlot({
    
    # Creating the graph
    graph_network <- ggnet2(G_sample2(),
                            label = label_nodes(),
                            color = str_detect(V(G_sample2())$name, '^\\d'), 
                            color.legend = "Variable?",
                            palette = c('FALSE' = 'dodgerblue', 'TRUE' = 'darkorange'),
                            size = node_name_code_size())+
      guides(size = FALSE)
    
    
    # Printing the network
    print(graph_network)
    
  })
  
})