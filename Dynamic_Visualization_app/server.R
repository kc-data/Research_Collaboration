# Author: Rory Pulvino
# Date:   Oct. 18, 2017
# Info:   An interactive SNA Shiny app. The app visualizes network data using networkD3
#         and allows for some interactivity. I'd like to eventually use the VisNetwork
#         package for these interactive graphs because they load faster, the outlay is 
#         more appealing to me, and the syntax is very ggplot-y, but the functionality
#         I needed to be able to turn node labels on and off and color nodes made 
#         networkD3 a better choice for now.
#         Used this tutorial on network viz: 
#         http://curleylab.psych.columbia.edu/netviz/netviz1.html#/33

library(shiny)
pkgs <- c("tidyverse", "lubridate", "readxl", 'stringr', 
          "reshape2", 'RColorBrewer', 'network', 'sna', 'igraph',
          'GGally', 'intergraph', 'RecordLinkage', 'networkD3')
lapply(pkgs, require, character.only = TRUE)
#library(datasets) # Need to load in data

# Likely need to load in bipartite and unipartite graphs
dffull <- read.csv('sample_network_data.csv', stringsAsFactors = FALSE) 

dfnet <- select(dffull, Name, Code) # Creating the edgelist to create igraph object

sample1_network <- simplify(graph_from_data_frame(dfnet, directed = FALSE)) # bipartite network of codes and names

full_network <- graph.edgelist(as.matrix(dfnet)) # Create a graph object to break down in unipartite graph
V(full_network)$type <- bipartite.mapping(full_network)$type # Creating the bipartite labels to be able to isolate the name network
sample2_network <- bipartite.projection(full_network)$proj1 # Creating the unipartite graph of just names


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  central_node <- reactive({
    # Ensuring that the input$name is upper case
    test_name <- tolower(input$name)
    
    # Calculate nearest strings
    distance = levenshteinSim(test_name, dffull$Name)
    
    if (test_name %in% dffull$Name){
      name_used <- test_name 
    }
    else if (max(distance) > 0.7){
      # Taking the best matched string given the user entry
      name_used <- dffull$Name[distance == max(distance)][1]
    }
    else {
      name_used <- test_name
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
    if (central_node() %in% dffull$Name){
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
      labeling <- 0
    }
    else {labeling <- 5}
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

  # Reactive induced dataframe that is based on input$name & input$degree_separation

  # Grabbing the dataframe for the name
  # example: john
  df_name <- reactive({
    
    name_list <- V(G_sample1())$name
    
    df <- dffull[dffull$Name %in% name_list,] 
    
    df <- data.frame(distinct(df, Name, .keep_all = TRUE))
    
    df
    
  })
  
  # Dataframe of node information
  output$person_node_dataframe <- renderDataTable({
    df <- select(df_name(), Name, Checked_Out_Book, Tot_Books_Checked_Out)
    df
  })
  
  # Setting the node color
  node_name_color <- reactive({
    
    if (input$variable == 'drove'){
      node_color <- df_name()$Checked_Out_Book
    }
    else {
      node_color <- 'no_spec'
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
  # 
  
  # Create a dataframe for forceNetwork that stores node info
  node_person_list <- reactive({
    
    # Creating the node list
    node_list <- data.frame(ID = c(0:(igraph::vcount(G_sample1()) - 1)),
                            Name = igraph::V(G_sample1())$name)
    
    # Adding attributes that will affect node color to the node_list
    df_node_attributes <- df_name() %>%
      select(Name, Checked_Out_Book)
    
    node_list <- merge(node_list, df_node_attributes, by = 'Name')
    
    # Add a 'no_spec' column for standard node coloring
    node_list$no_spec <- ifelse(node_list$Name == as.character(central_node()), as.character(central_node()), 'connections')
    
    # Adding attributes that will affect node size to the node_list
    node_list <- cbind(node_list, node_size = node_name_size())
    
    # Return the node list
    node_list <- arrange(node_list, ID)
    
  })
  
  
  # Create an edge list 
  edge_person_List <- reactive ({
    
    # Creating data frame of edges from graph
    edge_List <- as.data.frame(get.edgelist(G_sample1()))
    
    # Add node ids to the edge list
    edge_List$SourceID <- node_person_list()[match(edge_List$V1, node_person_list()$Name), 2]
    edge_List$TargetID <- node_person_list()[match(edge_List$V2, node_person_list()$Name), 2]
    
    # Return edge list
    edge_List
  })
  
  # The output$test_network visualizes subgraphs with the following aesthetics:
  #   1) centrality = size of node
  #   2) crime type = color of node
  # 
  
  output$network_graph_people <- renderForceNetwork({
    
    # Creating the graph
    person_network <- forceNetwork(Links = edge_person_List(),
                                    Nodes = node_person_list(),
                                    Source = 'SourceID',
                                    Target = 'TargetID',
                                    NodeID = 'Name',
                                    Nodesize = 'node_size',
                                    Group = node_name_color(),
                                    fontSize = 17,
                                    legend = TRUE,
                                    bounded = TRUE,
                                    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                                    opacityNoHover = label_nodes()
    )
    
    # Printing the network
    print(person_network)
    
  })
  
  
})