pkgs <- c("tidyverse", "lubridate", "readxl", 'stringr', 
          "reshape2", 'RColorBrewer', 'network', 'sna', 'igraph',
          'GGally', 'intergraph', 'RecordLinkage')
lapply(pkgs, require, character.only = TRUE)
#library(datasets) # Need to load in data

# Likely need to load in bipartite and unipartite graphs
dffull <- read.csv('sample_network_data.csv', stringsAsFactors = FALSE) 

dfnet <- select(dffull, Name, Code) # Creating the edgelist to create igraph object

sample1_network <- simplify(graph_from_data_frame(dfnet, directed = FALSE)) # bipartite network of codes and names

full_network <- graph.edgelist(as.matrix(dfnet)) # Create a graph object to break down in unipartite graph
V(full_network)$type <- bipartite.mapping(full_network)$type # Creating the bipartite labels to be able to isolate the name network
sample2_network <- bipartite.projection(full_network)$proj1 # Creating the unipartite graph of just names
