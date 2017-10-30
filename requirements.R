pkgs <- c("tidyverse", 'network', 'sna', 'igraph', 'stringr',
          'GGally', 'intergraph', 'RecordLinkage', 'shiny',
          'networkD3')
lapply(pkgs, install.packages, character.only = TRUE)