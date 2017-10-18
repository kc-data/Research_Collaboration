library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Name Network Visualization"),
  
  # Sidebar with controls to choose a suspect, select a centrality measure, and 
  # a crime to mark nodes with. 
  sidebarPanel(
    textInput(inputId = "name", 
              label = "Name:", 
              value = "Name"),
    
    selectInput(inputId = "centrality", 
                label = "Choose a centrality measure:", 
                choices = c("None", "Betweenness", "Closeness", "Degree",
                            "Eigen")),
    
    selectInput(inputId = "variable", 
                label = "Choose a variable type of interest:", 
                choices = c("None", "Checked out book")),
    
    numericInput(inputId = "degree_separation", 
                 label = "Degrees from name to view:", 
                 value = 1,
                 min = 1,
                 max = 5),
    
    selectInput(inputId = "labels", 
                label = "Include Node Labels?", 
                choices = c("No", "Yes"))
    
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("Network_Title")), 
    
    h5(textOutput("Network_Test")),
    
    tabsetPanel(
      tabPanel("Person Network", 
               forceNetworkOutput("network_graph_people"), 
               dataTableOutput('person_node_dataframe'))
    )
    
  )
))