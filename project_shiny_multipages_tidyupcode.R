install.packages("shiny")
install.packages("fmsb")

library(fmsb)
library(shiny)

# CREATE BASIC CHARACTER DATABASE - RUN ME FIRST ----
character.database <- data.frame(strength = c(10,0),
                                  magic = c(10,0),
                                  humour = c(10,0),
                                  cruelty = c(10,0),
                                  special = c(10,0))
rownames(character.database) <- c("max","min")

## USER INTERFACE FUNCTION - RUN ME SECOND ----

ui <- fluidPage(
  tabsetPanel(
    
#       UI - HOME PAGE  ----
    
    tabPanel("Home",
             titlePanel("Welcome to the battle arena!"),
             fluidRow(
               column(6,
                      textOutput("intro")
               )
             ),
             fluidRow(
               column(6,
                      actionButton("load.database","Load characters"), # button to load/reset character database
                      textOutput("database.loaded") # text shown when 'load database' button is pressed
               ),
             )
    ),
    
#       UI - ADD CHARACTER PAGE ----
    tabPanel("Add character",
             titlePanel("Add a new character..."),
             sidebarLayout(
               sidebarPanel(
                 textInput("name", "Name:"),
                 sliderInput("strength", "Physical strength:", min = 1, max = 10, value = 5),
                 sliderInput("magic", "Magical ability:", min = 1, max = 10, value = 5),
                 sliderInput("humour", "Comedy and wit:", min = 1, max = 10, value = 5),
                 sliderInput("cruelty", "Cruelty and mercilessness:", min = 1, max = 10, value = 5),
                 sliderInput("special", "Hidden talent:", min = 1, max = 10, value = 5),
               ),
               mainPanel(
                 plotOutput("radarplot"),
                 actionButton("add.to.database","Ready to fight!"), # button to add characters to database
                 textOutput("character.added") # text shown if button above is pressed
               )
             ) 
    ),
    
#       UI - COMPARE PAGE ----
    tabPanel("Compare",
             titlePanel("Compare fighters..."),
             fluidRow(
               column(6,
                      selectInput("dropdown1", label = "Select character:",
                                  choices = as.character(rownames(character.database)),
                                  selected = "Harry Potter"),
                      plotOutput("radarchart1")
               ),
               column(6,
                      selectInput("dropdown2", label = "Select character:",
                                  choices = as.character(rownames(character.database)),
                                  selected = "Gandalf"),
                      plotOutput("radarchart2")
               )
             )
    ),
    
#       UI - BATTLE PAGES - # need to make ----
    tabPanel("1 v 1 Battle"),
    tabPanel("2 v 2 Battle"),
  )
)

## SERVER FUNCTION - RUN ME THIRD ----

server <- function(input, output, session){
  
#       SERVER - HOME PAGE ----
  
  # create basic character database on opening with max and min
  character.database <<- data.frame(strength = c(10,0),
                                    magic = c(10,0),
                                    humour = c(10,0),
                                    cruelty = c(10,0),
                                    special = c(10,0))
  rownames(character.database) <<- c("max",
                                     "min")
  
  # define reactive values
  rv <<- reactiveValues()
  #initialize the data
  rv$character.database <<- data.frame(character.database)
  
  # text to appear on home screen
  output$intro <- renderText({
  "In this app, you can battle your favourite movie characters with each other!
   Load the starting character database and create your own characters in the
   'Add character' tab, then start a battle!
   You can also compare character attributes to design your strategy!
   We hope you enjoy playing!"
  })
  
  # text for when 'Load database' button is pressed
  output$database.loaded <- eventReactive(input$load.database, {
    return("Characters have been loaded and ready to fight, see who they are in the 'Compare' tab!")
  })
  
  # when 'Load database' button is pressed, new dataframe with more characters is created
  observeEvent(input$load.database,{
    # create df with max, min and some characters
    rv$character.database <<- data.frame(strength = c(10,0,3,5,5,6,1,4,7,5,7,9,10,10),
                                      magic = c(10,0,6,10,9,9,1,8,9,8,3,1,6,9),
                                      humour = c(10,0,3,3,1,6,5,2,3,9,8,2,2,7),
                                      cruelty = c(10,0,4,4,10,2,1,8,9,2,1,5,7,1),
                                      special = c(10,0,8,8,6,7,9,4,7,6,7,10,6,3))
    rownames(rv$character.database) <<- c("max",
                                          "min",
                                          "Harry Potter",
                                          "Albus Dumbledore",
                                          "Lord Voldemort",
                                          "Gandalf",
                                          "Bilbo Baggins",
                                          "Saruman",
                                          "Darth Vader",
                                          "Obi-wan Kenobi",
                                          "Spiderman",
                                          "Superman",
                                          "Geralt of Rivia",
                                          "Thor")
  })
  
#       SERVER - ADD CHARACTER PAGE ----
  
  # create data frame with max and min in row 1 and 2
  df <- data.frame(strength = c(10,0), magic = c(10,0), humour = c(10,0), cruelty = c(10,0), special = c(10,0))
  
  # create new character vector - reactive from sliders
  new.character <- reactive({
    c(input$strength,input$magic,input$humour,input$cruelty,input$special)
  })
  
  #add new character to temp df - observe values for changes
  observe({
    df <- rbind(df, new.character())
    
    #name the rows - max, min, name
    rownames(df) <- c("max","min",input$name)
    
    #plot radar
    plot.colours <- "blue"
    output$radarplot <- renderPlot(
      radarchart(df,
                 seg=5,
                 pcol=plot.colours,
                 plty=1,
                 plwd=3,
                 cglcol="black",
                 cglty=1,
                 cglwd=1.5,
                 vlcex=1,
                 title=paste("New character:",input$name)))
    
    # text for when add button is pressed
    output$character.added <- eventReactive(input$add.to.database, {
      return("Application to the fight club accepted!")
    })
  })
  
  # when button is pressed, adds character vector to character database  
  observeEvent(input$add.to.database, {
    rv$character.database <<- rbind(rv$character.database,new.character())
    rownames(rv$character.database)[nrow(rv$character.database)] <<- input$name
  })
  
#       SERVER - COMPARE PAGE ----
  
  # observe for changes in character.database, if changes, update drop down menus
  observe({
    updateSelectInput(session, "dropdown1", choices = as.character(rownames(rv$character.database)))
    updateSelectInput(session, "dropdown2", choices = as.character(rownames(rv$character.database)))
  })
  
  ## reactive expression - finds selected row of database and creates reactive vector
  selected.row1 <- reactive({
    rv$character.database[input$dropdown1, ]
  })
  
  selected.row2 <- reactive({
    rv$character.database[input$dropdown2, ]
  })
  
  # create data frame with max and min in row 1 and 2
  df <- data.frame(strength = c(10,0), magic = c(10,0), humour = c(10,0), cruelty = c(10,0), special = c(10,0))
  
  # create reactive dataframe1 to plot row from dropdown1
  df1 <- reactive ({
    rbind(df,selected.row1())
  })
  
  # create reactive dataframe2 to plot row from dropdown2
  df2 <- reactive ({
    rbind(df,selected.row2())
  })
  
  # plot radarchart1
  output$radarchart1 <- renderPlot({
    plot.colour1 <- "blue"
    radarchart(df1(),
               seg=5,
               pcol=plot.colour1,
               plty=1,
               plwd=3,
               cglcol="black",
               cglty=1,
               cglwd=1.5,
               vlcex=1,
               title=paste(input$dropdown1, ":"))
  })
  
  # plot radarchart2
  output$radarchart2 <- renderPlot({
    plot.colour1 <- "red"
    radarchart(df2(),
               seg=5,
               pcol=plot.colour1,
               plty=1,
               plwd=3,
               cglcol="black",
               cglty=1,
               cglwd=1.5,
               vlcex=1,
               title=paste(input$dropdown2, ":"))
  })
  
}

#       SERVER - BATTLE PAGES # need to make ----

# run shiny app - RUN ME LAST ----
shinyApp(ui,server)
