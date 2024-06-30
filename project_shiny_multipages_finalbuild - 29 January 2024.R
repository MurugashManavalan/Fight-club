install.packages("shiny")
install.packages("fmsb")
install.packages("vctrs")

library(fmsb)
library(shiny)
library(vctrs)

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
               column(12,
                      div(textOutput("intro1"), style = "margin-bottom:10px;"),
                      div(textOutput("intro2"), style = "margin-bottom:10px;"),
                      div(textOutput("intro3"), style = "margin-bottom:10px;"),
                      div(textOutput("intro4"), style = "margin-bottom:10px;"),
                      div(textOutput("intro5"), style = "margin-bottom:10px;"),
                      div(textOutput("intro6"), style = "margin-bottom:20px;"),
               )
             ),
             fluidRow(
               column(12,
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
    
#       UI - 1v1 BATTLE PAGE ----
tabPanel("1 v 1 Battle",
         titlePanel("1 v 1 Battle"),
         sidebarLayout(
           sidebarPanel(
             div(textOutput("parametertext1"), style = "margin-bottom:20px;"),
             div(selectInput("dropdown3", label = "In the blue corner...",
                             choices = as.character(rownames(character.database)),
                             selected = "Gandalf"), style = "margin-bottom:20px;"),
             div(selectInput("dropdown4", label = "In the red corner...",
                             choices = as.character(rownames(character.database)),
                             selected = "Harry Potter"), style = "margin-bottom:20px;"),
             div(textOutput("parametertext2"), style = "margin-bottom:20px;"),
             div(sliderInput("startlifepoints", "Starting life points:", min=20, max=250, value=50)),
             div(textOutput("parametertext3"), style = "margin-bottom:5px;"),
             div(textOutput("parametertext4"), style = "margin-bottom:5px;"),
             div(textOutput("parametertext5"), style = "margin-bottom:5px;"),
             div(textOutput("parametertext6"), style = "margin-bottom:20px;"),
             div(sliderInput("battlespeed", "Battle speed:", min=1, max=5, value=3)),
             div(textOutput("parametertext7"), style = "margin-bottom:30px;"),
             actionButton("sim.battle","SIMULATE A FIGHT", style = "color: white; background-color: blue"), # button to simulate a fight and store values
             actionButton("start.fight","START FIGHT", style = "color: white; background-color: green"), # button to start fighting loop
             actionButton("stop.fight","STOP FIGHT", style = "color: white; background-color: red"), # button to break fighting loop
           ),
           mainPanel(
             strong(div(textOutput("battlepreviewtext1"), style = "margin-bottom:20px;")),
             div(textOutput("battlepreviewtext2"), style = "margin-bottom:20px;"),
             plotOutput("radarchart3"), # radar chart showing both characters in battle preview
             strong(div(textOutput("commentary.title"), style = "margin-bottom:20px;")), # commentary title
             div(textOutput("commentary.text"), style = "margin-bottom:20px;"), # commentary, where text will change during battle
             actionButton("show.plot","Show/Hide Plot", style = "color: black; background-color: white"), # button to start fighting loop
             plotOutput("lifepoint.plot") # plot where the repeat loop will show life points each round
           )
         )
         
),
    tabPanel("Battle Royale"),
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
  output$intro1 <- renderText({"In this app, you can battle your favourite movie characters with each other!"})
  output$intro2 <- renderText({"Load the starting character database or create your own characters too!"})
  output$intro3 <- renderText({"Compare character attributes to design your strategy!"})
  output$intro4 <- renderText({"Start a 1v1 battle or go big in a Battle Royale!"})
  output$intro5 <- renderText({"We hope you enjoy playing!"})
  output$intro6 <- renderText({"Created by Miles Raishbrook and Murugash Manavalan!"})
  
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

#       SERVER - 1v1 BATTLE PAGE ----
  ## define reactive values ----
  rv <<- reactiveValues()
  #initialize the data
  rv$character.database <<- data.frame(character.database)
  
  rv$commentary <<- c("")
  rv$round.ch1.battle <- 1
  rv$round.ch2.battle <- 1
  rv$character1.start.life <- ""
  rv$character2.start.life <- ""
  rv$life.values.ch1 <- numeric(100)
  rv$life.values.ch2 <- numeric(100)
  rv$ attacks.ch1 <- numeric(100)
  rv$attacks.ch2 <- numeric(100)
  rv$attacks.name.ch1 <- character(100)
  rv$attacks.name.ch2 <- character(100)
  rv$random.attack.ch1 <- numeric(1)
  rv$random.attack.ch2 <- numeric(1)
  rv$winner <- ""
  
  rv$rounds.used <- ""
  rv$commentary.line.1 <- c()
  rv$commentary.line.2 <- c()
  rv$commentary.line.3 <- c()
  rv$commentary.line.4 <- c()
  rv$commentary.line.5 <- c()
  rv$commentary.line.6 <- c()
  rv$commentary.line.final.1 <- c()
  rv$commentary.line.final.1.3 <- c()
  rv$commentary.line.final.1.2 <- c()
  rv$commentary.line.final.1.1 <- c()
  rv$commentary.line.final.2 <- c()
  rv$commentary.line.final.3 <- c()
  rv$commentary.line.final.4.3 <- c()
  rv$commentary.line.final.4.2 <- c()
  rv$commentary.line.final.4.1 <- c()
  rv$rounds <- c()
  
  ## drop down menu text area ----
  output$parametertext1 <- renderText({
    "Who will fight this time?"
  })
  
  # text to go above input for amount of life points
  output$parametertext2 <- renderText({
    "How many life points should the fighters start with?"
  })
  
  # text to go under lifepoint slider
  output$parametertext3 <- renderText({
    "Battle length:"
  })
  output$parametertext4 <- renderText({
    "Short: 20-50 lifepoints"
  })
  output$parametertext5 <- renderText({
    "Medium: 50-150 lifepoints"
  })
  output$parametertext6 <- renderText({
    "Long: 150-250 lifepoints"
  })
  output$parametertext7 <- renderText({
    "(Fast) 1 <<< 3 >>> 5 (Slow)"
  })
  
  ## - battle preview section - ----   
  # observe for changes in character.database, if changes, update drop down menus
  observe({
    updateSelectInput(session, "dropdown3", choices = as.character(rownames(rv$character.database)))
    updateSelectInput(session, "dropdown4", choices = as.character(rownames(rv$character.database)))
  })
  
  # text to go at the top of the battle preview section (mainPanel)
  output$battlepreviewtext1 <- renderText({
    "Battle preview:"
  })
  output$battlepreviewtext2 <- renderText({
    paste(input$dropdown3, "and", input$dropdown4, "will fight! Let's see their stats...")
  })
  
  ## reactive expression - finds selected row of database and creates reactive vector
  battle.character1 <- reactive({
    rv$character.database[input$dropdown3, ]
  })
  battle.character2 <- reactive({
    rv$character.database[input$dropdown4, ]
  })
  
  # create data frame with max and min in row 1 and 2
  battle.df <- data.frame(strength = c(10,0), magic = c(10,0), humour = c(10,0), cruelty = c(10,0), special = c(10,0))
  
  # create reactive dataframe to plot rows from dropdown3+4
  battle.df1and2 <- reactive ({
    battle.df1and2 <- rbind(battle.df,battle.character1(),battle.character2())
  })
  
  # radar plot colours
  battleradar.colours <- c("blue","red")
  
  # plot radar chart from battle.df with both characters
  output$radarchart3 <- renderPlot({
    df3 <- battle.df1and2()
    radarchart(df3,
               seg=5,
               pcol=battleradar.colours,
               plty=1,
               plwd=3,
               cglcol="black",
               cglty=1,
               cglwd=1.5,
               vlcex=1,
               title=paste(input$dropdown3, "v.",input$dropdown4))
    
    legend(x=1.25, y=1.25,
           legend = rownames(battle.df1and2()[3:4, ]),
           bty = "n",
           col = battleradar.colours,
           pch=20,
           text.col = "black",
           cex=1,
           pt.cex=1)
  })
  
  
  ## - battle section ----
  
  output$commentary.title <- renderText({
    "Battle commentary:"
  })
  
  ## - BATTLE FUNCTION## ----
  simulate.fight <<- function(){
    
    # define the round counter for each character
    rv$round.ch1.battle <- 1
    rv$round.ch2.battle <- 1
    
    # define the number of life points each character starts with
    rv$character1.start.life <- input$startlifepoints
    rv$character2.start.life <- input$startlifepoints
    # create vectors to store the change in life over time
    rv$life.values.ch1 <- numeric(100)
    rv$life.values.ch2 <- numeric(100)
    # life values vector starts at the chosen starting life points
    rv$life.values.ch1[rv$round.ch1.battle] <- rv$character1.start.life
    rv$life.values.ch2[rv$round.ch2.battle] <- rv$character2.start.life
    
    # create vectors to store each characters attack values in each round
    rv$attacks.ch1 <- numeric(100)
    rv$attacks.ch2 <- numeric(100)
    
    # create vectors to store each characters attack name in each round
    rv$attacks.name.ch1 <- character(100)
    rv$attacks.name.ch2 <- character(100)
    
    ### inside repeat loop
    repeat{
      # define a random attack(column) from the attributes of selected characters
      rv$random.attack.ch1 <- sample(1:ncol(rv$character.database),size=1)
      rv$random.attack.ch2 <- sample(1:ncol(rv$character.database),size=1)
      # add the value of this randomly selected attack to the storage vector
      rv$attacks.ch1[rv$round.ch1.battle] <- rv$character.database[input$dropdown3, rv$random.attack.ch1] 
      rv$attacks.ch2[rv$round.ch2.battle] <- rv$character.database[input$dropdown4, rv$random.attack.ch2]
      # add the name of this randomly selected attack to the storage vector
      attributes <- colnames(rv$character.database)
      rv$attacks.name.ch1[rv$round.ch1.battle] <- attributes[rv$random.attack.ch1]
      rv$attacks.name.ch2[rv$round.ch2.battle] <- attributes[rv$random.attack.ch2]
      
      # each character makes an attack, store in life values vector for each character - 1st round
      rv$life.values.ch1[rv$round.ch1.battle + 1] <- (rv$life.values.ch1[rv$round.ch1.battle]) - rv$attacks.ch2[rv$round.ch2.battle]
      rv$life.values.ch2[rv$round.ch2.battle + 1] <- (rv$life.values.ch2[rv$round.ch2.battle]) - rv$attacks.ch1[rv$round.ch1.battle]
      
      # add 1 to the round counter for the next loop iteration
      rv$round.ch1.battle <- rv$round.ch1.battle + 1
      rv$round.ch2.battle <- rv$round.ch2.battle + 1
      
      # if each character loses all life, the other wins
      if(rv$life.values.ch1[rv$round.ch1.battle] <= 0 && rv$life.values.ch2[rv$round.ch2.battle] <= 0){
        rv$winner <- "No one"
        break()
      } else if(rv$life.values.ch1[rv$round.ch1.battle] <=0){
        rv$winner <- input$dropdown4
        break()
      } else if(rv$life.values.ch2[rv$round.ch2.battle] <=0){
        rv$winner<- input$dropdown3
        break()
      }
    }
  }
  
  ## - BUILD COMMENTARY FUNCTION ----   
  simulate.commentary <<- function(){
    
    ## create commentary vector from resulting battle storage vectors (simulate commentary button)
    # create round vector
    rv$rounds.used <- sum((rv$life.values.ch1 > 0))+1 # use in the for loop to create commentary vector
    rv$rounds <- 1:rv$rounds.used ## use as the x axis for the plot
    
    
    ### create commentary vector
    
    rv$commentary.line.1 <- c()
    for(i in 1:rv$rounds.used){
      rv$commentary.line.1[i] <- paste("Round", rv$rounds[i], "is starting...")
    }
    rv$commentary.line.2 <- c()
    for(i in 1:rv$rounds.used){
      rv$ commentary.line.2[i] <- paste(input$dropdown3, "attacked with", rv$attacks.name.ch1[i], "and dealt ",rv$attacks.ch1[i], " damage to ", input$dropdown4,"!")
    }
    rv$commentary.line.3 <- c()
    for(i in 1:rv$rounds.used){
      rv$commentary.line.3[i] <- paste(input$dropdown4, "attacked with", rv$attacks.name.ch2[i], "and dealt ",rv$attacks.ch2[i], " damage to ", input$dropdown3,"!")
    }
    rv$ commentary.line.4 <- c()
    for(i in 1:rv$rounds.used){
      rv$commentary.line.4[i] <- paste("At the end of Round", rv$rounds[i], "...")
    }
    rv$commentary.line.5 <- c()
    for(i in 1:rv$rounds.used){
      rv$commentary.line.5[i] <- paste(input$dropdown3," has ", rv$life.values.ch1[i], "/", input$startlifepoints, "life points remaining.")
    }
    rv$commentary.line.6 <- c()
    for(i in 1:rv$rounds.used){
      rv$commentary.line.6[i] <- paste(input$dropdown4," has ", rv$life.values.ch2[i], "/", input$startlifepoints, "life points remaining.")
    }
    
    # Use vec_interleave to interleave the vectors
    rv$commentary <- vec_interleave(rv$commentary.line.1,
                                    rv$commentary.line.2,
                                    rv$commentary.line.3,
                                    rv$commentary.line.4,
                                    rv$commentary.line.5,
                                    rv$commentary.line.6)
    
    ## now append the lines saying who won at the end
    rv$commentary.line.final.1 <- paste(input$dropdown3, "and", input$dropdown4, "are too tired to carry on!", rv$winner, "wins the battle!")
    rv$commentary.line.final.1.3 <- paste("The battle ended in a draw! Restarting in 3....")
    rv$commentary.line.final.1.2 <- paste("The battle ended in a draw! Restarting in 2....")
    rv$commentary.line.final.1.1 <- paste("The battle ended in a draw! Restarting in 1....")
    
    rv$commentary.line.final.2 <- paste(input$dropdown3, "is too tired to carry on!", rv$winner, "wins the battle")
    rv$commentary.line.final.3 <- paste(input$dropdown4, "is too tired to carry on!", rv$winner, "wins the battle")
    
    rv$commentary.line.final.4.3 <- paste(rv$winner, "won the battle! Restarting in 3....")
    rv$commentary.line.final.4.2 <- paste(rv$winner, "won the battle! Restarting in 2....")
    rv$commentary.line.final.4.1 <- paste(rv$winner, "won the battle! Restarting in 1....")
    
    if(rv$life.values.ch1[rv$round.ch1.battle] <= 0 && rv$life.values.ch2[rv$round.ch2.battle] <= 0){
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.1)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.1.3)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.1.2)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.1.1)
    } else if(rv$life.values.ch1[rv$round.ch1.battle] <=0){
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.2)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.3)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.2)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.1)
    } else if(rv$life.values.ch2[rv$round.ch2.battle] <=0){
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.3)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.3)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.2)
      rv$commentary <- append(rv$commentary, rv$commentary.line.final.4.1)
    }
  }
  
  ## timer for commentary text cycling when start fight button is pressed ----
  
  # Initialize the index to 0
  index <- reactiveVal(0)
  
  # Initialize a reactive value to control the timer
  timerRunning <- reactiveVal(FALSE)
  
  observeEvent(input$start.fight,{
    # Create a reactive timer that triggers every input(slider) seconds
    timer <- reactiveTimer(input$battlespeed*1000)
    
    observeEvent(timer(), {
      if (timerRunning()) {
        # Update the index to cycle through the lines
        index(ifelse(index() < length(rv$commentary), index() + 1, 0))
      }
    })
    
    # Set timerRunning to TRUE when the start button is clicked
    timerRunning(TRUE)
  })
  
  observeEvent(input$stop.fight, {
    # Set timerRunning to FALSE when the stop button is clicked, reset index to 0
    timerRunning(FALSE)
    index(0)
  })
  
  # Display the current line in the UI
  output$commentary.text <- renderText({
    rv$commentary[index()]
  })
  
  ## initiate functions when buttons are pressed ----
  observeEvent(input$sim.battle, {
    simulate.fight()
    simulate.commentary()
  })
  
  
  ## plot the life values generated from the simulated battle (with show/hide plot) ----
  
  # reactive value to control plot visibility
  showPlot <- reactiveVal(FALSE) 
  
  # observe button being pressed
  observeEvent(input$show.plot, {
    # invert TRUE/FALSE when button is pressed
    showPlot(!showPlot())
  })  
  
  output$lifepoint.plot <- renderPlot({
    # if showPlot is TRUE then plot....
    if (showPlot()) {
      plot(rv$rounds, rv$life.values.ch1[1:rv$rounds.used],
           type="b",
           ylim=c(0, input$startlifepoints),
           xlim=c(1, rv$rounds.used),
           col="blue",
           lwd=2,
           xlab="Round",
           ylab="Life points",
           main=paste("Battle summary:", rv$winner, "wins!"))
      lines(rv$rounds, rv$life.values.ch2[1:rv$rounds.used],
            type="b",
            col="red",
            lwd=2)
      legend("bottomleft", legend=c(input$dropdown3, input$dropdown4), col=c("blue", "red"), lty=1)
    }
  })   
}



## SHINY APP - RUN ME LAST ----
shinyApp(ui,server)

## bugs still there on 1v1 battle page....
## seems to be some problem --> life points "at end of round..." and attacks are not in sync, shifted by 1 round???
## battle speed seems to work from slider, but timer not reset if battlespeed is changed