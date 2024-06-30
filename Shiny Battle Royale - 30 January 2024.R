install.packages("shiny")
install.packages("fmsb")
install.packages("colormap")

library(fmsb)
library(shiny)
library(colormap)

battleroyale.character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore", "None"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8, NA),
  Magic = c(10,0,10, 1, 10, 3, 6, 2, NA),
  Humour = c(10,0,6, 1, 5, 8, 10, 9, NA),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2, NA),
  Special = c(10,0,10, 1, 3, 8, 2, 6, NA)
)

ui.battle.royale <- fluidPage(
  titlePanel("Battle Royale"), 
  sidebarLayout(
    sidebarPanel(
      div(
        selectInput("character1", 
                    label = "Select character 1:", 
                    choices = as.character(battleroyale.character.database[, 1]), 
                    selected = "Gandalf"), 
        style = "margin-bottom:20px;"
      ),
      div(selectInput("character2", 
                      label = "Select character 2:",
                      choices = as.character(battleroyale.character.database[ , 1]),
                      selected = "Harry Potter"),
          style = "margin-bottom:20px;"),
      
      div(selectInput("character3", 
                      label = "Select character 3:",
                      choices = as.character(battleroyale.character.database[ , 1]),
                      selected = "None"),
          style = "margin-bottom:20px;"),
      
      div(selectInput("character4", 
                      label = "Select character 4:",
                      choices = as.character(battleroyale.character.database[ , 1]),
                      selected = "None"),
          style = "margin-bottom:20px;"),
      
      div(selectInput("character5", 
                      label = "Select character 5:",
                      choices = as.character(battleroyale.character.database[ , 1]),
                      selected = "None"),
          style = "margin-bottom:20px;"),
      div(selectInput("character6", 
                      label = "Select character 6:",
                      choices = as.character(battleroyale.character.database[ , 1]),
                      selected = "None"),
          style = "margin-bottom:20px;"),
     
      
      div(
        selectInput("attribute1", 
                    label = "Select attack attribute:", 
                    choices = as.character(colnames(battleroyale.character.database[2:6])), 
                    selected = "Strength"), 
        style = "margin-bottom:20px;"
      ),
      
      actionButton("start.battle.royale","START BATTLE ROYALE", style = "color: white; background-color: green")
    ),
    mainPanel(
      div(textOutput("battle.result.text"), style = "margin-bottom:20px;"),
      plotOutput("radarcharts.battle.royale")
    )
  )
)

server.battle.royale <- function(input, output, session) {
  character.database <- battleroyale.character.database
  
  observeEvent(input$start.battle.royale, {
    characters <- c(input$character1, input$character2, input$character3, input$character4, input$character5, input$character6)
    attribute <- input$attribute1
    
    attacks <- numeric(length(characters))
    
    for (i in seq_along(characters)) {
      if (characters[i] != "None") {
        attack <- character.database[[attribute]][character.database$Character == characters[i]]
        attacks[i] <- attack
      }
    }
    
    characters <- characters[characters != "None"]
    attacks <- attacks[characters != "None"]
    
    winners <- characters[which(attacks == max(attacks))]
    
    output$battle.result.text <- renderText({
      if (length(winners) == 1) {
        paste("In a battle of", attribute, "between", paste(characters, collapse = ","), ";", winners, "wins!!")
      } else {
        paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and "))
      }
    })
    
    output$radarcharts.battle.royale <- renderPlot({
      n <- length(characters)
      if (n %% 3 == 0) {
        layout(matrix(1:n, ncol = 3, byrow = TRUE))
      } else if (n == 4) {
        layout(matrix(1:n, ncol = 2, byrow = TRUE))
      } else if (n == 5) {
        layout(matrix(1:n, ncol = 5, byrow = TRUE))  
      }
      
      for (i in 1:n) {
        char <- characters[i]
        character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
        colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
        colors_in =colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)
        char_color <- colors_border[i]
        char_fill <- colors_in[i]
        
        par(mar = c(0, 0, 4, 0))
        radarchart(character_data,
                   seg = 5,
                   pcol = char_color,
                   pfcol = char_fill,
                   plty = 1,
                   plwd = 4,
                   cglcol = "grey",
                   cglty = 1,
                   cglwd = 1.5,
                   vlcex = 1,
                   title = char)
      }
    })
  })
}

shinyApp(ui.battle.royale, server.battle.royale)
