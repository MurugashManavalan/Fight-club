#make data frame, random numbers, 5 columns
character.database <- as.data.frame(matrix(sample(1:10,5,replace=F),ncol=5))
character.database
#name the columns with characteristics
colnames(character.database) <- c("strength","magic","humour","cruelty","special")
#add two new rows, row 1 is radar chart max, row 2 is min, 5 columns
character.database <- rbind(rep(10,5),rep(0,5),character.database)
#name the rows, max, min, and first character
rownames(character.database) <- c("max","min","test1")
character.database

#####create vector of new character with random attributes
new_charac <- c(sample(1:10,5,replace=F),ncol=5)
new_charac
# add to character dataframe
character.database <- rbind(character.database,new_charac)
character.database
#then assign a rowname to the last row
rownames(character.database)[nrow(character.database)] <- "Frodo Baggins"
character.database

#### make a function that adds a new character to character.database

add.character <- function(character_name, strength, magic, humour, cruelty, special){
  new.character <- c(strength, magic, humour, cruelty, special)
  character.database <<- rbind(character.database,new.character)
  rownames(character.database)[nrow(character.database)] <<- character_name
  print(paste(character_name, "is ready to fight!"))
}

add.character("Harry Potter", strength=3, magic=6, humour=3, cruelty=4, special=8)
add.character("Severus Snape", strength=6, magic=8, humour=0, cruelty=9, special=7)
add.character("Gandalf", strength=6, magic=9, humour=6, cruelty=2, special=8)
add.character("Bilbo Baggins", strength=1, magic=1, humour=5, cruelty=1, special=9)
add.character("Dumbledore", strength=5, magic=9, humour=3, cruelty=4, special=8)
character.database


################
## now try to make a function to see who wins in a fight

character.database["Harry Potter", ]
character.database["Harry Potter","magic"]  # just testing the indexing

fight <- function(character1, character2, attribute){
  attack1 <- character.database[character1, attribute]
  attack2 <- character.database[character2, attribute]
  if(attack1 > attack2){
    print(paste("A battle of ", attribute, " between ", character1, " and ", character2, " occurred."))
    print(paste(character1," won."))
  }else{
    print(paste("A battle of ", attribute, " between ", character1, " and ", character2, " occurred."))
    print(paste(character2," won."))   
  }
}

fight("Harry Potter","Dumbledore","magic")

### what if it is a draw? edit the function to accommodate this

fight <- function(character1, character2, attribute){
  attack1 <- character.database[character1, attribute]
  attack2 <- character.database[character2, attribute]
  if(attack1 > attack2){
    print(paste("A battle of ", attribute, " between ", character1, " and ", character2, " occurred."))
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste("A battle of ", attribute, " between ", character1, " and ", character2, " occurred."))
    print(paste(character2," won."))  
  } else {
    print(paste("A battle of ", attribute, " between ", character1, " and ", character2, " occurred."))
    print("It's a tie!")
  }
}

character.database
fight("Gandalf","Dumbledore","magic")

### edit the function so that the two characters to fight can use different attacks

fight <- function(character1, character1.attack, character2, character2.attack){
  attack1 <- character.database[character1, character1.attack]
  attack2 <- character.database[character2, character2.attack]
  if(attack1 > attack2){
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character2," won."))  
  } else {
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print("It's a tie!")
  }
}

character.database
fight("Harry Potter","magic","Dumbledore","humour")
fight("Gandalf","magic","Dumbledore","magic")
fight("Bilbo Baggins","humour","Severus Snape","cruelty")


## change the function so that it also shows the radar charts once a fight begins

install.packages('fmsb')
library(fmsb)

?radarchart
character.database

## prepare code before integration into the function
#create new df with rows max, min and the characters selected for fight

fighters <- character.database[c("max", "min","Dumbledore","Harry Potter"), ]
fighters

# create vector defining the colours to use in the plots
plot.colours <- c("red","blue")

#radar chart test
radarchart(fighters,
           seg=5,
           pcol=plot.colours,
           plty=1,
           plwd=3,
           cglcol="black",
           cglty=1,
           cglwd=1.5,
           vlcex=0.8)

legend(x=2, y=1,
       legend = rownames(fighters[3:4, ]),
       bty = "n",
       col = plot.colours, # Use the same colors as in the plot
       pch=20,
       text.col = "black",
       cex=0.7,
       pt.cex=1)

## now integrate into the function so that it plots radar charts of
## the chosen characters

fight <- function(character1, character1.attack, character2, character2.attack){
  attack1 <- character.database[character1, character1.attack]
  attack2 <- character.database[character2, character2.attack]
  if(attack1 > attack2){
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character2," won."))  
  } else {
    print(paste(character1, " attacked with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print("It's a tie!")
  }
  who.is.fighting <- character.database[c("max", "min",character1,character2), ]
  plot.colours <- c("red","blue")
  
  radarchart(who.is.fighting,
             seg=5,
             pcol=plot.colours,
             plty=1,
             plwd=3,
             cglcol="black",
             cglty=1,
             cglwd=1.5,
             vlcex=1,
             title=paste(character1," v. ", character2))
  
  legend(x=1.25, y=1.25,
         legend = rownames(who.is.fighting[3:4, ]),
         bty = "n",
         col = plot.colours,
         pch=20,
         text.col = "black",
         cex=1,
         pt.cex=1)
}

fight("Harry Potter","magic","Dumbledore","humour")
fight("Gandalf","magic","Dumbledore","magic")
fight("Bilbo Baggins","humour","Severus Snape","cruelty")

###############################

######## Chatgpt Code

fight <- function(characters) {
  num_characters <- length(characters)
  if (num_characters < 2) {
    stop("At least two characters are needed for a fight.")
  }
  
  # Initialize the winners vector
  winners <- character(num_characters)
  
  for (i in 1:(num_characters - 1)) {
    character1 <- characters[i]
    character1.attack <- characters[i + 1]
    
    attack1 <- character.database[character1, character1.attack]
    
    for (j in (i + 1):num_characters) {
      character2 <- characters[j]
      character2.attack <- characters[j + 1]
      
      attack2 <- character.database[character2, character2.attack]
      
      if (attack1 > attack2) {
        winners[j] <- character1
      } else if (attack1 < attack2) {
        winners[j] <- character2
      } else {
        winners[j] <- "It's a tie!"
      }
    }
  }
  
  # Print the results
  for (i in 1:num_characters) {
    print(paste(characters[i], "attacked with", characters[i + 1], "!"))
    print(paste("Winner:", winners[i]))
  }
  
  # Prepare characters for the radar chart
  who.is.fighting <- character.database[c("max", "min", characters), ]
  plot.colours <- rainbow(num_characters)
  
  # Plot radar chart
  radarchart(who.is.fighting,
             seg = 5,
             pcol = plot.colours,
             plty = 1,
             plwd = 3,
             cglcol = "black",
             cglty = 1,
             cglwd = 1.5,
             vlcex = 1,
             title = paste(characters, collapse = " v. "))
  
  # Add legend to the radar chart
  legend(x = 1.25, y = 1.25,
         legend = characters,
         bty = "n",
         col = plot.colours,
         pch = 20,
         text.col = "black",
         cex = 1,
         pt.cex = 1)
}

# Example usage:
characters_in_fight <- c("Harry Potter", "magic", "Dumbledore", "humour", "Gandalf", "magic")
fight(characters_in_fight)