# create character database, new character vector, merge them - RUN ME FIRST! ----
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

# make a function that adds a new character to character.database

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

# fight - version 1 ----
## now try to make a function to see who wins in a fight

character.database["Harry Potter", ]
character.database["Harry Potter","magic"]  # just testing the indexing

battle <- function(character1, character2, attribute){
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

battle("Harry Potter","Dumbledore","magic")

# fight - version 2 - with ties ----
### what if it is a draw? edit the function to accommodate this

battle <- function(character1, character2, attribute){
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
battle("Gandalf","Dumbledore","magic")

# fight - version 3 - using different attacks ----
### edit the function so that the two characters to fight can use different attacks

battle <- function(character1, character1.attack, character2, character2.attack){
  attack1 <- character.database[character1, character1.attack]
  attack2 <- character.database[character2, character2.attack]
  if(attack1 > attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character2," won."))  
  } else {
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print("It's a tie!")
  }
}

character.database
battle("Harry Potter","magic","Dumbledore","humour")
battle("Gandalf","magic","Dumbledore","magic")
battle("Bilbo Baggins","humour","Severus Snape","cruelty")

# fight - version 4 - show radar chart at beginning of fight ----
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

battle <- function(character1, character1.attack, character2, character2.attack){
  attack1 <- character.database[character1, character1.attack]
  attack2 <- character.database[character2, character2.attack]
  if(attack1 > attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    print(paste(character2, " countered with ", character2.attack, "!"))
    print(paste(character2," won."))  
  } else {
    print(paste(character1, " challenged ", character2, " to a battle!"))
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
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

battle("Harry Potter","magic","Dumbledore","humour")
battle("Gandalf","magic","Dumbledore","magic")
battle("Bilbo Baggins","humour","Severus Snape","cruelty")

# fight - version 5 - add a delay for suspense ----
## add a delay to the function to add suspense during the battle

?Sys.sleep

battle <- function(character1, character1.attack, character2, character2.attack){
  attack1 <- character.database[character1, character1.attack]
  attack2 <- character.database[character2, character2.attack]
  if(attack1 > attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    Sys.sleep(3)
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    Sys.sleep(3)
    print(paste(character2, " countered with ", character2.attack, "!"))
    Sys.sleep(3)
    print(paste(character1," won."))
  } else if(attack1 < attack2){
    print(paste(character1, " challenged ", character2, " to a battle!"))
    Sys.sleep(3)
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    Sys.sleep(3)
    print(paste(character2, " countered with ", character2.attack, "!"))
    Sys.sleep(3)
    print(paste(character2," won."))  
  } else {
    print(paste(character1, " challenged ", character2, " to a battle!"))
    Sys.sleep(3)
    print(paste(character1, " attacked ", character2, " with ", character1.attack, "!"))
    Sys.sleep(3)
    print(paste(character2, " countered with ", character2.attack, "!"))
    Sys.sleep(3)
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

battle("Harry Potter","magic","Dumbledore","humour")
battle("Gandalf","magic","Dumbledore","magic")
battle("Bilbo Baggins","humour","Severus Snape","cruelty")


# AUTO BATTLE - version 1 ----
## make a special or auto battle function that:
# inputs - 2 characters to battle from the character database
# uses all of their attributes randomly to attack the other character
# define each character's life points and as each attack hits, their HP goes down
# first character to lose all their wins loses, the other wins
# plots both characters life on the graph for each round

character.database

auto.battle <- function(character1,character2,startinglifepoints=100){
  
  #start with showing comparison radar charts of characters
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
  
  # define the number of life points each character starts with
  character1.life <- startinglifepoints
  character2.life <- startinglifepoints
  
  # define the round counter for each character
  round.ch1 <- 1
  round.ch2 <- 1
  
  # create vectors to store the change in life over time
  life.values.ch1 <- numeric(100)
  life.values.ch2 <- numeric(100)
  
  ## use repeat loop to make attacks on each character
  repeat{
    
    # define a random attack(column) from the attributes of selected characters
    attack.ch1 <- character.database[character1, sample(1:ncol(character.database),size=1)] 
    attack.ch2 <- character.database[character2, sample(1:ncol(character.database),size=1)]
    
    # each character makes an attack
    character1.life <- character1.life - attack.ch2
    character2.life <- character2.life - attack.ch1
    
    # add text to narrate the battle
    print(paste("Round ",round.ch1,"is starting..."))
    Sys.sleep(3)
    print(paste(character1," dealt ",attack.ch1, " damage to ", character2,"!"))
    Sys.sleep(3)
    print(paste(character2," dealt ",attack.ch2, " damage to ", character1,"!"))
    Sys.sleep(3)
    print(paste("At the end of Round ",round.ch1,"..."))
    print(paste(character1," has ", character1.life, " life points remaining and ", character2, "has ", character2.life, "life points remaining."))
    Sys.sleep(3)
    
    # store the new life in a vector for each character
    life.values.ch1[round.ch1] <- character1.life
    life.values.ch2[round.ch2] <- character2.life
    
    # add 1 to the round counter for the next loop iteration
    round.ch1 <- round.ch1 + 1
    round.ch2 <- round.ch2 + 1
    
    # if each character loses all life, the other wins
    if(character1.life <=0){
      print(paste(character1," died! ",character2, " won the battle!"))
      break()
    }
    if(character2.life <=0){
      print(paste(character2," died! ",character1, " won the battle!"))
      break()
    }
  }
  # create new data frame of character life to plot
  # Combine the vectors into a dataframe:
  characters.life <- data.frame(round = 1:min(round.ch1, round.ch2), 
                   character1.life = life.values.ch1[1:min(round.ch1, round.ch2)], 
                   character2.life = life.values.ch2[1:min(round.ch1, round.ch2)])

  # plot the dataframe
  plot(characters.life$round, characters.life$character1.life,
       type="s",
       col="red",
       lwd=2,
       xlab="Round",
       ylab="Life points",
       main="Battle summary")
  lines(characters.life$round, characters.life$character2.life,
        type="s",
        col="blue",
        lwd=2)
  legend("topright", legend=c(character1, character2), col=c("red", "blue"), lty=1)
}

auto.battle("Harry Potter","Severus Snape", 50)

# AUTO BATTLE - version 2 - plot graph after every turn ----
# what if the function plotted a graph after every turn?
# also fix the x and y limits of the graphs
# plus tidy up some other things....
# if want to keep all of the plots remove "dev.off()"

auto.battle2 <- function(character1,character2,startinglifepoints=100){
  
  print(paste(character1, "and", character2, "are going to battle!"))
  Sys.sleep(3)
  print("It's a fight to the death!")
  Sys.sleep(3)
  print(paste(character1, "and", character2, "will use all of their talents in the battle!"))
  
  #start with showing comparison radar charts of characters
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
  
  # define the number of life points each character starts with
  character1.life <- startinglifepoints
  character2.life <- startinglifepoints
  
  # define the round counter for each character
  round.ch1 <- 1
  round.ch2 <- 1
  
  # create vectors to store the change in life over time
  life.values.ch1 <- numeric(100)
  life.values.ch2 <- numeric(100)
  
  Sys.sleep(3)
  
  ## use repeat loop to make attacks on each character
  repeat{
    
    # define a random attack(column) from the attributes of selected characters
    attack.ch1 <- character.database[character1, sample(1:ncol(character.database),size=1)] 
    attack.ch2 <- character.database[character2, sample(1:ncol(character.database),size=1)]
    
    # each character makes an attack
    character1.life <- character1.life - attack.ch2
    character2.life <- character2.life - attack.ch1
    
    # add text to narrate the battle
    print(paste("Round ",round.ch1,"is starting..."))
    Sys.sleep(5)
    print(paste(character1," dealt ",attack.ch1, " damage to ", character2,"!"))
    Sys.sleep(3)
    print(paste(character2," dealt ",attack.ch2, " damage to ", character1,"!"))
    Sys.sleep(3)
    print(paste("At the end of Round ",round.ch1,"..."))
    Sys.sleep(2)
    print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(3)
    print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(3)
    
    # store the new life in a vector for each character
    life.values.ch1[round.ch1] <- character1.life
    life.values.ch2[round.ch2] <- character2.life
    
    # create new data frame of character life to plot
    # Combine the vectors into a dataframe:
    characters.life <- data.frame(round = 1:min(round.ch1, round.ch2), 
                                  character1.life = life.values.ch1[1:min(round.ch1, round.ch2)], 
                                  character2.life = life.values.ch2[1:min(round.ch1, round.ch2)])
    
    # plot the dataframe
    dev.off() #remove the last plot to not overload the plot window
    plot(characters.life$round, characters.life$character1.life,
         type="b",
         ylim=c(0,startinglifepoints),
         xlim=c(1,round.ch1),
         col="red",
         lwd=2,
         xlab="Round",
         ylab="Life points",
         main=paste("Round",round.ch1))
    lines(characters.life$round, characters.life$character2.life,
          type="b",
          col="blue",
          lwd=2)
    legend("topright", legend=c(character1, character2), col=c("red", "blue"), lty=1)
    
    # add 1 to the round counter for the next loop iteration
    round.ch1 <- round.ch1 + 1
    round.ch2 <- round.ch2 + 1
    
    # if each character loses all life, the other wins
    if(character1.life <=0){
      print(paste(character1,"is too tired to carry on!",character2, " won the battle!"))
      winner <- character2
      break()
    }
    if(character2.life <=0){
      print(paste(character2,"is too tired to carry on!",character1, " won the battle!"))
      winner<- character1
      break()
    }
    if(character1.life <= 0 && character2.life <= 0){
      print(paste(character1, "and", character2, "are too tired to carry on! The battle ends in a draw!"))
      winner <- "It's a draw!"
    }
  }
  # create new data frame of character life to plot
  # Combine the vectors into a dataframe:
  characters.life <- data.frame(round = 1:min(round.ch1, round.ch2), 
                                character1.life = life.values.ch1[1:min(round.ch1, round.ch2)], 
                                character2.life = life.values.ch2[1:min(round.ch1, round.ch2)])
  
  # plot the dataframe
  dev.off() #remove the last plot
  plot(characters.life$round, characters.life$character1.life,
       type="b",
       ylim=c(0,startinglifepoints),
       xlim=c(1,round.ch1),
       col="red",
       lwd=2,
       xlab="Round",
       ylab="Life points",
       main=paste("Battle summary:",winner, "wins!"))
  lines(characters.life$round, characters.life$character2.life,
        type="b",
        col="blue",
        lwd=2)
  legend("bottomleft", legend=c(character1, character2), col=c("red", "blue"), lty=1)
}

character.database
add.character("Voldemort",strength=5, magic=10, humour=1, cruelty=9,special=7)
auto.battle2("Harry Potter","Severus Snape", 50)
auto.battle2("Voldemort", "Dumbledore",20)

# AUTO BATTLE - version 3 - state which attack was used each turn ----

## modify the function so that it mentions which attack is used randomly by the character

auto.battle3 <- function(character1,character2,startinglifepoints=100){
  
  print(paste(character1, "and", character2, "are going to battle!"))
  Sys.sleep(3)
  print("It's a fight to the death!")
  Sys.sleep(3)
  print(paste(character1, "and", character2, "will use all of their talents in the battle!"))
  
  #start with showing comparison radar charts of characters
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
  
  # define the number of life points each character starts with
  character1.life <- startinglifepoints
  character2.life <- startinglifepoints
  
  # define the round counter for each character
  round.ch1 <- 1
  round.ch2 <- 1
  
  # create vectors to store the change in life over time
  life.values.ch1 <- numeric(100)
  life.values.ch2 <- numeric(100)
  
  Sys.sleep(2)
  
  ## use repeat loop to make attacks on each character
  repeat{
    
    # define a random attack(column) from the attributes of selected characters
    random.attack.ch1 <- sample(1:ncol(character.database),size=1)
    random.attack.ch2 <- sample(1:ncol(character.database),size=1)
    attack.ch1 <- character.database[character1, random.attack.ch1] 
    attack.ch2 <- character.database[character2, random.attack.ch2]
    
    ## extract the column names also for stating which attack was used
    attributes <- colnames(character.database)
    which.attack.ch1 <- attributes[random.attack.ch1]
    which.attack.ch2 <- attributes[random.attack.ch2]
    
    # each character makes an attack
    character1.life <- character1.life - attack.ch2
    character2.life <- character2.life - attack.ch1
    
    # add text to narrate the battle
    print(paste("Round ",round.ch1,"is starting..."))
    Sys.sleep(3)
    print(paste(character1, "attacked with", which.attack.ch1, "and dealt ",attack.ch1, " damage to ", character2,"!"))
    Sys.sleep(3)
    print(paste(character2, "attacked with", which.attack.ch2, "and dealt ",attack.ch2, " damage to ", character1,"!"))
    Sys.sleep(3)
    print(paste("At the end of Round ",round.ch1,"..."))
    Sys.sleep(2)
    print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(3)
    print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(3)
    
    # store the new life in a vector for each character
    life.values.ch1[round.ch1] <- character1.life
    life.values.ch2[round.ch2] <- character2.life
    
    # create new data frame of character life to plot
    # Combine the vectors into a dataframe:
    characters.life <- data.frame(round = 1:min(round.ch1, round.ch2), 
                                  character1.life = life.values.ch1[1:min(round.ch1, round.ch2)], 
                                  character2.life = life.values.ch2[1:min(round.ch1, round.ch2)])
    
    # plot the dataframe
    dev.off() #remove the last plot to not overload the plot window
    plot(characters.life$round, characters.life$character1.life,
         type="b",
         ylim=c(0,startinglifepoints),
         xlim=c(1,round.ch1),
         col="red",
         lwd=2,
         xlab="Round",
         ylab="Life points",
         main=paste("Round",round.ch1))
    lines(characters.life$round, characters.life$character2.life,
          type="b",
          col="blue",
          lwd=2)
    legend("bottomleft", legend=c(character1, character2), col=c("red", "blue"), lty=1)
    
    # add 1 to the round counter for the next loop iteration
    round.ch1 <- round.ch1 + 1
    round.ch2 <- round.ch2 + 1
    
    # if each character loses all life, the other wins
    if(character1.life <= 0 && character2.life <= 0){
      print(paste(character1, "and", character2, "are too tired to carry on! The battle ends in a draw!"))
      winner <- "No one"
      break()
    } else if(character1.life <=0){
      print(paste(character1,"is too tired to carry on!",character2, " won the battle!"))
      winner <- character2
      break()
    } else if(character2.life <=0){
      print(paste(character2,"is too tired to carry on!",character1, " won the battle!"))
      winner<- character1
      break()
    }
  }
  # create data frame of the final character life to plot
  # Combine the vectors into a dataframe:
  characters.life <- data.frame(round = 1:min(round.ch1 - 1, round.ch2 - 1), 
                                character1.life = life.values.ch1[1:min(round.ch1 -1, round.ch2 - 1)], 
                                character2.life = life.values.ch2[1:min(round.ch1 -1, round.ch2 - 1)])
  
  # plot the dataframe
  dev.off() #remove the last plot
  plot(characters.life$round, characters.life$character1.life,
       type="b",
       ylim=c(-10,startinglifepoints),
       xlim=c(1,round.ch1),
       col="red",
       lwd=2,
       xlab="Round",
       ylab="Life points",
       main=paste("Battle summary:",winner, "wins!"))
  lines(characters.life$round, characters.life$character2.life,
        type="b",
        col="blue",
        lwd=2)
  legend("topright", legend=c(character1, character2), col=c("red", "blue"), lty=1)
}

character.database
add.character("Voldemort",strength=5, magic=10, humour=1, cruelty=9,special=7)
auto.battle3("Harry Potter","Severus Snape", 50)
auto.battle3("Voldemort", "Dumbledore",250)

# suggestions/ things to change/add ----
## see if you can start the graph at round 0 and the starting life points
## maybe the pauses are too long, could set down to 1 or 2 seconds?
## anything else could add?
## now look at shinyapp to see if can get user interface
## adding characters, single battle, character comparisons, auto battle
## adding images/ graphics?


