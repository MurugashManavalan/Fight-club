# Miles Code ----
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
character.database []

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

# Certain Ways that this code can be improved ----
# Adding more characters (Restrict to 4) and comparing them on same attribute
# Making the graph interactive such that the graph only shows who we select
# Making the plots 3-dimensional
# Integrating the shiny interface
# Rejected Code ----
character.database <- as.data.frame(matrix(sample(1:10,5,replace=F),ncol=5))
character.database
#name the columns with characteristics
colnames(character.database) <- c("Strength","Magic","Humour","Cruelty","Special")
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

attack1 <- character.database["Harry Potter", "Magic"]
attack2 <- character.database["Dumbledore", "Magic"]
attack3 <- character.database["Gandalf", "Magic"]
attack4 <- character.database["Bilbo Baggins", "Magic"]
attacks <- c(attack1,attack2,attack3, attack4)
attacks
unique(attacks)
length(uni)
attack2 == max(attacks)

fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) {
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) {
    stop("Please select at least 2 characters for the fight.")
  }
  else if (length(c(character1, character2, character3, character4)) == 4) {
    attack1 <- character.database[character1, attribute]
    attack2 <- character.database[character2, attribute]
    attack3 <- character.database[character3, attribute]
    attack4 <- character.database[character4, attribute]
    attacks <- c(attack1,attack2,attack3, attack4)
    if (attack1 == max(attacks)) {
      print(paste("In a battle of",attribute, "between", character1, character2, character3, "and", character4, character1, "wins!!"))
    }
  }
  else {
    print(paste(character1, "lost!!!"))
  }
}

fight("Harry Potter", "Severus Snape", "Gandalf", "Dumbledore", attribute = "Magic")
2 +3 

fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) {
  if (is.na(character2) || any(is.na(c(character1, character2, character3, character4)))) {
    stop("Please select at least 2 characters for the fight.")
  } else if (!exists("character.database")) {
    stop("Character database not found.")
  } else {
    attack1 <- character.database[character1, attribute]
    attack2 <- character.database[character2, attribute]
    attack3 <- character.database[character3, attribute]
    attack4 <- character.database[character4, attribute]
    
    attacks <- c(attack1, attack2, attack3, attack4)
    
    if (length(unique(attacks)) == 1) {
      print(paste("It's a tie! All characters have the same", attribute))
    } else {
      winner <- c(character1, character2, character3, character4)[which.max(attacks)]
      print(paste("In a battle of", attribute, "between", character1,",", character2, character3, character4, winner, "wins!!"))
    }
  }
}
fight("Harry Potter", "Severus Snape", "Gandalf", "Dumbledore", attribute = "Magic")

# Murugash's Code ----
character.names <- c("max", "min", "Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore")
strength <- c(10,0,sample(1:10,6))
magic <- c(10,0,sample(1:10,6))
humour <- c(10,0,sample(1:10,6))
cruelty <- c(10,0,sample(1:10,6))
special <- c(10,0,sample(1:10,6))
character.database <- data.frame(Character = character.names,
                                 Strength = strength,
                                 Magic = magic,
                                 Humour = humour,
                                 Cruelty = cruelty,
                                 Special = special)
character.database

character.database$Character[which.max(character.database$Strength)]
character.database$Strength
character.database$Character[which(max(character.database$Strength[,-1]))]
colnames(character.database)
character.database$Character[[max(character.database$Strength)]]
character.database$Magic[character.database$Character == "Harry Potter"] # Important

which.max(character.database$Strength)
character.database$Character[which.max(character.database$Strength)]

# Code for Single Attribute (Without Tie Functionality) ---- 
fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
{
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) # Checks whether Character 2 is NA and if the total number of characters is less than 2
  {
    stop("Please select at least 2 characters for the fight.")
  } 
  else if (length(c(character1, character2, character3, character4)) == 4) 
  {
    if (!exists("character.database")) # Checks if there is actually a database called "Character Database"
    {
      stop("character.database is not defined.")
    }
    
    if (attribute == "Strength") 
    {
      attack1 <- character.database$Strength[character.database$Character == character1]
      attack2 <- character.database$Strength[character.database$Character == character2]
      attack3 <- character.database$Strength[character.database$Character == character3]
      attack4 <- character.database$Strength[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winner <- c(character1, character2, character3, character4)[which.max(attacks)]
      print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!"))
    }
  }
}

# Code with 4 attributes (Without Tie Functionality) ---- 
fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
{
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } 
  else if (length(c(character1, character2, character3, character4)) == 4) 
    {
    if (!exists("character.database")) 
      {
      stop("character.database is not defined.")
      }
    
    if (attribute == "Strength") 
      {
      attack1 <- character.database$Strength[character.database$Character == character1]
      attack2 <- character.database$Strength[character.database$Character == character2]
      attack3 <- character.database$Strength[character.database$Character == character3]
      attack4 <- character.database$Strength[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winner <- c(character1, character2, character3, character4)[which.max(attacks)]
      print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!"))
      }
  else if (attribute == "Magic")
      {
    attack1 <- character.database$Magic[character.database$Character == character1]
    attack2 <- character.database$Magic[character.database$Character == character2]
    attack3 <- character.database$Magic[character.database$Character == character3]
    attack4 <- character.database$Magic[character.database$Character == character4]
    attacks <- c(attack1, attack2, attack3, attack4)
    
    winner <- c(character1, character2, character3, character4)[which.max(attacks)]
    print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!")) 
      }
  else if (attribute == "Humour")
      {
    attack1 <- character.database$Humour[character.database$Character == character1]
    attack2 <- character.database$Humour[character.database$Character == character2]
    attack3 <- character.database$Humour[character.database$Character == character3]
    attack4 <- character.database$Humour[character.database$Character == character4]
    attacks <- c(attack1, attack2, attack3, attack4)
    
    winner <- c(character1, character2, character3, character4)[which.max(attacks)]
    print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!")) 
      }
  else if(attribute == "Cruelty")  
      {
    attack1 <- character.database$Cruelty[character.database$Character == character1]
    attack2 <- character.database$Cruelty[character.database$Character == character2]
    attack3 <- character.database$Cruelty[character.database$Character == character3]
    attack4 <- character.database$Cruelty[character.database$Character == character4]
    attacks <- c(attack1, attack2, attack3, attack4)
    
    winner <- c(character1, character2, character3, character4)[which.max(attacks)]
    print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!")) 
  }
  else if (attribute == "Special")
  {
    attack1 <- character.database$Special[character.database$Character == character1]
    attack2 <- character.database$Special[character.database$Character == character2]
    attack3 <- character.database$Special[character.database$Character == character3]
    attack4 <- character.database$Special[character.database$Character == character4]
    attacks <- c(attack1, attack2, attack3, attack4)
    
    winner <- c(character1, character2, character3, character4)[which.max(attacks)]
    print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winner, "wins!!")) 
  }
  } 
}

# Code for Single attribute (With Tie Functionality) ----
attack1 <- character.database$Strength[character.database$Character == "Frodo Boggins"]
attack1
attack2 <- character.database$Strength[character.database$Character == "Bilbo Baggins"]
attack2
attack3 <- character.database$Strength[character.database$Character == "Severus Snape"]
attack3
attack4 <- character.database$Strength[character.database$Character == "Gandalf"]
attack4
attacks <- c(attack1, attack2, attack3, attack4)
attacks
winner <- c("Frodo Boggins", "Bilbo Baggins", "Severus Snape", "Gandalf")[which(attacks == max(attacks))]
winner
which(attacks == max(attacks))

fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
{
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) 
    {
    stop("Please select at least 2 characters for the fight.")
    } 
  else if (length(c(character1, character2, character3, character4)) == 4) 
      {
    if (!exists("character.database")) 
      {
      stop("character.database is not defined.")
      }
    
    if (attribute == "Strength") 
         {
      attack1 <- character.database$Strength[character.database$Character == character1]
      attack2 <- character.database$Strength[character.database$Character == character2]
      attack3 <- character.database$Strength[character.database$Character == character3]
      attack4 <- character.database$Strength[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
        {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
        }
      else if (length(winners) == 2)
      {
        print(paste("There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
        }
      }
}

fight("Frodo Boggins", attribute = "Magic")
fight(character1 = "Frodo Boggins",character2 =  "Bilbo Baggins",character3 =  "Severus Snape",character4 =  "Gandalf", attribute = "Strength")
character.database
character.database$Strength[character.database$Character == "Gandalf"] <- 5

# Code for 4 attributes (With Tie Functionality) ----
fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
{
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } 
  else if (length(c(character1, character2, character3, character4)) == 4) 
  {
    if (!exists("character.database")) 
    {
      stop("character.database is not defined.")
    }
    
    if (attribute == "Strength") 
    {
      attack1 <- character.database$Strength[character.database$Character == character1]
      attack2 <- character.database$Strength[character.database$Character == character2]
      attack3 <- character.database$Strength[character.database$Character == character3]
      attack4 <- character.database$Strength[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else if (length(winners) == 2)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
    }
    else if (attribute == "Magic")
    {
      attack1 <- character.database$Magic[character.database$Character == character1]
      attack2 <- character.database$Magic[character.database$Character == character2]
      attack3 <- character.database$Magic[character.database$Character == character3]
      attack4 <- character.database$Magic[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else if (length(winners) == 2)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
    }
    else if (attribute == "Humour")
    {
      attack1 <- character.database$Humour[character.database$Character == character1]
      attack2 <- character.database$Humour[character.database$Character == character2]
      attack3 <- character.database$Humour[character.database$Character == character3]
      attack4 <- character.database$Humour[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else if (length(winners) == 2)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
    }
    else if(attribute == "Cruelty")  
    {
      attack1 <- character.database$Cruelty[character.database$Character == character1]
      attack2 <- character.database$Cruelty[character.database$Character == character2]
      attack3 <- character.database$Cruelty[character.database$Character == character3]
      attack4 <- character.database$Cruelty[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else if (length(winners) == 2)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
    }
    else if (attribute == "Special")
    {
      attack1 <- character.database$Special[character.database$Character == character1]
      attack2 <- character.database$Special[character.database$Character == character2]
      attack3 <- character.database$Special[character.database$Character == character3]
      attack4 <- character.database$Special[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else if (length(winners) == 2)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1], "and", winners[2]))
      }
      else if (length(winners) == 3)
      {
        print(paste("In a battle of", attribute,"There was a tie between", winners[1],",",winners[2], "and", winners[3]))
      }
    }
  } 
}

fight("Frodo Boggins", attribute = "Magic")
fight(character1 = "Frodo Boggins",character2 =  "Bilbo Baggins",character3 =  "Severus Snape",character4 =  "Gandalf", attribute = "Strength")
character.database

# Code for 4 attributes (With Tie Functionality) (Simplified) ---- 
fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
{
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } 
  else if (length(c(character1, character2, character3, character4)) == 4) 
  {
    if (!exists("character.database")) 
    {
      stop("character.database is not defined.")
    }
    
    if (attribute == "Strength") 
    {
      attack1 <- character.database$Strength[character.database$Character == character1]
      attack2 <- character.database$Strength[character.database$Character == character2]
      attack3 <- character.database$Strength[character.database$Character == character3]
      attack4 <- character.database$Strength[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else {
        print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
      }
    }
    else if (attribute == "Magic")
    {
      attack1 <- character.database$Magic[character.database$Character == character1]
      attack2 <- character.database$Magic[character.database$Character == character2]
      attack3 <- character.database$Magic[character.database$Character == character3]
      attack4 <- character.database$Magic[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else {
        print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
      }
    }
    else if (attribute == "Humour")
    {
      attack1 <- character.database$Humour[character.database$Character == character1]
      attack2 <- character.database$Humour[character.database$Character == character2]
      attack3 <- character.database$Humour[character.database$Character == character3]
      attack4 <- character.database$Humour[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else {
        print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
      }
    }
    else if(attribute == "Cruelty")  
    {
      attack1 <- character.database$Cruelty[character.database$Character == character1]
      attack2 <- character.database$Cruelty[character.database$Character == character2]
      attack3 <- character.database$Cruelty[character.database$Character == character3]
      attack4 <- character.database$Cruelty[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else {
        print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
      }
    }
    else if (attribute == "Special")
    {
      attack1 <- character.database$Special[character.database$Character == character1]
      attack2 <- character.database$Special[character.database$Character == character2]
      attack3 <- character.database$Special[character.database$Character == character3]
      attack4 <- character.database$Special[character.database$Character == character4]
      attacks <- c(attack1, attack2, attack3, attack4)
      
      winners <- c(character1, character2, character3, character4)[which(attacks == max(attacks))]
      if (length(winners) == 1)  
      {
        print(paste("In a battle of", attribute, "between", character1,",", character2,",", character3, "and", character4,";", winners, "wins!!"))
      }
      else {
        print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
      }
    }
  } 
}

fight("Frodo Boggins", attribute = "Magic")
character.database
fight(character1 = "Frodo Boggins",character2 =  "Bilbo Baggins",character3 =  "Severus Snape",character4 =  "Gandalf", attribute = "Humour")

  
# Simplified Code (Using Chatgpt) ----

fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
  {
  if (is.na(character2) || length(c(character1, character2, character3, character4)) < 2) 
    {
    stop("Please select at least 2 characters for the fight.")
    } else if (length(c(character1, character2, character3, character4)) == 4) 
    {
    if (!exists("character.database")) 
      {
      stop("character.database is not defined.")
      }
    
    attacks <- numeric(4) # Creates a empty numeric vector with 4 elements
    characters <- c(character1, character2, character3, character4)
    
    for (i in seq_along(characters)) # Starts a loop where it goes each of the characters, takes their attribute value and creates a separate vector of their attribute value
      {
      attack <- character.database[[attribute]][character.database$Character == characters[i]]
      attacks[i] <- attack
      }
    
    winners <- characters[which(attacks == max(attacks))] # Looks into which character(s) have the max attribute value for a given value
    
    if (length(winners) == 1) 
      {
      print(paste("In a battle of", attribute, "between", paste(characters, collapse = ","), ";", winners, "wins!!"))
      } else 
      {
      print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
    }
  }
}
fight(character1 = "Frodo Boggins",character2 =  "Bilbo Baggins",character3 =  "Severus Snape",character4 =  "Gandalf", attribute = "Strength")
character.database

# Code accommodating for less than 4 players (Such as 3 or 2) ----
fight <- function(character1, character2 = NA, character3 = NA, character4 = NA, attribute) 
  {
  characters <- c(character1, character2, character3, character4)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) 
    {
    stop("Please select atleast 2 characters for the fight.")
    } else if (length(characters) >= 2) 
      {
    if (!exists("character.database")) 
      {
      stop("character.database is not defined.")
      }
    
    attacks <- numeric(length(characters))
    
    for (i in seq_along(characters)) 
      {
      attack <- character.database[[attribute]][character.database$Character == characters[i]]
      attacks[i] <- attack
      }
    
    winners <- characters[which(attacks == max(attacks))]
    
    if (length(winners) == 1) 
      {
      print(paste("In a battle of", attribute, "between", paste(characters, collapse = ","), ";", winners, "wins!!"))
      } else 
        {
      print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
    }
  }
}

fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", attribute = "Strength")

# Code accommodating for more than 4 characters such as 5 or even 6 ----

fight <- function(attribute, ...) {
  characters <- c(...)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } else if (length(characters) >= 2) {
    if (!exists("character.database")) {
      stop("character.database is not defined.")
    }
    
    attacks <- numeric(length(characters))
    
    for (i in seq_along(characters)) {
      attack <- character.database[[attribute]][character.database$Character == characters[i]]
      attacks[i] <- attack
    }
    
    winners <- characters[which(attacks == max(attacks))]
    
    if (length(winners) == 1) {
      print(paste("In a battle of", attribute, "between", paste(characters, collapse = ","), ";", winners, "wins!!"))
    } else {
      print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
    }
  }
}

fight("Attribute", "Character1", "Character2", "Character3", "Character4", "Character5")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf", character6 = "Frodo Boggins",attribute = "Strength")
character.database
