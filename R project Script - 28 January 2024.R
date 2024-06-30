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

# Miles Code - Fighting Simulation ----
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

# Suggestions in combining the code ----
# There seems to be an inherent difference in the way my character dataframe is created and how Mile's dataframe is created which means the way we index it in the function is slightly altered
# Hence the resulting function needs to be slightly altered from the parent functions to work coherently 
# What the final code will do:
## If the number of characters is equal to 2, it goes into Mile's battle simulation 
## If the number of characters is more than 2, it goes into my Battle royale
# Things that need to be changed to create the final function:
## Need to change Mile's code such than it can work with my dataframe 
## Need to add radar plots in my own battle royale

character.database
?radarchart
new 
radarchart(character.database)
str(character.database)
character.database[, -1] <- sapply(character.database[, -1], as.numeric)

# Example radar chart ----
character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)
sample_radarchart <- character.database[character.database$Character == c("max","min","Frodo Boggins","Harry Potter"), -1]
plot.colours <- c("red","blue")
radarchart(sample_radarchart,
           seg=5,
           pcol=plot.colours,
           plty=1,
           plwd=3,
           cglcol="black",
           cglty=1,
           cglwd=1.5,
           vlcex=1,
           title=paste("Frodo Boggins"," v. ", "Harry Potter"))
legend(x = "topright",
       legend = c("Fordo Boggins", "Harry Potter"),
       bty = "n",
       col = plot.colours,
       pch=20,
       text.col = "black",
       cex=1,
       pt.cex=1)

# Example Simulation ----

character.database[character.database$Character == "Harry Potter", sample(1:ncol(character.database),size=1)]
sample(1:ncol(character.database),size=1)
character.database[-1]

# Miles Code (Altered to work with my Data frame) ----
character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

auto.battle4 <- function(..., startinglifepoints = 100) {
  characters <- c(...)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } else if (length(characters) >= 2) {
    if (!exists("character.database")) {
      stop("character.database is not defined.")
    }
  }
  print(paste(paste(characters, collapse = " and "), "are going into battle!"))
  Sys.sleep(1)
  print("It's a fight to the death!")
  Sys.sleep(1)
  print(paste(paste(characters, collapse = " and "), "will use all of their talents in the battle!"))
  
  # Defining characters
  character1 <- characters[1]
  character2 <- characters[2]
  character_data <- character.database[character.database$Character %in% c("max","min", character1, character2), -1 ]
  plot.colours <- c("red","blue")
  
  #Creating radar chart
  radarchart(character_data,
             seg=5,
             pcol=plot.colours,
             plty=1,
             plwd=3,
             cglcol="black",
             cglty=1,
             cglwd=1.5,
             vlcex=1,
             title=paste(character1," v. ", character2))
  legend(x = "topright",
         legend = c(character1, character2),
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
  
  Sys.sleep(1)
  
  ## use repeat loop to make attacks on each character
  repeat{
    
    # define a random attack(column) from the attributes of selected characters
    random.attack.ch1 <- sample(2:ncol(character.database),size=1)
    random.attack.ch2 <- sample(2:ncol(character.database),size=1)
    attack.ch1 <- as.numeric(character.database[character.database$Character == character1, random.attack.ch1])
    attack.ch2 <- as.numeric(character.database[character.database$Character == character2, random.attack.ch2])
    
    
    ## extract the column names also for stating which attack was used
    attributes <- colnames(character.database)
    which.attack.ch1 <- attributes[random.attack.ch1]
    which.attack.ch2 <- attributes[random.attack.ch2]
    
    # each character makes an attack
    character1.life <- character1.life - attack.ch2
    character2.life <- character2.life - attack.ch1
    
    # add text to narrate the battle
    print(paste("Round ",round.ch1,"is starting..."))
    Sys.sleep(1)
    print(paste(character1, "attacked with", which.attack.ch1, "and dealt ",attack.ch1, " damage to ", character2,"!"))
    Sys.sleep(1)
    print(paste(character2, "attacked with", which.attack.ch2, "and dealt ",attack.ch2, " damage to ", character1,"!"))
    Sys.sleep(1)
    print(paste("At the end of Round ",round.ch1,"..."))
    Sys.sleep(1)
    print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(1)
    print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
    Sys.sleep(1)
    
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
auto.battle4("Harry Potter", "Bilbo Baggins")

# Miles Code (Without Delay) ----
character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

auto.battle4 <- function(..., startinglifepoints = 100) {
  characters <- c(...)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } else if (length(characters) >= 2) {
    if (!exists("character.database")) {
      stop("character.database is not defined.")
    }
  }
  print(paste(paste(characters, collapse = " and "), "are going into battle!"))
  print("It's a fight to the death!")
  print(paste(paste(characters, collapse = " and "), "will use all of their talents in the battle!"))
  
  # Defining characters
  character1 <- characters[1]
  character2 <- characters[2]
  character_data <- character.database[character.database$Character %in% c("max","min", character1, character2), -1 ]
  plot.colours <- c("red","blue")
  
  #Creating radar chart
  radarchart(character_data,
             seg=5,
             pcol=plot.colours,
             plty=1,
             plwd=3,
             cglcol="black",
             cglty=1,
             cglwd=1.5,
             vlcex=1,
             title=paste(character1," v. ", character2))
  legend(x = "topright",
         legend = c(character1, character2),
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
    random.attack.ch1 <- sample(2:ncol(character.database),size=1)
    random.attack.ch2 <- sample(2:ncol(character.database),size=1)
    attack.ch1 <- as.numeric(character.database[character.database$Character == character1, random.attack.ch1])
    attack.ch2 <- as.numeric(character.database[character.database$Character == character2, random.attack.ch2])
    
    
    ## extract the column names also for stating which attack was used
    attributes <- colnames(character.database)
    which.attack.ch1 <- attributes[random.attack.ch1]
    which.attack.ch2 <- attributes[random.attack.ch2]
    
    # each character makes an attack
    character1.life <- character1.life - attack.ch2
    character2.life <- character2.life - attack.ch1
    
    # add text to narrate the battle
    print(paste("Round ",round.ch1,"is starting..."))
    
    print(paste(character1, "attacked with", which.attack.ch1, "and dealt ",attack.ch1, " damage to ", character2,"!"))
    
    print(paste(character2, "attacked with", which.attack.ch2, "and dealt ",attack.ch2, " damage to ", character1,"!"))
    
    print(paste("At the end of Round ",round.ch1,"..."))
    
    print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
    
    print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
    
    
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
auto.battle4("Harry Potter", "Bilbo Baggins")



# Murugash Code (With Radar chart) ----

character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

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
  n <- length(characters)
  rows <- ceiling(n / 3)
  layout(matrix(1:n, ncol = 3, byrow = TRUE))
  
  for (i in 1:n) {
    char <- characters[i]
    # Defining character vector for each character
    character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
    plot.colours <- c("red", "blue", "gold", "green", "salmon", "purple")
    char_color <- plot.colours[i]
    
    # Calculate the bottom margin dynamically based on the number of charts per row
    bottom_margin <- ifelse(n %% 3 == 0, 5, 8)
    
    # Creating radar chart for each character with a unique color
    par(mar = c(bottom_margin, 0, 4, 0))  # Increased the right margin to 3
    radarchart(character_data,
               seg = 5,
               pcol = char_color,
               plty = 1,
               plwd = 3,
               cglcol = "black",
               cglty = 1,
               cglwd = 1.5,
               vlcex = 1,
               title = char)

  }
}
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",attribute = "Strength")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore",attribute = "Magic")
colors()


# Murugash Code (Improved Radar Chart) ----
library(colormap)
character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

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
  n <- length(characters)
  layout(matrix(1:n, ncol = 3, byrow = TRUE))
  
  for (i in 1:n) {
    char <- characters[i]
    # Defining character vector for each character
    character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
    colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
    colors_in =colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)
    char_color <- colors_border[i]
    char_fill <- colors_in[i]
    
    # Calculate the bottom margin dynamically based on the number of charts per row
    bottom_margin <- ifelse(n %% 3 == 0, 5, 8)
    
    # Creating radar chart for each character with a unique color
    par(mar = c(bottom_margin, 0, 4, 0))  # Increased the right margin to 3
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
}
character.database
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",character6 = "Frodo Boggins",attribute = "Strength")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore",attribute = "Magic")
# If I input number of characters between 3 and 6 such as 4 and 5, it freaks out and puts everything in random

# Murugash Code (Better panel arrangement) ----

character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

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
    # Defining character vector for each character
    character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
    colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
    colors_in =colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)
    char_color <- colors_border[i]
    char_fill <- colors_in[i]

    # Creating radar chart for each character with a unique color
    par(mar = c(0, 0, 4, 0))  # Increased the right margin to 3
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
}
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore",attribute = "Magic")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape",attribute = "Strength")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",attribute = "Strength")
fight(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",character6 = "Frodo Boggins",attribute = "Strength")
character.database

# Combined Code ----
install.packages("fmsb")
library(fmsb)
install.packages("colormap")
library(colormap)

character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

fight_club <- function (..., startinglifepoints = 100, attribute = NULL) {
  characters <- c(...)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } else if (!exists("character.database")) {
      stop("character.database is not defined.")
  } else if (length(characters) == 2) {
    print(paste(paste(characters, collapse = " and "), "are going into battle!"))
    Sys.sleep(1)
    print("It's a fight to the death!")
    Sys.sleep(1)
    print(paste(paste(characters, collapse = " and "), "will use all of their talents in the battle!"))
    
    # Defining characters
    character1 <- characters[1]
    character2 <- characters[2]
    character_data <- character.database[character.database$Character %in% c("max","min", character1, character2), -1 ]
    plot.colours <- c("red","blue")
    
    #Creating radar chart
    radarchart(character_data,
               seg=5,
               pcol=plot.colours,
               plty=1,
               plwd=3,
               cglcol="black",
               cglty=1,
               cglwd=1.5,
               vlcex=1,
               title=paste(character1," v. ", character2))
    legend(x = "topright",
           legend = c(character1, character2),
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
    
    Sys.sleep(1)
    
    ## use repeat loop to make attacks on each character
    repeat{
      
      # define a random attack(column) from the attributes of selected characters
      random.attack.ch1 <- sample(2:ncol(character.database),size=1)
      random.attack.ch2 <- sample(2:ncol(character.database),size=1)
      attack.ch1 <- as.numeric(character.database[character.database$Character == character1, random.attack.ch1])
      attack.ch2 <- as.numeric(character.database[character.database$Character == character2, random.attack.ch2])
      
      
      ## extract the column names also for stating which attack was used
      attributes <- colnames(character.database)
      which.attack.ch1 <- attributes[random.attack.ch1]
      which.attack.ch2 <- attributes[random.attack.ch2]
      
      # each character makes an attack
      character1.life <- character1.life - attack.ch2
      character2.life <- character2.life - attack.ch1
      
      # add text to narrate the battle
      print(paste("Round ",round.ch1,"is starting..."))
      Sys.sleep(1)
      print(paste(character1, "attacked with", which.attack.ch1, "and dealt ",attack.ch1, " damage to ", character2,"!"))
      Sys.sleep(1)
      print(paste(character2, "attacked with", which.attack.ch2, "and dealt ",attack.ch2, " damage to ", character1,"!"))
      Sys.sleep(1)
      print(paste("At the end of Round ",round.ch1,"..."))
      Sys.sleep(1)
      print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
      Sys.sleep(1)
      print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
      Sys.sleep(1)
      
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
  } else if (length(characters) > 2) {
    characters <- c(...)
    characters <- characters[!is.na(characters)]
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
      # Defining character vector for each character
      character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
      colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
      colors_in =colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)
      char_color <- colors_border[i]
      char_fill <- colors_in[i]
      
      # Creating radar chart for each character with a unique color
      par(mar = c(0, 0, 4, 0))  # Increased the right margin to 3
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
}
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore",attribute = "Magic")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",character6 = "Frodo Boggins",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins")



# Combine Code (Better Chart for Simulation) ----
install.packages("fmsb")
library(fmsb)
install.packages("colormap")
library(colormap)

character.database <- data.frame(
  Character = c("max","min","Frodo Boggins", "Harry Potter", "Severus Snape", "Gandalf", "Bilbo Baggins", "Dumbledore"),
  Strength = c(10,0,9, 7, 6, 4, 5, 8),
  Magic = c(10,0,10, 1, 10, 3, 6, 2),
  Humour = c(10,0,6, 1, 5, 8, 10, 9),
  Cruelty = c(10,0,9, 7, 10, 3, 6, 2),
  Special = c(10,0,10, 1, 3, 8, 2, 6)
)

fight_club <- function (..., startinglifepoints = 100, attribute = NULL) {
  characters <- c(...)
  characters <- characters[!is.na(characters)]
  
  if (length(characters) < 2) {
    stop("Please select at least 2 characters for the fight.")
  } else if (!exists("character.database")) {
    stop("character.database is not defined.")
  } else if (length(characters) == 2) {
    print(paste(paste(characters, collapse = " and "), "are going into battle!"))
    Sys.sleep(1)
    print("It's a fight to the death!")
    Sys.sleep(1)
    print(paste(paste(characters, collapse = " and "), "will use all of their talents in the battle!"))
    
    # Defining characters
    character1 <- characters[1]
    character2 <- characters[2]
    character_data <- character.database[character.database$Character %in% c("max","min", character1, character2), -1 ]
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9)  ) 
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )
    
    # Creating radar chart
    radarchart( character_data, axistype=1, 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4, plty=1 , 
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=1.1,
                #custom labels
                vlcex=0.8,  title=paste(character1," v. ", character2) 
    )
    
    legend(x = "topright",
           legend = c(character1, character2),
           bty = "n",
           col = colors_border,
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
    
    Sys.sleep(1)
    
    ## use repeat loop to make attacks on each character
    repeat{
      
      # define a random attack(column) from the attributes of selected characters
      random.attack.ch1 <- sample(2:ncol(character.database),size=1)
      random.attack.ch2 <- sample(2:ncol(character.database),size=1)
      attack.ch1 <- as.numeric(character.database[character.database$Character == character1, random.attack.ch1])
      attack.ch2 <- as.numeric(character.database[character.database$Character == character2, random.attack.ch2])
      
      
      ## extract the column names also for stating which attack was used
      attributes <- colnames(character.database)
      which.attack.ch1 <- attributes[random.attack.ch1]
      which.attack.ch2 <- attributes[random.attack.ch2]
      
      # each character makes an attack
      character1.life <- character1.life - attack.ch2
      character2.life <- character2.life - attack.ch1
      
      # add text to narrate the battle
      print(paste("Round ",round.ch1,"is starting..."))
      Sys.sleep(1)
      print(paste(character1, "attacked with", which.attack.ch1, "and dealt ",attack.ch1, " damage to ", character2,"!"))
      Sys.sleep(1)
      print(paste(character2, "attacked with", which.attack.ch2, "and dealt ",attack.ch2, " damage to ", character1,"!"))
      Sys.sleep(1)
      print(paste("At the end of Round ",round.ch1,"..."))
      Sys.sleep(1)
      print(paste(character1," has ", character1.life, "/", startinglifepoints, "life points remaining."))
      Sys.sleep(1)
      print(paste(character2," has ", character2.life, "/", startinglifepoints, "life points remaining."))
      Sys.sleep(1)
      
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
  } else if (length(characters) > 2) 
    {
    characters <- c(...) # Takes all the input characters and creates a vector for them
    characters <- characters[!is.na(characters)] # A QC step which just verifies whether any of the characters are not available 
    attacks <- numeric(length(characters)) # Creates an empty vector with the number of the elements determined by the number of characters  
    
    for (i in seq_along(characters)) {
      attack <- character.database[[attribute]][character.database$Character == characters[i]] #Goes through and extracts the specific attribute value of the specified characters 
      attacks[i] <- attack # Allocates the extracted value into the previously created attacks vector
    }
    
    winners <- characters[which(attacks == max(attacks))] #Determines the winner based on who has the maximum attribute value
    
    if (length(winners) == 1) {
      print(paste("In a battle of", attribute, "between", paste(characters, collapse = ","), ";", winners, "wins!!"))
    } else {
      print(paste("In a battle of", attribute, "There was a tie between", paste(winners, collapse = " and ")))
    } 
  }
  n <- length(characters)
  if (n %% 3 == 0) { #If the number of characters is divisible by 3, it creates a layout with the number of coloums as 3
    layout(matrix(1:n, ncol = 3, byrow = TRUE))
  } else if (n == 4) {
    layout(matrix(1:n, ncol = 2, byrow = TRUE))
  } else if (n == 5) {
    layout(matrix(1:n, ncol = 5, byrow = TRUE))  
  } else if (n %/% 4 > 1 && n %% 4 == 0) { #If the number of characters is divisible by 4 and the quotient is more than 1, it creates a layout with the number of columns as 4
    layout(matrix(1:n, ncol = 4, byrow = TRUE))
  } else if (n %/% 5 > 1 && n %% 5 == 0) { #If If the number of characters is divisible by 5 and the quotient is more than 1, it creates a layout with the number of columns as 5
    layout(matrix(1:n, ncol = 5, byrow = TRUE))
  }
  
  for (i in 1:n) {
    char <- characters[i]
    # Defining character vector for each character
    character_data <- character.database[character.database$Character %in% c("max", "min", char), -1]
    colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1) # Creates a color map using the virdis colour paletter for the outer border, alpha basically indicates the transparancy with lower number indicating more transparancy 
    colors_in =colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3) # Creates a separate color map for the inner side
    char_color <- colors_border[i] 
    char_fill <- colors_in[i] 
    
    # Creating radar chart for each character with a unique color
    par(mar = c(0, 0, 4, 0))  # Increased the right margin to 3
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
}
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore",attribute = "Magic")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins", character3= "Dumbledore", character4= "Severus Snape", character5= "Gandalf",character6 = "Frodo Boggins",attribute = "Strength")
fight_club(character1 = "Harry Potter",character2 =  "Bilbo Baggins")
fight_club("Harry Potter","Bilbo Baggins","Dumbledore",attribute = "Magic") # Doesnt need character names to be specified.
# Add dev.off
?colormap
