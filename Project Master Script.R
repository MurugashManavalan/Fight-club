# Radar Plot for Movie Characters

# Installing Packages
install.packages("tidyverse")
install.packages("viridis")
install.packages("patchwork")
install.packages("hrbrthemes")
install.packages("fmsb")
install.packages("colormap")

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(fmsb)
library(colormap)

## Imported Code

# Create data
set.seed(1)
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Custom the radarChart !
par(mar=c(0,0,0,0))
radarchart( data, axistype=1, 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

## ChatGPT Data for a plot for a single character
rm(your_values)
# Assign your own values to the subjects
your_values <- matrix(c(
  # Your values for the first row (maximum values)
  20, 18, 16, 14, 12, 10, 8, 6, 4, 2), ncol = 10, byrow = TRUE)

# Convert the matrix to a data frame
data <- as.data.frame(your_values)

# Assign column names
colnames(data) <- c("math", "english", "biology", "music", "R-coding", "data-viz", "french", "physic", "statistic", "sport")

# Add Max and Min Values to Data
data <- rbind(rep(20, 10), rep(0, 10), data)

# Customize Radar Chart
par(mar = c(0, 0, 0, 0))
radarchart(data, axistype = 1,
           pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 20, 5), cglwd = 0.8,
           vlcex = 0.8)

## My own data for a plot for a single character 
## Range is set to maximum of 20 and 10 attributes have been assigned
your_values <- matrix(c(
  # Your values for the first row (maximum values)
  10, 9, 9, 8, 7, 6, 6, 8, 6, 7), ncol = 10, byrow = TRUE)

# Convert the matrix to a data frame
data <- as.data.frame(your_values)

# Assign column names
colnames(data) <- c("Physical Strength", "Intellectual Prowess", "Emotional Depth", "Charisma", "Moral Code", "Resourcefulness", "Skill Mastery", "Leadership", "Backstory", "Vulnerability")

# Add Max and Min Values to Data
data <- rbind(rep(20, 10), rep(0, 10), data)

# Customize Radar Chart
par(mar = c(0, 0, 0, 0))
radarchart(data, axistype = 1,
           pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 20, 5), cglwd = 0.8,
           vlcex = 0.8)

## My own data for a plot for a single character 
## Range is set to maximum of 10 and 5 attributes have been assigned
# Assign your own values to the subjects
your_values <- matrix(c(
  10, 9, 8, 7, 6
), ncol = 5, byrow = TRUE)

# Convert the matrix to a data frame
data <- as.data.frame(your_values)
data
# Assign column names
colnames(data) <- c("Strength", "Speed", "Durability", "Skills/IQ", "Special Moves")  # Adjust the column names

# Add Max and Min Values to Data
data <- rbind(rep(10, 5), rep(0, 5), data)  # Set rep(10, 5) for 5 subjects

# Customize Radar Chart
par(mar = c(0, 0, 0, 0))
radarchart(data, axistype = 1,
           pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 10, 2), cglwd = 0.8,
           vlcex = 0.8)
data