# Flight Club - Shiny App

The tool was created by Miles Joseph Raishbrook and Murugash Manavalan

This tool is a game which helps you compare your favorite fictional characters and to see who would actually win a fight. There is no limit to the number of characters which can be added. 

Viewers may choose to either pit their characters in a 1 v 1 battle where the tool simulates a battle between two characters based on their different stats or a battle-royale where are a roster of characters are pitted against each other on a single stat to see who would come out on top.

# Installation

First and foremost if you havent yet, download [R and R Studio](https://posit.co/download/rstudio-desktop/).

Next, simply download this R script, [Fight Club](https://github.com/Murugash-12/Fight-club/blob/main/R%20for%20Life%20-%20Shiny%20Build%20-%201%20February%202024%20-%20Final.R) and run the app in R studio. You may receive a popup in R studio asking you to update loaded packages, you may simply ignore it.

# App Interface

## Home

Once you run the app, you will be greeted with the home page giving you a simple overview of the entire app.

If you wish to use certain popular fictional characters which are already loaded, you may click the Load Characters button to add them to the roster.

## Add Character

In this page, you may create your own character and assign your preferred stat scores. A corresponding radar plot can be viewed in the right.

Once done, click the 'Ready to Fight!' button to add them to the list of characters.

## Compare

This page simply lets you view the radar plots of any two characters which you create

## 1 v 1 Battle

Choose any two characters (each in different corners) and specify the life points which each would have in the beginning and also the battle speed. These two parameters will chiefly determine the duration of the battle simulation.

After doing this, click on the 'simulate fight' button, this will essentially simulate the battle and load it in after which you may click the 'start fight' button to view the results.

In the top right, you may see the Battle Preview which gives you the radar plot of the two characters.

In the mid right, you will see the Battle Commentary which gives you a commentary of the entire fight. Each round involves either characters attacking the other using one of their specified stats. The damage dealt by the character is dependent on the value of the stat assigned to them. The battle will continue until one of the characters reaches 0 life points at which point the winner is declared.

Below the commentary you will find the plot which specifies the deterioration of lifepoints of both characters over the course of the battle.

![Screenshot 2024-06-30 at 4 57 10 PM](https://github.com/Murugash-12/Fight-club/assets/174246983/fff7f301-51b9-4270-854a-c91d19ad12e4)

## Battle Royale

Choose the characters which you wish to compare in the right side bar, select the attack attibute based on which they will be compared on and click start 'battle royale'.

The tool will compare the stat score of the selected attack attribute of the characters and gives a result of who would win in a fight based on that particular attribute.

Radar plots of the chosen characters are also produced.

![Screenshot 2024-06-30 at 5 14 48 PM](https://github.com/Murugash-12/Fight-club/assets/174246983/8a9915f1-87f5-41cc-a9a8-2757955cc3ef)





