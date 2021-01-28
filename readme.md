# Accidental Drug Related Deaths From 2012 Through 2018

## Goal
These charts were created to answer the following questions:  
1. What percentage of males died
2. How many deaths involved opioids
3. Which country had the highest deaths 
4. Which age group has the lowest deaths 
5. Which location had the second most deaths 
6. How many deaths occurred in a hospital  


## Installation
1. Need to install R. Click on the link below and it will take you to the CRAN Mirrors page. Choose the mirror closest to you and then download it for the operating system you have.   
   - LINK: https://cran.r-project.org/mirrors.html 
2. Install R studio   
   - LINK: https://rstudio.com/products/rstudio/download/
3. Once R studio is downloaded open it up. There are two ways to install r shiny on your computer. 
   - One method is type in the following into the r studio console   
	```
		install.packages("shiny")
	```
    - Another is go to the lower right hand corner and click on the **Packages** tab and then click install. Type in the name **shiny** in the input box that appears. 
4. Install the libraries that are used in the code. If you choose to you can install these packages by going to Packages and clicking install and typing in each packages name. You can also install these packages by typing these lines into the r studio console  
```
install.packages("ggplot2")  
install.packages("dplyr")  
install.packages("plotly")  
```