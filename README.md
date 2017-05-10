## Authors
Justin Midiri (jamidiri@dons.usfca.edu)  
Roger Wu (rcwu@dons.usfca.edu)

## Requirements

* `shiny`
* `shinythemes`
* `tm`
* `plyr`
* `dplyr`
* `RColorBrewer`
* `wordcloud2`
* `scales`
* `plotly`
* `ggplot2`
* `lubridate`
* `ggvis`

To run the app, use this line of code: `shiny::runGitHub('Rustin-Joger-final', 'usfviz')`. The dataset is somewhat large (~50MB) and may take a bit of time to load.

## Lending Club Loan Data

Lending Club is a peer-to-peer lending platform. This data can be found on [Kaggle](https://www.kaggle.com/wendykan/lending-club-loan-data) and it contains all issued loans from 2007 to 2015. The data set contains approximately 890,000 observations and 75 features. Some of the features include loan amount, loan type, credit scores and geographic data. We supplemented the Lending Club data with population data from the 2010 census.

## App

Our final app consists of three tabs:

1. Choropleth
2. Linked barchart/histograms
3. Word cloud

##### Choropleth

A choropleth map is used to show which states have the most issued loans (per 1000 persons). States/counties filled with a darker green indicate more issued loans in that state. When a user hovers over a state, a tooltip appears which displays the region and relevant figures. Clicking on a state changes the state-county map to the selected state. Alternatively, the user can use the dropdown menu to select a state.

##### Linked barchart/histograms
A bar chart is used to visualize the frequency of the grade of the issued loans. There are 7 grades (A-G) with 5 subgrades (1-5). The higher the loan grade, the higher the interest rate will be. A grade of A1 has the lowest interest rate while a grade of G5 has the highest interest rate. 

Two histograms are used to visualize the distribution of the loan amounts and the interest rate. The user can filter on the specific loan grade by double clicking on a sub-portion of the bar chart. The histograms will then be replotted with the observations from that selected grade.

##### Word cloud
Lastly, a word cloud is used to visualize the descriptions/reasons the borrower needs a loan. There are three filters that control this plot: the maximum number of words to display, the minimum occurrence for a word to be shown, and the state of the borrower. A tooltip with the word and the number of occurrences pops up when a user hovers over a word.

## Interface

The current interface consists of three tabs. Each tab includes interactions (clicking on charts, tooltips, and filters). We included short descriptions to describe the data and to help the users understand the plots and the available interactions.


## Snapshots
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/map-2.png)
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/charts-2.png)
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/word_cloud-2.png)
