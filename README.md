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

Lending Club is a peer-to-peer lending platform. This data can be found on [Kaggle](https://www.kaggle.com/wendykan/lending-club-loan-data) and it contains all issued loans from 2007 to 2015. The data set contains approximately 890,000 observations and 75 features. Some of the features include loan amount, loan type, credit scores and geographic data.

## Prototype/Techniques

Our current prototype consists of four plots:

1. Geographic map
2. Stacked bar chart
3. Histogram of loan amounts
4. Word cloud

##### Geographic map

A choropleth map is used to show which states have the most borrowers. States filled with a darker blue indicates more issued loans in that state. When a user hovers over a state, a tooltip appears which displays the state and the number of loans. Clicking on a state changes the filters for plots (2)-(4). The user can also use a dropdown menu to change the filter.

##### Stacked bar chart
A stacked bar chart is used to visualize the frequency of the grade of the issued loans. There are 7 grades (A-G) with 5 subgrades (1-5). The higher the loan grade, the higher the interest rate will be. A grade of A1 has the lowest interest rate while a grade of G5 has the highest interest rate. 

##### Histogram (Loan amount)
A histogram is used to visualize the distribution of the loan amounts. The user can filter on the specific loan grade by double clicking on a sub-portion of the stacked bar chart in plot 2. The histogram will then be replotted with the loans of that selected grade.

##### Word cloud
Lastly, a word cloud is used to visualize the descriptions/reasons the borrower needs a loan. There are two filters that control this plot: the maximum number of words to display and the minimum occurrence for a word to be shown. As we mentioned before, clicking on a state in the choropleth map will also filter the word cloud. A tooltip with the word and the number of occurrences pops up when a user hovers over the word.

## Interface

The current interface is a single page with a scroll bar. We decided on this implementation because all the charts have some sort of connection between them (see the above section). We included short descriptions to describe the data and to help the users understand the plots and the available interactions.


## Snapshots
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/map-2.png)
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/charts-2.png)
![](https://github.com/usfviz/Rustin-Joger-final/blob/master/screenshots/word_cloud-2.png)
