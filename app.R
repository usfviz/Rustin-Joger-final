library(shiny)
library(shinythemes)
library(tm)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(scales)
library(plotly)
library(ggplot2)
library(lubridate)
library(ggvis)

# Data
loan.data <- read.csv("data_subset.csv", stringsAsFactors = FALSE) %>%
  mutate(int_rate = int_rate / 100)

#######################
### MAP PREPARATION ###
#######################
all_states <- read.csv('map_state_data.csv', stringsAsFactors = F) %>%
  mutate(loan_per_pop = cnt / pop,
         col = ifelse(is.na(loan_per_pop), "white",
               ifelse(loan_per_pop < 1.25, "#edf8e9",
               ifelse(loan_per_pop < 2.50, "#bae4b3",
               ifelse(loan_per_pop < 3.75, "#74c476", "#238b45")))))
counties <- read.csv('map_county_data.csv', stringsAsFactors = F) %>%
  mutate(loan_per_pop = cnt / pop,
         col = ifelse(is.na(loan_per_pop), "white",
               ifelse(loan_per_pop < 1.25, "#edf8e9",
               ifelse(loan_per_pop < 2.50, "#bae4b3",
               ifelse(loan_per_pop < 3.75, "#74c476", "#238b45")))))
states_list <- sort(unique(all_states[!is.na(all_states$cnt), 'Region']))


# Helper function to update filters based on clicks
update_selection <- function(data, location, session){
  if(is.null(data$loan_per_pop)) {
    updateSelectInput(session, "state", selected = 'All')
  } else {
    updateSelectInput(session, "state", selected = data$Region)
  }
}

# Helper functions to add tooltips to the map
map_tooltip <- function(dat) {
  cnt_str <- format(dat$cnt, big.mark = ',', scientific = F)
  pop_str <- format(1000 * dat$pop, big.mark = ',', scientific = F)
  val_str <- round(dat$loan_per_pop, 4)
  str <- paste0('State: ', dat$Region, '<br>Loans: ', cnt_str,
                '<br>Population: ', pop_str, '<br>Loans (Per 1,000): ', val_str)
}
map_tooltip_county <- function(dat) {
  if (!is.null(dat$Subregion) && !is.null(dat$loan_per_pop)) {
    cnt_str <- format(dat$cnt, big.mark = ',', scientific = F)
    pop_str <- format(1000 * dat$pop, big.mark = ',', scientific = F)
    val_str <- format(round(dat$loan_per_pop, 4), big.mark = ',', scientific = F)
    str <- paste0('County: ', dat$Subregion, '<br>Loans: ', cnt_str,
                  '<br>Population: ', pop_str, '<br>Loans (Per 1,000): ', val_str)
  } else {
    str <- paste0('County: ', dat$Subregion, 
                  '<br>Loans: NULL<br>Population: NULL<br>Loans (Per 1,000): NULL')
  }
}

# Begin Shiny App
ui <- fluidPage(theme = shinytheme("yeti"),
                img(src = 'lending-club-logo.png'),
                p("This data set comes to us from Kaggle's Lending Club loan data. The data consists of all loans issued from 2007-2015 with more than 800,000 rows and 75 features of which a random subset was taken for the purpose of this project."),
                tabsetPanel(
                  tabPanel(title = "Choropleth",
                           fluidRow(column(12,
                                           h3("Loans (Per 1000 Persons) By State & County"),
                                           p("Below, you can investigate the number of loans (per 1,000 persons) issued across the United States. Feel free to select a state from the dropdown
                                             menu or just simply click on the state of interest to begin exploring."))),
                           fluidRow(column(4,
                                           selectInput("state", label = "Select a State:", choices = states_list, selected = 'California'))),
                           fluidRow(column(8,
                                           ggvisOutput("map")),
                                    column(4, 
                                           ggvisOutput("state_map")))),
                  tabPanel(title = "Loan Characteristics",
                           fluidRow(column(12,
                                           h3("Loan Characteristics"),
                                           p("Based upon the selected state, one can see the distributions of both the loan grades, loan amounts, and interest rates. 
                                             Click on a loan grade to see the distribution of its loan amount and interest rate (below). 
                                             Double-click the white-space within the loan grade plot to remove the loan grade filter."),
                                           p("Quick review of loan grades: loan grades are determined based upon an applicant's loan application and credit score. The grades range from A-G, with A being the best and G being the worst. 
                                             Lastly, within each grade there are 5 levels, or sub grades, ranging between 1-5 where 1 is the best and 5 is the worst. 
                                             After an individual completes a loan application, their loan grade is assigned along with the associated interest rate."))),
                           fluidRow(column(4,
                                           selectInput("state.grade", label = "Select a State:", choices = c('All', states_list), selected = 'All'))),
                           fluidRow(column(10,
                                           br(),
                                           plotlyOutput("subgrade.hist", height = 250),
                                           br())),
                           fluidRow(column(5,
                                           br(),
                                           plotOutput("loan.amnt.hist"),
                                           br()),
                                    column(5,
                                           br(),
                                           plotOutput("loan.int.hist"),
                                           br()))),
                  tabPanel(title = "Reasons For A Loan",
                           fluidRow(column(12,
                                           h3("Why The Loan?"),
                                           p("Here we come to the word cloud, a tool used to display the most frequent words in a document. We generated the wordcloud to determine the most common reasons why an individual requested a loan. 
                                             Based upon the state of interest, one can gather why those individuals needed the loan. Feel free to adjust the maximum number of words to display as well as placing a resriction on the minimum number of occurrences 
                                             for each word. Having trouble seeing a word? No problem! Just scroll over it and both the word and its frequency (count) will be displayed in a tooltip."))),
                           fluidRow(column(4,
                                           selectInput("state.word", label = "Select a State:", choices = c('All', states_list), selected = 'All'))),
                           fluidRow(br(),
                                    sidebarPanel(sliderInput("word.freq", label = "Maximum Words To Display:", min = 2, max = 200, value = 100),
                                                 sliderInput("word.occ", label = "Minimum Occurrence:", min = 1, max = 50, value = 25)),
                                    mainPanel(wordcloud2Output("wordcloud")),
                                    br()))
                ))

server <- function(input, output) {
  # Dataframe used for the wordcloud
  loan.df.wordcloud <- reactive({
    if (input$state.word != 'All') {
      loan.data %>% 
        filter(addr_state == state.abb[match(input$state.word, state.name)])
    } else {
      loan.data
    }
  })
  # Dataframe used for the histograms of the grades, sub-grades, and loan amounts
  loan.df.grade <- reactive({
    if (input$state.grade != 'All') {
      loan.data %>% 
        filter(addr_state == state.abb[match(input$state.word, state.name)])
    } else {
      loan.data
    }
  })
  sub.grade.vector <- reactive({sort(unique(loan.df.grade()$sub_grade))})
  ###########
  ### MAP ###
  ###########
  # County map
  plt <- reactive({
    counties %>%
      filter(Region == input$state) %>%
      group_by(group, Subregion, cnt, pop, loan_per_pop) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(fill := ~col) %>%
      scale_numeric(property = 'fill', domain = c(0, 4.25), clamp = T,
                    range = c("#edf8fb", "#238b45")) %>%
      hide_axis("x") %>% 
      hide_axis("y") %>%
      ggvis::hide_legend('fill') %>%
      set_options(height = 300, width = 300) %>%
      add_tooltip(map_tooltip_county, on = c("hover", "click"))
    })
  plt %>% bind_shiny("state_map")
  
  # US map
  all_states %>%
    group_by(group, Region, cnt, pop, loan_per_pop) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill := ~col) %>%
    scale_numeric(property = 'fill', domain = c(0, 4.25), clamp = T,
                  range = c("#edf8fb", "#238b45")) %>%
    hide_axis("x") %>% 
    hide_axis("y") %>%
    handle_click(update_selection) %>%
    set_options(height = 500, width = 800) %>%
    add_tooltip(map_tooltip, on = c("hover", "click")) %>%
    add_legend(scales = 'fill', title = 'Loans Per 1,000') %>%
    bind_shiny("map")
  
  ##################
  ### WORD CLOUD ###
  ##################
  wc.data <- reactive({
    word.freq <- as.data.frame(table(unlist(strsplit(gsub('[[:digit:]]+|[[:punct:]]', '', tolower(loan.df.wordcloud()$title)), split = " ")))) # remove punctuation and numbers
    word.freq <- word.freq[word.freq$Freq >= input$word.occ & word.freq$Var1 != "" & !(word.freq$Var1 %in% stopwords(kind = "en")), ]
    word.freq <- word.freq %>%
      mutate(Freq = ifelse(Freq < 500, Freq, 500)) %>%
      # mutate(Freq = (Freq)**(1/3)) %>%
      arrange(desc(Freq))
    word.freq
  })
  output$wordcloud <- renderWordcloud2({
    wc.data() %>%
      head(input$word.freq) %>%
      wordcloud2(color = rep(brewer.pal(8, "Dark2"), length.out = input$word.freq),
               backgroundColor = "white", size = 0.25,
               minRotation = pi/2, maxRotation = pi/2)
  })
  #####################
  ### LINKED GRAPHS ###
  #####################
  output$subgrade.hist <- renderPlotly({
    subgrade.plot <- ggplot(data = loan.df.grade(), aes(x = sub_grade)) +
      geom_histogram(stat = "count", width = 0.6, fill = "#41ae76") +
      scale_y_continuous(name = "Frequency\n", labels = comma, expand = c(0,0)) +
      geom_blank(aes(y = 1.1*..count..), stat = "count") +  # Hack to expand y axis
      scale_x_discrete(name = "\nSub Grades", expand = c(0,0)) +
      ggtitle("Frequency of Loan Grades") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 90),
            panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
            panel.grid.minor.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill = "white", color = "black", size = 0.5))
    ggplotly(subgrade.plot, tooltip = "all") %>% layout(dragmode = "select")
  })
  
  output$loan.amnt.hist <- renderPlot({
    d <- event_data("plotly_click")
    if (is.null(d)){
      ggplot(data = loan.df.grade(), aes(loan_amnt)) + 
        geom_histogram(fill = "#41ae76", color = "black") +
        ggtitle("Loan Amount (All Grades)") +
        scale_y_continuous(name = "Frequency\n", expand = c(0,0), labels = comma) +
        geom_blank(aes(y = 1.1*..count..), stat="bin") +  # Hack to expand y axis
        scale_x_continuous(name = "\nLoan Amount", limits = c(0, max(loan.data$loan_amnt)), expand = c(0,0), labels = comma) +
        theme(panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.major.x = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = "black", size = 0.5),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 12))
    } else {
      subgrade.from.click <- sub.grade.vector()[d$x]
      filtered.df <- loan.df.grade() %>% filter(sub_grade == subgrade.from.click)
      ggplot(data = filtered.df, aes(loan_amnt)) + 
        geom_histogram(fill = "#41ae76", color = "black") +
        ggtitle(paste("Loan Amount (", subgrade.from.click, ")", sep = "")) +
        scale_y_continuous(name = "Frequency\n", expand = c(0,0), labels = comma) +
        geom_blank(aes(y = 1.1*..count..), stat="bin") +  # Hack to expand y axis
        scale_x_continuous(name = "\nLoan Amount", limits = c(0, max(loan.data$loan_amnt)), expand = c(0,0), labels = comma) +
        theme(panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.major.x = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = "black", size=0.5),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 12))
    }
  }, height = 300)
  
  output$loan.int.hist <- renderPlot({
    d <- event_data("plotly_click")
    if (is.null(d)){
      ggplot(data = loan.df.grade(), aes(int_rate)) + 
        geom_histogram(fill = "#41ae76", color = "black", binwidth = 0.005) +
        ggtitle("Interest Rate (All Grades)") +
        scale_y_continuous(name = "Frequency\n", expand = c(0,0), labels = comma) +
        geom_blank(aes(y = 1.1*..count..), stat="bin") +  # Hack to expand y axis
        scale_x_continuous(name = "\nInterest Rate", limits = c(min(loan.data$int_rate), max(loan.data$int_rate)), expand = c(0,0), labels = percent) +
        theme(panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.major.x = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = "black", size = 0.5),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 12))
    } else {
      subgrade.from.click <- sub.grade.vector()[d$x]
      filtered.df <- loan.df.grade() %>% filter(sub_grade == subgrade.from.click)
      ggplot(data = filtered.df, aes(int_rate)) + 
        geom_histogram(fill = "#41ae76", color = "black", binwidth = 0.005) +
        ggtitle(paste("Interest Rate (", subgrade.from.click, ")", sep = "")) +
        scale_y_continuous(name = "Frequency\n", expand = c(0,0), labels = comma) +
        geom_blank(aes(y = 1.1*..count..), stat="bin") +  # Hack to expand y axis
        scale_x_continuous(name = "\nInterest Rate", limits = c(min(loan.data$int_rate), max(loan.data$int_rate)), expand = c(0,0), labels = percent) +
        theme(panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.major.x = element_line(colour = "grey90", size=0.5, linetype="dashed"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = "black", size=0.5),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 12))
    }
  }, height = 300)
}

shinyApp(ui = ui, server = server)
