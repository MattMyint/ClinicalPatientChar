library(shiny)
library(ggplot2)

## load raw dataset
df <- read.csv(file = "Patient Clinical Characteristics (No Dup_Trimmed Genetics).csv")

#### data cleaning ####
## remove NA
df[is.na(df)] <- 0
local({
  for(i in c("5h", "6h", "7h", "8h", "9h", "10h", "11h", "13h", "14h")){
    df <<- df[,-grep(i,colnames(df))]
  }
})

## correct the column data classes
local({
  to_convert <- c("Site_Code", "GenderCode", "BMICode", "RaceCode", "StatinCode", 
                  "StatinDoseCode", "MI", "Renal_Problems", "Liver_Problems", "Hypertension", 
                  "Diabetes_Mellitus", "Hypercholesterolemia", "Myalgia", "Myalgia_Score", "Blood_Thinner",
                  "Glucose_lowering", "Cholesterol_Lowering", "Heart_protective", "BP_lowering")
  conv_to_factor <- function(datafram, c){
    datafram[, c] <- as.factor(datafram[, c])
    return(datafram)
  }
  for(i in to_convert){
    df <<- conv_to_factor(df, i)
  }
  df$Subject_ID <<- as.character(df$Subject_ID)
})

## identify which columns are classed wrong (skipped since I found alr)
type_df <- list()

local({
  data_headers <- colnames(df)
  types <- character()
  for(i in seq_along(data_headers)){
    type_i <- class(df[, i])
    types <- c(types, type_i)
  }
  type_df <<- list(header = data_headers, type = types)
})
type_df <- as.data.frame(type_df)

## get grouping variables for x axis plotting
f_groupings <- as.character(type_df[type_df$type == "factor",1])

## remove outliers from continous data
remove_outliers <- function(x, na.rm = TRUE){
  qnt <- quantile(x, probs = c(.02, .98), na.rm = na.rm)
  x[x <= qnt[1]] <- qnt[1]
  x[x >= qnt[2]] <- qnt[2]
  x
}

#### Data Visualisation ####
## boxplots 
g.boxplot <- function(x,y,df){
  data <- df[,c(x, y)]
  g <- ggplot(data, aes_string(x, y)) + geom_boxplot(aes_string(fill = x))
  plot(g)
}

## scatterplots
g.scatterplot <- function(x,y,z,df){
  data <- df[,c(x, y, z)]
  g <- ggplot(data, aes_string(x, y)) + geom_point(aes_string(colour = z))
  plot(g)
}

#### App Body ####
# Define UI ----
ui <- fluidPage(
  titlePanel("EDA of Statin Dataset"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("cobas.only", "Use only data with COBAS results:"),
      wellPanel(
        h4("Boxplot Options"),
        selectInput(inputId = "x.box", label = "X variable", choices = f_groupings),
        selectInput(inputId = "y.box", label = "Y variable", choices = type_df$header[type_df$type == "numeric"])
      ),
      wellPanel(
        h4("Scatterplot Options"),
        selectInput(inputId = "x.scat", label = "X variable", choices = type_df$header[type_df$type == "numeric"]),
        selectInput(inputId = "y.scat", label = "Y variable", choices = type_df$header[type_df$type == "numeric"]),
        selectInput(inputId = "fill.scat", label = "Dot Colour", choices = f_groupings)
      ),
      plotOutput("hist"),
      fluidRow(
        column(6,
               selectInput(inputId = "hist.var", label = NULL, choices = type_df$header[type_df$type == "numeric"])),
        column(6,
               checkboxInput("log.trans", "Log-scale"))
      )
    ),
    mainPanel(
      fluidRow(
        column(8,
               plotOutput("boxplot")),
        column(4,
               htmlOutput("box.compare"))
      ),
      hr(),
      fluidRow(
        column(8,
               plotOutput("scatter")),
        column(4,
               htmlOutput("cor.val"))
      )
      
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  current <- reactiveValues(df = df)
  
  observe({
    if(input$cobas.only){
      current$df <- df[df$COBAS_hsCRP != 0,]
    }else{
      current$df <- df
    }
  })
  
  output$hist <- renderPlot({
    in.hist <- current$df[,input$hist.var]
    if(input$log.trans){
      in.hist <- log(in.hist)
    }
    hist(in.hist, col = "gray",
         main = NULL)
  })
  
  output$boxplot <- renderPlot({
    g.boxplot(input$x.box, input$y.box, current$df)
  })
  
  output$box.compare <- renderText({
    stat_test <- kruskal.test(x = current$df[,input$y.box], g = current$df[,input$x.box])
    paste(stat_test$method, paste0("p-value: ", round(stat_test$p.value, digits = 4)), sep = "<br>")
  })
  
  output$scatter <- renderPlot({
    g.scatterplot(input$x.scat, input$y.scat, input$fill.scat, current$df)
  })
  
  output$cor.val <- renderText({
    cor_test <- cor(current$df[,input$x.scat], current$df[,input$y.scat], method = "spearman")
    paste0("r^2 value: ", cor_test)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)