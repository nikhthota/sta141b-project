
library(shiny)
library(jsonlite)
library(httr)
library(tidyverse)
library(styler)
library(shinyjs)

# Getting API Key for Cocktail DB stored in .Renviron file
readRenviron(".Renviron")
api_key = Sys.getenv("API_KEY")

ifAlcohol_options <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/list.php?a=list",
                                       api_key = api_key))$drinks$strAlcoholic
glass_options <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/list.php?g=list",
                                   api_key = api_key))$drink$strGlass
firstChar_options <- c(toupper(letters), 0:9, "'")
ingred_options <- sort(fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/list.php?i=list",
                                         api_key = api_key))$drinks$strIngredient1)

shinyUI(fluidPage(
    useShinyjs(),
    # Application title
    headerPanel("Cocktail Recipe App"),
    
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h5("Find me a random recipe:"),
                actionButton("random", "Random!"),
            ),
            h4("Select one of the following filters to apply:"),
            wellPanel(
                h5("Filter by Ingredients:"),
                selectizeInput("mult_ingredients",
                               label = NULL,
                               choices = ingred_options, 
                               multiple = TRUE)
            ), 
            wellPanel(
                h5("Filter by Type of Drink:"),
                selectInput("ifAlcohol", 
                            label = NULL,
                            choices = c("Select a Type", ifAlcohol_options))
            ),
            wellPanel(
                h5("Filter by Type of Glass:"),
                selectInput("glassType",
                            label = NULL,
                            choices = c("Select a Type", glass_options))
            ),
            wellPanel(
                h5("Filter by First Letter/Character:"),
                selectInput("byLetter", 
                            label = NULL,
                            choices = c("Select a Letter", firstChar_options))
            ),
            wellPanel(
                h5(strong("Matching Recipes:")),
                h6("(by most recently selected filter)"),
                uiOutput("recipeMatchInputs"),
                class = "wellPanelRecipe", id = "wP-R"
            )
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Recipe",
                                 br(),
                                 fluidRow(
                                     column(
                                         6,
                                         textOutput("recipeTitle"),
                                         br(),
                                         wellPanel(tableOutput("recipeIngr_Measure")), 
                                         textOutput("recipeInstructions")
                                     ),
                                     column(
                                         6,
                                         htmlOutput("recipeImage")#,
                                         #verbatimTextOutput("testInputOptions")
                                     )
                                 )
                        ),
                        tabPanel("How does my drink compare?",
                                 br(),
                                 fluidRow(
                                     column(6, plotOutput("numIngredPlot")),
                                     column(6, plotOutput("byDrinkPlot"))
                                 ),
                                 fluidRow(
                                     column(12, plotOutput("byLetterPlot"))
                                 ),
                                 fluidRow(
                                     column(12, plotOutput("byGlassPlot"))
                                 )
                        ) #close compare tabPanel
            ) #close tabsetPanel
        ) #close Main Panel
        
        
    ),#close sidebar Layout
    
    tags$head(tags$style("#recipeTitle{color: black;
                                 font-size: 18px;
                                 font-style: bold;}"),
              tags$style("#wP-R{background-color: #c9d7e8;}"),
              tags$style("#matchingRecipeHeader{font-size: 14px; font-style: bold;}")
              )
))
