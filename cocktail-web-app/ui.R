
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
        div(
            sidebarPanel(
                wellPanel(
                    h5("Give me a random recipe:"),
                    actionButton("random", "Random!")
                ),
                wellPanel(
                    h5("Filter by Ingredient:"),
                    selectizeInput("mult_ingredients",
                                   label = "Ingredients",
                                   choices = ingred_options, 
                                   multiple = TRUE)
                ), 
                wellPanel(
                    h5("Filter by Type of Drink:"),
                    selectInput("ifAlcohol", 
                                label = "Type of Drink",
                                choices = c("Select a Type", ifAlcohol_options))
                ),
                wellPanel(
                    h5("Filter by Type of Glass:"),
                    selectInput("glassType",
                                label = "Type of Glass",
                                choices = c("Select a Type", glass_options))
                ),
                wellPanel(
                    h5("Filter by First Letter/Character:"),
                    selectInput("byLetter", 
                                label = "First Letter/Character",
                                choices = c("Select a Letter", firstChar_options))
                ),
                wellPanel(
                    h5("Recipes Matching Latest Selected Filter:"),
                    uiOutput("recipeMatchInputs")
                )
            )),
        
        mainPanel(
            textOutput("recipeTitle"),
            br(),
            tableOutput("recipeIngr_Measure"), 
            textOutput("recipeInstructions"),
            htmlOutput("recipeImage"),
            plotOutput("numIngredPlot"),
            plotOutput("byLetterPlot"),
            plotOutput("byGlassPlot"),
            plotOutput("byDrinkPlot")
            #verbatimTextOutput("testInputOptions"),
        )
    )
)
)