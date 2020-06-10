
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
            br(),
            radioButtons("filterBy", 
                         label = "Find drinks by:",
                         choices = c("Ingredients", "Letter", "Type of Drink", "Type of Glass"), 
                         selected = "None"),
            conditionalPanel(
                condition = "input.filterBy == 'Ingredients'",
                selectizeInput("mult_ingredients",
                               label = "Ingredients",
                               choices = ingred_options,
                               multiple = TRUE)
            ),
            conditionalPanel(
                condition = "input.filterBy == 'Type of Drink'",
                selectizeInput("ifAlcohol",
                               label = "Type of Beverage",
                               choices = c("Pick a type", ifAlcohol_options),
                               multiple = FALSE)
            ), 
            conditionalPanel(
                condition = "input.filterBy == 'Letter'",
                selectizeInput("letter",
                               label = "Letter",
                               choices = c("Pick a letter", firstChar_options),
                               multiple = FALSE)
            ),
            conditionalPanel(
                condition = "input.filterBy == 'Type of Glass'",
                selectizeInput("glassType",
                               label = "Glass",
                               choices = c("Pick a type", glass_options),
                               multiple = FALSE)
            ),
            uiOutput("recipesMatchingLink"),
            conditionalPanel(
                condition = "!is.null(input.recipesMatchingLink)",
                actionButton("recipeSubmit", "Get me my recipe!")
                )
        )),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Recipe", br(),
                                 fluidRow(
                                     column(6, 
                                            conditionalPanel(
                                                condition = "input.recipeSubmit != 0",
                                                textOutput("recipeTitle"),
                                                br(),
                                                tableOutput("recipeIngr_Measure"),
                                                textOutput("recipeInstructions"),
                                            )
                                     ),
                                     column(6, 
                                            conditionalPanel(
                                                condition = "input.recipeSubmit != 0 || input.random != 0", 
                                                htmlOutput("recipeImage")
                                            )
                                     )
                                 )
                        ),
                        tabPanel("How does my drink compare?", br(),
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
                        )
            )
        )
    ),
    
    tags$head(tags$style("#recipeTitle{color: black;
                                 font-size: 18px;
                                 font-style: bold;}"
                        )
            )
    )
)
