
library(shiny)
library(jsonlite)
library(httr)
library(tidyverse)
library(styler)
library(pool)
library(curl)

readRenviron(".Renviron")
api_key = Sys.getenv("API_KEY")

#drink_details_df <- read.csv("drink_details.csv")
#drink_details_df <- drink_details_df %>% select(idDrink, strDrink, strCategory, strAlcoholic, strGlass, strInstructions, strDrinkThumb, strIngredient1:strIngredient15, strMeasure1:strMeasure15) %>%
#    mutate(across(starts_with("str"), na_if, "NA")) %>% 
#    mutate(across(starts_with("str"), na_if, "<NA>")) %>% 
#    mutate(across(starts_with("str"), na_if, "")) %>% 
#    mutate(across(starts_with("str"), na_if, ''))

shinyServer(function(input, output, session) {

   
    
    ##### Random Recipe Generation -----------------------------
    
    # Get random cocktail recipe only when the user presses the 'Random!' button

    recipe <- reactiveValues(details = NULL)
    
    observeEvent(input$random, {
        recipe$details <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/random.php",
                                            api_key = api_key))$drinks
    })

    output$recipeTitle <- renderText ({
        req(!is.null(recipe$details))
        recipe$details$strDrink
    })
    
    output$recipeIngr_Measure <- renderTable({
        req(!is.null(recipe$details))
        drink <- recipe$details
        
        
        # Get ingredients list from drink information
        ingredients <- drink %>% mutate_if(is.logical, as.character) %>% 
            mutate(across(starts_with("str"), na_if, "NA")) %>% 
            mutate(across(starts_with("str"), na_if, "<NA>")) %>% 
            mutate(across(starts_with("str"), na_if, "")) %>% 
            dplyr::select(starts_with("strIngredient")) %>% 
            pivot_longer(starts_with("strIngredient")) %>% 
            drop_na() %>% mutate(Ingredient = value) %>% dplyr::select(Ingredient) 
        len_ingred <- ingredients %>% dplyr::summarize(n = n()) %>% pull(n)
        # Get measurements from drink information
        measurements <- drink %>% mutate_if(is.logical, as.character) %>% 
            mutate(across(starts_with("str"), na_if, "NA")) %>% 
            mutate(across(starts_with("str"), na_if, "<NA>")) %>% 
            mutate(across(starts_with("str"), na_if, "")) %>% 
            dplyr::select(starts_with("strMeasure")) %>% 
            pivot_longer(starts_with("strMeasure")) %>% 
            replace_na(list(value = "See Instructions")) %>% 
            slice_head(n = len_ingred) %>% mutate(Quantity = value) %>% 
            dplyr::select(Quantity) 
        
        # Combine and print as table
        cbind(ingredients, measurements)
        
    })
    
    output$recipeInstructions <- renderText({
        # Print out recipe instructions
        req(!is.null(recipe$details))
        recipe$details$strInstructions        
    })
    
    output$recipeImage <- renderText({
        # Print out image next to recipe info
        req(!is.null(recipe$details))
        src = paste0(recipe$details$strDrinkThumb)
        paste0('<img src="', src,'" ', 'style="width:250px;height:250px;">')
        
    })
    
    
    
    
    
    
    
    
    
    
    ##### Populating recipes that match the ingredient filter --------------------------
    
    # Create selectInput object based on ingredients selected
    
#    observeEvent(input$mult_ingredients, {
#        output$recipesMatchingLink <- renderUI ({
#              if(req(input$filterBy == "Ingredients")) {
#                if(!is.null(input$mult_ingredients)) {
#                    multiple_ingreds <- str_replace_all(paste(input$mult_ingredients, collapse = ','), pattern = " ", replacement = "_")
#                    url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?i={ingr}", 
#                                api_key = api_key,
#                                ingr = multiple_ingreds)
#                    drink_list <- fromJSON(url)$drinks
#                    if(drink_list != "None Found") {
#                        selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
#                                choices = drink_list$idDrink %>% set_names(drink_list$strDrink))
#                    } else {
#                        print("No matching drinks found!")
#                        
#                    }
#                }
#            }
#        })
#    })
    
    #observeEvent(input$mult_ingredients, {
    #    if(req(input$filterBy == "Ingredients")) {
    #        if(!is.null(input$mult_ingredients)) {
    #            multiple_ingreds <- str_replace_all(paste(input$mult_ingredients, collapse = ','), pattern = " ", replacement = "_")
    #            url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?i={ingr}", 
    #                            api_key = api_key,
    #                            ingr = multiple_ingreds)
    #            recipe$drinks <- fromJSON(url)$drinks
    #        }
    #    }
    #})
    
    #output$recipesMatchingLink <- renderUI ({
    #    if(recipe$drinks != "None Found") {
    #        selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
    #                    choices = recipe$drinks$idDrink %>% set_names(recipe$drinks$strDrink))
    #    } else {
    #        print("No matching drinks found!")
    #        
    #    }
    #})
        
       # output$recipesMatchingLink <- renderUI ({
    #        if(req(input$filterBy == "Ingredients")) {
    #            if(!is.null(input$mult_ingredients)) {
    #                multiple_ingreds <- str_replace_all(paste(input$mult_ingredients, collapse = ','), pattern = " ", replacement = "_")
    #                url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?i={ingr}", 
    #                                api_key = api_key,
    ##                                ingr = multiple_ingreds)
    #               drink_list <- fromJSON(url)$drinks
    #                
    #                
    #                if(drink_list != "None Found") {
    #                    selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
    #                                choices = drink_list$idDrink %>% set_names(drink_list$strDrink))
    ##                } else {
    #                    print("No matching drinks found!")
    #                    
    #                }
    #            }
    #        }
    #    })
    #})
    
    ##### Populating recipes that match the type filter ------------------
    
    # Create selectInput objet based on type of beverage selected
    
#    observeEvent(input$ifAlcohol, {
#        output$recipesMatchingLink <- renderUI ({
#            if(req(input$filterBy == "Type of Drink")) {
#                if(input$ifAlcohol != "Pick a type") {
#                    url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?a={type}",
#                                api_key = api_key, type = str_replace_all(input$ifAlcohol, ' ', '_'))
#                    drink_list <- fromJSON(url)$drinks
#                    selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
#                            choices = c("Select a Recipe", drink_list$idDrink %>% set_names(drink_list$strDrink)))
#                }
#            } 
#        })
#        
#    })
    
    ##### Populating recipes that match the letter filter -----------------------
    
    # Create selectInput object based on letter selected
    
#    observeEvent(input$letter, {
#        output$recipesMatchingLink <- renderUI ({
#            if(req(input$filterBy == "Letter")) {
#                if(input$letter != "Pick a letter") {
#                    url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/search.php?f={letter}",
#                                    api_key = api_key, letter = input$letter)
#                    drink_list <- fromJSON(url)$drinks
#                    selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
#                                choices = c("Select a Recipe", drink_list$idDrink %>% set_names(drink_list$strDrink)))
#                }
#            } 
#        })
#        
#    })
    
    ##### Populating recipes that match the glass filter -----------------------
    
    # Create selectInput object based on letter selected
    
#    observeEvent(input$glassType, {
#        output$recipesMatchingLink <- renderUI ({
#            if(req(input$filterBy == "Type of Glass")) {
#                if(input$glassType != "Pick a type") {
#                    url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?g={glass_type}",
#                                    api_key = api_key, glass_type = str_replace_all(input$glassType, ' ', '_'))
#                    drink_list <- fromJSON(url)$drinks
#                    selectInput("recipesMatchingOptions", label = "All Recipes Matching", 
#                                choices = c("Select a Recipe", drink_list$idDrink %>% set_names(drink_list$strDrink)))
#                }
#            } 
#        })
#        
#    })
    
    
    ##### GETTING DRINK RECIPE ---------------------------------------------------------------
        
    # recipe reactive Value changes when user clicks 'get my drink'
    
    #observeEvent(input$recipeSubmit, {
    #    # update reactiveValue only if an actual recipe is selected
    #    if (input$recipesMatchingOptions != "Select a Recipe") {
    #        recipe$details <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/lookup.php?i={recipe_id}",
    #                                api_key = api_key, recipe_id = input$recipesMatchingOptions))$drinks
    #    }
    #})

    
    ###### RECIPE OUTPUT IN MAIN PANEL -------------------------------------------------------
    
    # if statements so that output is only shown when the reactiveValue is not null

    
    
    ##### OUTPUT PLOTS ------------------------------------------------------------------
    
#    output$numIngredPlot <- renderPlot({
#        if(!is.null(recipe$details)) {
#            drink <- recipe$details
#       
#        # does not include '57 with a chevy in drink_details_df since starts with "'"
#            recipeID <- drink$idDrink
#            num_ingred <- drink_details_df %>% select(idDrink, starts_with("strIngredient")) %>% mutate(numIngredients = rowSums(!is.na(.))-1)
#        # Plot to show bar plot with the number of ingredients that they selected
#            ggplot(data = num_ingred, aes(x = numIngredients, fill = factor(ifelse(numIngredients == (num_ingred %>% filter(idDrink == recipeID) %>% pull(numIngredients)), 
#                                                                                   "Highlighted", "Normal"))))  + 
#                geom_bar(show.legend = FALSE) + scale_fill_manual(name = "numIngredients", values=c("#7B0828","grey50")) +
#                xlab("Number of Ingredients") +
#                ylab("Number of Drinks") +
#                ggtitle("How many ingredients are in my drink?") +
#                theme(axis.text = element_text(size = rel(1.5)))
#        }
#    })
    
#    observeEvent(input$letter, {
#        output$byLetterPlot <- renderPlot({
#            if(!is.null(recipe$details)) {
#                drink <- recipe$details
#                
#                recipeID <- drink$idDrink
#                byLetter <- drink_details_df %>% 
#                    select(idDrink, strDrink) %>% mutate(firstLetter = substr(strDrink, 1, 1))
#                
#                ggplot(data = byLetter, aes(x = firstLetter, fill = factor(ifelse(firstLetter == (byLetter %>% filter(idDrink == recipeID) %>% pull(firstLetter)), 
#                                                                                  "Highlighted", "Normal"))))  + 
#                    geom_bar(show.legend = FALSE) + scale_fill_manual(name = "firstLetter", values=c("#7B0828","grey50")) +
#                    xlab("Character") +
#                    ylab("Number of Drinks") +
#                    ggtitle("Drinks by First Character") +
#                    theme(axis.text = element_text(size = rel(1.5)))
#            }
##        })
#    })
    
#    observeEvent(input$ifAlcohol, {
#        output$byDrinkPlot <- renderPlot({
#            if(!is.null(recipe$details)) {
#                drink <- recipe$details
#                
#                recipeID <- drink$idDrink
#                byDrink <- drink_details_df %>% 
#                    select(idDrink, strAlcoholic)
#                
#                ggplot(data = byDrink, aes(x = strAlcoholic, fill = factor(ifelse(strAlcoholic == (byDrink %>% filter(idDrink == recipeID) %>% pull(strAlcoholic)), 
#                                                                                  "Highlighted", "Normal"))))  + 
#                    geom_bar(show.legend = FALSE) + scale_fill_manual(name = "strAlcoholic", values=c("#7B0828","grey50")) +
#                    xlab("Type of Drink") +
#                    ylab("Number of Drinks") +
#                    ggtitle("Drinks by Type of Drink") +
#                    theme(axis.text.y = element_text(size = rel(1.5)),
#                          axis.text.x = element_text(size = rel(1.25)))
#                    
#            }
#        })
#    })
#    
#    observeEvent(input$glassType, {
#        output$byGlassPlot <- renderPlot({
#            if(!is.null(recipe$details)) {
#                drink <- recipe$details
#                
#                recipeID <- drink$idDrink
#                byGlass <- drink_details_df %>% 
#                    select(idDrink, strGlass)
#                
#                ggplot(data = byGlass, aes(x = strGlass, fill = factor(ifelse(strGlass == (byGlass %>% filter(idDrink == recipeID) %>% pull(strGlass)), 
#                                                                                  "Highlighted", "Normal"))))  + 
#                    geom_bar(show.legend = FALSE) + scale_fill_manual(name = "strGlass", values=c("#7B0828","grey50")) +
#                    xlab("Type of Glass") +
#                    ylab("Number of Drinks") +
#                    ggtitle("Drinks by Type of Glass") +
#                    theme(axis.text.x = element_text(angle = 33, size = rel(1.0)), 
#                          axis.text.y = element_text(size = rel(1.5)))
#            }
#        })
#    })
#    
    
    

})
