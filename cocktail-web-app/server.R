library(shiny)
library(jsonlite)
library(httr)
library(tidyverse)
library(styler)
library(pool)
library(curl)

readRenviron(".Renviron")
api_key = Sys.getenv("API_KEY")

#letters_and_num <- c(0:9, letters, "'")
#drink_details_df <- data.frame()
#for (i in letters_and_num) {
#    url <- str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/search.php?f={letter}", 
#                    api_key = Sys.getenv("API_KEY"), letter = i)
#    drinks_details <- fromJSON(url)$drinks
#    drink_details_df <- rbind(drink_details_df, drinks_details)
#    print("reading lines")
#}

drink_details_df <- read.csv("drink_details.csv")
drink_details_df <- drink_details_df %>% select(idDrink, strDrink, strCategory, strAlcoholic, strGlass, strInstructions, strDrinkThumb, strIngredient1:strIngredient15, strMeasure1:strMeasure15) %>%
    mutate(across(starts_with("str"), na_if, "NA")) %>%
    mutate(across(starts_with("str"), na_if, "<NA>")) %>%
    mutate(across(starts_with("str"), na_if, "")) %>%
    mutate(across(starts_with("str"), na_if, ''))

shinyServer(function(input, output, session) {
    
    ##### CREATE REACTIVE VALUES FOR RECIPES ----------------------
    
    recipe <- reactiveValues(details = NULL)
    
    ##### RANDOM RECIPE GENERATION --- -----------------------------
    
    # random recipe 
    observeEvent(input$random, {
        print("random button clicked")
        recipe$details <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/random.php",
                                            api_key = api_key))$drinks
    })
    
    ##### OBSERVEEVENT FOR FILTERING --------------------------------
    
    # filter by ingredient
    observeEvent(input$mult_ingredients, {
        print("ingredients selection updated")
        req(!is.null(input$mult_ingredients))
        multiple_ingreds <- str_replace_all(paste(input$mult_ingredients, collapse = ','), pattern = " ", replacement = "_")
        recipe$drinks <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?i={ingr}", 
                                           api_key = api_key,
                                           ingr = multiple_ingreds))$drinks
    })
    
    # filter by type of category
    observeEvent(input$ifAlcohol, {
        print("type of category selection updated")
        req(!is.null(input$mult_ingredients))
        if(input$ifAlcohol != "Select a type"){
            recipe$drinks <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?a={type}",
                                                      api_key = api_key, type = str_replace_all(input$ifAlcohol, ' ', '_')))$drinks
        }
    })
    
    # filter by type of glass
    observeEvent(input$glassType, {
        print("type of glass selection updated")
        req(!is.null(input$glassType))
        if(input$glassType != "Select a type"){
            recipe$drinks <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/filter.php?g={glass_type}",
                                               api_key = api_key, glass_type = str_replace_all(input$glassType, ' ', '_')))$drinks
        }
    })
    
    # filter by first letter/digit
    observeEvent(input$byLetter, {
        print("letter selection updated")
        req(!is.null(input$byLetter))
        if(input$byLetter != "Select a Letter") {
            recipe$drinks <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/search.php?f={letter}",
                                               api_key = api_key, letter = tolower(input$byLetter)))$drinks
        }
    })
    
    # create a selectInput object that has all of the recipe names that match the selected filter
    output$recipeMatchInputs <- renderUI ({
        req(!is.null(recipe$drinks))
        print("drink matches showing")
        if(recipe$drinks != "None Found") {
            selectInput("recipeMatch", label = NULL,
                        choices = recipe$drinks$idDrink %>% set_names(recipe$drinks$strDrink))
        } else {
            print("No matching drinks found, try another filter!")
        } 
    })
    
    # When a recipe is selected, save the value in the reactiveValue
    observeEvent(input$recipeMatch, {
        print("recipe match selected")
        recipe$details <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v2/{api_key}/lookup.php?i={recipe_id}",
                                            api_key = api_key, recipe_id = input$recipeMatch))$drinks
    })
    
    output$testInputOptions <- renderPrint({
        req(!is.null(recipe$drinks))
        print("input options selected")
        recipe$drinks
    })
    
    ##### RECIPE DETAILS PRINTED ON MAIN RECIPE TAB -----------------------
    
    # print out the selected recipe title
    output$recipeTitle <- renderText ({
        req(!is.null(recipe$details))
        print("title updated")
        recipe$details$strDrink
    })
    
    # print out the selected recipe ingredients and measurements
    output$recipeIngr_Measure <- renderTable({
        req(!is.null(recipe$details))
        drink <- recipe$details
        
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
    
    # print out the selected recipe instructions
    output$recipeInstructions <- renderText({
        req(!is.null(recipe$details))
        print("instructions updated")
        recipe$details$strInstructions
    })
    
    # print out the selected recipe image from URL 
    output$recipeImage <- renderText({
        req(!is.null(recipe$details))
        print("image updated")
        src = paste0(recipe$details$strDrinkThumb)
        paste0('<img src="', src,'" ', 'style="width:250px;height:250px;">')
    })
    
    ##### PLOTS ON COMPARISON TAB -------------------------------------------
    
    # bar plot by ingredient
    output$numIngredPlot <- renderPlot({
        req(!is.null(recipe$details))
        print("plotting num ingreds bar plot")
        drink <- recipe$details
        recipeID <- drink$idDrink
        num_ingred <- drink_details_df %>% select(idDrink, starts_with("strIngredient")) %>% mutate(numIngredients = rowSums(!is.na(.))-1)
        # Plot to show bar plot with the number of ingredients that they selected
        ggplot(data = num_ingred, aes(x = numIngredients, 
                                      fill = factor(ifelse(numIngredients == (num_ingred %>% filter(idDrink == recipeID) %>% pull(numIngredients)), 
                                                           "Highlighted", "Normal"))))  + 
            geom_bar(show.legend = FALSE) + scale_fill_manual(name = "numIngredients", values=c("#7B0828","grey50")) +
            xlab("Number of Ingredients") +
            ylab("Number of Drinks") +
            ggtitle("How many ingredients are in my drink?") +
            theme(axis.text = element_text(size = rel(1.5)))
        
    })
    
    # bar plot by letter
    output$byLetterPlot <- renderPlot({
        req(!is.null(recipe$details))
        print("plotting by letter bar plot")
        drink <- recipe$details
            
        recipeID <- drink$idDrink
        byLetter <- drink_details_df %>% 
            select(idDrink, strDrink) %>% mutate(firstLetter = substr(strDrink, 1, 1))
            
        ggplot(data = byLetter, aes(x = firstLetter, fill = factor(ifelse(firstLetter == (byLetter %>% filter(idDrink == recipeID) %>% pull(firstLetter)), 
                                                                              "Highlighted", "Normal"))))  + 
            geom_bar(show.legend = FALSE) + scale_fill_manual(name = "firstLetter", values=c("#7B0828","grey50")) +
            xlab("Character") +
            ylab("Number of Drinks") +
            ggtitle("Drinks by First Character") +
            theme(axis.text = element_text(size = rel(1.5)))
    })
    
    # bar plot by type of drink
    output$byDrinkPlot <- renderPlot({
        req(!is.null(recipe$details)) 
        print("plotting by type of drink plot")
        drink <- recipe$details
    
        recipeID <- drink$idDrink
        byDrink <- drink_details_df %>% 
                select(idDrink, strAlcoholic)
            
        ggplot(data = byDrink, aes(x = strAlcoholic, fill = factor(ifelse(strAlcoholic == (byDrink %>% filter(idDrink == recipeID) %>% pull(strAlcoholic)), 
                                                                          "Highlighted", "Normal"))))  + 
            geom_bar(show.legend = FALSE) + scale_fill_manual(name = "strAlcoholic", values=c("#7B0828","grey50")) +
            xlab("Type of Drink") +
            ylab("Number of Drinks") +
            ggtitle("Drinks by Type of Drink") +
            theme(axis.text.y = element_text(size = rel(1.5)),
                  axis.text.x = element_text(size = rel(1.25)))
    })
    
    # bar plot by glass
    output$byGlassPlot <- renderPlot({
        req(!is.null(recipe$details))
        print("plotting by glass plot")
        drink <- recipe$details
            
        recipeID <- drink$idDrink
        byGlass <- drink_details_df %>% 
                select(idDrink, strGlass)
            
        ggplot(data = byGlass, aes(x = strGlass, fill = factor(ifelse(strGlass == (byGlass %>% filter(idDrink == recipeID) %>% pull(strGlass)), 
                                                                      "Highlighted", "Normal"))))  + 
            geom_bar(show.legend = FALSE) + scale_fill_manual(name = "strGlass", values=c("#7B0828","grey50")) +
            xlab("Type of Glass") +
            ylab("Number of Drinks") +
            ggtitle("Drinks by Type of Glass") +
            theme(axis.text.x = element_text(angle = 33, size = rel(1.0)), 
                  axis.text.y = element_text(size = rel(1.5)))
    })
    
    
})