#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Ref: Shiny tags: https://shiny.rstudio.com/articles/tag-glossary.html
# Ref; shifting: https://stackoverflow.com/questions/59171164/shifting-strings-using-r-functions
# Ref: dashboardlayouts with navbarpage: https://stackoverflow.com/questions/59517901/how-to-insert-valuebox-inside-navbarpage-layout

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(shinyWidgets)

# Setup
alphabet <- data.frame(alpha = LETTERS, num = 0:25)

# Define UI for application that draws a histogram
ui <- navbarPage(

    ###################################################
    # header
    ###################################################
    title = "Cipher | Practice",
    theme = shinythemes::shinytheme("flatly"),

    # See the ref above!
    header = tagList(
        useShinydashboard()
    ),
    ###################################################
    # ui/shift_ciphers/start
    ###################################################
    tabPanel(
        title="Shift ciphers",
        sidebarLayout(
            sidebarPanel(

                ############################################
                # Shift Selectors
                ############################################
                sliderInput(inputId = "encrypt_shift_num",
                            label = "Encrypt Shift",
                            min = 0,
                            max = 26,
                            value = 1,
                            animate = FALSE,
                            dragRange = TRUE),

                sliderInput(inputId = "decrypt_shift_num",
                            label = "Decrypt Shift",
                            min = 0,
                            max = 26,
                            value = 1,
                            animate = FALSE,
                            dragRange = TRUE)
            ),

            mainPanel(

                ############################################
                # Encrypt with Shift cipher
                ############################################
                fluidRow(
                    box(width=12,
                        #title = "COVID-19 Observations",
                        tags$h4("Encrypt with Shift Cipher"),
                        status = "success", # primary (blue), success (green), info (blue), warning (orange), danger (red)

                        # Message we want to encyrpt
                        textInput(
                            inputId = "encrypt_me",
                            label = "What do you want to encrypt?",
                            placeholder = "Goonies never say dieZ",
                            value = "Goonies never say dieZ",
                            width = "100%"
                        ),

                        tags$h4("Key space"),
                        htmlOutput("key_space_text"),
                        #tags$a(htmlOutput("encrypted_key_space")),

                        tags$h4("Encrypted Text"),
                        tags$a(textOutput("encrypted_text_shift"))
                    )

                ),

                ############################################
                # Decrypt with Shift Cipher
                ############################################
                fluidRow(
                    box(width=12,
                        #title = "COVID-19 Observations",
                        tags$h4("Decrypt with Shift Cipher"),
                        status = "success", # primary (blue), success (green), info (blue), warning (orange), danger (red)

                        # Message we want to decrypt
                        textInput(
                            inputId = "decrypt_me",
                            label = "What do you want to decrypt?",
                            placeholder = "HPPOJFTOFWFSTBZEJFA",
                            value = "HPPOJFTOFWFSTBZEJFA",
                            width = "100%"
                        ),

                        #tags$a("Selected shift"),

                        #textOutput("shift_num"),

                        tags$h4("Decrypted Text"),

                        tags$a(textOutput("decrypted_text_shift"))
                    )

                )
            )
        )
    ),

    ###################################################
    # ui/shift_cypers/end
    ###################################################

    ###################################################
    # ui/alpha_ciphers/start
    ###################################################

    tabPanel("Alphabetic Ciphers",
             fluidRow(
                 box(width=12,
                     #title = "COVID-19 Observations",

                     ############################################
                     # Search Input
                     ############################################
                     textInput(
                         inputId = "shift_alpha",
                         label = "Alphabetic Keyword",
                         placeholder = "babyruth",
                         value = "babyruth",
                         width = "100%"
                     ),

                     ############################################

                     tags$h4("Alphabetic Cipher"),
                     status = "success"

                     #tags$h4("Encrypted Text (Alpha)"),

                     #textOutput("encrypted_text_alpha"),
                 )

             )
    ),

    ###################################################
    # ui/alpha_ciphers/end
    ###################################################

    ###################################################
    # ui/references/start
    ###################################################

    tabPanel("References",
             fluidRow(
                 column(12,
                        includeMarkdown("references.md")
                 )
             )
    )

    ###################################################
    # ui/references/end
    ###################################################

)


###################################################
# Server function
###################################################

server <- function(input, output, session) {

    # Grab the Shift Number
    output$shift_num <- renderText({
        input$shift_num
    })

    ###################################################
    # server/encrypt_shift/start
    ###################################################

    # Listen for changes to any of these inputs
    toListenEncrypt <- reactive({
        list(input$encrypt_me, input$encrypt_shift_num)
    })

    # If listen is triggered, execute:
    observeEvent(toListenEncrypt(),{
        encrypt_me <- input$encrypt_me
        encrypt_shift_num <- input$encrypt_shift_num

        # Do the Shift encryption
        output$encrypted_text_shift <- renderText({
            # Add the processing here

            # Test phrase
            #encrypt_me <- 'goonies never say diez'
            #encrypt_shift_num <- 1

            # Remove whitespace
            # Ref: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
            encrypt_me_trim <<- gsub(" ", "", encrypt_me, fixed = TRUE)

            # Make it uppercase
            encrypt_me_upper <- toupper(encrypt_me_trim)

            # Remove all special characters
            # Ref: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
            x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
            encrypt_me_plain <- str_replace_all(encrypt_me_upper, "[[:punct:]]", " ")

            # Split words into individual letters
            encrypt_me_letters <- strsplit(encrypt_me_plain, "")[[1]]

            # Convert the letters to numbers
            encrypt_me_numbers <- alphabet$num[match(unlist(encrypt_me_letters), alphabet$alpha)]

            # Add the shift number to each letter
            encrypted_numbers_shift <- encrypt_me_numbers + encrypt_shift_num

            # Use modular division
            encrypted_numbers_shift_mod <- encrypted_numbers_shift %% 26

            # Revert back to letters the encryption
            encrypted_text_shift <- alphabet$alpha[match(unlist(encrypted_numbers_shift_mod), alphabet$num)]

            # Send it back
            encrypted_text_shift
        })

    })

    # # Text explaining key space
    output$key_space_text <- renderUI({
        HTML(paste("How many possible combinations have keys do we have?"
                   , "This is how hard it would be to solve this through 'brute force'."
                   , sep = "<br/>"))
    })


    # Calculate shift key space
    output$key_space_text <- renderText({
        26*1^length(encrypt_me_trim)
    })

    ###################################################
    # server/encrypt_shift/end
    ###################################################

    ###################################################
    # server/decrypt_shift/start
    ###################################################

    # Listen for changes to any of these inputs
    toListenDecrypt <- reactive({
        list(input$decrypt_me, input$decrypt_shift_num)
    })

    # If listen is triggered, execute:
    observeEvent(toListenDecrypt(),{
        decrypt_me <- input$decrypt_me
        decrypt_shift_num <- input$decrypt_shift_num

        # Do the Shift encryption
        output$decrypted_text_shift <- renderText({

            # Test phrase
            #decrypt_me <- 'HPPOJFTOFWFSTBZEJFA'
            #decrypt_shift_num <- 1

            # Remove whitespace
            # Ref: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
            decrypt_me_trim <- gsub(" ", "", decrypt_me, fixed = TRUE)

            # Make it uppercase
            decrypt_me_upper <- toupper(decrypt_me_trim)

            # Remove all special characters
            # Ref: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
            x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
            decrypt_me_plain <- str_replace_all(decrypt_me_upper, "[[:punct:]]", " ")

            # Split words into individual letters
            decrypt_me_letters <- strsplit(decrypt_me_plain, "")[[1]]

            # Convert the letters to numbers
            decrypt_me_numbers <- alphabet$num[match(unlist(decrypt_me_letters), alphabet$alpha)]

            # Add the shift number to each letter
            decrypted_numbers_shift <- decrypt_me_numbers + 26 - decrypt_shift_num

            # Use modular division
            decrypted_numbers_shift_mod <- decrypted_numbers_shift %% 26

            # Revert back to letters the encryption
            decrypted_text_shift <- alphabet$alpha[match(unlist(decrypted_numbers_shift_mod), alphabet$num)]

            # Send it back
            decrypted_text_shift
        })


    })

    ###################################################
    # server/decrypt_shift/end
    ###################################################

}




###################################################
# Call function
###################################################
shinyApp(ui, server)
#shinyApp(ui, server, options = list(display.mode = 'showcase'))
