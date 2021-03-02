# ciphers
# Ref: Shiny tags: https://shiny.rstudio.com/articles/tag-glossary.html
# Ref; shifting: https://stackoverflow.com/questions/59171164/shifting-strings-using-r-functions
# Ref: dashboardlayouts with navbarpage: https://stackoverflow.com/questions/59517901/how-to-insert-valuebox-inside-navbarpage-layout

navbarPage(

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
                        title = "Encrypt with Shift Cipher (Caesar)",
                        solidHeader = TRUE,
                        #tags$h4("Encrypt with Shift Cipher"),
                        status = "primary", # primary (blue), success (green), info (blue), warning (orange), danger (red)

                        # Message we want to encyrpt
                        textInput(
                            inputId = "encrypt_me",
                            label = "What do you want to encrypt?",
                            placeholder = "Goonies never say die!",
                            value = "Goonies never say die!",
                            width = "100%"
                        ),

                        #tags$h4("Key space"),
                        #tags$i(htmlOutput("key_space_text")),
                        tags$i("Max number of combinations we would have to try to solve this with brute force."),
                        tags$a(textOutput("key_space_num")),

                        tags$i("Format text"),
                        tags$a(textOutput("format_text")),

                        tags$i("Convert text to numbers"),
                        tags$a(textOutput("convert_to_numbers")),

                        tags$i("Shift the numbers"),
                        tags$a(textOutput("shift_to_numbers")),

                        tags$h4("Convert back to text (our encryption!)"),
                        tags$a(textOutput("encrypted_text_shift"))
                    )

                ),

                ############################################
                # Decrypt with Shift Cipher
                ############################################
                fluidRow(
                    box(width=12,
                        title = "Decrypt",
                        #tags$h4("Decrypt with Shift Cipher"),
                        solidHeader = TRUE,
                        status = "info", # primary (blue), success (green), info (blue), warning (orange), danger (red)

                        # Message we want to decrypt
                        textInput(
                            inputId = "decrypt_me",
                            label = "What do you want to decrypt?",
                            placeholder = "HPPOJFTOFWFSTBZEJF",
                            value = "HPPOJFTOFWFSTBZEJF",
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
    # ui/freqency_plot/start
    ###################################################

    tabPanel("Frequency Plots",
             sidebarLayout(
                  sidebarPanel(

                 ),

                 mainPanel(
                     fluidRow(

                         box(width=10,
                             title = "Frequency Plot",
                             solidHeader = TRUE,
                             #tags$h4("Encrypt with Shift Cipher"),
                             status = "primary", # primary (blue), success (green), info (blue), warning (orange), danger (red)

                             textAreaInput(
                                 inputId = "word_freq_text",
                                 label = "Type in some text, and we'll plot the frequency of each letter",
                                 placeholder = "There is a theory which states that if ever anyone discovers exactly what the Universe is for and why it is here, it will instantly disappear and be replaced by something even more bizarre and inexplicable. There is another theory mentioned, which states that this has already happened!",
                                 value = "There is a theory which states that if ever anyone discovers exactly what the Universe is for and why it is here, it will instantly disappear and be replaced by something even more bizarre and inexplicable. There is another theory mentioned, which states that this has already happened!",
                                 width = "100%",
                                 height = 125
                             ),

                             # Plot the frequency plot

                            plotlyOutput("word_freq_plot")

                         )

                     )
                 )
             )
    ),

    ###################################################
    # ui/frequency_plot/end
    ###################################################

    ###################################################
    # ui/alpha_ciphers/start
    ###################################################

    tabPanel("Polyalphabetic",
             sidebarLayout(
                 sidebarPanel(

                     ############################################
                     # Search Input
                     ############################################
                     textInput(
                         inputId = "shift_alpha",
                         label = "Polyalphabetic Keyword",
                         placeholder = "babyruth",
                         value = "babyruth",
                         width = "100%"
                     ),

                 ),

                 mainPanel(
                     fluidRow(
                         box(width=12,
                             title = "Polyalphabetic Cipher",
                             solidHeader = TRUE,
                             status = "success"

                             #tags$h4("Encrypted Text (Alpha)"),

                             #textOutput("encrypted_text_alpha"),
                             )
                         )
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
