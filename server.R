# ciphers
# Ref: Shiny tags: https://shiny.rstudio.com/articles/tag-glossary.html
# Ref; shifting: https://stackoverflow.com/questions/59171164/shifting-strings-using-r-functions
# Ref: dashboardlayouts with navbarpage: https://stackoverflow.com/questions/59517901/how-to-insert-valuebox-inside-navbarpage-layout
function(input, output, session) {

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
            #encrypt_me <- 'goonies never say die!'
            #encrypt_shift_num <- 1

            # Remove whitespace
            # Ref: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
            encrypt_me_trim <<- gsub(" ", "", encrypt_me, fixed = TRUE)

            # Make it uppercase
            encrypt_me_upper <- toupper(encrypt_me_trim)

            # Remove all special characters
            # Ref: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
            x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
            encrypt_me_plain <- str_replace_all(encrypt_me_upper, "[[:punct:]]", "")

            # Split words into individual letters
            encrypt_me_letters <<- strsplit(encrypt_me_plain, "")[[1]]

            # Convert the letters to numbers
            encrypt_me_numbers <<- alphabet$num[match(unlist(encrypt_me_letters), alphabet$alpha)]

            # Add the shift number to each letter
            encrypted_numbers_shift <- encrypt_me_numbers + encrypt_shift_num

            # Use modular division
            encrypted_numbers_shift_mod <<- encrypted_numbers_shift %% 26

            # Revert back to letters the encryption
            encrypted_text_shift <- alphabet$alpha[match(unlist(encrypted_numbers_shift_mod), alphabet$num)]

            # Send it back
            encrypted_text_shift
        })

    })

    # Text explaining key space
    output$key_space_text <- renderUI({
        HTML(paste("Max number of combinations we would have to try to solve this with brute force."
                   , sep = "<br/>"))
    })

    # Text explaining key space
    observeEvent(toListenEncrypt(),{
        output$format_text <- renderText({
            encrypt_me_letters
        })
    })

    # Text explaining key space
    observeEvent(toListenEncrypt(),{
        output$convert_to_numbers <- renderText({
            encrypt_me_numbers
        })
    })

    # Text explaining key space
    observeEvent(toListenEncrypt(),{
        output$shift_to_numbers <- renderText({
            encrypted_numbers_shift_mod
        })
    })

    # Calculate shift key space
    observeEvent(toListenEncrypt(),{
        output$key_space_num <- renderText({
            26*1^length(encrypt_me_trim)
        })
    })

    ###################################################
    # server/encrypt_shift/end
    ###################################################

    ###################################################
    # server/freq_plot/start
    ###################################################

    # Test phrase
   # word_freq_text <- 'evie loves pancakes and bananaZ'

    # Grab the Shift Number
    output$word_freq_plot <- renderPlotly({

        word_freq_text <- input$word_freq_text

        # Count characters in a string
        # https://statisticsglobe.com/count-occurrence-of-certain-character-in-string-in-r
        #all_chars <- word_freq_text %>% str_count() # count all characters
        #a <- word_freq_text %>% toupper() %>% str_count("a") # count all a's
        letter_freq <- word_freq_text %>% toupper() %>% str_count(LETTERS) # count all letters
        letter_freq_df <- data.frame(letters = LETTERS, counts = letter_freq)
        letter_freq_df$freq <- round(letter_freq_df$counts / sum(letter_freq_df$counts), 2)

        # What is the accepted typical frequency plot
        # this, but why doesnt it add up to 100? come on man
        # https://crypto.interactive-maths.com/frequency-analysis-breaking-the-code.html
        common_freq <- data.frame(letters = LETTERS, freq = c(.082, .015, .028, .043, .127, .022, .02, .061, .07, .0015, .008, .04, .024, .067, .075, .019, .001, .06, .063, .091, .028, .01, .024, .0015, .02, .0007))

        # Create a df of actual and expected values
        combined_freq <- sqldf("select a.letters
                            , a.counts as actual_counts
                            , a.freq as actual_freq
                            , b.freq as expected_freq
                        from letter_freq_df as a
                        left join common_freq as b
                            on a.letters = b.letters
                        ;")

        combined_freq$expected_count <- round(combined_freq$expected_freq * sum(combined_freq$actual_counts), 1)

        # Create plot
        plt <- ggplot(data = combined_freq, aes(text = paste0(letters, "<br>",
                                                             "Actual Freq:", actual_freq, "<br>",
                                                             "Expected Freq:", expected_freq))) +

            # Expected frequency
            geom_bar(aes(x = letters
                           , y = expected_freq
                     )
                     , stat = 'identity'
                     , fill = "black"
                     , alpha = 0.25
                     #, color = "black"
            ) +

            # Actual frequdncy
            geom_bar(aes(x = letters
                           , y = actual_freq
                           , fill = letters
                     )
                     , stat = 'identity'
                     , alpha = 0.5
                     #, color = "black"
            ) +
            scale_fill_viridis_d(option = "viridis") +
            labs(x = ""
                 , y = "Frequency"
                 , title = ""
                 ) +
            #ylim(0, 0.25) +
            theme(legend.position = "none")

        ggplotly(plt, tooltip = "text") #%>%
            # layout(title = list(text = paste0('Frequency Plot',
            #                                   '<br>',
            #                                   '<sup>',
            #                                   'Gray boxes represent the expected frequency of each letter',
            #                                   '</sup>')))

    })


    ###################################################
    # server/freq_plot/end
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
#shinyApp(ui, server)
#shinyApp(ui, server, options = list(display.mode = 'showcase'))
