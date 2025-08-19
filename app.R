# Shiny App for Data Science Club and Math Club at the Student Engagement Expo 2025

# included packages ------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)

# functions --------------------------------------------------------------------

verify_email_format <- function(email, output) {
  name_address <- strsplit(email, split = "@")[[1]]
  if (length(name_address) != 2) {
    output$error <- renderText("<p style = 'color: orange'>Invalid Email Address (Missing @)</p>")
    return(FALSE)
  }
  name <- name_address[1]
  name <- strsplit(name, split = "[.]")[[1]]
  if (length(name) < 2 || length(name) > 3) {
    output$error <- renderText(HTML("Invalid Email Address (Invalid Name Format)"))
    return(FALSE)
  }
  address <- name_address[2]
  if (address != "jacks.sdstate.edu") {
    output$error <- renderText(HTML("Invalid Email Address (Invalid Address Format)"))
    return(FALSE)
  }
  output$error <- renderText("")
  return(TRUE)
}

# ui ---------------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(bootswatch = "materia"),
  card(height = 860,
       card_header("Data Science Club & Math Club At SEE 2025"),
       layout_column_wrap(
         heights_equal = "row",
         width = 1/2,
         card(card_header("Get Email Updates For DS And/Or Math Club!"), 
              textInput(inputId = "user_email", 
                        label = "Enter Your Jacks Email",
                        placeholder = "first.last@jacks.sdstate.edu"
              ),
              htmlOutput(outputId = "error"),
              span("Select Which Clubs Interest You:"),
              checkboxInput(inputId = "ds_club",
                            label = "DS Club"
              ),
              checkboxInput(inputId = "math_club",
                            label = "Math Club"
              ),
              actionButton(inputId = "submit",
                           label = "Submit"
              )
         ),
         card(card_header("Data Science Club Info"),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  value_box(title = "Time",
                            value = "5:30pm",
                            theme = "success",
                            showcase = icon("clock")
                  ),
                  value_box(title = "Location",
                            value = "AME 220",
                            theme = "info",
                            showcase = icon("building")
                  ),
                  value_box(title = "First Meeting",
                            value = "9/4/25",
                            theme = "warning",
                            showcase = icon("calendar-days")
                  ),
                  value_box(title = "Ok but is there free food?",
                            value = "Free Pizza!",
                            theme = "danger",
                            showcase = icon("pizza-slice")
                  )
                ),  
              )
         ),
         card(card_header("Math Club Info"),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  value_box(title = "Time",
                            value = "7:00pm",
                            theme = "success",
                            showcase = icon("clock")
                  ),
                  value_box(title = "Location",
                            value = "AME 210",
                            theme = "info",
                            showcase = icon("building")
                  ),
                  value_box(title = "First Meeting",
                            value = "9/11/25",
                            theme = "warning",
                            showcase = icon("calendar-days")
                  ),
                  value_box(title = "Ok but is there free food?",
                            value = "Also Free Pizza!",
                            theme = "danger",
                            showcase = icon("pizza-slice")
                  )
                ),  
              )
         ),
         navset_card_pill(
           nav_panel(title = "Code",
                     value_box(title = "Want to see the code running the app?",
                               value = "Scan the QR Code to see the GitHub page",
                               showcase = img(src = "github_qr.png", 
                                              height = "225px",
                                              width = "225px"),
                               showcase_layout = "left center"
                     )
           ),
           nav_panel(title = "LinkedIn",
                     value_box(title = "Data Science Club is on LinkedIn!",
                               value = "Find Us at the QR Code",
                               showcase = img(src = "linkedin_qr.png", 
                                              height = "225px",
                                              width = "225px"),
                               showcase_layout = "left center"
                     )
           )
         )
       )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output) {
  observe({
    email <- input$user_email
    ds_club <- input$ds_club
    math_club <- input$math_club
    if (verify_email_format(email, output)) {
      if (!file.exists("email_interest_list.csv")) {
        empty_data_frame <- data.frame(Email = character(),
                                       DS_Club = logical(),
                                       Math_Club = logical())
        write.csv(empty_data_frame, "email_interest_list.csv")
      }
      new_data_row <- data.frame(Email = email,
                                 DS_Club = ds_club,
                                 Math_Club = math_club)
      data_file <- read.csv("email_interest_list.csv",
                       colClasses = c("character", "character", "logical", "logical")) %>% select(-X)
      updated_data_file <- data_file %>% add_row(new_data_row)
      write.csv(updated_data_file, file = "email_interest_list.csv")
    }
  }) %>% bindEvent(input$submit)
}

# create app -------------------------------------------------------------------

shinyApp(ui = ui, server = server)