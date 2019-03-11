#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
dates <- as_date(bb_shots$date)

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Roanoke College Basketball"),

    # Sidebar with various input
    sidebarLayout(
        sidebarPanel(
            h4("Input"),
            selectInput("mw", 
                        "Men/Women:",
                        choice = c("Men", "Women")),
            selectInput("date",
                        "Select Date:",
                        choices = dates), 
            textInput("player", "Select Player: ", value = ""),
            h4("Instructions"), 
            p("You can select a game and the team, you are also able to type the name of a player to see their shot chart (this must be exact)."), 
            br(), 
            p("If no output is displayed, ensure that the date/team match and there was a game that day."),
            br(), 
            p("For use in live situation, refresh page for updated information."), 
            br(),
            h4("Maintenance"),
            p("Please contact Michael Johnson at", a("mkjohnson@mail.roanoke.edu")),
            br(),
            h4("Contribute"),
            p("Learning Shiny? Have improvements? Submit a pull request on GitHub."),
            p(a("yahah"))
            ),

        # main panel
        mainPanel(
            # plotOutput("plots",height = "360px"),
            uiOutput("plots"),
            htmlOutput("kable_table")

        )
    )
))
