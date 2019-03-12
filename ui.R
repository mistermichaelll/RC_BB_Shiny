
dates <- as_date(bb_shots$date)

library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Roanoke College Basketball"),

    # sidebar & information 
    # =======================================================================================
    sidebarLayout(
        sidebarPanel(
            h4("Input"),
            selectInput("mw", 
                        "Men/Women:",
                        choice = c("Men", "Women")),
            selectInput("season", 
                        "Single Game/Full Season:",
                        choice = c("Game", "Season")),
            selectInput("date",
                        "Select Date:",
                        choices = dates), 
            textInput("player", "Select Player: ", value = ""),
            p("You can select a game or see the full season's stats, the team, you are also able to type the name of a player to see their shot chart (this must be exact)."), 
            br(), 
            p("If no output is displayed, ensure that the date/team match and there was a game that day."),
            br(), 
            p("For use in live situation, refresh page for updated information."), 
            br(),
            h4("Maintenance"),
            p("Please contact Michael Johnson at", a("mkjohnson@mail.roanoke.edu")),
            br(),
            h4("Contribute"),
            p("RC student working with R? Have improvements? Submit a pull request on GitHub."),
            p(a(".../mistermichaelll/RC_BB_Shiny"))
            ),
        # =======================================================================================
        
        # main panel
        # ----------
        mainPanel(
            uiOutput("plots"),
            htmlOutput("kable_table")
        )
    )
))
