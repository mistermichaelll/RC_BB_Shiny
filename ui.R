
dates <- as_date(bb_shots$date)

library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Roanoke College Basketball"),
    h3(season_start_year, " - ", season_start_year + 1, " Season"),

    # sidebar & information 
    # =======================================================================================
    sidebarLayout(
        sidebarPanel(width = 3,
            selectInput("mw", 
                        "Men/Women:",
                        choice = c("Men", "Women")),
            selectInput("season", 
                        "Single Game/Full Season:",
                        choice = c("Game", "Season"),
                        selected = "Season"),
            selectInput("date",
                        "Select Date:",
                        choices = dates),
            selectInput("player",
                        "Select Shot Chart:", 
                        choices = roster_options_men),
            p("You can select a game or see the full season's stats, the team, and the name of a player to see their shot chart."), 
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
        mainPanel(width = 9, 
            uiOutput("plots"),
            htmlOutput("kable_table")
        )
    )
))
