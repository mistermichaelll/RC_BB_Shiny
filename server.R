library(shiny)

shinyServer(function(input, output, session) {
    
    # define some needed reactive variables
    # -------------------------------------
    team_input <- reactive({
        input$team
    })
    
    mw_input <- reactive({
        input$mw
    })
    
    date_input <- reactive({
        input$date
    })
    
    season_input <- reactive({
        input$season
    })
    
    # reactive dates
    # --------------
    
    # refresh certain pieces every 
    # two (?) minutes
    # -----------------------------
    autoRefresh <- reactiveTimer(120000)
    
    
    # define reactive plot dataframe
    # -------------------------------
    gg_info <- reactive({
        
        autoRefresh()
        # season functionality
        # --------------------
        if (season_input() == "Game"){
            bb_shots <-
                bb_shots %>%
                filter(date >= as_date(date_input()) &
                           date < as_date(date_input()) + 1) %>%
                filter(teamMW == mw_input())
        } else{ 
            bb_shots <- 
                bb_shots %>%
                filter(teamMW == mw_input())
        }
        
        bb_shots <- if (mw_input() == "Men") {
            bb_shots %>% left_join(mens_roster, bb_shots, by = "jerseyNumber")
        } else{
            bb_shots %>% left_join(womens_roster, bb_shots, by = "jerseyNumber")
        }
        
        
    })
    
    observe({
        
        if (input$mw == "Men"){
            updateSelectInput(session, "date", "Select Date:", choices = men_dates)
        } else{ 
            updateSelectInput(session, "date", "Select Date:", choices = women_dates)
        }
        
    })
    
    
    # define the dataframe & clean it
    # -------------------------------
    dash_data <- reactive({
        
        autoRefresh()
        
        # men/women...
        # ------------
        bb_shots <-
            if (mw_input() == "Men") {
                bb_shots %>%
                    filter(teamMW == "Men")
            } else{
                bb_shots %>%
                    filter(teamMW == "Women")
            }
        
        # move on to date...
        # ------------------
        
        # season functionality!
        # ---------------------
        if (season_input() == "Game"){
            bb_shots <-
                bb_shots %>%
                filter(date >= as_date(date_input()) & date <= as_date(date_input()) + 1)
        } else{
            bb_shots <- 
                bb_shots
        }
        
        # we only want the table for ROA
        # ------------------------------
        basketball <- bb_shots %>% filter(team == "ROA")
        
        # merge roster with dataframe
        # ---------------------------
        basketball <- if (mw_input() == "Men") {
            basketball %>% left_join(mens_roster, basketball, by = "jerseyNumber")
        } else{
            basketball %>% left_join(womens_roster, basketball, by = "jerseyNumber")
        }
        
        # replace NA values with "NOT IN ROSTER" so we
        # know who is mising.
        # ----------------------------------------------
        basketball$name <- tidyr::replace_na(basketball$name, "NOT IN ROSTER")
        
        # make `name` a factor (for plotting in report)
        # ----------------------------------------------
        basketball$name <- as.factor(basketball$name)
        
        # validation
        # ----------
        validate(
            need(
                try(
                    basketball %>%
                        group_by(name, type) %>%
                        summarize(countMade = sum(madeMiss == 1)) %>%
                        spread(key = type, value = countMade) %>%
                        dplyr::rename(foulMade = Foul,
                                      fshotMade = FShot,
                                      fgMade = Shot)
                    
                ), ""
            )
        )
        
        # create table for shots identified as "made"
        # ---------------------------------------------
        tbl_made <-
            basketball %>%
            group_by(name, type) %>%
            summarize(countMade = sum(madeMiss == 1)) %>%
            spread(key = type, value = countMade) %>%
            dplyr::rename(foulMade = Foul,
                          fshotMade = FShot,
                          fgMade = Shot)
        
        # create table for shots identified as "missed"
        # ----------------------------------------------
        tbl_miss <-
            basketball %>%
            group_by(name, type) %>%
            summarize(countMiss = sum(madeMiss == 0)) %>%
            spread(key = type, value = countMiss) %>%
            dplyr::rename(foulMiss = Foul,
                          fshotMiss = FShot,
                          fgMiss = Shot)
        
        # create table for points made
        # ----------------------------
        points_made <- 
            basketball %>%
            group_by(name, shotClass) %>%
            summarize(points = sum(points)) %>%
            spread(key = shotClass, value = points)
        
        # merge tables using "jerseyNumber" as the key
        # --------------------------------------------
        full_table <- left_join(tbl_made, tbl_miss, by = "name")
        full_table <- left_join(full_table, points_made, by = "name")
        
        
        # remove NA values
        # (replace with 0)
        # --------------------
        full_table <-
            full_table %>%
            replace_na(
                list(
                    foulMade = 0,
                    fshotMade = 0,
                    fgMade = 0,
                    foulMiss = 0,
                    fshotMiss = 0,
                    fgMiss = 0,
                    Corner3 = 0,
                    Mid = 0,
                    Reg3 = 0,
                    Rim = 0,
                    FoulShot = 0,
                    Paint = 0
                )
            )
        
        # data validation
        # ensure table is generated even if
        # certain shot types are not recorded in a game
        # ---------------------------------------------
        full_table <-
            fix_cols(
                full_table,
                c(
                    "foulMade",
                    "fshotMade",
                    "fgMade",
                    "foulMiss",
                    "fshotMiss",
                    "fgMiss",
                    "Corner3",
                    "Mid",
                    "Reg3",
                    "Rim",
                    "FoulShot",
                    "Paint"
                )
            )
        
        # additional check 
        # (some games act wonky)
        # ----------------------
        full_table$Corner3 <- replace_na(full_table$Corner3, 0)
        full_table$Paint <- replace_na(full_table$Paint, 0)
        
        # arrange table by points made
        # -------------------------------
        full_table <-
            full_table %>%
            mutate(totalPointsMade = Corner3 + Mid + Paint + Reg3 + Rim + foulMade) %>%
            arrange(desc(totalPointsMade))
        
        # creates "attempted" columns, rearranges columns
        # as specified by Dr. Minton, renames columns.
        # -----------------------------------------------
        full_table <-
            full_table %>%
            mutate(fgAtt = fgMade + fgMiss + fshotMade,
                   ftAtt = foulMiss + foulMade) %>%
            select(-fgMiss,-fshotMiss,-foulMiss) %>%
            select(
                name,
                fgMade,
                fshotMade,
                fgAtt,
                foulMade,
                ftAtt,
                Rim,
                Mid,
                Paint,
                Corner3,
                Reg3,
                totalPointsMade
            ) %>%
            mutate(field1 = fgMade + fshotMade) %>% # intermediate variable to ensure proper %
            mutate(
                fgper = ((field1 / fgAtt) * 100),
                ftper = (foulMade / ftAtt * 100),
                trueper = 100 * ((totalPointsMade) / ((2 * (fgAtt)) + (.88 * ftAtt)))
            ) %>%
            select(-field1) # remove intermediate variable
        
        # replace NaN from calculations
        # -----------------------------
        full_table$ftper <- tidyr::replace_na(full_table$ftper, 0)
        
        # round decimals
        # --------------
        full_table$fgper <- round(full_table$fgper, 1)
        full_table$ftper <- round(full_table$ftper, 1)
        full_table$trueper <- round(full_table$trueper, 1)
        
        # rename columns
        # ---------------
        full_table <-
            full_table %>%
            dplyr::rename(
                "Made" = fgMade,
                "Att." = fgAtt,
                "Made" = foulMade,
                "Att." = ftAtt,
                "And1" = fshotMade,
                "Total Pts." = totalPointsMade,
                "FG %" = fgper,
                "FT %" = ftper,
                "True %" = trueper,
                "Name" = name
            )
        
        full_table
        
    })
    # need width to be reactive based on whether
    # input is for the full team (blank) or for 
    # a specific player.
    # ------------------------------------------
    width <- reactive({
        width <- ifelse(input$player == "", "100%", "63%")
    })
    
    # render plot based on "Player" input
    # -----------------------------------
    output$contents <- renderPlot({
        
        if (input$player == "") {
            chart_roa <- 
                gg_info() %>%
                filter(team == "ROA") %>%
                filter(shotClass != "FoulShot") %>%
                filter(madeMiss != "NA") %>%
                ggplot()+
                annotation_custom(court)+
                aes(x = -x_one_side, y = -y_one_side)+
                geom_point(aes(
                    col = factor(madeMiss),
                    shape = factor(madeMiss)), alpha = .9, size = 3.75)+
                coord_flip()+
                gg_court_theme()+
                xlim(-470, -5)+
                labs(title = "ROA", color = NULL, shape = NULL)+
                scale_color_discrete(name = "",
                                     labels = c("Miss", "Made"))+
                ylim(-504, 2)+
                scale_shape_manual(labels = c("Miss", "Made"), values = c(4, 1))+
                scale_color_manual(name = "madeMiss",
                                   values = c("#161616", "#2c4fa0"),
                                   labels = c("Miss", "Made"), guide = F)
            
            chart_opp <-  
                gg_info() %>%
                filter(team == "OPP") %>%
                filter(shotClass != "FoulShot") %>%
                filter(madeMiss != "NA") %>%
                ggplot()+
                annotation_custom(court) +
                aes(x = -x_one_side, y = -y_one_side) +
                geom_point(aes(col = factor(madeMiss), shape = factor(madeMiss)), alpha = .9, size = 3.75)+
                coord_flip()+
                gg_court_theme()+
                xlim(-470, -5)+
                labs(title = "OPP", color = NULL) +
                ylim(-504, 2)+
                scale_shape_manual(labels = c("Miss", "Made"), values = c(4, 1))+
                scale_color_manual(name = "madeMiss",
                                   values = c("#161616", "#2c4fa0"),
                                   labels = c("Miss", "Made"), guide = F)
            
            validate(
                need(
                    try(cowplot::get_legend(chart_roa)), 
                    "No game for this team on this date. Change to men/women's team or pick a different date.")
            )
            
            legend <- cowplot::get_legend(chart_roa)
            
            grid_shots <-
                cowplot::plot_grid(
                    chart_roa + theme(legend.position = "none"),
                    chart_opp + theme(legend.position = "none"),
                    hjust = -.45,
                    align = "h",
                    vjust = 25,
                    ncol = 2,
                    rel_widths = c(.95, .95),
                    rel_heights = c(.5, .5)
                )
            
            team_plot <- cowplot::plot_grid(grid_shots, legend, rel_widths = c(.29, .05))
            
            team_plot
            
        } else{
            player_plot <-
                gg_info() %>%
                filter(team == "ROA") %>%
                filter(shotClass != "FoulShot") %>%
                filter(madeMiss != "NA") %>%
                filter(name == input$player) %>%
                ggplot()+
                annotation_custom(court)+
                aes(x = -x_one_side, y = -y_one_side)+
                geom_point(aes(
                    col = factor(madeMiss),
                    shape = factor(madeMiss)), alpha = .9, size = 4.5)+
                coord_flip()+
                gg_court_theme()+
                xlim(-470, -5)+
                labs(title = input$player, color = NULL, shape = NULL)+
                ylim(-502, 2)+
                scale_shape_manual(labels = c("Miss", "Made"), values = c(4, 1))+
                scale_color_manual(name = "madeMiss",
                                   values = c("#161616", "#2c4fa0"),
                                   labels = c("Miss", "Made"), guide = F)
            
            player_plot
            
        }
    })
    
    # render the plot to the UI 
    # w/ reactive width
    # -------------------------
    output$plots <- renderUI({
        plotOutput("contents", height = "380px", width = width())
    })
    
    # render table as kable
    # ---------------------
    output$kable_table <- renderText({
        kable(dash_data()) %>%
            kable_styling(
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                font_size = 14,
                position = "left"
            ) %>%
            add_header_above(c(
                " " = 1,
                "Field Goals" = 3,
                "Foul Shots" = 2,
                "Points Made" = 6,
                "Efficiency" = 3
            ))
    })
})
