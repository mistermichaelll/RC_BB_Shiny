library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # reactive dataframe, responds to key pieces of 
    # user input to provide basis for the rest of 
    # the app (plotting, table, etc.)
    # -------------------------------------------------
    
    # define some reactive variables
    team_input <- reactive({input$team})
    
    mw_input <- reactive({input$mw})
    
    date_input <- reactive({input$date})
    
    autoRefresh <- reactiveTimer(120000)
    
    
    # define reactive plot dataframe
    # -------------------------------
    gg_info <- reactive({
        autoRefresh()
        bb_shots <- 
            bb_shots %>% 
            filter(date >= as_date(date_input()) & date <= as_date(date_input()) + 1) %>%
            filter(teamMW == mw_input())
        
        bb_shots <- if(mw_input() == "Men"){
            bb_shots %>% left_join(mens_roster, bb_shots, by = "jerseyNumber")
        } else{
            bb_shots %>% left_join(womens_roster, bb_shots, by = "jerseyNumber")
        }
            

    })

    
    # define the dataframe
    # ---------------------
    dash_data <- reactive({
        
        autoRefresh()

        # men/women...
        # -----------------------
            bb_shots <- 
                if(mw_input() == "Men"){
                bb_shots %>%
                    filter(teamMW == "Men")
                } else{
                    bb_shots %>%
                        filter(teamMW == "Women")
                }
            
        # # move on to date...
        # # ---------------------
        bb_shots <-
            bb_shots %>%
            filter(date >= as_date(date_input()) &
                       date <= as_date(date_input()) + 1)
        
            
            basketball <- bb_shots %>% filter(team == "ROA")
            
            # merge roster with dataframe
            # ---------------------------
            basketball <- if(mw_input() == "Men"){
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
            
            
            # create table for shots identified as "made"
            # ---------------------------------------------
            tbl_made <- basketball %>%
                group_by(name, type) %>%
                summarize(countMade = sum(madeMiss == 1)) %>%
                spread(key = type, value = countMade) %>%
                dplyr::rename(foulMade = Foul,
                              fshotMade = FShot,
                              fgMade = Shot)
            
            # create table for shots identified as "missed"
            # ----------------------------------------------
            tbl_miss <- basketball %>%
                group_by(name, type) %>%
                summarize(countMiss = sum(madeMiss == 0)) %>%
                spread(key = type, value = countMiss) %>%
                dplyr::rename(foulMiss = Foul,
                              fshotMiss = FShot,
                              fgMiss = Shot)
            
            # create table for points made
            # ----------------------------
            points_made <- basketball %>%
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
                replace_na(list(foulMade = 0, fshotMade = 0, fgMade = 0,
                                foulMiss = 0, fshotMiss = 0, fgMiss = 0,
                                Corner3 = 0, Mid = 0, Reg3 = 0, Rim = 0, FoulShot = 0, Paint = 0))
            
            
            # data validation
            # ensure table is generated even if
            # certain shot types are not recorded in a game
            # ---------------------------------------------
            full_table <-
                fix_cols(full_table, c("foulMade", "fshotMade", "fgMade",
                                       "foulMiss", "fshotMiss", "fgMiss",
                                       "Corner3", "Mid", "Reg3", "Rim", "FoulShot", "Paint"))
            
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
                mutate(fgAtt = fgMade + fgMiss + fshotMade, ftAtt = foulMiss + foulMade) %>%
                select(-fgMiss, -fshotMiss, -foulMiss) %>%
                select(name, fgMade, fshotMade, fgAtt, foulMade,
                       ftAtt, Rim, Mid, Paint, Corner3, Reg3, totalPointsMade) %>%
                mutate(new = fgMade + fshotMade) %>% # intermediate variable to ensure proper %
                mutate(fgper = ((new / fgAtt) * 100), ftper = (foulMade / ftAtt * 100),
                       trueper = 100 * (totalPointsMade / (2 * (fgAtt + ftAtt)))) %>%
                select(-new) # remove intermediate variable
            
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
                dplyr::rename("Made" = fgMade, "Att." = fgAtt, "Made" = foulMade, 
                       "Att." = ftAtt, "And1" = fshotMade, "Total Pts." = totalPointsMade, 
                       "FG %" = fgper, "FT %" = ftper, "True %" = trueper, "Name" = name)
            
            full_table
    
    })
    
    width <- reactive({
        width <- ifelse(input$player == "", "100%", "68%")
    })
    
    output$contents <- renderPlot({
        
        
        if (input$player == ""){
        chart_roa <- gg_info() %>% 
            filter(team == "ROA") %>%
            filter(shotClass != "FoulShot") %>%
            filter(madeMiss != "NA") %>% 
            ggplot()+
            annotation_custom(court)+
            aes(x = -x_one_side, y = -y_one_side)+
            geom_point(aes(col = factor(madeMiss), shape = factor(madeMiss)), alpha = .9, size = 1.75)+
            coord_flip()+
            gg_court_theme()+
            xlim(-475, 5)+
            labs(title = "ROA", color = NULL)+
            scale_color_discrete(name = "",
                                 labels = c("Miss", "Made"))+
            ylim(-510, 3)+
            scale_shape_manual(values = c(4, 1), guide = F)+
            scale_color_manual(values = c("#ad0000", "#0e6b00"), labels = c("Miss", "Made"))
        
        chart_opp <-  gg_info() %>% 
            filter(team == "OPP") %>% 
            filter(shotClass != "FoulShot") %>%
            filter(madeMiss != "NA") %>% 
            ggplot()+
            annotation_custom(court)+
            aes(x = -x_one_side, y = -y_one_side)+
            geom_point(aes(col = factor(madeMiss), shape = factor(madeMiss)), alpha = .9, size = 1.75)+
            coord_flip()+
            gg_court_theme()+
            xlim(-475, 5)+
            labs(title = "OPP", color = NULL)+
            ylim(-510, 3)+
            scale_shape_manual(values = c(4, 1), guide = F)+
            scale_color_manual(values = c("#ad0000", "#0e6b00"), labels = c("Miss", "Made"))

        
        legend <- cowplot::get_legend(chart_roa)
        grid_shots <- cowplot::plot_grid(chart_roa + theme(legend.position = "none"),
                                         chart_opp + theme(legend.position = "none"),
                                         hjust = -.45,
                                         align = "h",
                                         vjust = 25,
                                         ncol = 2,
                                         rel_widths = c(.95, .95),
                                         rel_heights = c(.5, .5))
        
        team_plot <- cowplot::plot_grid(grid_shots, legend, rel_widths = c(.29, .05))
        
        
        team_plot} else{
            
            player_plot <- 
                gg_info() %>% 
                filter(team == "ROA") %>% 
                filter(shotClass != "FoulShot") %>%
                filter(madeMiss != "NA") %>% 
                filter(name == input$player) %>%
                ggplot()+
                annotation_custom(court)+
                aes(x = -x_one_side, y = -y_one_side)+
                geom_point(aes(col = factor(madeMiss), shape = factor(madeMiss)), alpha = .9, size = 2.45)+
                coord_flip()+
                gg_court_theme()+
                xlim(-475, 5)+
                labs(title = input$player, color = NULL)+
                ylim(-510, 3)+
                scale_shape_manual(values = c(4, 1), guide = F)+
                scale_color_manual(values = c("#ad0000", "#0e6b00"), labels = c("Miss", "Made"))
            
            
            player_plot
        
        }
        
        
    })
    
    output$plots <- renderUI({
        plotOutput("contents", height = "360px", width = width())
    })
    
    
    
    
    # render table as kable
    # ---------------------
    output$kable_table <- renderText({
        
        kable(dash_data()) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                          full_width = F, font_size = 14, position = "left") %>%
            add_header_above(c(" " = 1, "Field Goals" = 3, "Foul Shots" = 2, "Points Made" = 6,
                               "Efficiency" = 3))
        
    })

    

    

})