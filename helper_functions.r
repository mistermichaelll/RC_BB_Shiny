# ==================================================================
# HELPER FUNCTIONS
# 
# PURPOSE: Some useful functions for analysis.  
# These range from simple plotting to transformations. 
# These are intended to mainly clean up code/make things 
# easier in the future. Note that some of these functions 
# require addditional packages. 
#
# ==================================================================

# ==================================================================
# NAME: dist_transform
# 
# PURPOSE: Since we have already established the transformation
# necessary for converting x/y coordinates to distance, it is  
# wrapped into a function here which creates an empty column 
# called "dist" into a given dataframe and then performs the 
# transformation through the ifelse function. 
# 
# PARAMS
# data: dataframe to be transformed 
#    x: column which contains x-coordinates
#    y: column which contains y-coordinates 
#
# EXAMPLE
# -----------
# bb_shots <- dist_transform(bb_shots, x, y)
# ==================================================================

dist_transform <- function(data, x, y){
    data$dist <- ifelse(data$x < 470, (sqrt(((data$x-50)^2) + ((data$y-250)^2))), 
           ifelse(data$x > 470, (sqrt(((data$x-885)^2) + ((data$y-250)^2))), NA))
    return(data)
}


# ==================================================================
# NAME: get_shot_class
# 
# PURPOSE: This function classifies shots into four categories:
# Rim, Mid, Corner3, and Reg3. Note that this does not replace 
# our original "type" column. This function also requires that 
# the dplyr package is in use. 
# 
# PARAMS
# data: dataframe to use 
#    x: column which contains x-coordinates
#    y: column which contains y-coordinates 
#
# EXAMPLE
# -----------
# basketball <- get_shot_class(basketball, x, y)
# ==================================================================

get_shot_class <- function(data, x, y){
    # get needed distance calculation 
    data$xt <- ifelse(data$x < 470, 
                      data$x, 
                      940 - data$x)
    data$yt <- ifelse(data$x < 470, 
                      data$y,
                      500 - data$y)
    data$dist2 <- sqrt(((data$xt-50)^2) + ((data$yt-250)^2))
    
    # get shot type into new column 
    data$shotClass <- ifelse(data$xt < 140 & (data$yt > 470 | data$yt < 30), 
                             "Corner3", ifelse(data$dist2 < 60, "Rim", 
                                               ifelse(data$dist2 > 240 & data$xt > 140, 
                                                      "Reg3", ifelse((data$yt > 170 & data$yt < 330) & data$xt < 190, "Paint", "Mid" ))))
    # make sure that regular shots are not 
    # confused with foul shots.
    # ---------------------------------
    data$shotClass <- ifelse(data$type != "Foul", data$shotClass, "FoulShot")
    
    # remove intermediary columns before returning new dataframe 
    data <- data %>% select(-xt, -yt, -dist2)
    
    # return new dataframe
    return(data)
}


# ===================================================================
# NAME: gg_glm 
# 
# PURPOSE: Easy creation of binary logistic regression chart in 
# ggplot. 
# 
# PARAMS
# -----------
# data: dataframe to be plotted 
#    x_col: column containing x-axis values (must be contained in "")
#    y_col: column containing y-axis values (must be contained in "")
#      ...: additional ggplot2 arguments. 
# 
# EXAMPLE
# -----------
# gg_glm(cars, "hp", "am")+
#   theme_light()
# ===================================================================

gg_glm <- function(data, x_col, y_col, ...){
    ggplot(data = data, aes_string(x = x_col, y = y_col))+
        geom_point(size = 1.5)+
        geom_smooth(method = glm,
                    method.args = list(family = "binomial"),
                    se = FALSE, col = "red", size = .55)
}

# ===================================================================
# NAME: theme_rc
# 
# PURPOSE: Custom theme for ggplots.
# 
# EXAMPLE
# -----------
# gg_glm(cars, "hp", "am")+
#   theme_rc()
# ===================================================================

theme_rc <- function(){
    theme_bw(base_family = "Avenir", base_size = 12) %+replace%
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) 
}

# ===================================================================
# NAME: gg_court_theme
#
# PURPOSE: Semi-void theme for plotting court images. 
# ===================================================================


gg_court_theme <- function(){
        theme_classic(base_size = 12) %+replace%
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              line = element_blank())
}

# ===================================================================
# NAME: create_player_shot_chart_grid
#
# PURPOSE: Utilizes similar code from our "plotting" scripts. This 
# function creates a grid of player shot charts. 
# ===================================================================

create_player_shot_chart_grid <- function(data, ...){
    basketball %>% 
        filter(team == "ROA") %>% 
        filter(madeMiss != "NA") %>% 
        filter(shotClass != "FoulShot") %>%
        ggplot()+
        annotation_custom(court)+
        aes(x = x_one_side * -1, y = y_one_side)+
        geom_point(aes(col = factor(madeMiss)), size = 1.2, alpha = .7)+
        facet_wrap(~name, ncol = 3)+
        coord_flip()+
        gg_court_theme()+
        xlim(-480, 0)+
        labs(title = '')+
        scale_color_discrete(name = "",
                             labels = c("Miss", "Made")) 
}


# ==========================================
# TIME FUNCTIONS
# Purpose: Helps us with datetime adjustment
# for defining a game. 
# ==========================================

# calculate hours
# ---------------
hrs <- function(u) { 
    x <- u * 3600
    return(x)
}

# calculate minutes
# -----------------
mns <- function(m) { 
    x <- m * 60
    return(x)
}

# ===========================================
# DATA VALIDATION FUNCTIONS 
# Purpose: help us with various data cleaning 
# needs as well as accounting for program-
# breaking test cases. 
# ===========================================

# ==========
# FIX COLS 
# ==========
# ensures that we have the necessary columns
# for creating our shot tables.
# If column doesn't exist, function creates 
# column and populates it with NAs. 
# -------------------------------------------
fix_cols <- function(data, column_name){
    add <-column_name[!column_name%in%names(data)] # determine whether column is in data
    
    if(length(add)!=0) data[add] <- NA # if the column DNE, replace with column of NA 
    data                               # returns modified data
}




