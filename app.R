library(pacman)
pacman::p_load(plyr, tidyverse, plotly, DT, shiny, shinythemes,
               shinydashboard, fontawesome, htmltools, scales, reactablefmtr)

# Read the Raw Data ------------------------------------------------------------

t20 <- readRDS("./data/t20.rds")

# Data Pre-processing -----------------------------------------------------------

t20_bkp <- t20
t20[t20 == ""] <- NA  ## Filling the blank entries as NA
t20[is.na(t20)] <- 0  ## Filling the NAs as 0

t20$over = ceiling(t20$ball) ## Creating a column for nth over number (n = 1 to 20)
t20$over_type = ifelse(t20$over >= 1 & t20$over <= 6, "Powerplay",
                       ifelse(t20$over >= 7 & t20$over <= 16, "Middle Over", "Death Over"))
t20$isDot = ifelse(t20$runs_off_bat == 0, 1, 0)         # No of dots
t20$isOne = ifelse(t20$runs_off_bat == 1, 1, 0)         # No of 1's
t20$isTwo = ifelse(t20$runs_off_bat == 2, 1, 0)         # No of 2's
t20$isThree = ifelse(t20$runs_off_bat == 3, 1, 0)       # No of 3's
t20$isFour = ifelse(t20$runs_off_bat == 4, 1, 0)        # No of 4's
t20$isSix = ifelse(t20$runs_off_bat == 6, 1, 0)         # No of 6's
t20$isOut = ifelse(t20$wicket_type != "0", 1, 0)        # No of out
t20$Runs = t20$runs_off_bat + t20$extras                # Team Runs
tot_mat = t20 |> select(match_id) |>                    # Total Number of matches
    unique() |> 
    nrow() 

last_date <- t20$start_date |> 
    unique() |> 
    as.Date(format = "%Y-%m-%d") |>
    max()
m <- format(last_date, "%m") |> 
    as.numeric()
d <- format(last_date, "%d")
y <- format(last_date, "%Y")
last_date_updated <- paste0(month.name[m], " ", d,", ",y)

# For getting all players name as a vector -------------------------------------

players_name_list = unique(c(t20$striker, t20$non_striker, t20$bowler)) |> 
    as.character() |> sort()

# Batting Analysis -------------------------------------------------------------

t20 <- t20[t20$innings == 1 | t20$innings == 2, ]   # Removing Super-Over Balls

# Shiny UI ---------------------------------------------------------------------

header = dashboardHeader(title = "Performance Analysis in T20I",
                         titleWidth = 450)

sidebar = dashboardSidebar(
    selectInput("player_name", "Select your Cricketer", choices = as.character(players_name_list)),
        sidebarMenu(
            menuItem("Batting Analysis", tabName = "bat", icon = icon("untappd", lib = "font-awesome")),
            menuItem("Bowling Analysis", tabName = "bowl", icon = icon("basketball-ball", lib = "font-awesome")),
            menuItem("About", tabName = "about", icon = icon("address-card", lib = "font-awesome"))
            ))


body = dashboardBody(
    tabItems(
        tabItem(
            tabName = "bat",
            span(textOutput("name", inline = T), style = "font-size: 45px; font-style: bold"),
            tags$br(),
            fluidRow(
                column(12,
                   valueBoxOutput("value1", width = 2),
                   valueBoxOutput("value2", width = 2)))
        )))



ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "black")

server <- function(session, input, output) {
    
    stat_react <- reactive({
        ## Computing Total Number of Dismissal
        
        diss <- t20 |> 
            dplyr::filter(player_dismissed == input$player_name) |>
            dplyr::select(match_id,  wicket_type, over_type, bowler)
        
        plyr_data <- t20 |> 
            dplyr::filter(striker == input$player_name & wides == 0)
        
        innings <- t20 |> 
            dplyr::filter(striker == input$player_name | non_striker == input$player_name) |> 
            dplyr::summarise(Innings = n_distinct(match_id)) |> as.numeric()
        
        stat1 <- plyr_data |> dplyr::filter(striker == input$player_name) |> 
            dplyr::group_by(start_date, match_id) |> 
            dplyr::summarise(Runs = sum(runs_off_bat), Balls = length(runs_off_bat),
                             SR = round(Runs/Balls*100,2), 
                             Fours = sum(isFour), Sixes = sum(isSix),
                             Dots = sum(isDot))
        
        stat2 <- left_join(stat1, diss) ## Innings wise Player's Data
        
        stat2$wicket_type <- ifelse(is.na(stat2$wicket_type), "not out", stat2$wicket_type)
        stat2$isThirty <- ifelse(stat2$Runs >= 30 & stat2$Runs < 50, 1, 0) # No of 30's
        stat2$isFifty <- ifelse(stat2$Runs >= 50 & stat2$Runs < 100, 1, 0) # No of 50's
        stat2$isHundred <- ifelse(stat2$Runs >= 100, 1, 0) # No of 100's
        stat2$isNO <- ifelse(stat2$wicket_type == "not out", 1, 0) ## Not Out Innings flag
        
        stat3 <- plyr_data |> 
            dplyr::filter(striker == input$player_name) |>
            dplyr::group_by(match_id) |> 
            dplyr::select(match_id, innings, bowling_team, venue) |> unique()
        
        stat4 <- left_join(stat2, stat3)   
        
        stat5 <- t20 |> 
            dplyr::group_by(match_id, innings) |> 
            dplyr::summarise(team_runs = sum(Runs))
        
        stat6 <- left_join(stat4, stat5, by = c("match_id", "innings"))  
        
        stat6$contribution = round(stat6$Runs/stat6$team_runs*100, 2)
        stat6$Year = substr(stat6$start_date, 1, 4)
        
        stat6$bowler <- ifelse(stat6$wicket_type == "run out", NA, stat6$bowler)
        stat6
    })

    # Output -----------------------------------------------------------------------

                
    output$name <- renderText({paste("Career Overview of", input$player_name)})
    
    output$value1 <- renderValueBox({
        valueBox(nrow(stat_react()), 
                 "Total Innings Played", 
                 color = "light-blue")
    })
    
    output$value2 <- renderValueBox({
        valueBox(sum(stat_react()$Runs), 
                 "Total Runs Scored", 
                 color = "blue")
    })
}


shinyApp(ui = ui, server = server)