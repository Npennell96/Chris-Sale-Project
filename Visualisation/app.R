# Packges -----
    library(shiny) # shiny
    library(htmlwidgets) # html and javascript
    library(shinythemes) # shiny themes
    library(readr) # read in files
    library(jsonlite) # read json
    library(dplyr) # cleaing data
    library(tidyr) # cleaning data
    library(data.table) # cleaning data
    library(lubridate) # date formation
    library(XML) # scraping
    library(RCurl) # scraping
    library(rlist) # scraping
    library(plotly) # plot
    library(knitr) # html table
    library(kableExtra) # html table
    library(RColorBrewer) # colour pallet

# Functions----------------
    # Changing class of varibles
        convert.magic <- function(obj, type)
        {
            FUN1 <- switch(type,
                           character = as.character,
                           numeric = as.numeric,
                           factor = as.factor,
                           logical = as.logical)
            out <- lapply(obj, FUN1)
            as.data.frame(out)
        } 

    # formate function, formates baseballr to the formate I like
        baseballR_full_format <- function(data){
            # add true outcome
                data$true_outcome <- data$description
                
                data$true_outcome <- ifelse(data$true_outcome %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score"),
                                            data$events, data$true_outcome)
            
            # change data type
                factor.varibles  <- c("pitch_type", "player_name", "batter", "pitcher", "events", "game_type", "stand", "p_throws" ,
                                      "home_team", "away_team", "type", "bb_type", "balls", "strikes", "game_year", "outs_when_up",
                                      "inning_topbot")
                logical.varibles <- c("on_1b", "on_2b", "on_3b")
                
                data$inning <- as.numeric(data$inning)
                data[,factor.varibles] <- convert.magic(data[,factor.varibles], "factor")
                data[,logical.varibles] <- convert.magic(data[,logical.varibles], "logical")
                
            # relabel levels
                # pitch_type
                    levels(data$pitch_type) <- list("Two-Seam Fastball" = "FT",
                                                    "Slider" = "SL",
                                                    "Changeup" = "CH",
                                                    "Four-Seam Fastball" = "FF",
                                                    "Intentional Walk" = "IN",
                                                    "Fastball" = "FA",
                                                    "Unknow" = "UN",
                                                    "Pitch Out" = "PO",
                                                    "Splitter" = "FS")
                # Game Type
                    levels(data$game_type) <- list("Regular Season Game" = "R",
                                                   "Divisional Playoff Game" = "D",
                                                   "League Playoff Game" = "L",
                                                   "World Serires Game" = "W")
                # Stand
                    levels(data$stand) <- list("Left" = "L",
                                               "Right" = "R")
                # p_throws
                    levels(data$p_throws) <- list("Left" = "L",
                                                  "Right" = "R")
            
            # Remove unkknow pitches
                data <- data %>% filter(!is.na(pitch_type))
            # remove unwatned pitches
                `%!in%` = Negate(`%in%`)
                data <- data %>% filter(pitch_type %!in% c("Unknow", "Intentional Walk", "Pitch Out"))

            
            
            return(data)
        }

    #  put data into time serires format (by day) to plot
        time_series_day <- function(data){
            # Pitches by date and type
                pitch.by.date <- table(data$game_date, data$pitch_type, 
                                       dnn = c("date", "pitch_type")) %>% as.data.frame()
            # Pitches by date
                total.pitch.date <- table(data$game_date, dnn = "date") %>% as.data.frame()
            # Join and order
                pitch.by.date <- full_join(pitch.by.date, total.pitch.date, by = "date")
                pitch.by.date <- pitch.by.date %>% arrange(date)
            # Formate and adding percentage as well as year days
                colnames(pitch.by.date) <- c("date", "pitch_type", "type_freq", "date_freq")
                pitch.by.date$perc <- pitch.by.date$type_freq / pitch.by.date$date_freq
            # Removing zeros
                pitch.by.date[pitch.by.date == 0] <- NA
                pitch.by.date$date <- pitch.by.date$date %>% ymd()
            # creat gap between years
                # list of years
                    years <- list(as.Date("2011-01-01"), as.Date("2012-01-01"), as.Date("2013-01-01"), 
                                  as.Date("2014-01-01"), as.Date("2015-01-01"), as.Date("2016-01-01"),
                                  as.Date("2017-01-01"), as.Date("2018-01-01"), as.Date("2019-01-01"))
            # initialize data frame
                break.year <- data.frame("date" = as.Date(character()),
                                         "pitch_type" = character(),
                                         "type_freq" = double(),
                                         "date_freq" = double(),
                                         "rerc" = double())
            # for loop to append to break.year
                for (i in years) {
                    # what will be appeneded
                        holder <-  data.frame("date" = i,
                                              "pitch_type" = c("Slider", "Changeup", "Four-Seam Fastball", "Two-Seam Fastball"),
                                              "type_freq" = NA,
                                              "date_freq" = NA,
                                              "perc" = NA)
                    # append
                        break.year <- break.year %>% bind_rows(holder)
                }
            # append break year
                pitch.by.date <- pitch.by.date %>% bind_rows(break.year)
            
            # order by date
                pitch.by.date <- pitch.by.date[order(as.Date(pitch.by.date$date, format="%Y-%m-%Y")),]
            
            return(pitch.by.date)
            
        }
        
    #  put data into time serires format (by year) to plot  
        time_series_year <- function(data){
            
            # Pitches by date and type
                pitch.by.date <- table(data$game_year, data$pitch_type, 
                                       dnn = c("date", "pitch_type")) %>% as.data.frame()
            # Pitches by date
                total.pitch.date <- table(data$game_year, dnn = "date") %>% as.data.frame()
            # Join and order
                pitch.by.date <- full_join(pitch.by.date, total.pitch.date, by = "date")
                pitch.by.date <- pitch.by.date %>% arrange(date)
            # Formate and adding percentage as well as year days
                colnames(pitch.by.date) <- c("date", "pitch_type", "type_freq", "date_freq")
                pitch.by.date$perc <- pitch.by.date$type_freq / pitch.by.date$date_freq
            # Removing zeros
                pitch.by.date[pitch.by.date == 0] <- NA
                
                pitch.by.date$date <- paste(pitch.by.date$date, '-01-01', sep = '')
                pitch.by.date$date <-  pitch.by.date$date %>% ymd()
            
            return(pitch.by.date)
            
        }
    
    # format json files
        json.format <- function(data){
            
            # take wanted coloums
                data <- data[, c(1:4, 14, 15)]
            # rename coloums to help join
                colnames(data) <- c("plate_x", "plate_z", "release_speed", "pitch_type", "game_date", "link")
            # format the same as the csv to help join
                numeric.varibles <- c("plate_x", "plate_z", "release_speed")
                data[,numeric.varibles] <- convert.magic(data[,numeric.varibles], "numeric")

                data$release_speed <- round(data$release_speed, 1)
                
                data$pitch_type <- data$pitch_type %>% as.factor()
                levels(data$pitch_type) <- list("Two-Seam Fastball" = "SIFT",
                                                "Slider" = "SL",
                                                "Changeup" = "CH",
                                                "Four-Seam Fastball" = "FF")
                data$game_date <- data$game_date %>% strtrim(10)
                data$game_date <- data$game_date %>% ymd()
            # creat full link
                data$link <- paste('https://baseballsavant.mlb.com/sporty-videos?playId=', data$link, sep = '')
            
            return(data)
            
        }


# load in files ------------
    # baseball r csv======
        # 2010
            data.2010 <- read_csv("Data/baseballr data/chris sale 2010.csv")
            data.2010 <- data.2010[ ,c(2:4,7:11,18:23,25:40)]
            data.2010 <- baseballR_full_format(data.2010)
        # 2011
            data.2011 <- read_csv("Data/baseballr data/chris sale 2011.csv")
            data.2011 <- data.2011[ ,c(2:4,7:11,18:23,25:40)]
            data.2011 <- baseballR_full_format(data.2011)
        # 2012
            data.2012 <- read_csv("Data/baseballr data/chris sale 2012.csv")
            data.2012 <- data.2012[ ,c(2:4,7:11,18:23,25:40)]
            data.2012 <- baseballR_full_format(data.2012)
        # 2013
            data.2013 <- read_csv("Data/baseballr data/chris sale 2013.csv")
            data.2013 <- data.2013[ ,c(2:4,7:11,18:23,25:40)]
            data.2013 <- baseballR_full_format(data.2013)
        # 2014
            data.2014 <- read_csv("Data/baseballr data/chris sale 2014.csv")
            data.2014 <- data.2014[ ,c(2:4,7:11,18:23,25:40)]
            data.2014 <- baseballR_full_format(data.2014)
        # 2015
            data.2015 <- read_csv("Data/baseballr data/chris sale 2015.csv")
            data.2015 <- data.2015[ ,c(2:4,7:11,18:23,25:40)]
            data.2015 <- baseballR_full_format(data.2015)
        # 2016
            data.2016 <- read_csv("Data/baseballr data/chris sale 2016.csv")
            data.2016 <- data.2016[ ,c(2:4,7:11,18:23,25:40)]
            data.2016 <- baseballR_full_format(data.2016)
        # 2017
            data.2017 <- read_csv("Data/baseballr data/chris sale 2017.csv")
            data.2017 <- data.2017[ ,c(2:4,7:11,18:23,25:40)]
            data.2017 <- baseballR_full_format(data.2017)
        # 2018
            data.2018 <- read_csv("Data/baseballr data/chris sale 2018.csv")
            data.2018 <- data.2018[ ,c(2:4,7:11,18:23,25:40)]
            data.2018 <- baseballR_full_format(data.2018)
        # 2019
            data.2019 <- read_csv("Data/baseballr data/chris sale 2019.csv")
            data.2019 <- data.2019[ ,c(2:4,7:11,18:23,25:40)]
            data.2019 <- baseballR_full_format(data.2019)
        # Join
            full.data <- data.2010 %>% bind_rows(data.2011) %>% bind_rows(data.2012) %>% bind_rows(data.2013) %>% 
                bind_rows(data.2014) %>% bind_rows(data.2015) %>% bind_rows(data.2016) %>% bind_rows(data.2017) %>% 
                bind_rows(data.2018) %>% bind_rows(data.2019)
        # reomve vaules with pitch types that really occur and the left over levels
          full.data <- full.data %>% group_by(pitch_type) %>% filter(n()>= 50) %>% ungroup()
          full.data$pitch_type <- factor(full.data$pitch_type)
            
    # baseball player ID ======
        player.id <- read_csv("Data/Player ID/playerid_list.csv")
        player.id <- player.id[, c(1,2,5)]
        player.id$FULLNAME <- paste(player.id$FIRSTNAME, player.id$LASTNAME)
        player.id <- player.id[, c(3,4)]
        player.id$MLBCODE <- as.character(player.id$MLBCODE)
        colnames(player.id) <- c("batter", "batter_name")

    # baseball field csv ======
        # load in field
            fields <- read_csv("Data/Baseball field shape/fields.csv")
        # formate
            fields[,c("team", "segment")] <- convert.magic(fields[,c("team", "segment")], "factor")
        # subset
            field <- fields %>% filter(team == "royals") 
            field.list <- split( field, f =  field$segment)
                    
    # Load in json data =====    
        # CH
            json.CH.2019 <- read_json('Data/json/2019/CH_2019.json', simplifyVector = TRUE) %>% as.data.frame()
            json.CH.2019 <- json.CH.2019 %>% json.format()
        # FF
            json.FF.2019 <- read_json('Data/json/2019/FF_2019.json', simplifyVector = TRUE) %>% as.data.frame()
            json.FF.2019 <- json.FF.2019 %>% json.format()
        # SIFT
            json.SIFT.2019 <- read_json('Data/json/2019/SIFT_2019.json', simplifyVector = TRUE) %>% as.data.frame()
            json.SIFT.2019 <- json.SIFT.2019 %>% json.format()
        # SL
            json.SL.2019 <- read_json('Data/json/2019/SL_2019.json', simplifyVector = TRUE) %>% as.data.frame()
            json.SL.2019 <- json.SL.2019 %>% json.format()
        # join
            json.2019 <- json.CH.2019 %>% bind_rows(json.FF.2019) %>%  bind_rows(json.SIFT.2019) %>%  bind_rows(json.SL.2019)
        
        
    # scraped table======
        # # site
        #   url <- "https://baseballsavant.mlb.com/savant-player/chris-sale-519242?stats=career-r-pitching-mlb"
        # # scrape
        #   theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )
        #   tables <- readHTMLTable(theurl)
        # # wanted table
        #   wanted.table <- tables[[5]] %>% as.data.frame()
        #   wanted.table <- wanted.table[, -1]
        #   wanted.table[] <- lapply(wanted.table, as.character)
        # # format
        #   wanted.table[,c(4:18)] <- convert.magic(wanted.table[,c(4:18)], "numeric")
        #   allstart.year <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")
        #   wanted.table$Season <- ifelse(wanted.table$Season %in% allstart.year,
        #                                 paste(wanted.table$Season, '*', sep = ''), wanted.table$Season)
        # # save
        #   save(wanted.table, file = "bio_table.Rda")
        # load
            load("Data/bio_table.Rda")

    # Carrer events =====
        events <- read_csv("Data/Events/chris sale evnts (day).csv")
        events$date <- events$date %>% dmy()
         
    # location plot, home plate shape ========
        hp.x <- c(-0.95, -0.95, 0, 0.95, 0.95, -0.95)
        hp.y <- c(0, -0.475, -0.95, -0.475, 0, 0)
        hp <- data.frame(hp.x, hp.y)
    
    # join dat files ========
        # full data and player id's
            full.data <- full.data %>% left_join(player.id, by = "batter")
        # full.data and json
            full.data <- full.data %>% left_join(json.2019, by = c("plate_x", "plate_z", "release_speed", "pitch_type", "game_date"))
            full.data$has_link <- NA
            full.data$has_link <- ifelse(is.na(full.data$link), "No", "Yes")

    # Groups =======
        ball.in.play <- c("double", "double_play", "field_error", "field_out", "fielders_choice", "fielders_choice_out", 
                          "force_out", "grounded_into_double_play", "home_run", "sac_bunt", "sac_fly", "single", 
                          "triple", "triple_play")
        hits <- c("single", "double", "triple", "home_run")
        strikes.outcome <- c("called_strike", "swinging_strike")
        
        playoff.game <- c("Playoff Game", "League Playoff Game", "World Serires Game")
    # color pallet=====
        # pitch type 
            pitch_col <- brewer.pal(length(unique(full.data$pitch_type)),"Set1")
            pitch_col <- setNames(pitch_col, c("Two-Seam Fastball", "Slider", "Changeup", "Four-Seam Fastball"))
# define Java script --------
    js.1 <- "function(el, x) {
             el.on('plotly_click', function(d) {
               var point = d.points[0];
               var url = point.data.customdata[point.pointIndex];
               window.open(url);
             });
           }"
            
# Define UI for application ---------
ui <- fluidPage(
    # theme
    #theme = shinytheme("slate"),
    theme = shinythemes::shinytheme("cosmo"),
    
    # Application title
    titlePanel("FIT5147 Visualisation Project"),
    navbarPage("Chris Sale",
        # Bio page
            tabPanel("Player Bio",
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        img(src = "https://securea.mlb.com/mlb/images/players/head_shot/519242.jpg"),
                        br(), br(),
                        p(strong("Full Name:"), "Christopher Allen Sale"),
                        p(strong("Position:"), "Pitcher"),
                        p(strong("Bats:"), "Left - ", strong("Throws:"), "Left"),
                        p(strong("Height:"), "6-6, (198cm)", strong("Weight:"), "180lb, ( 81kg)"),
                        p(strong("Current Team:"), "Boston Red Sox"),
                        p(strong("Former Team:"), " Chicago White Sox (2010-2016)"),
                        p(strong("Born:"), "March 30, 1989, in Lakeland, FL "),
                        p(strong("Draft:"), "2010, Chicago White Sox, 1st rd. (13th overall)"),
                        p(strong("College:"), "Florida Gulf Coast"),
                        p(strong("Debut:"), "August 6, 2010 (Age 21-129d)"),
                        p(strong("Nicknames:"), "The Conductor")   
                    ),
                    mainPanel(
                        h1(strong("Carrer MLB Pitching Statistics")),
                        br(),
                        tableOutput("table_kable")
                    )
                )
    
            ),
        # Dash Board
            tabPanel("Dash Baord",
                # shrink distance between widgest
                    tags$head(
                        tags$style(
                            HTML(
                                ".form-group {
                                    margin-bottom: 0 !important;
                                }"
                            )
                        )
                    ),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        dateRangeInput("dates", label = h3("Date range"),
                                       start = '2018-06-01', end = '2020-01-01',
                                       min = '2010-01-01', max = '2020-01-01'),
                        selectInput("event", label = h3("Pitch Events"), 
                                    choices = list("All pitches" = 1, "Ball's In Play" = 2, "Hits" = 3,
                                                   "Strikes" = 4), 
                                    selected = 1),
                        checkboxGroupInput("stand", label = h3("Batter Hand"), 
                                           choices = list("Left" = "Left", "Right" = "Right"),
                                           selected = c("Left", "Right")),
                        checkboxGroupInput("ball", label = h3("Ball count"), 
                                           choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3),
                                           selected = c(0:3)),
                        checkboxGroupInput("strikes", label = h3("Strike Count"), 
                                           choices = list("0" = 0, "1" = 1, "2" = 2),
                                           selected = c(0:2)),
                        checkboxGroupInput("outs", label = h3("Outs"), 
                                           choices = list("0" = 0, "1" = 1, "2" = 2),
                                           selected = c(0:2)),
                        checkboxGroupInput("base", label = h3("Player On Base"), 
                                           choices = list("First" = 1, "Second" = 2, "Third" = 3),
                                           selected = NONET)
                        
                    ),
                    mainPanel(
                        div(plotlyOutput("time_serires", width = "100%", height = "100%"),align = "center"),
                        br(),
                        div(fluidRow(
                            column(8,
                                   plotlyOutput("location_plot", width = "100%", height = "100%")),
                            column(4,
                                   plotlyOutput("hit_plot", width = "100%", height = "100%")),
                            align = "center"
                        ))
                        
                    )
                )
            )
    )
)

            
# Define server ---------
server <- function(input, output) {

    # bio page table =========
        output$table_kable <- function(){
            wanted.table %>% 
                mutate(
                    Tm = cell_spec(Tm, "html",
                                   color = ifelse(Tm == 'CWS', "#27251F", "#0C2340"),
                                   background = ifelse(Tm == 'CWS', "#C4CED4", ifelse(Tm == 'BOS', 
                                                                                      "#BD3039", "NONET"))),
                    W = cell_spec(W, "html", background = ifelse(W == sort(W , partial = 11 - 3)[11 - 3],
                                                                 "#e17777", "NONET")),
                    L = cell_spec(L, "html", background = ifelse(L >= sort(L , partial = 11 - 3)[11 - 3],
                                                                 ifelse(L == max(L),"NONET", "#e17777"), "NONET")),
                    ERA = cell_spec(ERA, "html", background = ifelse(ERA >= sort(ERA , partial = 11 - 3)[11 - 3],
                                                                     ifelse(ERA == max(ERA),"NONET", "#e17777"), "NONET")),
                    G = cell_spec(G, "html", background = ifelse(G >= sort(G , partial = 11 - 3)[11 - 3],
                                                                 ifelse(G == max(G),"NONET", "#e17777"), "NONET")),
                    GS = cell_spec(GS, "html", background = ifelse(GS >= sort(GS , partial = 11 - 3)[11 - 3],
                                                                   ifelse(GS == max(GS),"NONET", "#e17777"), "NONET")),
                    SV = cell_spec(SV, "html", background = ifelse(SV >= sort(SV , partial = 11 - 2)[11 - 2],
                                                                   ifelse(SV == max(SV),"NONET", "#e17777"), "NONET")),
                    IP = cell_spec(IP, "html", background = ifelse(IP >= sort(IP , partial = 11 - 2)[11 - 2],
                                                                   ifelse(IP == max(IP),"NONET", "#e17777"), "NONET")),
                    H = cell_spec(H, "html", background = ifelse(H >= sort(H , partial = 11 - 3)[11 - 3],
                                                                 ifelse(H == max(H),"NONET", "#e17777"), "NONET")),
                    R = cell_spec(R, "html", background = ifelse(R >= sort(R , partial = 11 - 3)[11 - 3],
                                                                 ifelse(R == max(R),"NONET", "#e17777"), "NONET")),
                    ER = cell_spec(ER, "html", background = ifelse(ER >= sort(ER , partial = 11 - 3)[11 - 3],
                                                                   ifelse(ER == max(ER),"NONET", "#e17777"), "NONET")),
                    HR = cell_spec(HR, "html", background = ifelse(HR >= sort(HR , partial = 11 - 3)[11 - 3],
                                                                   ifelse(HR == max(HR),"NONET", "#e17777"), "NONET")),
                    BB = cell_spec(BB, "html", background = ifelse(BB >= sort(BB , partial = 11 - 3)[11 - 3],
                                                                   ifelse(BB == max(BB),"NONET", "#e17777"), "NONET")),
                    SO = cell_spec(SO, "html", background = ifelse(SO >= sort(SO , partial = 11 - 3)[11 - 3],
                                                                   ifelse(SO == max(SO),"NONET", "#e17777"), "NONET")),
                    WHIP = cell_spec(WHIP, "html", background = ifelse(WHIP >= sort(WHIP , partial = 11 - 3)[11 - 3],
                                                                       ifelse(WHIP == max(WHIP),"NONET", "#e17777"), "NONET"))
                ) %>%
                kable(format = 'html', escape = F, table.attr = "style = \"color: black;\"") %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                              full_width = TRUE, position = "center", font_size = 20) %>%
                row_spec(11, color = "white", background = "grey") %>% 
                footnote(
                    general = 'highlight top 3 for each year, and Tm(Team)',
                    symbol = "All Star Year")
        }
    
    # Time serires plot =====
        output$time_serires <- renderPlotly({
            #  filter data
                # apply function
                    data1 <- full.data %>% time_series_year()
                    data2 <- full.data %>% time_series_day()
                # filter from date range
                    data1 <- data1 %>% filter(date %between% input$dates)
                    data2 <- data2 %>% filter(date %between% input$dates)
                    events <- events %>% filter(date %between% input$dates)
                    
            # plot
                 p1 <- plot_ly(source = "select") %>% 
                    # year
                    add_trace(data = data1, x = ~date, y = ~perc, type = 'scatter', mode = 'lines+markers',
                              color = ~pitch_type, colors = pitch_col, visible = FALSE,
                              text = ~paste(paste('Year: ', date),
                                            paste('Total Pitches for the year: ', date_freq),
                                            paste('Total', paste(pitch_type, "'s", sep = ""), 
                                                  'for the year: ', type_freq),
                                            paste('Percentage of', paste(pitch_type, "'s", sep = ""), 
                                                  'for the year: ', perc),
                                            sep = "\n"), hoverinfo = 'text') %>%
                    #day
                    add_trace(data = data2, x = ~date, y = ~perc, type = 'scatter', 
                              color = ~pitch_type, colors = pitch_col, mode = 'lines+markers', 
                              text = ~paste(paste('Date: ', date),
                                            paste('Total Pitches for the game: ', date_freq),
                                            paste('Total', paste(pitch_type, "'s", sep = ""), 
                                                  'for the game: ', type_freq),
                                            paste('Percentage of', paste(pitch_type, "'s", sep = ""), 
                                                  'for the game: ', perc),
                                            sep = "\n"), hoverinfo = 'text') %>%
                    add_trace(data = events, x = ~date, y = 1.2, type = 'scatter', mode = 'markers', 
                              marker = list(color = "#f23ed7", opacity = 0.5), 
                              name = "events (with link)", text = ~paste(paste('Date: ', date),
                                                             paste('Event: ', event), sep = "\n"), 
                              hoverinfo = 'text') %>%
                     add_trace(data = na.omit(events), x = ~date, y = 1.2, type = 'scatter', mode = 'markers', 
                               marker = list(color = "#f23ed7"), customdata = ~link,
                               name = "events (with out link)",
                               text = ~paste(paste('Date: ', date),
                                            paste('Event: ', event), sep = "\n"), hoverinfo = 'text') %>% 
                     
                    layout(
                        dragmode = "select",
                        title = "Pitch Frequency Time Series",
                        yaxis = list(title = "holder", tickvals = list(0.2, 0.4, 0.6, 0.8, 1, 'Events')),
                        updatemenus = 
                            list(list(y = 0.8 , 
                                      buttons = list(
                                          list(method = "restyle", label = "By Game", 
                                               args = list("visible", list(FALSE,FALSE,FALSE,FALSE, 
                                                                           TRUE,TRUE,TRUE,TRUE, 
                                                                           TRUE,TRUE))
                                          ),
                                          list(method = "restyle", label = "By Year", 
                                               args = list("visible", list(TRUE,TRUE,TRUE,TRUE, 
                                                                           FALSE,FALSE,FALSE,FALSE, 
                                                                           FALSE,FALSE))
                                          )
                                      )
                            )
                            )
                    )


                 p1 %>% onRender(js.1)
        })

    # location plot =======
        output$location_plot <- renderPlotly({
            # filter data
                # date
                    location <- full.data %>% filter(game_date %between% input$dates)
                # hand
                    location <- location %>% filter(stand %in% input$stand)
                # event
                    if (input$event == 3) {
                        location <- location %>% filter(true_outcome %in% hits)
                    } else if (input$event == 2) {
                        location <- location %>% filter(true_outcome %in% ball.in.play)
                    } else if (input$event == 4) {
                        location <- location %>% filter(true_outcome %in% strikes.outcome)
                        
                    }
                # ball strike outs
                    location <- location %>% filter(balls %in% input$ball)
                    location <- location %>% filter(strikes %in% input$strikes)
                    location <- location %>% filter(outs_when_up %in% input$outs)
                # on_base
                    if (1 %in% input$base){
                        location <- location %>% filter(on_1b == TRUE)
                    } 
                    if (2 %in% input$base){
                        location <- location %>% filter(on_2b == TRUE)
                    } 
                    if (3 %in% input$base){
                        location <- location %>% filter(on_3b == TRUE)
                    }

            
            # split data by pitch_type
                location.split <- location %>% split(location$pitch_type)
                len <- length(location.split)
            # for loop to facet        
                for (i in 1:len) {
                    word <- location.split[[1]][1,1] %>% unlist() %>% as.character()
                    # holder plot
                        holder <- 
                            plot_ly() %>%
                            # pitch scatter plot
                            add_trace(data = location.split[[i]], x = ~plate_x, y = ~plate_z, 
                                      type = 'scatter', mode = 'markers', 
                                      color = ~pitch_type, colors = ~pitch_col,
                                      marker = list(size = 10, opacity = 0.8, 
                                                    line = list(color = 'black', width = 2)),
                                      text = ~paste(paste('batter: ', batter_name),
                                                    paste('outcome: ', true_outcome),
                                                    paste('Velocity: ', release_speed, 'mph'),
                                                    paste('link: ', has_link),sep = "\n"),
                                      hoverinfo = 'text', customdata = ~link) %>%
                            # home plate
                            add_trace(data = hp, x= ~hp.x, y = ~hp.y, type = 'scatter', mode = 'lines',
                                      showlegend = FALSE, hoverinfo = FALSE, line = list(color = '#e6ab02')) %>%
                            # layout and strike zone
                            layout(
                                shapes = list(type = 'rect', line = list(color = '#e6ab02'),
                                                 x0 = -0.95, x1 = 0.95, xref = 'x',
                                                 y0 = 1.6, y1 = 3.5, yref = 'y'),
                                yaxis = list(range = c(-2,7.75), title = "", zeroline = FALSE,
                                             showline = FALSE, showticklabels = FALSE),
                                xaxis = list(range = c(-4.5,4.5), title = "", zeroline = FALSE,
                                             showline = FALSE, showticklabels = FALSE))
                    # assign plot to a varible      
                        assign(paste('p',i, sep = ''), holder)
                }
            
            # plot    
              subplot(p1, p2, p3, p4, nrows = 1, shareY = TRUE, shareX = TRUE) %>% 
                  layout(title = "Pitch Location Chart", legend = list(orientation = 'h')) %>% onRender(js.1)
            
        })
        
    # Hit plot =========
        output$hit_plot <- renderPlotly({
            # filter data
                # date
                    hit.data <- full.data %>% filter(game_date %between% input$dates)
                # hand
                    hit.data <- hit.data %>% filter(stand %in% input$stand)
                # event
                    if (input$event == 3) {
                        hit.data <- hit.data %>% filter(true_outcome %in% hits)
                    } else if (input$event == 2) {
                        hit.data <- hit.data %>% filter(true_outcome %in% ball.in.play)
                    } else if (input$event == 4) {
                        hit.data <- hit.data %>% filter(true_outcome %in% strikes.outcome)
                        
                    }
                # ball strike outs
                    hit.data <- hit.data %>% filter(balls %in% input$ball)
                    hit.data <- hit.data %>% filter(strikes %in% input$strikes)
                    hit.data <- hit.data %>% filter(outs_when_up %in% input$outs)
                # on_base
                    if (1 %in% input$base){
                        hit.data <- hit.data %>% filter(on_1b == TRUE)
                    } 
                    if (2 %in% input$base){
                        hit.data <- hit.data %>% filter(on_2b == TRUE)
                    } 
                    if (3 %in% input$base){
                        hit.data <- hit.data %>% filter(on_3b == TRUE)
                    }
            
            # plot
            p <- plot_ly() %>%
                # set up field
                    # outfield outer
                    add_trace(data = field.list[[6]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = 'grey'), 
                              fillcolor  = 'white', showlegend = FALSE, hoverinfo = FALSE) %>%
                    # outfiled inner
                    add_trace(data = field.list[[5]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = '#54bcab', opacity = 1), 
                              fillcolor  = '#54bcab', showlegend = FALSE, hoverinfo = FALSE, opacity = 0.5) %>%
                    # infield outer
                    add_trace(data = field.list[[4]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = 'grey'), 
                              fillcolor  = 'white', showlegend = FALSE, hoverinfo = FALSE) %>%
                    # infiled inner
                    add_trace(data = field.list[[3]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = '#54bcab', opacity = 1), 
                              fillcolor  = '#54bcab', showlegend = FALSE, hoverinfo = FALSE, opacity = 0.5) %>%
                    # foul lines
                    add_trace(data = field.list[[1]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = 'grey'), 
                              fillcolor  = 'grey', showlegend = FALSE, hoverinfo = FALSE) %>%
                    # home base
                    add_trace(data = field.list[[2]], x = ~x, y = ~-y, type = 'scatter', mode = 'lines',
                              fill = 'toself', line  = list(color = 'black'), 
                              fillcolor  = 'black', showlegend = FALSE, hoverinfo = FALSE)  %>%
                    # text
                    add_text(text = c("330", "385", "410", "385", "330"), 
                             x = c(15, 50, 125, 200, 235), y = c(-85, -50, -25, -50, -85), 
                             textfont = list(size = 20), xanchor = 'center', showarrow = FALSE,
                             showlegend = FALSE, hoverinfo = 'skip') %>%
                
                # points
                add_trace(data = hit.data, x = ~hc_x, y = ~-hc_y, type = 'scatter', color = ~events, 
                          mode = 'markers', marker = list(line = list(color = 'black', width = 1)),
                          text = ~paste(paste('batter: ', batter_name),
                                       paste('outcome: ', true_outcome),
                                       paste('link: ', has_link), sep = "\n"),
                          hoverinfo = 'text', customdata = ~link) %>%
                # layout  
                layout(
                    title = "Hit Spray Chart",
                    yaxis = list(scaleanchor = "x", domain = c(218.56, 34.94174),
                                 title = "", zeroline = FALSE, showline = FALSE,
                                 showticklabels = FALSE, showgrid = FALSE),
                    xaxis = list(domain = c(34.94174, 218.56),title = "",
                                 zeroline = FALSE, showline = FALSE, showticklabels = FALSE,
                                 showgrid = FALSE),
                    showlegend = FALSE
                ) %>% onRender(js.1)
            
        })
    
        

        
}

# Run the application ------------
    shinyApp(ui = ui, server = server)
