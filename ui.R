# ## ui.R
library(shiny)
library(shinydashboard)
# library(proxy)
# library(recommenderlab)
# library(reshape2)
# library(plyr)
# library(dplyr)
# library(DT)
# library(RCurl)
# # 
# # 
d <- file.path("C:","Users", "ellen", "Desktop","again")
movies <- read.csv("moviesclean.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("ratingsclean.csv", header = TRUE)
#movies <- read.csv("~/data/moviesclean.csv", header = TRUE,stringsAsFactors=FALSE,encoding="UTF-8")
#ratings <- read.csv("~/data/ratingsclean.csv", header = TRUE,fileEncoding="UTF-8")

ui <- shinyUI(dashboardPage(skin="blue",
                            
                            dashboardHeader(title = "電影出租店"),
                            dashboardSidebar(
                              sidebarMenu( 
                                menuItem(
                                  list( 
                                    selectInput("select", label = h5("選擇一部你喜歡的電影"),
                                                choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                                                selectize = TRUE,
                                                selected = "Toy Story (1995)"),
                                    submitButton("完成")
                                  )
                                )
                              )
                            ),
                            
                            
                            dashboardBody( 
                              includeCSS("data/custom.css"),
                              fluidRow(
                                box(
                                  width=3,
                                  height=400,
                                  status = "primary", 
                                  title = "電影介紹",
                                  textOutput("name1"),
                                  #br(),  #空格
                                  imageOutput("myImage",height="300px")
                                  #valueBoxOutput("tableRatings1")
                                                 
                                ),

                                box(
                                  width=8,
                                  height=400,
                                  status = "primary",
                                  title = "劇情",
                                  textOutput("des1"),
                                  br(),
                                  h4("評價"),
                                  br(),
                                  valueBoxOutput("tableRatings1")
                                 )
                               ),
                              
                              fluidRow(
                                h4("你可能也會喜歡"),
                                box(
                                  width=2,
                                  height=220,
                                  status = "danger",
                                  imageOutput("myImage1",height="150px"),
                                  textOutput("myreco1")
                                  #textOutput("myrate1")
                                ),
                                box(
                                  width=2,
                                  height=220,
                                  status = "danger",
                                  imageOutput("myImage2",height="150px"),
                                  textOutput("myreco2")
                                  #textOutput("myrate2")
                                ),
                                box(
                                  width=2,
                                  height=220,
                                  status = "danger",
                                  imageOutput("myImage3",height="150px"),
                                  textOutput("myreco3")
                                  #textOutput("myrate3")
                                ),
                                box(
                                  width=2,
                                  height=220,
                                  status = "danger",
                                  imageOutput("myImage4",height="150px"),
                                  textOutput("myreco4")
                                  #textOutput("myrate4")
                                ),
                                box(
                                  width=2,
                                  height=220,
                                  status = "danger",
                                  imageOutput("myImage5",height="150px"),
                                  textOutput("myreco5")
                                  #textOutput("myrate5")
                                )
                              )
                              
                            )
                            
)
)   
