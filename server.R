#server.R
library(shiny)
library(shinydashboard)
library(proxy)
library(recommenderlab)
library(reshape2)
library(plyr)
library(dplyr)
library(DT)
library(RCurl)

d <- file.path("C:","Users", "ellen", "Desktop","again")
movies <- read.csv("moviesclean.csv", header = TRUE,stringsAsFactors=FALSE,encoding="UTF-8")
ratings <- read.csv("ratingsclean.csv", header = TRUE,fileEncoding="UTF-8")
#movies <- read.csv("~/data/moviesclean.csv", header = TRUE,stringsAsFactors=FALSE,encoding="UTF-8")
#ratings <- read.csv("~/data/ratingsclean.csv", header = TRUE,fileEncoding="UTF-8")


server <- shinyServer(function(input, output) {
  
  output$name1 <- renderText({
    input$select
  })
  
  output$myImage <- renderImage({
    list(src = paste0("data/photo/", input$select, ".png"), width=201, height=300) 
  }, deleteFile = FALSE)
  
  output$des1 <- renderText({
    movies$des[movies$title == input$select]
  })
  
  
  
  
  
  Table <- reactive({
    
    
    movie_recommendation <- function(input){
      row_num <- which(movies[,3] == input)
      userSelect <- matrix(NA,length(unique(ratings$movieId)))
      userSelect[row_num] <- 5 #hard code first selection to rating 5
      # userSelect[row_num2] <- 4 #hard code second selection to rating 4
      # userSelect[row_num3] <- 4 #hard code third selection to rating 4
      userSelect <- t(userSelect)
      
      ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
      ratingmat <- ratingmat[,-1]
      colnames(userSelect) <- colnames(ratingmat)
      ratingmat2 <- rbind(userSelect,ratingmat)
      ratingmat2 <- as.matrix(ratingmat2)
      
      #Convert rating matrix into a sparse matrix
      ratingmat2 <- as(ratingmat2, "realRatingMatrix")
      
      #Create Recommender Model
      recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
      recom <- predict(recommender_model, ratingmat2[1], type = "topNList", n=20)   
      recom_list <- as(recom, "list")
      recom_result <- data.frame(matrix(NA,20))
      recom_result[1:20,1] <- movies[as.integer(recom_list[[1]][1:20]),3]
      recom_result <- data.frame(na.omit(recom_result[order(order(recom_result)),]))
      recom_result <- as.character(recom_result[1:20,])
      # recom_result <- data.frame(recom_result[1:5,])
      # colnames(recom_result) <- " "
      return(recom_result)
    }
    
    movie_recommendation(input$select)
  })
  #sample(Table()[1:30], 3)
  #ar <- sample(c(Table()[1],Table()[2]),1)
 #ra <- sample(c(Table()[1],Table()[2],Table()[3],Table()[4],Table()[5],Table()[6],Table()[7],Table()[8],Table()[9],Table()[10]),5,replace=FALSE)
  #Table()[1,]
######################################################################################  

  
  output$myImage1 <- renderImage({
    list(src = paste0("data/photo/", Table()[2], ".png"), width=100, height=150)
  }, deleteFile = FALSE)
  
  output$myreco1 <- renderText({
    Table()[2]
    #ar[[1]]
    
   
  })
  # output$myrate1 <- renderText({
  #   rate.avg1 <- summarise(subset(movie.ratings, title==Table()[1]),
  #                          Average_Rating = mean(rating, na.rm = TRUE))
  #   as.numeric(rate.avg1)
  # })
 #~~~~~~~~~~~~~~~~~~~~~~~ 
  output$myImage2 <- renderImage({
    list(src = paste0("data/photo/", Table()[5], ".png"), width=100, height=150)
  }, deleteFile = FALSE)
  
  output$myreco2 <- renderText({
    Table()[5]
    
  
  })
#   output$myrate2 <- renderText({
#     rate.avg2 <- summarise(subset(movie.ratings, title==Table()[2]),
#                            Average_Rating = mean(rating, na.rm = TRUE))
#     as.numeric(rate.avg2)
#   })
#~~~~~~~~~~~~~~~~~    
    output$myImage3 <- renderImage({
      list(src = paste0("data/photo/", Table()[6], ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco3 <- renderText({
      Table()[6]
      
    })
 #   output$myrate3 <- renderText({
    #   rate.avg3 <- summarise(subset(movie.ratings, title==Table()[3]),
    #                          Average_Rating = mean(rating, na.rm = TRUE))
    #   as.numeric(rate.avg3)
    #   
    # })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$myImage4 <- renderImage({
      list(src = paste0("data/photo/", Table()[13], ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco4 <- renderText({
      Table()[13]
      
      
    })
#    output$myrate4 <- renderText({
    #   rate.avg4 <- summarise(subset(movie.ratings, title==Table()[4]),
    #                          Average_Rating = mean(rating, na.rm = TRUE))
    #   as.numeric(rate.avg4)
    # })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$myImage5 <- renderImage({
      list(src = paste0("data/photo/", Table()[17], ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco5 <- renderText({
      Table()[17]
      
      
    })
#    output$myrate5 <- renderText({
    #   rate.avg5 <- summarise(subset(movie.ratings, title==Table()[5]),
    #                          Average_Rating = mean(rating, na.rm = TRUE))
    #   as.numeric(rate.avg5)
    #   
    # })

  
  
  
  movie.ratings <- merge(ratings, movies)
  output$tableRatings1 <- renderValueBox({
    movie.avg1 <- summarise(subset(movie.ratings, title==input$select),
                            Average_Rating1 = mean(rating, na.rm = TRUE))
    valueBox(
      value = format(movie.avg1, digits = 3),
      subtitle = HTML("&nbsp;"),   #NULL,  #input$select
      icon = if (movie.avg1 >= 3) icon("thumbs-up") else icon("thumbs-down"),
      color = if (movie.avg1 >= 3) "aqua" else "red"
    )
    
  })
})
  
  
