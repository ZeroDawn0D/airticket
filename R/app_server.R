#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(ggplot2)
app_server <- function(input, output, session) {
  # Your application server logic

#    output$plot1 <- renderPlot({
        dynamic <- reactiveValues();
        dynamic$YR <- c("1958","1959","1960")
        dynamic$LG <- TRUE
        dynamic$PG <- FALSE
        dynamic$X1958 <- "#FF0000"
        dynamic$X1959 <- "#00FF00"
        dynamic$X1960 <- "#0000FF"
        observeEvent(input$YR,{
          print("input$YR event called")
          dynamic$YR <- input$YR
          output$plot1 <- renderPlot({renderAirplot(dynamic)})
        })
        observeEvent(input$LG,{
          dynamic$LG <- input$LG
          output$plot1 <- renderPlot({renderAirplot(dynamic)})
        })
        observeEvent(input$PG,{
          dynamic$PG <- input$PG
          renderAirplot(dynamic)
        })
        observeEvent(input$X1958,{
          dynamic$X1958 <- input$X1958
          output$plot1 <- renderPlot({renderAirplot(dynamic)})
        })
        observeEvent(input$X1959,{
          dynamic$X1959 <- input$X1959
          output$plot1 <- renderPlot({renderAirplot(dynamic)})
        })
        observeEvent(input$X1960,{
          dynamic$X1960 <- input$X1960
          output$plot1 <- renderPlot({renderAirplot(dynamic)})
        })
 #     })


}
printDynamic <- function(dynamic = reactiveValue()){
  print(dynamic$YR)
}
renderAirplot <- function(dynamic){
  print("render called")
  airtravel <- read.csv("airtravel.csv");
  years_shown <- dynamic$YR;
  color_list <- c();
  is1958 <- FALSE
  is1959 <- FALSE
  is1960 <- FALSE
  print(years_shown)
  if("1958" %in% years_shown){
    is1958 <- TRUE
    color_list <- c(color_list, dynamic$X1958)
  }else{
    is1958 <- FALSE
  }
  if("1959" %in% years_shown){
    is1959 <- TRUE
    color_list <- c(color_list, dynamic$X1959)
  }else{
    is1959 <- FALSE
  }
  if("1960" %in% years_shown){
    is1960 <- TRUE
    color_list <- c(color_list, dynamic$X1960)
  }else{
    is1960 <- FALSE
  }
  airplot <- ggplot(
    airtravel,
    mapping = aes(
      x = Month,
      group = 1)
  )+labs(
      title = "Price(USD) v/s Month",
      x = "Months",
      y = "Price(USD)",
      colour = "Years"
    )+ scale_color_manual(
      values = color_list
    );

    if(is1958){
      if(dynamic$LG){
        print("here58")
        airplot <- airplot + geom_line(mapping = aes(
          y = X1958,
          colour = "1958"
        ))
      }
      if(dynamic$PG){
        airplot <- airplot + geom_point(mapping = aes(
          y = X1958,
          colour = "1958"
        ))
      }
    }
  if(is1959){
    if(dynamic$LG){
      print("here59")
      airplot <- airplot + geom_line(mapping = aes(
        y = X1959,
        colour = "1959"
      ))
    }
    if(dynamic$PG){
      airplot <- airplot + geom_point(mapping = aes(
        y = X1959,
        colour = "1959"
      ))
    }
  }
  if(is1960){
    if(dynamic$LG){
      print("here60")
      airplot <- airplot + geom_line(mapping = aes(
        y = X1960,
        colour = "1960"
      ))
    }
    if(dynamic$PG){
      airplot <- airplot + geom_point(mapping = aes(
        y = X1960,
        colour = "1960"
      ))
    }
  }

    airplot;
}
