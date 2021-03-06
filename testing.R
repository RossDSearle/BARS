if (interactive()) {
  library(shiny)
  library(shinyMobile)
  
  shiny::shinyApp(
    ui = f7Page(
      title = "Expandable Cards",
      f7SingleLayout(
        navbar = f7Navbar(
          title = "Expandable Cards",
          hairline = FALSE,
          shadow = TRUE
        ),
        f7ExpandableCard(
          id = "card1",
          title = "Expandable Card 1",
          img = "https://i.pinimg.com/originals/73/38/6e/73386e0513d4c02a4fbb814cadfba655.jpg",
          "Framework7 - is a free and open source HTML mobile framework
         to develop hybrid mobile apps or web apps with iOS or Android
         native look and feel. It is also an indispensable prototyping apps tool
         to show working app prototype as soon as possible in case you need to."
        ),
        
        hr(),
        f7BlockTitle(title = "Click below to expand the card!") %>% f7Align(side = "center"),
        f7Button(inputId = "go", label = "Go"),
        br(),
        f7ExpandableCard(
          id = "card2",
          title = "Expandable Card 2",
          fullBackground = TRUE,
          img = "https://i.ytimg.com/vi/8q_kmxwK5Rg/maxresdefault.jpg",
          "Framework7 - is a free and open source HTML mobile framework
               to develop hybrid mobile apps or web apps with iOS or Android
               native look and feel. It is also an indispensable prototyping apps tool
               to show working app prototype as soon as possible in case you need to."
        )
      )
    ),
    server = function(input, output, session) {
      
      observeEvent(input$go, {
        updateF7Card(id = "card2", session = session)
      })
      
      observe({
        list(
          print(input$card1),
          print(input$card2)
        )
      })
    }
  )
}
