library(shiny)
library(shinyjs)

ui <- fluidPage( 
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("font_size", "Font Size:", min = 1, max = 200, value = 70)
    ),
    mainPanel(
      wellPanel(
        id = "textPanel",
        style = "overflow-y:scroll; max-height: 50vh; font-size: 70%",
      paste0("Hello", collapse = ""))
    )
  )
)

server <- function(input, output) {
  observeEvent(input$font_size, {
    runjs(paste0('$("#textPanel").css("font-size","', input$font_size, '%")'))
  })
}

shinyApp(ui = ui, server = server)