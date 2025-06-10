library(shiny)

# carregando modelo
modelo <- lm(NUMDEFECTS ~ sumLOC_TOTAL, data = readxl::read_excel("dataset_KC1_classlevel_numdefect.xlsx"))

ui <- fluidPage(
  titlePanel("Previsão de Defeitos por linhas de código - LOC"),
  sidebarLayout(
    sidebarPanel(
      numericInput("loc", "Linhas de Código (LOC):", value = 100),
      actionButton("prever", "Prever")
    ),
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$prever, {
    pred <- predict(modelo, newdata = data.frame(sumLOC_TOTAL = input$loc))
    output$resultado <- renderPrint({
      paste("Número estimado de defeitos:", round(pred, 2))
    })
  })
}

shinyApp(ui = ui, server = server)
