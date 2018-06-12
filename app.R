library(shiny)
library(plotly)
library(shinythemes)


responses  = read.csv("data/responses-csv.csv", header = T, stringsAsFactors = F,check.names=FALSE)


ui <- fluidPage( theme = shinytheme("yeti"),
                 titlePanel("Muestra situacion actual Becarios Retornados"),
                 img(src='pp.png', align = "right"),
                 img(src='logoPhD.jpeg', align = "right"),
                 HTML("<br></br>"),                 
h4("Esta infografía interactiva muestra los resultados de la encuesta elaborada y 
   aplicada por el grupo Phd Network, y con apoyo del grupo de BecariosRetornadosEC. 
   El objetivo de la misma fue evaluar el estatus laboral actual de los becarios de los
   distintas convocatorias de becas ofrecidas por la Secretaria Nacional de
   Ciencia y Tecnología (SENESCYT)"),
                 HTML("<br></br>"),
hr(),
                 h2("Situación laboral en base a categorias de becarios"),
                 h4("Para observar las relaciones, selecciona un grupo descriptor de becarios (desplegable izquierdo)
                    y elige un descriptor de la situación laboral (desplegable derecho)"),
br(),
                 
                 column(6,  
                        selectInput(inputId = "x", label = "Descriptor de Becarios",
                                        choices = names(responses)[2:8])),
                 column(6,selectInput(inputId = "y", label = "Descriptor de Situación Laboral",
              choices = names(responses)[c(9,11:20,22:28, 32,33)])),
            HTML("<br></br><br></br><br></br>"),
fluidRow(
  column(4,
         h5("Descriptor de Becarios"),
         br(),
         textOutput("becDes"),
         br(),
         h5("Descriptor de Situacion Laboral"),
         textOutput("labDes")


         ),
  column(8,
         plotlyOutput("plot"))),
  HTML("<br></br>"),
  hr(),
  h6("Infografia elaborada por: fgabriel1891")
)

server <- function(input, output) {
  
  becDes1 = eventReactive(input$x,{
    desci = read.csv("data/responseDescript.csv", header = T, 
                     stringsAsFactors = F,check.names=FALSE)
    wh <- desci$Descripcion[match(input$x, desci$Descriptor)]
   wh
    })

  labDes1 = eventReactive(input$y,{
    desci1 = read.csv("data/responseDescrip2.csv", header = T, 
                      stringsAsFactors = F,check.names=FALSE)
    wh1 <- desci1$Descripcion[match(input$y, desci1$Descriptor)]
   wh1
  })

  output$becDes = renderPrint({
    becDes1()
  })
  output$labDes = renderPrint({
    labDes1()})

  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    
    df <- responses
    df$xx <- df[[input$x]]
    df$yy <- df[[input$y]]
    # values = prop.table(table(df$yy,df$xx))
    # print(data.frame(values))
    plot = plot_ly(df,x = ~df$xx, split = ~df$yy, 
                   type = "histogram")  %>% 
      layout( xaxis = list( title = "Descriptor Becarios"), 
              yaxis = list( title = "Descriptor Laboral" ))
    plot$elementId <- NULL
    plot
  })

  
  
}

shinyApp(ui, server)



