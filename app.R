library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyMobile)

# Load trivia data from a CSV file
datos <- read.csv("data/PreguntasTriviaV2.csv")

# Extract individual columns into variables
pregunta <- datos$Pregunta
respuesta1 <- datos$Respuesta1
respuesta2 <- datos$Respuesta2
respuesta3 <- datos$Respuesta3
respuesta4 <- datos$Respuesta4
respuestaC <- datos$RespuestaCorrecta
img <- datos$Imagen

# Create options for the radio buttons
opciones <- c(respuesta1[1], respuesta2[1], respuesta3[1], respuesta4[1])

# Define UI
ui <- f7Page(
  useShinyjs(),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/jsqr@1.4.0/dist/jsQR.min.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('removeHeight', function(message) {
        document.getElementById(message).style.height = 'auto';
      });
      document.addEventListener('DOMContentLoaded', function() {
        var divs = document.getElementsByTagName('div');
        for (var i = 0; i < divs.length; i++) {
          divs[i].style.marginTop = '0';
        }
      });

      Shiny.addCustomMessageHandler('start_qr_scanner', function(message) {
        var video = document.getElementById('video');
        var canvas = document.getElementById('canvas');
        var context = canvas.getContext('2d');

        navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
          video.srcObject = stream;
          video.setAttribute('playsinline', true); // required to tell iOS safari we don't want fullscreen
          video.play();
          requestAnimationFrame(tick);
        });

        function tick() {
          if (video.readyState === video.HAVE_ENOUGH_DATA) {
            canvas.height = video.videoHeight;
            canvas.width = video.videoWidth;
            context.drawImage(video, 0, 0, canvas.width, canvas.height);
            var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
            var code = jsQR(imageData.data, imageData.width, imageData.height, {
              inversionAttempts: 'dontInvert',
            });
            if (code) {
              Shiny.setInputValue('qr_code_data', code.data);
            }
          }
          requestAnimationFrame(tick);
        }
      });

      Shiny.addCustomMessageHandler('scrollTo', function(message) {
        var iframe = document.querySelector('.iframe-content');
        if (iframe) {
          console.log('Iframe found, sending scroll command...');
          iframe.contentWindow.postMessage({ action: 'scroll', value: message.value }, '*');
        } else {
          console.log('Iframe not found.');
        }
      });
    ")),
    tags$style(HTML("
      .iframe-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
        width: 100%;
      }
      .iframe-content {
        width: 100%;
        height: 600px;
        border: none;
      }
      @media (max-width: 768px) {
        .iframe-content {
          height: 80vh;
        }
      }
    "))
  ),
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Museo El Centenario",
      hairline = FALSE,
      shadow = TRUE,
      leftPanel = FALSE,
      rightPanel = FALSE
    ),
    f7Tabs(
      id = "tabs",
      animated = TRUE,
      f7Tab(
        tabName = "Trivia",
        icon = f7Icon("gamecontroller"),
        active = TRUE,
        f7Block(
          strong = TRUE,
          textOutput("pregunta"),
          imageOutput("imagen1"),
          f7Radio("respuesta", label = NULL, choices = opciones),
          f7Button("confirmar", "Confirmar respuesta"),
          f7Button("continuar", label = "Continuar"),
          br(),
          textOutput("resultado"),
          textOutput("resultadofinal"),
          textOutput("puntaje")
        )
      ),
      f7Tab(
        tabName = "Guia",
        icon = f7Icon("today"),
        f7Block(
          title = "Orígenes del municipio",
          div(class = "iframe-container",
              tags$iframe(src = "https://rainy-malachite-jay.glitch.me/", class = "iframe-content")
          )
        )
      ),
      f7Tab(
        tabName = "QR",
        icon = f7Icon("qrcode"),
        f7Block(
          title = "QR Code Scanner",
          f7Button("start_qr_scanner", label = "Start QR Scanner"),
          tags$video(id = "video", autoplay = TRUE, style = "width:100%; max-width:400px;"),
          br(),
          tags$canvas(id = "canvas", width = "320", height = "240", style = "display:none;"),
          textOutput("qr_result"),
          hidden(textInput("qr_instructions", label = NULL))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  hide("resultadofinal")
  hide("puntaje")
  hide("continuar")
  
  puntaje <- reactiveValues(countervalue = 0)
  nPregunta <- reactiveValues(countervalue = 1)
  
  nPregunta2 <- eventReactive(input$continuar, {
    nPregunta$countervalue <- nPregunta$countervalue + 1
    nPregunta$countervalue
  })
  
  output$pregunta <- renderText({
    if (nPregunta$countervalue == 1) {
      pregunta[1]
    } else {
      pregunta[nPregunta2()]
    }
  })
  
  output$imagen1 <- renderImage({
    session$sendCustomMessage('removeHeight', 'imagen1')
    list(
      src = paste0("images/", img[nPregunta$countervalue]),
      contentType = "image/png",
      alt = "Question Image",
      style = "width: 100%; height: auto; max-width: 300px; margin: 10px auto; display: block;"
    )
  }, deleteFile = FALSE)
  
  observeEvent(input$continuar, {
    if (nPregunta2() <= length(pregunta)) {
      enable("confirmar")
      enable("respuesta")
      hide("continuar")
      hide("puntaje")
      hide("resultado")
      
      updateF7Radio(
        session = session,
        inputId = "respuesta",
        label = NULL,
        choices = c(respuesta1[nPregunta2()], respuesta2[nPregunta2()], respuesta3[nPregunta2()], respuesta4[nPregunta2()])
      )
      
      output$imagen1 <- renderImage({
        session$sendCustomMessage('removeHeight', 'imagen1')
        list(
          src = paste0("images/", img[nPregunta2()]),
          contentType = "image/png",
          alt = "Question Image",
          style = "width: 100%; height: auto; max-width: 300px; margin: 10px auto; display: block;"
        )
      }, deleteFile = FALSE)
    } else {
      hide("pregunta")
      hide("resultado")
      hide("confirmar")
      hide("continuar")
      hide("imagen1")
      hide("respuesta")
      show("resultadofinal")
      output$resultadofinal <- renderText("Tu resultado final fue: ")
    }
  })
  
  observeEvent(input$confirmar, {
    if (!is.null(input$respuesta)) {
      if (input$respuesta == respuestaC[nPregunta$countervalue]) {
        show("resultado")
        output$resultado <- renderText("Respuesta correcta!")
        show("continuar")
        show("puntaje")
        disable("respuesta")
        disable("confirmar")
        puntaje$countervalue <- puntaje$countervalue + 40
      } else {
        show("resultado")
        output$resultado <- renderText("Respuesta incorrecta. Inténtalo de nuevo.")
        puntaje$countervalue <- puntaje$countervalue - 10
      }
    }
  })
  
  output$puntaje <- renderText({
    paste("Puntaje:", puntaje$countervalue)
  })
  
  observeEvent(input$start_qr_scanner, {
    session$sendCustomMessage("start_qr_scanner", list())
  })
  
  observe({
    input$qr_code_data
    if (!is.null(input$qr_code_data)) {
      output$qr_result <- renderText({
        paste("QR code detected:", input$qr_code_data)
      })
      updateTextInput(session, "qr_instructions", value = input$qr_code_data)
    }
  })
  
  observeEvent(input$qr_instructions, {
    qr_data <- input$qr_instructions
    if (!is.null(qr_data) && qr_data != "") {
      instructions <- strsplit(qr_data, ",")[[1]]
      for (instruction in instructions) {
        parts <- strsplit(instruction, "=")[[1]]
        if (length(parts) == 2) {
          action <- parts[1]
          value <- parts[2]
          
          # Print the action and value for debugging
          cat("Action:", action, "Value:", value, "\n")
          
          if (action == "tab") {
            updateF7Tabs(session, id = "tabs", selected = value)
          } else if (action == "scroll") {
            session$sendCustomMessage('scrollTo', list(value = as.integer(value)))
          }
        }
      }
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
