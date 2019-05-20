#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(rtweet)
require(tidyr)
require(tidytext)
require(magrittr)
require(purrr)
require(qdapRegex)
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(stopwords)
require(wordcloud2)
require(radarchart)
require(shinycssloaders)
require(twitteR)
require(dplyr)
require(magrittr)
require(purrr)
require(qdapRegex)


#- API Twitter Conection
api_key <- "sILmtvIwBWhVexvnDpZqprhSQ"
api_secret <- "Aln19TcuYrEGGlIV80xRNIXDLkMBsl0NBaKmcuJfhxUDlSLg5Y"
access_token <- "53513135-prwArxpW3L5OU7ldskjQOpjHqtXWr9azm5hUXnGQ5"
access_secret <- "MxyWuM1g8OFpworxaYnVceAKWe82VvRrqHv6wLMh1EYQH"

create_token(app = "rtweet-Mining1",api_key, api_secret, access_token, access_secret)

#- Load functions
source("functions/functions.R")

#- Log_Pesquisa
log_pesquisa <- read.csv("log_pesquisa.csv", sep= ";")

#- Inicia DB
db <- data.frame(screenName = c(0,0,0,0),
                 text = c(0,0,0,0),
                 text_limpo = c(0,0,0,0),
                 id = c(0,0,0,0),
                 date = c(0,0,0,0),
                 latitude = c(0,0,0,0),
                 longitude = c(0,0,0,0))

#- Data Sentimental Analysis
palavras_pt <- readRDS("data/Portugues.rds") 
palavras_es <- readRDS("data/Espanhol.rds")
palavras_en <- readRDS("data/Ingles.rds")

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  skin = "yellow-light",
  
  #- Dashboard Title
  dashboardHeader(title = span(tagList(icon("twitter"), "@IesBrazil"))),
  
  
  #- Left Menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Principal", tabName = "home", icon = icon("home")),
      menuItem("Busca", tabName = "search", icon = icon("search")),
      menuItem("Nuvem de Palavras", tabName = "cloud", icon = icon("cloud")),
      menuItem("Análise de Sentimento", tabName = "sentimental", icon = icon("eye"))
    )
  ),
  
  dashboardBody(
    
    #- Remove error mensages
    tags$style(
      type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    setShadow("box"),
    
    tabItems(
      
      #- First TAB
      tabItem(
        tabName = "home",
        
        fluidRow(
          widgetUserBox(
            title = "Rodrigo Eiras",
            subtitle = "Solution Consultant",
            width = 7,
            type = 2,
            src = "imagens/profile.png",
            color = "yellow",
            align = "center",
            
            socialButton(
              url = "https://www.linkedin.com/in/rodrigoeiras",
              type = "linkedin"
            ),
            
            socialButton(
              url = "https://twitter.com/rsveiras",
              type = "twitter"
            ),
            
            socialButton(
              url = "rsveiras@gmail.com",
              type = "google"
            ),
            
            socialButton(
              url = "https://github.com/rodrigoeiras",
              type = "github"
            ),
            
            closable = FALSE,
            footer = "Essa aplicação trabalha recuperando tweets postados por usuários da rede social para então, avaliar o sentimentos da comunidade sobre o determinado termo pesquisado."
          ),
          
          box(width = 5,
              title = h2("Últimas 5 palavras buscadas",
                         style = "color:#3C8DBC",
                         align = "center"),
              
              tags$hr(),
              
              h3(log_pesquisa$termo[nrow(log_pesquisa)], 
                 style = "color:#00A7D0",
                 align = "center"),
              
              tags$hr(),
              
              h3(log_pesquisa$termo[nrow(log_pesquisa)-1], 
                 style = "color:#00A7D0",
                 align = "center"),
              
              tags$hr(),
              
              h3(log_pesquisa$termo[nrow(log_pesquisa)-2], 
                 style = "color:#00A7D0",
                 align = "center"),
              
              tags$hr(),
              
              h3(log_pesquisa$termo[nrow(log_pesquisa)-3], 
                 style = "color:#00A7D0",
                 align = "center"),
              
              tags$hr(),
              
              h3(log_pesquisa$termo[nrow(log_pesquisa)-4], 
                 style = "color:#00A7D0",
                 align = "center")
          )
          
        )
        
      ),
      
      #- Second TAB
      tabItem(
        tabName = "search",
        
        fluidRow(
          box(
            title = "Siga os passos para buscar no Twitter:",
            
            timelineBlock(
              
              timelineItem(
                title = "Primeiro passo",
                icon = "signature",
                color = "aqua-active",
                selectInput("lang", 
                            "Selecione idioma:",
                            c("Portuguese"="pt","Spanish"="es","English"="en"))
              ),
              
              timelineItem(
                title = "Segundo passo",
                icon = "signature",
                color = "aqua-active",
                sliderInput("n", 
                            "Número de tweets para retornar:", 
                            50, 1000, 100)
              ),
              
              timelineItem(
                title = "Terceiro passo",
                icon = "signature",
                color = "aqua-active",
                textInput(inputId = "text",
                          label = "Palavra chave para buscar no Twitter:",
                          placeholder = "ex: Brasil"),
                submitButton(text = " Buscar ", icon = icon("search"))
              ),
              
              timelineStart(color = "aqua-active"),
              
              tags$hr(),
              
              uiOutput("tamanho_base") 
              
              
              
            )
            
            
          ),
          
          
          uiOutput("preview1"),
          
          uiOutput("preview2"),
          
          uiOutput("preview3")
          
        )
        
        
        
        
        
        
        
      ),
      
      #- Third TAB
      tabItem(
        tabName = "cloud",
        
        fluidRow(
          box(
            textInput(inputId = "stop",
                      label = "Adicionar StopWords",
                      placeholder = "ex: de, como, para"),
            
            numericInput("freq_min", 
                         label = "Frequência minima para construir a nuvem de palavras:", 
                         value = 2,
                         min = 1, 
                         max = 100, 
                         step = 1,
                         width = NULL),
            
            prettyToggle(
              inputId = "div_words",
              label_on = "Separar Positivas/Negativas",
              label_off = "Não separar Positivas/Negativas",
              icon_on = icon("check"),
              icon_off = icon("remove"),
              animation = "smooth"
            ),
            
            prettyCheckboxGroup(
              inputId = "show_words",
              selected = c("green", "red", "blue"),
              label = "Mostrar palavras",
              thick = TRUE,
              choices = c("Positivas"="green", "Negativas"="red", "Neutras"="blue"),
              animation = "pulse",
              status = "info"
            ),
            
            submitButton(text = " Atualizar ", icon = icon("sync-alt")),
            
            tags$hr(),
            
            downloadButton("freq_palavras", "Download das Palavras")
            
          ),
          
          box(
            wordcloud2Output("grafico1")
          )
          
          
        )
        
      ),
      
      #- Third TAB
      tabItem(
        tabName = "sentimental",
        
        fluidRow(
          box(
            width = 4,
            textInput(inputId = "pal_positive",
                      label = "Adicionar termos positivos",
                      #value = " ",
                      placeholder = "ex: one, two"),
            
            textInput(inputId = "pal_negative",
                      label = "Adicionar termos negativos",
                      #value = " ",
                      placeholder = "ex: one, two"),
            
            prettyToggle(
              inputId = "ponderar",
              label_on = "Weighting by n",
              label_off = "No Weighting by n",
              icon_on = icon("check"),
              icon_off = icon("remove")
            ),
            
            submitButton(text = " Atualizar ", icon = icon("sync-alt"))
            
          ),
          
          
          box(
            width = 8,
            withSpinner(chartJSRadarOutput("grafico2"), 
                        type = getOption("spinner.type", default = 1))
          )
          
        )
      )
      
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) 
{
  
  
  #Declarando a Busca
  pesquisa <- reactive({
    
    db <- search_tweets2(
      q = input$text, 
      lang = input$lang, 
      n = input$n)
    
    db <- db %>%
      mutate(text_limpo = clean_text(text))
    
    dt_informacoes_novas <- data.frame(termo = input$text,
                                       idioma = input$lang,
                                       num = input$n)
    
    dt_informacoes_velhas <- read.csv("log_pesquisa.csv", 
                                      sep = ";")
    
    dt_informacoes <- bind_rows(dt_informacoes_novas, dt_informacoes_velhas)
    
    write.table(dt_informacoes,
                file = "log_pesquisa.csv",
                sep=";",
                row.names = FALSE)
    
    return(db)
  })
  
  
  #- TAMANHO BASE
  tam_base <- reactive({
    p <- pesquisa()
    
    if(is_empty(p))
    {
      texto <- paste0("No tweet was found.")
      
      retorno_ui <- box(width = 12,
                        h4(texto,
                           style = "color:#00A7D0",
                           align = "center")
      ) 
      
    }
    
    if(!is_empty(p))
    {
      texto <- paste0("It was found ",nrow(p)," distinct tweets about ",input$text,".")
      retorno_ui <- box(width = 12,
                        h4(texto, 
                           style = "color:#00A7D0",
                           align = "center"),
                        tags$hr(),
                        downloadButton("tweets", "Download Tweets")
      ) 
    }
    
    return(retorno_ui)
    
  })
  
  
  # Contando frequência de palavras
  palavras_freq <- reactive({
    db <- pesquisa()
    my_stop <- as_tibble(c(unlist(strsplit(input$stop, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")), 
                           stopwords(input$lang)))
    
    palavras <- db %>%
      unnest_tokens(word, text_limpo) %>%
      count(word, sort = TRUE) %>%
      anti_join(my_stop, by = c('word' = 'value'))
    
    return(palavras)
  })
  
  
  # Criando a núvem de palavras
  word_cloud <- reactive({
    d <- palavras_freq()
    freq_min <- input$freq_min
    div_words <- input$div_words
    show_words <- input$show_words
    
    if(input$lang == 'pt')
    {
      palavras <- palavras_pt %>%
        select(word, positive, negative)
    }
    
    if(input$lang == 'es')
    {
      palavras <- palavras_es %>%
        select(word, positive, negative)
    }
    
    if(input$lang == 'en')
    {
      palavras <- palavras_en %>%
        select(word, positive, negative)
    }
    
    if(div_words == TRUE)
    {
      d <- left_join(d, palavras, by = c("word" = "word")) %>%
        mutate(
          positive = as.numeric(if_else(is.na(positive), 0L, positive)),
          negative = as.numeric(if_else(is.na(negative), 0L, negative)),
          cores = case_when(
            positive != 0 ~ 'green',
            negative != 0 ~ 'red',
            positive == 0 & negative == 0 ~ 'blue'
          )) %>%
        select(word, n, cores) %>%
        filter(cores %in% show_words)
      
      grafico1 <- wordcloud2(d, minSize = freq_min, color = d$cores)
    }
    
    if(div_words == FALSE)
    {
      grafico1 <- wordcloud2(d, minSize = freq_min)
    }
    
    
    return(grafico1)
    
  })
  
  
  #- RADAR CHART
  radar <- reactive({
    d <- palavras_freq()
    
    if(input$lang == 'pt')
    {
      palavras <- palavras_pt
    }
    
    if(input$lang == 'es')
    {
      palavras <- palavras_es
    }
    
    if(input$lang == 'en')
    {
      palavras <- palavras_en 
    }
    
    if(input$pal_positive != "")
    {
      palavras <- palavras %>%
        add_row(word = unlist(strsplit(input$pal_positive, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")),
                positive = 1,
                negative = 0,
                anger = 0,
                anticipation = 0,
                disgust = 0,
                fear = 0,
                joy = 0,
                sadness = 0,
                surprise = 0,
                trust = 0)
    }
    
    if(input$pal_negative != "")
    {
      palavras <- palavras %>%
        add_row(word = unlist(strsplit(input$pal_negative, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")),
                positive = 0,
                negative = 1,
                anger = 0,
                anticipation = 0,
                disgust = 0,
                fear = 0,
                joy = 0,
                sadness = 0,
                surprise = 0,
                trust = 0)
    }
    
    
    
    
    
    
    # Ponderado pelo n 
    if(input$ponderar == TRUE)
    {
      d2 <- left_join(d, palavras, by = c("word" = "word")) %>%
        replace_na(
          list(
            positive = 0,
            negative = 0,
            anger = 0,
            anticipation = 0,
            disgust = 0,
            fear = 0,
            joy = 0,
            sadness = 0,
            surprise = 0,
            trust = 0)) %>%
        mutate(
          positive = positive * n,
          negative = negative *n,
          anger = anger * n,
          anticipation = anticipation * n,
          disgust = disgust * n,
          fear = fear * n,
          joy = joy * n,
          sadness = sadness * n,
          surprise = surprise * n,
          trust = trust * n
        )
    }
    
    
    # Nao ponderado por n
    if(input$ponderar == FALSE)
    {
      d2 <- left_join(d, palavras, by = c("word" = "word")) %>%
        replace_na(
          list(
            positive = 0,
            negative = 0,
            anger = 0,
            anticipation = 0,
            disgust = 0,
            fear = 0,
            joy = 0,
            sadness = 0,
            surprise = 0,
            trust = 0))
    }
    
    
    # Grafico Radar
    labs <- c("positive",
              "negative",
              "anger",
              "anticipation",
              "disgust",
              "fear",
              "joy",
              "sadness",
              "surprise",
              "trust")
    
    score <- list(
      "Score" = c(sum(d2$positive),
                  sum(d2$negative),
                  sum(d2$anger),
                  sum(d2$anticipation),
                  sum(d2$disgust),
                  sum(d2$fear),
                  sum(d2$joy),
                  sum(d2$sadness),
                  sum(d2$surprise),
                  sum(d2$trust)))
    
    grafico2 <- chartJSRadar(scores = score, labs = labs, width = 12, height = 12)
    
    return(grafico2)
    
    
  })
  
  
  #- PREVIEW
  preview1 <- reactive({
    db <- pesquisa()
    
    box1 <- widgetUserBox(
      title =  db$name[1],
      subtitle = db$description[1],
      socialButton(
        url = db$status_url[1],
        type = "twitter"
      ),
      type = 2,
      src = db$profile_image_url[1],
      background = TRUE,
      backgroundUrl = db$profile_background_url[1],
      footer = db$text[1]
    )
    
    return(box1)
  })
  
  preview2 <- reactive({
    db <- pesquisa()
    
    box1 <- widgetUserBox(
      title =  db$name[2],
      subtitle = db$description[2],
      socialButton(
        url = db$status_url[2],
        type = "twitter"
      ),
      type = 2,
      src = db$profile_image_url[2],
      background = TRUE,
      backgroundUrl = db$profile_background_url[2],
      footer = db$text[2]
    )
    
    return(box1)
  })
  
  preview3 <- reactive({
    db <- pesquisa()
    
    box1 <- widgetUserBox(
      title =  db$name[3],
      subtitle = db$description[3],
      socialButton(
        url = db$status_url[3],
        type = "twitter"
      ),
      type = 2,
      src = db$profile_image_url[3],
      background = TRUE,
      backgroundUrl = db$profile_background_url[3],
      footer = db$text[3]
    )
    
    return(box1)
  })
  
  
  #- LISTA DE OUTPUTS   
  output$tamanho_base <- renderUI(tam_base())
  
  output$grafico1 <- renderWordcloud2(word_cloud())
  
  output$grafico2 <- renderChartJSRadar(radar())
  
  output$preview1 <- renderUI(preview1())
  
  output$preview2 <- renderUI(preview2())
  
  output$preview3 <- renderUI(preview3())
  
  
  #- DOWNLOADS
  output$tweets<-downloadHandler(
    filenam=function()
    {
      paste0('tweets','.csv')
    },
    content=function(arquivo)
    {
      if(input$text!="zxdt94banana62")
      {
        write.table(pesquisa(),arquivo, sep = ";", row.names = FALSE)
      }
      else
      {
        write.csv(read.table("log_pesquisa.txt"),arquivo)
      }
    })
  
  output$freq_palavras<-downloadHandler(
    filenam = function()
    {
      paste0('freq_palavras','.csv')
    },
    content = function(arquivo)
    {
      write.table(palavras_freq(), arquivo, sep = ";", row.names = FALSE)
    })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
