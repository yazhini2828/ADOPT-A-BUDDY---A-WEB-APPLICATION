library(shinydashboard)
library(DT)
library(readr)
library(ijtiff)
library(raster)
library(imager)
library(shiny)
library(shinyWidgets)
library(RMySQL)
library(DBI)
library(shinythemes)
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = 'yazh2828',
                 dbname = 'rpro', host = 'localhost')

dbExecute(con, 'CREATE TABLE adopt_a_buddy(pet_id varchar(50),pet_breed varchar(50),colour_type varchar(50),height int,weight int,length int,months_of_stay int);')
ui <- fluidPage(theme = shinytheme("flatly"),
                # Show a plot of the generated distribution
                titlePanel("DOG SHELTER MANAGEMENT"),
                
                mainPanel(
                  img(src = "header.png", height = 200, width = 1000),
                  
                  tabsetPanel(type = "tabs",
                              tabPanel("HOME",fluid=TRUE,sidebarPanel(img(src = "dog.png", height = 170, width = 170),h1("MISSION"),br(),
                                                                      p("To rescue and rehabilitate animals and nature in distress in our immediate environment.

To provide free medical care, shelter and lifetime care for those animals that cannot be rehabilitated."),
                                                                      p("To analyse the causes of the existing problems and create solutions and improvements as to how the suffering can be relieved or prevented.

To promote animal friendly farm practices in organic farming.
")),    mainPanel(
  h1("ADOPT A BUDDY"),
  p("This non-profit organisation and animal shelter helps homeless animals by taking them into the shelter and nursing them back to health, as well as finding them a new home. But more than that, they feed homeless animals, provide them with collars and identification tags and vaccinate and spay the stray animals.They have a very thorough adoption process for the pet, that involves a screening, a home visit and signing of the adoption papers. Scan will also make visits regularly to check up on the animal in their new home. "),br(),p("And moreover, they also provide veterinary services. "))),  
  
  tabPanel("GALLERY",fluid=TRUE,mainPanel(img(src = "gallery.png", height = 700, width = 800))),
  
  tabPanel("ADOPTION",fluid=TRUE,titlePanel("FILL THE DETAILS :"),
           textInput('pet_id', 'pet_id', value = ''),
           textInput('pet_breed', 'pet_breed', value = ''),
           textInput('colour_type', 'colour_type', value = ''),
           numericInput('height', 'height', value = 0, step = 0),
           numericInput('weight', 'weight', value = 0, step = 0),
           numericInput('length', 'length', value = 0, step = 0),
           numericInput('months_of_stay', 'months_of_stay', value = 0, step = 0),
           actionButton('writetodb', 'submit'),
           tableOutput('dbtable')
           ,fluidRow(
             actionButton("submit", "SUBMIT", class = "btn-lg btn-success"))),
  
  tabPanel("ABOUT US",fluid=TRUE,mainPanel(
    h1("OBJECTIVES:"),br(),p("To provide free first-aid, veterinary care and shelter at Karuna's in and out patient department..",
                             br(),p("To implement Animal Birth Control and Anti Rabies program for Indian dogs."
                                    ,br(),p("Conducting fund raising events."),
                                    p("To encourage any farming or industrial activity which is eco and animal friendly.")))))))
  ,sidebarPanel(shinyauthr::loginUI(id = "login"),div(class = "pull-right", shinyauthr::logoutUI(id = "logout"))))


server <- function(input, output) {
  mytableinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT pet_id,pet_breed,colour_type,height,weight,length,months_of_stay from adopt_a_buddy')
  })
  observeEvent(input$writetodb, {
    sql1 = "INSERT INTO adopt_a_buddy (pet_id,pet_breed,colour_type,height,weight,length,months_of_stay) VALUES (?pet_id,?pet_breed,?colour_type,?height,?weight,?length,?months_of_stay)"
    
    sql <- sqlInterpolate(con, sql1,pet_id=input$pet_id, pet_breed=input$pet_breed,colour_type=input$colour_type,height=input$height,weight=input$weight,length=input$length,months_of_stay=input$months_of_stay)
    dbExecute(con, sql)
    dbtrigger$trigger()
  })
  output$dbtable <- renderTable({
    mytableinshiny()
  })
}
shinyApp(ui = ui, server = server)