library(shiny)
library(shinyjs)




appCSS <- "
#loading-content {
position: absolute;
background: #ff0000;
padding-top: 20%;
padding-left: 20%;
padding-right: 20%;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

useShinyjs()
inlineCSS(appCSS)

shinyUI(fluidPage(
  
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h1("JANUS* WORD PREDICTION APP"),
    h2("Proof of Concept"),
    h2("The application is currently loading its database.This can take up to 1 minute. The app reads 8.2 Mio n-Grams. Please wait. Once the database is loaded this red screen will go away and you will see a grey form with input and output fields."),
    h2("THANK YOU and SORRY for the inconvenience!!"),
    h2("Alex Sickert"),
    h4("---"),
    h4("* Wikipedia: Janus is the god of beginnings, gates, transitions, time, doorways, passages, and endings. He is usually depicted as having two faces, since he looks to the future and to the past.")
  ),
 
  
  
  hidden(
    div(
      id = "app-content",
      
      headerPanel("Word prediction (proof of concept). Please write a word or several words and the system will predict the next word"),
      sidebarPanel(
        textInput("textIn", label = h3("Text input"), value = ""), 
        h4("Note: You don't need to press enter. The app automatically starts the prediction. I usually takes 1-5 seconds until the result will be displayed.")
      ),
      mainPanel(
        h3('Predicted word:'),
        h3(textOutput("infoText")),
        verbatimTextOutput("textOut"),
        h3('Info about the prediction algorithm and result:'),
        htmlOutput("textOut2")
        
      )
    )
  )
  
  
  
  

))
