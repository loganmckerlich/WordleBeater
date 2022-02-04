library(tidyverse)


alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","")

notContainInput <- textInput(inputId ="notContain", label = "Letters not in word:", 
                                      value = "",
                                      placeholder = "qwerty")
  
containInput <- textInput(inputId ="contain", label = "Letters in word:", 
                                   value = "",
                                   placeholder = "qwerty")

notHere1Input <- textInput(inputId ="notHere1", label = "Letters in word, but not first letter", 
                                    value = "",
                           placeholder = "qwerty")

notHere2Input <- textInput(inputId ="notHere2", label = "Letters in word, but not second letter", 
                                    value = "",
                           placeholder = "qwerty")

notHere3Input <- textInput(inputId ="notHere3", label = "Letters in word, but not third letter", 
                                    value = "",
                           placeholder = "qwerty")

notHere4Input <- textInput(inputId ="notHere4", label = "Letters in word, but not fourth letter", 
                                    value = "",
                           placeholder = "qwerty")

notHere5Input <- textInput(inputId ="notHere5", label = "Letters in word, but not fifth letter", 
                                    value = "",
                           placeholder = "qwerty")

myWord1Input <- selectInput(inputId ="myWord1", label = "First letter of my word:", 
                                   choices = alphabet,
                                  selected = "")

myWord2Input <- selectInput(inputId ="myWord2", label = "Second letter of my word:", 
                                   choices = alphabet,
                                   selected = "")

myWord3Input <- selectInput(inputId ="myWord3", label = "Third letter of my word:", 
                                   choices = alphabet,
                                   selected = "")

myWord4Input <- selectInput(inputId ="myWord4", label = "Fourth letter of my word:", 
                                   choices = alphabet,
                                   selected = "")

myWord5Input <- selectInput(inputId ="myWord5", label = "Fifth letter of my word:", 
                                   choices = alphabet,
                                   selected = "")
  
  
myUIBW <- fluidPage(
  #title
  titlePanel("Beating Wordle", windowTitle = "Beating Wordle"),
  sidebarLayout(
    mainPanel(
      containInput,
      notContainInput,
      myWord1Input,
      myWord2Input,
      myWord3Input,
      myWord4Input,
      myWord5Input,
      notHere1Input,
      notHere2Input,
      notHere3Input,
      notHere4Input,
      notHere5Input),
    sidebarPanel(
      tableOutput("table")
      ),
    fluid = TRUE
    )
  )

  

  
  
  
  
  