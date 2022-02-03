library(tidyverse)

words <-read.csv("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", stringsAsFactors = F) %>% 
  rename(words = X2)

fiveLet <- words %>%
  filter(str_length(words) == 5) %>% 
  mutate(words = str_to_lower(words)) %>% 
  mutate(words = as.character(words)) %>% 
  mutate(let1 = str_sub(fiveLet$words, 1,1)) %>% 
  mutate(let2 = str_sub(fiveLet$words, 2,2)) %>% 
  mutate(let3 = str_sub(fiveLet$words, 3,3)) %>% 
  mutate(let4 = str_sub(fiveLet$words, 4,4)) %>% 
  mutate(let5 = str_sub(fiveLet$words, 5,5))

alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","")

notContainInput <- selectInput(inputId ="notContain", label = "Letters not in word:", 
                                      choices = alphabet,
                                      selected = "",
                                      multiple = TRUE)
  
containInput <- selectInput(inputId ="contain", label = "Letters in word:", 
                                   choices = alphabet,
                                   selected = "",
                                   multiple = TRUE)

notHere1Input <- selectInput(inputId ="notHere1", label = "Letters in word, but not first letter", 
                                    choices = alphabet,
                                    selected = "",
                                    multiple = TRUE)
  
notHere2Input <- selectInput(inputId ="notHere2", label = "Letters in word, but not second letter", 
                                    choices = alphabet,
                                    selected = "",
                                    multiple = TRUE)
  
notHere3Input <- selectInput(inputId ="notHere3", label = "Letters in word, but not third letter", 
                                    choices = alphabet,
                                    selected = "",
                                    multiple = TRUE)
  
notHere4Input <- selectInput(inputId ="notHere4", label = "Letters in word, but not fourth letter", 
                                    choices = alphabet,
                                    selected = "",
                                    multiple = TRUE)
  
notHere5Input <- selectInput(inputId ="notHere5", label = "Letters in word, but not fifth letter", 
                                    choices = alphabet,
                                    selected = "",
                                    multiple = TRUE)

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
      myWord1Input,
      myWord2Input,
      myWord3Input,
      myWord4Input,
      myWord5Input,
      containInput,
      notContainInput,
      notHere1Input,
      notHere2Input,
      notHere3Input,
      notHere4Input,
      notHere5Input),
    sidebarPanel(
      tableOutput("table")
      )
    )
  )
  

  
  
  
  
  