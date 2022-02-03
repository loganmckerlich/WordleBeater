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


# initialize
ReactiveMyWord <- reactive({matrix(nrow = 27, ncol = 5)})
ReactiveContain <- reactive ({vector(length = 5, mode = "list") })
ReactiveNotContain <- reactive ({vector(length = 27, mode = "list") })
ReactiveNotHere <- reactive ({matrix(nrow = 27, ncol = 5)})

# ReactiveMyWord[,1] <- alphabet
# ReactiveMyWord[,2] <- alphabet
# ReactiveMyWord[,3] <- alphabet
# ReactiveMyWord[,4] <- alphabet
# ReactiveMyWord[,5] <- alphabet

# ReactiveContain[1] <- ""
# ReactiveContain[2] <- ""
# ReactiveContain[3] <- ""
# ReactiveContain[4] <- ""
# ReactiveContain[5] <- ""

# ReactiveNotHere[,1] <- ""
# ReactiveNotHere[,2] <- ""
# ReactiveNotHere[,3] <- ""
# ReactiveNotHere[,4] <- ""
# ReactiveNotHere[,5] <- ""



myServerBW<-function(input, output) {
  
  ReactiveMyWord <- matrix(nrow = 27, ncol = 5)
  ReactiveContain <- reactive ({vector(length = 5, mode = "list") })
  ReactiveNotContain <- reactive ({vector(length = 27, mode = "list") })
  ReactiveNotHere <- reactive ({matrix(nrow = 27, ncol = 5)})
  
  firstLetter <- reactive({input$myWord1})
  secondLetter <-reactive({input$myWord2}) 
  thirdLetter <- reactive({input$myWord3}) 
  fourthLetter <- reactive({input$myWord4}) 
  fifthLetter <- reactive({input$myWord5}) 
  
  ReactiveMyWord[,1] <- firstLetter()
  ReactiveMyWord[,2] <- secondLetter()
  ReactiveMyWord[,3] <- thirdLetter()
  ReactiveMyWord[,4] <- fourthLetter()
  ReactiveMyWord[,5] <- fifthLetter()
  
  
  
 
  
    output$table <- renderTable({
    tableData <- fiveLet%>% 
      filter(let1 %in% ReactiveMyWord[,1]) %>% 
      filter(let2 %in% ReactiveMyWord[,2]) %>% 
      filter(let3 %in% ReactiveMyWord[,3]) %>% 
      filter(let4 %in% ReactiveMyWord[,4]) %>% 
      filter(let5 %in% ReactiveMyWord[,5]) %>% 
      filter(grepl(ReactiveContain[1],words)) %>% 
      filter(grepl(ReactiveContain[2],words)) %>% 
      filter(grepl(ReactiveContain[3],words)) %>% 
      filter(grepl(ReactiveContain[4],words)) %>% 
      filter(grepl(ReactiveContain[5],words)) %>% 
      filter(!(let1 %in% ReactiveNotContain)) %>% 
      filter(!(let2 %in% ReactiveNotContain)) %>% 
      filter(!(let3 %in% ReactiveNotContain)) %>% 
      filter(!(let4 %in% ReactiveNotContain)) %>% 
      filter(!(let5 %in% ReactiveNotContain)) %>% 
      filter(!(let1 %in% ReactiveNotHere[,1])) %>% 
      filter(!(let2 %in% ReactiveNotHere[,2])) %>% 
      filter(!(let3 %in% ReactiveNotHere[,3])) %>% 
      filter(!(let4 %in% ReactiveNotHere[,4])) %>% 
      filter(!(let5 %in% ReactiveNotHere[,5]))
    t<-tibble(tableData)
    print(t)
  })
}