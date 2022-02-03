library(tidyverse)

words <-read.csv("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", stringsAsFactors = F) %>% 
  rename(words = X2)

fiveLet <- words %>%
  filter(str_length(words) == 5) %>% 
  mutate(words = str_to_lower(words)) %>% 
  mutate(words = as.character(words))

fiveLet<- fiveLet %>% 
  mutate(let1 = str_sub(fiveLet$words, 1,1)) %>% 
  mutate(let2 = str_sub(fiveLet$words, 2,2)) %>% 
  mutate(let3 = str_sub(fiveLet$words, 3,3)) %>% 
  mutate(let4 = str_sub(fiveLet$words, 4,4)) %>% 
  mutate(let5 = str_sub(fiveLet$words, 5,5))

alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","")

# goals of server
# 1 take input and add it into vectors/matrix that i initialize
# 2 use these vectors/matrix to display a tibble of possible words

myServerBW <- function(input, output){
  
  output$table <- renderTable({
    # 
    # # create vectors and matrix
    # myWord <- matrix(nrow = 27, ncol = 5)
    # contain <- vector(length = 5, mode = "list")
    # notContain <- vector(length = 27, mode = "list")
    # notHere <- matrix(nrow = 27, ncol = 5)
    
    # update vectors
    
    # if blank, its alpohabet
    contain[1] <- ifelse(input$contain[1] == "", alphabet,input$contain[1])
    contain[2] <- ifelse(input$contain[2] == "", alphabet,input$contain[2])
    contain[3] <- ifelse(input$contain[3] == "", alphabet,input$contain[3])
    contain[4] <- ifelse(input$contain[4] == "", alphabet,input$contain[4])
    contain[5] <- ifelse(input$contain[5] == "", alphabet,input$contain[5])
    
    # if blank its nothing
    notContain <- ifelse(input$notContain == "", "",c(input$notContain))
    
    # update matrix if it is blank, its any letter
    myWord[,1] <-ifelse(input$myWord1 =="", alphabet,c(print(input$myWord1)))
    myWord[,2] <-ifelse(input$myWord2 =="", alphabet,c(print(input$myWord2)))
    myWord[,3] <-ifelse(input$myWord3 =="", alphabet,c(print(input$myWord3)))
    myWord[,4] <-ifelse(input$myWord4 =="", alphabet,c(print(input$myWord4)))
    myWord[,5] <-ifelse(input$myWord5 =="", alphabet,c(print(input$myWord5)))
    
    # update not here matrix, if its blank, its nothing
    notHere[,1] <- ifelse(input$notHere1 == "", "",c(print(input$notHere1)))
    notHere[,2] <- ifelse(input$notHere2 == "", "",c(print(input$notHere2)))
    notHere[,3] <- ifelse(input$notHere3 == "", "",c(print(input$notHere3)))
    notHere[,4] <- ifelse(input$notHere4 == "", "",c(print(input$notHere4)))
    notHere[,5] <- ifelse(input$notHere5 == "", "",c(print(input$notHere5)))
    
    # making data from vector and matrix
    possibleWords <- fiveLet %>% 
      filter(let1 %in% myWord[,1]) %>% 
      filter(let2 %in% myWord[,2]) %>% 
      filter(let3 %in% myWord[,3]) %>% 
      filter(let4 %in% myWord[,4]) %>% 
      filter(let5 %in% myWord[,5]) %>% 
      filter(grepl(contain[1],words)) %>% 
      filter(grepl(contain[2],words)) %>% 
      filter(grepl(contain[3],words)) %>% 
      filter(grepl(contain[4],words)) %>% 
      filter(grepl(contain[5],words)) %>% 
      filter(!(let1 %in% notContain)) %>% 
      filter(!(let2 %in% notContain)) %>% 
      filter(!(let3 %in% notContain)) %>% 
      filter(!(let4 %in% notContain)) %>% 
      filter(!(let5 %in% notContain)) %>% 
      filter(!(let1 %in% notHere[,1])) %>% 
      filter(!(let2 %in% notHere[,2])) %>% 
      filter(!(let3 %in% notHere[,3])) %>% 
      filter(!(let4 %in% notHere[,4])) %>% 
      filter(!(let5 %in% notHere[,5])) %>% 
      select(words)
    
    t <- tibble(possibleWords)
    print(t)
  
  })
  
  
  
  
  

}