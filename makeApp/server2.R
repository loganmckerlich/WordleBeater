library(tidyverse)

fiveLet <-read.csv("fiveLet.csv")

alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")







# goals of server
# 1 take input and add it into vectors/matrix that i initialize
# 2 use these vectors/matrix to display a tibble of possible words
# reactive vector for contain and not contain
# reactive matrix for myWord and notHere

myServerBW <- function(input, output){
  #### initialize ####
  score <- function(x) {
    i=1
    score <- vector(length = length(x))
    for(i in 1:length(x)) {
      word <- x[i]
      score[i] <- allLetters %>% 
        filter(letter == substr(word,1,1)|
                 letter == substr(word,2,2)|
                 letter == substr(word,3,3)|
                 letter == substr(word,4,4)|
                 letter == substr(word,5,5)) %>% 
        select(count) %>% 
        sum()
    }
    score
  }
  myWord <- matrix(nrow = 26, ncol = 5)
  contain <- vector(length = 5, mode = "list")
  notContain <- vector(length = 26, mode = "list")
  notHere <- matrix(nrow = 26, ncol = 5)
  
  First <- ""
  Second <- ""
  Third <- ""
  Fourth <- ""
  Fifth <- ""
  
  # put yellows here
  yellow <- ""
  
  #put greys here
  grey <- ""
  
  # when a letter is correct but not in this spot
  notFirst <- ""
  notSecond <- ""
  notThird <- ""
  notFourth <- ""
  notFifth <- ""
  #### ####

  
  output$table <- renderTable({
    #### ####
    contain <-c(str_split(input$contain,"",n=nchar(input$contain), simplify = T))[1:5]
    contain[is.na(contain)] <- ""
    
    notContain <- c(str_split(input$notContain,"",n=nchar(input$notContain), simplify =T))[1:nchar(input$notContain)][1:26]
    notContain[is.na(notContain)] <- ""
    
    notHere[,1] <- c(str_split(input$notHere1,"",n=nchar(input$notHere1), simplify = T))[1:26]
    notHere[,2] <- c(str_split(input$notHere2,"",n=nchar(input$notHere2), simplify = T))[1:26]
    notHere[,3] <- c(str_split(input$notHere3,"",n=nchar(input$notHere3), simplify = T))[1:26]
    notHere[,4] <- c(str_split(input$notHere4,"",n=nchar(input$notHere4), simplify = T))[1:26]
    notHere[,5] <- c(str_split(input$notHere5,"",n=nchar(input$notHere5), simplify = T))[1:26]
    notHere[is.na(notHere)] <- ""
    
    myWord[,1] <- if(input$myWord1==""){alphabet}else{input$myWord1}
    myWord[,2] <- if(input$myWord2==""){alphabet}else{input$myWord2}
    myWord[,3] <- if(input$myWord3==""){alphabet}else{input$myWord3}
    myWord[,4] <- if(input$myWord4==""){alphabet}else{input$myWord4}
    myWord[,5] <- if(input$myWord5==""){alphabet}else{input$myWord5}
    
    possibleWords <- fiveLet %>% 
      filter(let1 %in% myWord[,1]) %>% 
      filter(let2 %in% myWord[,2]) %>% 
      filter(let3 %in% myWord[,3]) %>% 
      filter(let4 %in% myWord[,4]) %>% 
      filter(let5 %in% myWord[,5]) %>% 
      filter(grepl(contain[1],word)) %>% 
      filter(grepl(contain[2],word)) %>% 
      filter(grepl(contain[3],word)) %>% 
      filter(grepl(contain[4],word)) %>% 
      filter(grepl(contain[5],word)) %>% 
      filter(!(let1 %in% notContain)) %>% 
      filter(!(let2 %in% notContain)) %>% 
      filter(!(let3 %in% notContain)) %>% 
      filter(!(let4 %in% notContain)) %>% 
      filter(!(let5 %in% notContain)) %>% 
      filter(!(let1 %in% notHere[,1])) %>% 
      filter(!(let2 %in% notHere[,2])) %>% 
      filter(!(let3 %in% notHere[,3])) %>% 
      filter(!(let4 %in% notHere[,4])) %>% 
      filter(!(let5 %in% notHere[,5]))
    
    allLetters <- matrix(nrow = 5*nrow(possibleWords), ncol = 1)

    allLetters[,1] <-c(possibleWords$let1,possibleWords$let2, possibleWords$let3,
                       possibleWords$let4, possibleWords$let5)

    allLetters <- as.data.frame(allLetters) %>%
      group_by(V1) %>%
      summarize(count = n()) %>%
      rename(letter = V1)

    possibleWords$score <- score(possibleWords$word)

    possibleWords <- possibleWords %>%
      arrange(-score)
    #### ####
    t <- tibble(head(possibleWords %>% 
                       select(word,score),30))
    print(t)
  })
  
  
  
  
  

}