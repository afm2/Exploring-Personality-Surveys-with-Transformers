library(tidyverse)
setwd("/Users/Alex/Library/Mobile Documents/com~apple~CloudDocs/School/Dissertation/Defense/Results")
df <- openxlsx::read.xlsx(xlsxFile = "./Collab Output/HEXACO 100-Items_GPT-2 Large RoBERTa Large Jul-18-2021.xlsx", sheet = 2)
df <- openxlsx::read.xlsx(xlsxFile = "./R Output/R Screened Items 8-8-2021.xlsx", sheet = 1)

df %>%
   filter(!is.na(`SYNTAX/SEMANTICS`) | !is.na(ENTAILMENT) | !is.na(CONTEXT)) %>% 
   #filter(is.na(`SYNTAX/SEMANTICS`) & is.na(ENTAILMENT) & is.na(CONTEXT)) %>% 
   select(Factor) %>% unlist(., use.names = F) %>% 
   table(.)
   

dim(df)
df = df %>% distinct(Item, .keep_all = T)
head(df)
df$Item = gsub(x = df$Item, pattern = "I am", replacement = "I'm")
df$Item = gsub(x = df$Item, pattern = "do not", replacement = "don't")
df$Item = gsub(x = df$Item, pattern = "don‖t", replacement = "don't")
df$Item = gsub(x = df$Item, pattern = "I‖m", replacement = "I'm")
df$Item = gsub(x = df$Item, pattern = "I‡m", replacement = "I'm")
df$Item = gsub(x = df$Item, pattern = "‖", replacement = "")
df$Item = gsub(x = df$Item, pattern = "don‡t", replacement = "don't")
df$Item = gsub(x = df$Item, pattern = "‡", replacement = "")
df$Item = gsub(x = df$Item, pattern = "�", replacement = "'")
#df$Item = gsub(x = df$Item, pattern = "I have", replacement = "I've")
#df$Item = gsub(x = df$Item, pattern = "I will", replacement = "I'll")
df$Item = gsub(x = df$Item, pattern = "cannot", replacement = "can't")
df$Item = gsub(x = df$Item, pattern = "can not", replacement = "can't")
df$Item = gsub(x = df$Item, pattern = "have not", replacement = "haven't")
df$Item = gsub(x = df$Item, pattern = "are not", replacement = "aren't")
df$Item = gsub(x = df$Item, pattern = "will not", replacement = "won't")
df$Item = gsub(x = df$Item, pattern = "would not", replacement = "wouldn't")
df$Item = gsub(x = df$Item, pattern = "could not", replacement = "couldn't")
df$Item = gsub(x = df$Item, pattern = "who is", replacement = "who's")
#df$Item = gsub(x = df$Item, pattern = "I would", replacement = "I'd")

df = df %>% filter(!grepl(pattern = "that\\.", x = df$Item))
df = df %>% filter(!grepl(pattern = "that's", x = df$Item))
df = df %>% filter(!grepl(pattern = "that is why", x = df$Item))
df = df %>% filter(!grepl(pattern = "I think that", x = df$Item))
df = df %>% filter(!grepl(pattern = "don\\.", x = df$Item))
#df = df %>% filter(!grepl(pattern = "do this", x = df$Item))
df = df %>% filter(!grepl(pattern = "this", x = df$Item))
df = df %>% filter(!grepl(pattern = "these", x = df$Item))
#df = df %>% filter(!grepl(pattern = " thing\\.", x = df$Item))
df = df %>% filter(!grepl(pattern = " here", x = df$Item))

df = df %>% filter(!grepl(pattern = " his ", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = " him ", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = " her ", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = " hers ", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = " her\\.", x = df$Item))
df = df %>% filter(!grepl(pattern = " him\\.", x = df$Item))
df = df %>% filter(!grepl(pattern = " she ", x = df$Item))
df = df %>% filter(!grepl(pattern = " he ", x = df$Item))

df = df %>% filter(!grepl(pattern = "girl", x = df$Item))
df = df %>% filter(!grepl(pattern = "guy", x = df$Item))
df = df %>% filter(!grepl(pattern = "boy", x = df$Item))
df = df %>% filter(!grepl(pattern = "man", x = df$Item))
df = df %>% filter(!grepl(pattern = "woman", x = df$Item))
df = df %>% filter(!grepl(pattern = "children", x = df$Item))
df = df %>% filter(!grepl(pattern = "kids", x = df$Item, perl = T, ignore.case = T))
df = df %>% filter(!grepl(pattern = "mom", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = "dad", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = "father", x = Item))
df = df %>% filter(!grepl(pattern = "mother", x = Item))
df = df %>% filter(!grepl(pattern = "sister", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = "brother", x = df$Item, ignore.case = T))
df = df %>% filter(!grepl(pattern = "parent", x = df$Item))

df = df %>% filter(!grepl(pattern = "(?=I)[A-H]|[^Q][J-Z]", x = df$Item, perl = T)) # No proper nouns (except items referencing IQ...)
df = df %>% filter(!grepl(pattern = "[^people]they", x = df$Item, ignore.case = T)) # Only allow instances using they, if it also contains people (such items tend to avoid entailment issues with prompt)
df = df %>% filter(!grepl(pattern = ";", x = df$Item, ignore.case = T)) # Drop obvious double barrels
df = df %>% filter(!grepl(pattern = "[[:digit:]]", x = df$Item, perl = T, ignore.case = T))
df = df %>% filter(!grepl(pattern = " do that ", x = df$Item, perl = T, ignore.case = T))
 
df = df %>% filter(!grepl(pattern = " used to ", x = Item))

drop_X1 = df %>% filter(!grepl(pattern = "people|friend|other|person|character", x = Item, perl = T, ignore.case = T)) %>%
   filter(grepl(pattern = "their", x = Item, perl = T, ignore.case = T)) %>% 
   select(X1) %>% unlist(., use.names = F)

df = df %>% filter(!(X1 %in% drop_X1)) 

 length(unique(df$Item))

df %>% 
  filter(ENTAILMENT == 1)
df %>% 
  filter(`SYNTAX/SEMANTICS` == 1)


 df %>% filter(grepl(pattern = "‡", x = df$Item, perl = T, ignore.case = F))
 df %>% filter(grepl(pattern = "‗", x = df$Item, perl = T, ignore.case = F))
 df %>% filter(grepl(pattern = "‖", x = df$Item, perl = T, ignore.case = F))
 df %>% filter(grepl(pattern = "‘", x = df$Item, perl = T, ignore.case = F))
 df %>% filter(grepl(pattern = "that is ", x = df$Item, perl = T, ignore.case = F))
 df %>% filter(grepl(pattern = "[[:digit:]]", x = df$Item, perl = T, ignore.case = T))
 df %>% filter(grepl(pattern = " one\\.", x = df$Item))


 
 #df %>% filter(grepl(pattern = "I \\w* the \\w* \\.", x = df$Item))
 df %>% filter(grepl(pattern = "�", x = df$Item))
 df %>% filter(grepl(pattern = "this", x = df$Item)) %>% 
   select(Item)#%>% View(.)

 df %>% filter(grepl(pattern = "don ", x = df$Item))
 df %>% filter(!grepl(pattern = "don\\.", x = df$Item))
 today <- paste(lubridate::month(lubridate::today()), lubridate::day(lubridate::today()), lubridate::year(lubridate::today()), sep = "-")
openxlsx::write.xlsx(df, paste0("./R Output/R Screened Items V2 ", today, ".xlsx"), T)
 