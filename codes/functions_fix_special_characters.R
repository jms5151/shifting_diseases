library(stringr)

# replace 
replace_spanish_characters <- function(x){
  x <- iconv(x, 'latin1', 'ASCII//TRANSLIT')
  x <- gsub('_', ' ', x)
  x <- gsub('i\\?.|A\\?A\\?A\\?A\\?A\\?A\\?', 'n', x)
  x <- gsub('A\\-', 'i', x)
  x <- gsub('  |^ | $|\\?|\\!|\\.', '', x)
  x <- str_to_title(x)
  x
}
