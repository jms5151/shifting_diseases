library(stringr)

format_capitalization_and_punctuation <- function(df, colName){
  df[, colName] <- gsub('_', ' ', df[, colName])
  df[, colName] <- str_to_title(df[, colName])
  return(df)
}


# spanish to english spelling 
replace_spanish_characters <- function(x){
  x <- iconv(x, 'latin1', 'ASCII//TRANSLIT')
  x <- gsub('_', ' ', x)
  x <- gsub('i\\?.|A\\?A\\?A\\?A\\?A\\?A\\?', 'n', x)
  x <- gsub('A\\-', 'i', x)
  x <- gsub('  |^ | $|\\?|\\!|\\.', '', x)
  x <- str_to_title(x)
  x
}

# replace in list
spanish_countries <- c(
  'Brazil'
  , 'Argentina'
  , 'Cuba'
  , 'El Salvador'
  , 'Nicaragua'
  , 'Panama'
  , 'Venezuela Bolivarian Republic Of'
)

replace_char_in_list <- function(df){
 if(length(grep(unique(df$Country), spanish_countries)) == 1){
   df$Admin_unit <- replace_spanish_characters(x = df$Admin_unit)
 }
  return(df)
}
