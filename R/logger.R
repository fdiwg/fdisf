#basic logger facility

logger <- function(type, text){
  cat(sprintf("[fdisf][%s] %s \n", type, text))
}

INFO <- function(text){
  logger("INFO", text)
}

WARN <- function(text){
  logger("WARNING", text)
}

ERROR <- function(text){
  logger("ERROR", text)
}
