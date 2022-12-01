# Custom functions -------------------------------------------------------------
# function to transform scientific numbers to % format
myfun <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(round(x*100L, 1), "%")) 
  } else x 
}

# function to set default value if none variable value is provided
setdefault   <-  function(name, value) {
  if ( ! exists(name) ) {
    cat("assign",name, "the value", value)
    assign(name, value, pos=1) # in the parent environment
  } else {
    cat(name, "")
  }
}