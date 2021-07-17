Sys.which("make")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
install.packages("jsonlite", type = "source")
