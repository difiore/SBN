# Or...

library(shinyDirectoryInput)
dir <- choose.dir()
f <- list.files(path=dir,recursive=FALSE,ignore.case=FALSE, pattern="csv",full.names=TRUE)

head(f)

e <- tibble()
for (i in f){
  x <- read_csv(i, guess_max = 10000) # guess max set close to length of largest
  message(paste0(nrow(x)), " records in file: ", basename(i))
  e <- bind_rows(e, x) # combines tables together, any fields not in a particular table will be filled with NA
}
