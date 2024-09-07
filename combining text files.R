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

# define the "friends" matrix

f <- matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0), ncol = 4, byrow = TRUE)


# print the friends matrix

f

# define the "enemies" matrix

e <- matrix(c(0,0,1,1,0,0,1,1,1,1,0,0,1,1,0,0), ncol = 4, byrow = TRUE)

# print the enemies matrix

e

# define the compound relationship matrix "enemies of friends"

fe <- f %*% e

# print the compound relationship matrix

fe
