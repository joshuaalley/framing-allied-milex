# Joshua Alley
# Present text analysis and survey experiment results

# load packages 
library(tidyverse)
library(xtable)
library(naniar)
library(forcats)


# create a table of key words in neutral
nato.base.res <- cbind.data.frame(
  names(head(sort(nato.budget.sim[,1], decreasing = TRUE), 20)),
  names(head(sort(nato.budget.sim.congress[,1], decreasing = TRUE), 20))
)
colnames(nato.base.res) <- c("President", "Congress")


# create a table of key words in contexts
nato.collective.res <- cbind.data.frame(
  names(head(sort(nato.collective.sim[,1], decreasing = TRUE), 20)),
  names(head(sort(nato.collective.sim.congress[,1], decreasing = TRUE), 20))
  )
colnames(nato.collective.res) <- c("President", "Congress")


# create a table of key words in exchange
nato.exchange.res <- cbind.data.frame(
  names(head(sort(nato.exchange.sim[,1], decreasing = TRUE), 20)),
  names(head(sort(nato.exchange.sim.congress[,1], decreasing = TRUE), 20))
)
colnames(nato.exchange.res) <- c("President", "Congress")



# try it with columns
nato.text.res.cols <- bind_cols(nato.base.res, nato.collective.res, 
                           nato.exchange.res)
nato.text.res.cols[] <- lapply(nato.text.res.cols, as.character)
# replace stopwords with NA
nato.text.res.cols <- nato.text.res.cols %>%
                   replace_with_na_all(
                     condition = ~.x %in% stopwords("english")
                     )
nato.text.res.cols <- mutate_all(nato.text.res.cols, 
                           ~ fct_rev(ordered(., levels = .)))
                            
# add order to factors and sort to move missing to the end 
select.key <- function(x){x <- x[!is.na(x)]}
nato.text.res.cols <- apply(nato.text.res.cols, 2, select.key)

# turn lists into dataframe with NAs at the end
nato.text.res.cols <- as.data.frame(t(plyr::ldply(nato.text.res.cols, 
                                rbind)))
nato.text.res.cols <- nato.text.res.cols[2:nrow(nato.text.res.cols), ]
colnames(nato.text.res.cols) <- rep(c("President", "Congress"), 
                                    ncol(nato.text.res.cols)/2)

# plot for NATO 
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('\\multicolumn{2}{c}{Neutral} & 
                                   \\multicolumn{2}{c}{Collective} &
                                   \\multicolumn{2}{c}{Exchange}',
                                   collapse=''), '\\\\')
print(
  xtable(nato.text.res.cols, 
         caption = c("Most similar words to linear combinations of NATO, defense, and spending
                     in GloVe word embedding models of Presidential and Congressional orations about NATO.
                     Each vector taken from the top 20 most similar words, based on cosine similarity.
                     Blank entries indicate stopwords in the vector."),
         label = c("tab:glove-vectors"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)
