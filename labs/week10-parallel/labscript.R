# *one* of these...
# devtools::install_github("conjugateprior/austin")
# remotes::install_github("conjugateprior/austin")

library(tidyverse)

itemcodes <- read_csv("data/itemcodes.csv")
allcmp20 <- read_csv("data/MPDataset_MPDS2020a.csv")
allcmp20$partyabbrev[allcmp20$partyname == "Pirates"] <- "Piraten"
cmp20 <- allcmp20 %>%
  select(edate, countryname, partyname, partyabbrev,
         total, voteper = pervote, uncoded = peruncod,
         matches("per\\d\\d\\d$")) %>% # per%d%d%d% are subcategories, so we ignore them
  mutate(edate = as.Date(edate, format = "%d/%m/%Y"),
         eyear = lubridate::year(edate), # make a nice year just in case we want to filter with it
         label = paste(partyabbrev, eyear, sep = ":"), # for graphing
         across(c(starts_with("per"), "uncoded"), function(x) round(total * (x/100))))
write_csv(cmp20, "data/cmp20.csv")

oldest <- as.Date("1990-01-01")
de <- read_csv("data/cmp20.csv") %>%
  filter(edate > oldest, countryname == "Germany")

counts <- select(de, starts_with("per"))
de_rest <- select(de, -starts_with("per"))

de_mat <- data.matrix(counts)
dimnames(de_mat) <- list("party-year" = de_rest$label,
                         topic = itemcodes$name)
de_mat <- data.matrix(counts)
rownames(de_mat) <- de_rest$label
colnames(de_mat) <- itemcodes$name

############################ scaling time

library(austin)

de_mat <- wfm(de_mat, word.margin = 2)
wf <- wordfish(de_mat, dir = c(3,4))

theta <- wf$theta
data.frame(predict(wf, interval = "confidence"),
           year = de_rest$eyear,
           party = de_rest$partyname) %>%
  ggplot(aes(year, fit, color = party,
             ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point() +
  geom_linerange()

################################## bootstrapping time: sample counts

N <- nrow(de_mat)
V <- ncol(de_mat)
doclens <- rowSums(de_mat)
# take the original counts and divide each element by its row total
probs <- sweep(de_mat, 1, doclens, FUN = '/')

################################## with a loop and a container

# this will hold all our theta estimates
B <- 20
thetas <- matrix(0, nrow = N, ncol = B)
rownames(thetas) <- de_rest$label

# make a place for each bootstrap data set to land in
boot_sample <- wfm(de_mat, word.margin = 2)

# a function to create a new data set, fit a model, flip
# signs as necessary, extract theta and beta, and store them
boot_mat <- function(b) {
  # report every 100 iterations
  if (b %% 100 == 0)
    cat(".")
  # re-sample a new data set
  for (i in 1:N)
    boot_sample[i,] <- rmultinom(1, prob = probs[i,], size = doclens[i])
  # fit a model to it
  res <- wordfish(boot_sample, dir = c(3,4))
  # record these values in the bootstrap sample collection
  # that lives outside the loop
  thetas[,b] <<- res$theta
}

# run this to get B bootstrap estimates in thetas and betas
for (b in 1:B) boot_mat(b)

########################################### with lapply

boot_mat2 <- function(b, boot_sample, probs, doclens) {
  for (i in 1:nrow(boot_sample))
    boot_sample[i,] <- rmultinom(1, prob = probs[i,], size = doclens[i])
  res <- austin::wordfish(boot_sample, dir = c(3,4))
  theta <- res$theta
  names(theta) <- res$docs
  theta
}

######################################### which is kind of slow

B <- 100
system.time({bm2 <- lapply(1:B, boot_mat2, boot_sample, probs, doclens)})
out <- bind_rows(bm2)

######################################### but quite easy to parallelize

library(parallel)
detectCores()
detectCores(logical = FALSE)

cl <- makeCluster(10)

B <- 1000
system.time({bm2p <- parLapply(cl, 1:B, boot_mat2, boot_sample, probs, doclens)})
out <- bind_rows(bm2p)

########################################## careful to make this reproducible

clusterSetRNGStream(cl, 12)

B <- 50
bm2p <- parLapply(cl, 1:B, boot_mat2, boot_sample, probs, doclens)
out1 <- bind_rows(bm2p)

clusterSetRNGStream(cl, 12)

bm2p <- parLapply(cl, 1:B, boot_mat2, boot_sample, probs, doclens)
out2 <- bind_rows(bm2p)

all_equal(out1, out2)

########################################### switch it off at the wall

stopCluster(cl)
