library(quanteda)
library(dplyr)
library(ggplot2)
library(ggrepel)

theme_set(theme_minimal())

load("data/df_pba_debate_by_speaker.rda")
names(df_pba_debate_by_speaker)

corp <- corpus(df_pba_debate_by_speaker,
               text_field = "contributions", docid_field = "speaker")
docvars(corp, "speaker") <- df_pba_debate_by_speaker$speaker # add a speaker field


corpdfm <- dfm(corp)


wfish <- textmodel_wordfish(corpdfm, dir = c(3, 21))
summary(wfish)


preds  <- predict(wfish, interval = "confidence")

# grab the (only) internal element 'fit' and make it a data frame
preds <- as_tibble(preds$fit)
preds_dv <- mutate(bind_cols(docvars(corpdfm), preds),
                   speaker_order = rank(fit)) # add a left to right ordering

ggplot(preds_dv, aes(x = fit, xmin = lwr, xmax = upr,
                     y = speaker_order, col = party)) +
  geom_point() +
  geom_errorbarh(height = 0) +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(labels = preds_dv$speaker,
                     minor_breaks = NULL,
                     breaks = preds_dv$speaker_order) +
  labs(x = "Position", y = "Speaker") +
  ggtitle("Estimated Positions from Senate Partial-Birth Abortion Debate",
          subtitle = "October 21st, 2003")


wscores <- tibble(word = wfish$features,
                  score = wfish$beta,
                  offset = wfish$psi)

testwords <- c("life", "choice", "womb", "her", "woman", "health",
               "born", "baby", "little", "gruesome", "kill", "roe",
               "wade", "medical", "her", "his", "child", "religion",
               "catholic", "doctor", "nurse")

testscores <- arrange(filter(wscores, word %in% testwords),
                      score)
testscores


ggplot(wscores, aes(score, offset, label = word)) +
  geom_point(color = "grey", alpha = 0.2) +
  geom_text_repel(data = testscores, col = "black") +
  geom_point(data = testscores) +
  labs(x = "Word score", y = "Offset parameter") +
  ggtitle("Estimated Word Positions for Selected Debate Vocabulary",
          subtitle = "Note: Offset parameter is roughly proportional to word frequency")

# warning: somewhat gruesome
kwic(corp, "baby", window = 15)

