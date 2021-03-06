text3_v <- tolower(paste(scan("data/text/billie.txt", what = "character", sep = "\n"), collapse = " "))
text3_words_l <- strsplit(text3_v,"[^A-Za-z0-9']")
text3_word_v <- unlist(text3_words_l)
not_blanks_v <- which(text3_word_v != " ") # getting rid of blanks
text3_word_v <- text3_word_v[not_blanks_v]

text3_freqs_t <- table(text3_word_v)
sorted_text3_freqs_t <- sort(text3_freqs_t, decreasing = TRUE) # all the words, sorted by their frequency, decreasing
text3_length_v <- sum(sorted_text3_freqs_t) # 3917 is the sum
sorted_text3_rel_freqs_t <- 100*(sorted_text3_freqs_t/text3_length_v) # the percentage of each word in the whole text, decreasing
#sorted_text3_rel_freqs_t["the"] # relative "the" frequency (for example)

plot(sorted_text3_rel_freqs_t[1:10], type = "p",xlab = "Top Ten Words in Happier Than Ever", 
  ylab = "Percentage of Full Text", 
  xaxt = "n")
axis(1, 1:10, labels = names(sorted_text3_rel_freqs_t [1:10])) # percentage graph

n_time_v <- seq(from = 1, to = length(text3_word_v))
is_v <- which(text3_word_v == "i")
w_count_v <- rep(NA, times = length(n_time_v))
w_count_v[is_v] <- 1
plot(w_count_v,
  main = "Dispersion Plot of 'i' in Happier Than Ever",
  xlab = "the course of the album",
  ylab = "i",
  type = "h",
  ylim = c(0, 1), yaxt = 'n') # "i" dispertion graph

n2_time_v <- seq(from = 1, to = length(text3_word_v))
yous_v <- which(text3_word_v == "i")
w2_count_v <- rep(NA, times = length(n2_time_v))
w2_count_v[yous_v] <- 1
plot(w2_count_v,
  main = "Dispersion Plot of 'you' in Happier Than Ever",
  xlab = "the course of the album",
  ylab = "you",
  type = "h",
  ylim = c(0, 1), yaxt = 'n') # "you" dispertion graph

adverbs3_t <- sort(table(text3_word_v[grep("ly$", text3_word_v)]), decreasing = TRUE)
adverbs3_t # -ly adverbs counter

he_hits <- grep("he|his|him|he's",
  text3_word_v)
she_hits <- grep("she|her|hers|she's",
  text3_word_v)
i_hits <- grep("i|i'm|me|my|mine",
  text3_word_v)
you_hits <- grep("you|you're|your|yours",
  text3_word_v)
they_hits <- grep("they|they're|their|theirs",
  text3_word_v)
# pronouns with forms
he_percent <- 100*length(he_hits)/length(text3_word_v)
she_percent <- 100*length(she_hits)/length(text3_word_v)
i_percent <- 100*length(i_hits)/length(text3_word_v)
you_percent <- 100*length(you_hits)/length(text3_word_v)
they_percent <- 100*length(they_hits)/length(text3_word_v) # pronouns & forms frequencies
