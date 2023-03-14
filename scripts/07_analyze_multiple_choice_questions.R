# analyze multiple choice questions

# Load library and set theme ----
library(tidyverse)
library(ggpubr)

options(scipen=999, dplyr.print_max = 20)
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=12, colour='black'), axis.text.y=element_text(size=12, colour='black'), 
                 axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=10),
                 legend.text = element_text(size = 10),
                 legend.title = element_text(size = 12),
                 panel.background = element_rect(fill = NA, color = "black"), 
                 text = element_text(size = 18))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
#####

# Load in files ----
qs <- read.csv('./data_processed/multiple_choice_answers_graded_public.csv')

ans_key <- read_csv("data_processed/mod8_answer_key.csv")
ans_key <- ans_key %>% 
  mutate(answer = recode(answer, "a" = 1, "b" = 2, "c" = 3, "d" = 4, "e" = 5))
tgt_q <- c("Q6", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15") 
ans_key2 <- ans_key %>% 
  mutate(question = factor(q_no, levels = tgt_q),
         answer = factor(answer)) %>% 
  filter(question %in% tgt_q)

# Calculate percentage of correct answers in pre & post
out2 <- plyr::ddply(qs, c("timing", "question"), function(x) {
  print(dim(x))
  idx <- which(ans_key2$question == x$question[1])
  corr_ans <- ans_key2$answer[idx]
  x <- na.exclude(x)
  n = nrow(x)
  pct <- (sum(as.character(x$response) == as.character(corr_ans)) / n) * 100
  return(data.frame(pct = pct, n = n))
})
out2$label <- paste0(out2$question, " (n = ", out2$n, ")") # Add label for plots
unique(out2$label)
#####

out2$label <-  paste0(out2$question, " (n = ", out2$n, ")") # Add label for plots
out2$label2 <- paste0(out2$level, " (n = ", out2$n, ")") # Add label for plots
unique(out2$label2)

# add significance
out3 <- plyr::ddply(qs, c("question"), function(x) {
  print(dim(x))
  
  idx <- which(ans_key2$question == x$question[1])
  corr_ans <- ans_key2$answer[idx]
  x <- na.exclude(x)
  n = nrow(x)
  x$value = as.numeric(as.character(x$response) == as.character(corr_ans))
  
  # x$response <- as.numeric(x$response)
  wid <- pivot_wider(x, id_cols = uni_id, names_from = timing, values_from = value)
  res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
  pvalue <- res$p.value
  n = nrow(x)
  y <- ((sum(wid$Post, na.rm = TRUE) / nrow(wid)) * 100) + 10
  return(data.frame(pvalue = pvalue, y = y, n = n/2))
})
out3
out3$signif <- "***"
out3$signif[out3$pvalue > 0.05] <- ""


out4 <- left_join(out2, out3, by = "question")

###################################3
out4$category <- NA
for(i in 1:nrow(out4)){
  if(out4$question[i]=='Q6' | out4$question[i]=='Q10'){
    out4$category[i] <- 'Foundational'
  }
  if(out4$question[i]=='Q11' ){
    out4$category[i] <- 'Decision'
  }
  if(out4$question[i]=='Q13' | out4$question[i]=='Q14'| out4$question[i]=='Q12'| out4$question[i]=='Q15'){
    out4$category[i] <- 'Communication'
  }
}


out4$description <- NA
for(i in 1:nrow(out4)){
  if(out4$question[i]=='Q6'){
    out4$description[i] <- 'Q1: Define'
  }
  if(out4$question[i]=='Q10'){
    out4$description[i] <- 'Q2: Uncertainty'
  }
  if(out4$question[i]=='Q11'){
    out4$description[i] <- 'Q8: Identify PrOACT'
  }
  if(out4$question[i]=='Q12'){
    out4$description[i] <- 'Q5: Interpret'
  }
  if(out4$question[i]=='Q13'){
    out4$description[i] <- 'Q6: Contrast'
  }
  if(out4$question[i]=='Q14'){
    out4$description[i] <- 'Q7: Distinguish'
  }
  if(out4$question[i]=='Q15'){
    out4$description[i] <- 'Q9: Connect to user'
  }
}


out4$description <- factor(out4$description, ordered = TRUE, levels = c('Q1: Define', 'Q2: Uncertainty',
                                                                  'Q5: Interpret', 'Q6: Contrast', 'Q7: Distinguish',
                                                                   'Q9: Connect to user'))
out4$label3 <- paste0('Pre/Post ', out4$label2)
out4$timing_category <- NA

for(i in 1:nrow(out4)){
#  if(out4$timing[i]=='Post'){
    out4$timing_category[i] <- paste0(out4$timing[i], " ", out4$category[i])
 # }else(out4$timing_category[i] <- 'Pre')
}

out4$timing_category <- factor(out4$timing_category, levels = c('Pre Foundational', 
                                                                'Post Foundational', 
                                                                'Pre Decision',
                                                                'Post Decision',
                                                                'Pre Communication',
                                                                'Post Communication'))


out4$panel_label <- NA
for(i in 1:nrow(out4)){
  if(out4$question[i]=='Q6'){
    out4$panel_label[i] <- '(a) Q1: Define'  #'Q1: Define'
  }
  if(out4$question[i]=='Q10'){
    out4$panel_label[i] <-  '(b) Q2: Uncertainty' #'Q2: Uncertainty'
  }
  if(out4$question[i]=='Q11'){
    out4$panel_label[i] <- 'Q8: Identify PrOACT'
  }
  if(out4$question[i]=='Q12'){
    out4$panel_label[i] <- '(c) Q5: Interpret'  #'Q5: Interpret'
  }
  if(out4$question[i]=='Q13'){
    out4$panel_label[i] <-  '(d) Q6: Contrast' #'Q6: Contrast'
  }
  if(out4$question[i]=='Q14'){
    out4$panel_label[i] <-  '(e) Q7: Distinguish' #'Q7: Distinguish'
  }
  if(out4$question[i]=='Q15'){
    out4$panel_label[i] <-  '(f) Q8: Connect to user' #'Q8: Connect to user'
  }
}

out4$panel_label <- factor(out4$panel_label, ordered = TRUE, 
                           levels = c('(a) Q1: Define', 
                                      '(b) Q2: Uncertainty',
                                      '(c) Q5: Interpret', 
                                      '(d) Q6: Contrast', 
                                      '(e) Q7: Distinguish',
                                      '(f) Q8: Connect to user') )  

decision <- out4[out4$category=='Decision',]
out5 <- out4[out4$category!='Decision',]

mc <- ggplot(out5, aes(timing, pct, fill = timing_category)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = signif,x = 1.5, y = y - 3), size = 6) +
  # geom_histogram(aes(color = timing, pct)) +
  facet_wrap( ~ panel_label,
              labeller = labeller(type = label_parsed),
              #ncol = 3,
              drop = FALSE,
              scales = 'free_x') +
  scale_fill_manual(values = c('#90bab8','#426B69', '#FDC1B1',  '#FC7A57')) +
  labs(fill = "Timing") +
  ylab("Percentage correct answers (%)") +
  xlab("Level") +  mytheme +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ylim(0, 90)
mc
ggsave('./figures/mult_choice_no_decision_newcolors.png', mc, scale = 1.3)

##################################################################################
# SI Fig for PrOACT question
q8 <- ggplot(decision, aes(timing, pct, fill = timing_category)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = signif,x = 1.5, y = y - 3), size = 4) +
  # geom_histogram(aes(color = timing, pct)) +
  scale_fill_manual(values = c('black', '#ACC196')) +
  labs(fill = "Timing") +
  ggtitle('Q8: Identify PrOACT') +
  ylab("Percentage correct answers (%)") +
  xlab("Level") +  mytheme +
  theme_bw(base_size = 18) +
  ylim(0, 90)
q8
