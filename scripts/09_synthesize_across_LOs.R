# synthesize across learning objectives
library(tidyverse)
library(ltm)
library(ggalluvial)
library(RColorBrewer)
library(ggpubr)


mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=12, colour='black'), axis.text.y=element_text(size=12, colour='black'), 
                 axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=10),
                 legend.text = element_text(size = 10),
                 legend.title = element_text(size = 12),
                 panel.background = element_rect(fill = NA, color = "black"), 
                 text = element_text(size = 18))

######### read in multiple choice responses
mc <- read.csv('./data_processed/multiple_choice_answers_graded_public.csv')


# read in answer key
tgt_q <- c("Q6", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15") 
ans_key <- read_csv("data_processed/mod8_answer_key.csv")
ans_key <- ans_key %>% 
  mutate(answer = recode(answer, "a" = 1, "b" = 2, "c" = 3, "d" = 4, "e" = 5))
ans_key2 <- ans_key %>% 
  mutate(question = factor(q_no, levels = tgt_q),
         answer = factor(answer)) %>% 
  filter(question %in% tgt_q)

# score multiple choice answers in pre & post 
mc_graded <- plyr::ddply(mc, c("uni_id", "timing", "question", "response"), function(x) {
  print(dim(x))
  idx <- which(ans_key2$question == x$question[1])
  corr_ans <- ans_key2$answer[idx]
  x <- na.exclude(x)
  n = nrow(x)
  #pct <- (sum(as.character(x$response) == as.character(corr_ans)) / n) * 100
  correct <- ifelse(x$response==as.character(corr_ans), 1, 0)
  return(data.frame(correct = correct))
})

mc_graded <- mc_graded %>% 
  mutate(#fscore = factor(response, levels = c(1, 2, 3, 4, 5)),
    timing = factor(timing, levels = c("Pre", "Post")),
    correct_factor = factor(ifelse(correct==1, 'yes', 'no'))) 

####### read in Q7 data
dat_7 <- read.csv('./data_processed/ef_benefits_public.csv')

mlt <- dat_7 %>% 
  mutate(uni_id = paste0(course_id, "_", student_index)) %>% 
  dplyr::select(-course_id, -student_index)


# calculate number of benefits identified per student
benefits <- mlt %>% 
  filter(question!='IDK') %>% 
  group_by(uni_id, timing) %>% 
  mutate(q7_score = sum(value)) %>% 
  distinct(uni_id, timing, .keep_all = TRUE) %>% 
  mutate(correct = q7_score) %>%    #ifelse(q7_score > 0, 1, 0)) %>% # if at least one benefit, then correct, else incorrect
  mutate(question = 'Q7') %>% 
  dplyr::select(uni_id, timing, question, correct) 

###### add in Q8 data
# read in Q8 responses
dat8 <- read.csv('./data_processed/UC_comm_methods_public.csv')
# Rename columns
colnames(dat8) <- gsub(" ", "_", colnames(dat8))
answer_cols <- seq(9,22)
dat8$timing <- factor(dat8$name, levels = c("Pre", "Post"))
dat_long <- pivot_longer(dat8, cols = IDK:color, names_to = 'variable', values_to = 'answer')
dat_long <- dat_long %>% 
  dplyr::select(uni_id, timing, variable, answer)

uc <- dat_long %>% 
  filter(variable=='num_correct') %>% 
  group_by(uni_id, timing) %>% 
  mutate(correct = answer) %>%    #ifelse(answer > 0, 1, 0)) %>% # if at least one benefit, then correct, else incorrect
  mutate(question = 'Q8') %>% 
  dplyr::select(uni_id, timing, question, correct) 
  

## merge together with multiple choice
ids <- as.vector(unique(benefits$uni_id))
mc_graded <- mc_graded %>% 
  filter(uni_id %in% ids)
uc <- uc %>% 
  filter(uni_id %in% ids)


all_graded <- full_join(mc_graded, benefits)
all_graded <- full_join(all_graded, uc)

# add learning objective
all_graded$LO <- NA
for(i in 1:nrow(all_graded)){
  if(all_graded$question[i]=='Q6' | all_graded$question[i]=='Q7' | all_graded$question[i]=='Q10'){
    all_graded$LO[i] <- 'LO1'
  }
  if(all_graded$question[i]=='Q8' | all_graded$question[i]=='Q13'| all_graded$question[i]=='Q14'| all_graded$question[i]=='Q12' | all_graded$question[i]=='Q15'){
    all_graded$LO[i] <- 'LO5'
  }
  if(all_graded$question[i]=='Q11' ){
    all_graded$LO[i] <- 'LO2.3.4.6'
  }
#  if(all_graded$question[i]=='Q15'){
#    all_graded$LO[i] <- 'LO3.4.6'
#  }
  
}


# calculate cronbach's alpha on the learning objectives
# calculate cronbach's alpha on pre/post
out <- data.frame(matrix(nrow = 2, ncol = 2))
rownames(out) <- c('Pre', 'Post')
colnames(out) <- c('LO1', 'LO5')
timing <- c('Pre', 'Post')
LO <- c('LO1', 'LO5')
for (i in 1:2) {
  for(j in 1:2){
    temp <- all_graded[all_graded$timing==timing[i] & all_graded$LO==LO[j],]
    temp <- temp %>% 
      dplyr::select(uni_id, timing, question, correct) %>% 
      pivot_wider(names_from = question, values_from = correct) %>% 
      dplyr::select(-c(uni_id, timing))
    c <- cronbach.alpha(temp)
    out[i, j] <- round(c$alpha, 3)
    
  }
}
out


all_scores <- all_graded %>% 
  mutate(correct = ifelse(correct > 0, 1, correct)) %>%  # for Q7/Q8, code as 1 for any response (even if more than 1), 0 for no response
  group_by(LO, timing, uni_id) %>% 
  mutate(score = round(sum(correct)/length(unique(question))*100),
         fscore = factor(score, ordered = TRUE, levels = c('100', '80', '67', '60', '40', '33', '20', '0')),
         timing = factor(timing, levels = c("Pre", "Post")),
         score_fraction = factor(sum(correct), ordered = TRUE, c('4', '3', '2', '1', '0'))) %>%
  distinct(uni_id, timing, LO, .keep_all = TRUE)

all_scores$LO <- factor(all_scores$LO, 
                        levels = c('LO1', 'LO2.3.4.6','LO5'),
                        labels = c('Foundational', 'Decision', 'Communication'))

sub <- all_scores %>% 
  filter(LO!='Decision')

p1 <- ggplot(sub, aes(x = timing, stratum = fscore, alluvium = uni_id, fill = fscore, label = fscore)) +
  #geom_flow(stat = "alluvium") +
  geom_stratum(width = 0.6) +
  ylab("Students (N)") +
  xlab('Timing') +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11,"RdYlGn"))(15),
                    name = 'Correct (%)') +
  facet_wrap(~as.factor(LO)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.border = element_rect(colour = "black", fill = NA)) +
  mytheme
p1

# now look at pre-level vs change for each LO

df <- all_graded %>% 
  mutate(correct = ifelse(correct > 0, 1, correct)) %>%  # for Q7/Q8, code as 1 for any response (even if more than 1), 0 for no response
  group_by(LO, timing, uni_id) %>% 
  mutate(score = round(sum(correct)/length(unique(question))*100)) %>% 
  #mutate(score_fraction = factor(sum(correct), ordered = TRUE, c('4', '3', '2', '1', '0'))) %>% 
  dplyr::select(-question, -response, - correct, -correct_factor) %>%
  distinct(uni_id, timing, LO, .keep_all = TRUE) %>% 
  pivot_wider(names_from = timing, values_from = score) %>% 
  group_by(uni_id, LO) %>% 
  mutate(diff = Post - Pre) %>% 
  mutate(pre_level = ifelse(Pre < 34, 'low', 'NA')) %>% 
  mutate(pre_level = ifelse(Pre > 33 & Pre <= 67, 'medium', pre_level)) %>% 
  mutate(pre_level = ifelse(Pre > 67, 'high', pre_level)) %>% 
  dplyr::select(uni_id, LO, diff, pre_level, Pre) %>% 
  group_by(LO) %>% 
  mutate(level = ifelse(diff < -32, 'low', 'NA')) %>% 
  mutate(level = ifelse(diff >= -32 & diff < 33, 'medium', level)) %>% 
  mutate(level = ifelse(diff >= 33, 'high', level))  
df$pre_level <- factor(df$pre_level, ordered = 'TRUE', levels = c('low', 'medium', 'high'))
df$level <- factor(df$level, ordered = 'TRUE', levels = c('high', 'medium', 'low'))


# or look at the number correct instead of percent
num <- all_graded %>% 
  mutate(correct = ifelse(correct > 0, 1, correct)) %>%  # for Q7/Q8, code as 1 for any response (even if more than 1), 0 for no response
  group_by(LO, timing, uni_id) %>% 
  mutate(score_fraction = sum(correct)) %>% 
  dplyr::select(-question, -response, - correct, -correct_factor) %>%
  distinct(uni_id, timing, LO, .keep_all = TRUE) %>% 
  pivot_wider(names_from = timing, values_from = score_fraction) %>% 
  group_by(uni_id, LO) %>% 
  mutate(diff = Post - Pre) %>% 
  dplyr::select(uni_id, LO, diff, Pre) %>% 
  group_by(LO) %>% 
  mutate(level = ifelse(diff < -32, 'low', 'NA')) %>% 
  mutate(level = ifelse(diff >= -32 & diff < 33, 'medium', level)) %>% 
  mutate(level = ifelse(diff >= 33, 'high', level))  

num$fdiff <-  factor(num$diff, ordered = TRUE, c('4', '3', '2', '1', '0')) 
num$LO <- factor(num$LO, 
                        levels = c('LO1', 'LO2.3.4.6','LO5'),
                        labels = c('Foundational', 'Decision', 'Communication'))

num_sub <- num %>% 
  filter(LO!='Decision')

num_sub <- num_sub %>% 
  mutate(num_questions = ifelse(LO=='Foundational', 3, 5))

mean_change <- num_sub %>% 
  mutate(mean_change = mean(diff),
         mean_corr_q = mean(diff)/num_questions,
         median_change = median(diff),
         median_corr_q = median(diff)/num_questions) %>% 
  distinct(LO, .keep_all = TRUE) %>% 
  dplyr::select(LO, mean_change:median_corr_q)

p2 <- ggplot(num_sub, aes(x = as.factor(Pre), y = as.factor(diff), col = forcats::fct_rev(as.factor(Pre)))) +
  geom_hline(yintercept = '0') +
  geom_jitter(size = 3) +
  facet_wrap(~LO) +
  scale_color_manual(values = colorRampPalette(brewer.pal(8,"RdYlGn"))(13),
                     name = 'Correct on Pre (n)') +
  xlab('Number correct on Pre') +
  ylab('Change after module') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.border = element_rect(colour = "black", fill = NA)) +
  mytheme

p2

synthesis <- ggarrange(p1, p2, labels = c('(a)', '(b)'))
synthesis
ggsave('./figures/synthesis_fig.png', synthesis, scale = 1.3)

df$pre_level <- factor(df$pre_level, ordered = 'TRUE', levels = c('low', 'medium', 'high'))
df$level <- factor(df$level, ordered = 'TRUE', levels = c('high', 'medium', 'low'))
