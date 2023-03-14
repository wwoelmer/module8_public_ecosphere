library(coin)
library(tidyverse)

## add preceding code from analyze_multiple_choice.R
qs <- read.csv( './data_processed/multiple_choice_answers_graded_public.csv')

ans_key <- read_csv("data_processed/mod8_answer_key.csv")
ans_key <- ans_key %>% 
  mutate(answer = recode(answer, "a" = 1, "b" = 2, "c" = 3, "d" = 4, "e" = 5))
tgt_q <- c("Q6", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15") 
ans_key2 <- ans_key %>% 
  mutate(question = factor(q_no, levels = tgt_q),
         answer = factor(answer)) %>% 
  filter(question %in% tgt_q)

stat <- plyr::ddply(qs, c("question"), function(x) {
  print(dim(x))
  
  idx <- which(ans_key2$question == x$question[1])
  corr_ans <- ans_key2$answer[idx]
  x <- na.exclude(x)
  n = nrow(x)
  x$value = as.numeric(as.character(x$response) == as.character(corr_ans))
  pre <- x[x$timing=='Pre',]
  pct_pre <- (sum(as.character(pre$response) == as.character(corr_ans)) / (n/2)) * 100
  post <- x[x$timing=='Post',]
  pct_post <- (sum(as.character(post$response) == as.character(corr_ans)) / (n/2)) * 100
  

  # x$response <- as.numeric(x$response)
  wid <- pivot_wider(x, id_cols = uni_id, names_from = timing, values_from = value)
  res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
  pvalue <- res$p.value
  n = nrow(x)
  test = res$statistic
  z <- wilcoxsign_test(wid$Pre ~wid$Post, paired = TRUE)
  effsize <- z@statistic@teststatistic/sqrt(n/2)
  return(data.frame(pvalue = round(pvalue, 3), 
                    test, 
                    effsize = abs(round(effsize, 2)),  
                    pct_pre = round(pct_pre, 0), 
                    pct_post = round(pct_post, 0), 
                    n = n/2))
})

stat

write.csv(stat, './data_processed/stats_mc.csv', row.names = FALSE)

#########################################################################################
# calculate for Q7
dat_7 <- read.csv('./data_processed/ef_benefits_public.csv')

mlt <- dat_7 %>% 
  mutate(uni_id = paste0(course_id, "_", student_index)) %>% 
  dplyr::select(-course_id, -student_index)

# calculate number of benefits identified per student
benefits <- mlt %>% 
  filter(question!='IDK') %>% 
  group_by(uni_id, timing) %>% 
  mutate(score = sum(value)) %>% 
  distinct(uni_id, timing, .keep_all = TRUE) %>% 
  dplyr::select(uni_id, timing, score)

wid <- pivot_wider(benefits, id_cols = uni_id, names_from = timing, values_from = score)
res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
pvalue <- res$p.value
n = nrow(wid)
test = res$statistic
z <- wilcoxsign_test(wid$Pre ~wid$Post, paired = TRUE)
effsize <- z@statistic@teststatistic/sqrt(n/2)
mean_sd <- benefits %>% 
  group_by(timing) %>% 
  mutate(mean = mean(score),
         sd = sd(score)) %>% 
  distinct(timing, .keep_all= TRUE) 

mean_sd

q7 <- data.frame(
  Question = 'Q7',
  pvalue = round(pvalue, 4),
  test,
  effsize = abs(round(effsize, 2)),
  mean_pre = round(mean_sd$mean[mean_sd$timing=='Pre'], 2),
  mean_post = round(mean_sd$mean[mean_sd$timing=='Post'], 2),
  sd_pre = round(mean_sd$sd[mean_sd$timing=='Pre'], 2),
  sd_post = round(mean_sd$sd[mean_sd$timing=='Post'], 2),
  n = n)

q7

###############
## Q8 data
dat8 <- read.csv('./data_processed/UC_comm_methods_public.csv')
# Rename columns
colnames(dat8) <- gsub(" ", "_", colnames(dat8))
answer_cols <- seq(9,22)
dat8$timing <- factor(dat8$name, levels = c("Pre", "Post"))
dat_long <- pivot_longer(dat8, cols = IDK:color, names_to = 'variable', values_to = 'answer')
dat_long <- dat_long %>% 
  dplyr::select(uni_id, timing, variable, answer)


# calculate significance
dat_sig <- dat_long %>% 
  dplyr::select(c(uni_id, timing, variable, answer)) %>% 
  filter(variable!='IDK') %>% 
  group_by(uni_id, timing) %>% 
  mutate(score = sum(answer)) %>% 
  distinct(uni_id, timing, .keep_all = TRUE) %>% 
  dplyr::select(uni_id, timing, score)

dat_sig

wid <- pivot_wider(dat_sig, id_cols = uni_id, names_from = timing, values_from = score)
res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
pvalue <- res$p.value
n = nrow(wid)
test = res$statistic
z <- wilcoxsign_test(wid$Pre ~wid$Post, paired = TRUE)
effsize <- z@statistic@teststatistic/sqrt(n/2)
mean_sd <- dat_sig %>% 
  group_by(timing) %>% 
  mutate(mean = mean(score),
         sd = sd(score)) %>% 
  distinct(timing, .keep_all= TRUE) 

mean_sd

q8 <- data.frame(
  Question = 'Q8',
  pvalue = round(pvalue, 4),
  test,
  effsize = abs(round(effsize, 2)),
  mean_pre = round(mean_sd$mean[mean_sd$timing=='Pre'], 2),
  mean_post = round(mean_sd$mean[mean_sd$timing=='Post'], 2),
  sd_pre = round(mean_sd$sd[mean_sd$timing=='Pre'], 2),
  sd_post = round(mean_sd$sd[mean_sd$timing=='Post'], 2),
  n = n)

q8

qual <- rbind(q7, q8)
write.csv(qual, './data_processed/stats_qual.csv', row.names = FALSE)
