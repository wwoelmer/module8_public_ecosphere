# Load library and set theme ----
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggalluvial)


options(scipen=999, dplyr.print_max = 20)
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=10, colour='black'), axis.text.y=element_text(size=10, colour='black'), 
                 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),
                 strip.text.x = element_text(size=12), strip.text.y = element_text(size=8),
                 legend.text = element_text(size = 8),
                 legend.title = element_text(size = 10),
                 panel.background = element_rect(fill = NA, color = "black"), 
                 text = element_text(size = 16))
scale_colour_discrete <- ggthemes::scale_colour_colorblind # Set default colors to colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

# read in data
mlt <- read.csv('./data_processed/ef_benefits_public.csv')

# Calculate percentage of correct answers in pre & post - UG v. Grad ----
out2 <- plyr::ddply(mlt, c("timing", "question"), function(x) {
  print(dim(x))
  n = nrow(x)
  pct <- (sum(x$value, na.rm = TRUE) / n) * 100
  return(data.frame(pct = pct, n = n))
})


out3 <- plyr::ddply(mlt, c("question"), function(x) {
  print(dim(x))
  
  wid <- pivot_wider(x, id_cols = student_index, names_from = timing, values_from = value)
  res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
  pvalue <- res$p.value
  n = nrow(x)
  y <- ((sum(wid$Post, na.rm = TRUE) / nrow(wid)) * 100) + 7
  return(data.frame(pvalue = pvalue, y = y, n = n/2))
})
out3
out3$signif <- "***"
out3$signif[out3$pvalue > 0.05] <- ""

out4 <- left_join(out2, out3, by = "question")
out4$y[out4$question=='IDK'] <- out4$pct[out4$question=='IDK' & out4$timing=='Pre'] + 7

out4$question <- factor(out4$question, ordered = TRUE, levels = c("Decision", "Manage", "Policy", "Predict", "Understanding", "IDK"))

p1 <- ggplot(out4) +
  geom_col(aes(question, pct, fill = timing), position = "dodge") +
  geom_text(aes(label = signif, x = question, y = y), size = 6) +
  geom_vline(xintercept = 5.5) +
  ylab("\nPercentage included (%)") +
  xlab("") +
  ylim(0, 100) +
  ggtitle("Forecast Benefits") +
  scale_fill_manual(values = c('#90bab8', '#426B69'), name = 'Timing') +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, vjust = 2))

p1

# calculate mean number of benefits
mean_benefits <- mlt %>% 
  mutate(uni_id = paste0(course_id, "_", student_index)) %>% 
  filter(question!='IDK') %>% 
  group_by(timing, uni_id) %>% 
  mutate(num_benefits = sum(value)) %>% 
  group_by(timing) %>% 
  mutate(mean = mean(num_benefits),
         sd = sd(num_benefits))

benefits <- mean_benefits %>% 
  dplyr::select(uni_id, timing, num_benefits) %>% 
  distinct(uni_id, timing, .keep_all = TRUE)
benefits$num_benefits <- factor(benefits$num_benefits, ordered = TRUE, c('3', '2', '1', '0'))
ben <- ggplot(benefits, aes(x = timing, stratum = num_benefits, alluvium = uni_id, fill = forcats::fct_rev(num_benefits), label = num_benefits)) +
  geom_flow(stat = "alluvium") +
  geom_stratum(width = 0.6)  +
  ylab("Students (N)")  +
  xlab('Timing') +
  scale_fill_manual(values = c( '#90bab8', '#609c99','#426b69', '#2c4745'),
                    name = 'Number Benefits') +
  mytheme
ben

mean_benefits_plot <- mean_benefits %>% 
  dplyr::select(timing, num_benefits)

benefit_diff <- mean_benefits %>% 
  dplyr::select(-question, -value, -mean, -sd) %>% 
  mutate(uni_id = paste0(course_id, "_", student_index)) %>% 
  dplyr::select(-course_id, -student_index) %>% 
  distinct(uni_id, timing, .keep_all = TRUE) %>% 
  pivot_wider(names_from = timing, values_from = num_benefits)


res <- wilcox.test(benefit_diff$Pre, benefit_diff$Post, paired = TRUE)
pvalue <- res$p.value

##############################################################################################################################################
# read in Q8 responses
dat_public <- read.csv('./data_processed/UC_comm_methods_public.csv')
dat_long <- pivot_longer(dat_public, cols = IDK:color, names_to = 'variable', values_to = 'answer')

# Calculate percentage of correct answers in pre & post 
out2 <- plyr::ddply(dat_long, c("timing", "variable"), function(x) {
  print(dim(x))
  n = nrow(x)
  pct <- (sum(x$answer, na.rm = TRUE) / n) * 100
  return(data.frame(pct = pct, n = n))
})

#############################
# calculate significance
dat_sig <- dat_long %>% 
  dplyr::select(c(uni_id, timing, variable, answer))

out3 <- plyr::ddply(dat_sig, c("variable"), function(x) {
  print(dim(x))
  wid <- pivot_wider(x, id_cols = uni_id, names_from = timing, values_from = answer)
  res <- wilcox.test(wid$Pre, wid$Post, paired = TRUE)
  pvalue <- res$p.value
  n = nrow(x)
  y <- ((sum(wid$Post, na.rm = TRUE) / nrow(wid)) * 100) + 7
  return(data.frame(pvalue = round(pvalue, 4), n = n/2, y = y))
})
out3
out3$signif <- "***"
out3$signif[out3$pvalue > 0.05] <- ""
out_graded <- out3 %>% 
  filter(variable %in% c('visual_representation', 'numeric_summary', 'IDK', 'probability', 'text_statement', 
                         'multiple_predictions', 'num_correct'))


graded_categories <- out2 %>% 
  filter(variable %in% c('visual_representation', 'numeric_summary', 'IDK', 'probability', 'text_statement', 
                         'multiple_predictions', 'num_correct'))
graded_categories$n <- as.numeric(graded_categories$n)
out4 <- left_join(graded_categories, out_graded, by = c('variable', 'n'))

# create ordered factor
out4$variable <- factor(graded_categories$variable, ordered = TRUE, 
                        levels = c('numeric_summary', 'visual_representation','probability', 
                                   'multiple_predictions', 'text_statement', 'IDK', 'num_correct'),
                        labels = c('Numeric', 'Visual', 'Probability', 'Mult. Predictions', 'Text', 'IDK', 'Number Correct'))
out4 <- out4 %>% 
  group_by(variable) %>% 
  mutate(y = max(pct) + 2) %>% 
  mutate(y = ifelse(timing=='Pre', NA, y))


a <- ggplot(data = out4[out4$variable!='Number Correct',], aes(x = variable, y = pct, fill = timing)) +
  geom_col(position = "dodge") +
  geom_text(aes(x = variable, y = y, label = signif), size = 6) +
  geom_vline(xintercept = 5.5) +
  scale_fill_manual(values = c('#FDC1B1', '#FC7A57'), name = 'Timing') +
  mytheme +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('UC Communication Type') +
  ylab(' ') +
  xlab("") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 2))
a

correct_answers <- dat_public %>% 
  dplyr::select(timing, num_correct)

b <- ggplot(data = correct_answers, aes(x = timing, y = num_correct, fill = timing)) +
  geom_boxplot(notch = TRUE, size = 1.2, outlier.size = 2, alpha = 0.7) +
  geom_text(aes(label = '***', y = 3, x = 1.5)) +
  stat_summary(fun=mean, geom="point", shape=23, size=8, fill = 'black') +
  scale_fill_manual(values = c('black', '#FC7A57')) +
  scale_color_manual(values = c('black', '#FC7A57')) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('') +
  xlab('Timing')
b

# calculate mean correct answers
mean_correct <- correct_answers %>% 
  group_by(timing) %>% 
  mutate(mean = mean(num_correct),
         sd = sd(num_correct),
         percent_1 = sum(num_correct > '1')/nrow(correct_answers)/2*100)

pre <- correct_answers[correct_answers$timing=='Pre',]
percent_one = sum(pre$num_correct=='1')/nrow(pre) * 100
post <- correct_answers[correct_answers$timing=='Post',]
percent_post = sum(post$num_correct)/nrow(post) * 100

# calculate percent of students with correct answers
per_correct <- dat_public %>% 
  dplyr::select(uni_id, timing, num_correct)

# calculate the number incorrect and correct
cor_incor <- dat_public %>% 
  dplyr::select(uni_id, timing, num_correct, IDK)

cor_incor$num_correct <- factor(cor_incor$num_correct, ordered = TRUE, c('3', '2', '1', '0'))
UC <- ggplot(cor_incor, aes(x = timing, stratum = num_correct, alluvium = uni_id, fill = forcats::fct_rev(num_correct), label = num_correct)) +
  geom_flow(stat = "alluvium") +
  geom_stratum(width = 0.6)  +
  ylab("Students (N)") +
  xlab('Timing') +
  scale_fill_manual(values = c( '#fee5de', '#fd997e','#FC7A57', '#fb3d0a'),
                    name = 'Number Correct') +
  mytheme +
  theme(legend.text = )
UC

per_ans <- cor_incor %>% 
  group_by(timing, num_correct) %>% 
  mutate(percent = length(num_correct)/240) %>% 
  distinct(timing, num_correct, .keep_all = TRUE)

allplot <- ggarrange(p1 + rremove("legend"), a, ben, UC,
                     nrow = 2, ncol = 2, labels = c('(a)', '(b)', '(c)', '(d)'),
                     legend = "top")
allplot

qualplot <- ggarrange(p1,a,legend = "top",
                      labels = c('(a)', '(b)'))
qualplot
ggsave('./figures/qual_answers.png', qualplot, scale = 1.2)

aluv_plot <- ggarrange(ben, UC, legend = "top", labels = c("(c)", "(d)"))
aluv_plot
ggsave('./figures/qualitative_fig_alluvial.png', aluv_plot, scale = 1.2)

########################################################################################################
# now plot the average number correct pre/post
summ <- plyr::ddply(dat, c("timing"), function(x) {
  data.frame(sd_correct = sd(x$num_correct, na.rm = TRUE),
             mean_num_correct = mean(x$num_correct, na.rm = TRUE))
})

b <- ggplot(data = summ, aes(fill = timing)) +
  geom_point(aes(x = as.factor(timing), y = mean_num_correct, size = 4)) +
  geom_errorbar(aes(x = as.numeric(timing), 
                    ymin = mean_num_correct - sd_correct, 
                    ymax = mean_num_correct + sd_correct, 
                    size = 1.5, width = 0.05)) +
  scale_fill_manual(values = c('black', '#FC7A57')) +
  ylim(0, 2) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, size = 18, hjust = 1)) +
  ylab('Correct Answers per Student') +
  xlab('Timing')
b
ggarrange(a, b, common.legend = TRUE)

keywd <- dat %>% 
  select(uni_id, icon, bar_graph, color, timing) %>% 
  pivot_longer(cols = icon:color, names_to = 'keyword', values_to = 'yes_no')

keywd <- keywd %>% 
  group_by(timing, keyword) %>% 
  mutate(total = sum(yes_no)) %>% 
  distinct(timing, keyword, .keep_all = TRUE)

ggplot(data = keywd, aes(x = keyword, y = total, fill = timing)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c('black', '#FC7A57')) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, size = 18, hjust = 1))

io <- dat %>% 
  select(uni_id, index, output, stated, explain, timing) %>% 
  pivot_longer(cols = index:output, names_to = 'type', values_to = 'yes_no') %>% 
  group_by(timing, type) %>% 
  mutate(total = sum(yes_no)) %>% 
  filter(yes_no==1)

io <- io %>% 
  group_by(timing, type) %>% 
  mutate(prop_stated = sum(stated)/total)


###############################################################################################################
# calculate richness and evenness
#install.packages('chemodiv')
library(vegan)
library(chemodiv)
dat_eco <- dat %>% 
  select(uni_id, name, visual_representation, numeric_summary, probability, multiple_predictions, text_statement)

eco_pre <- dat_eco %>% 
  filter(name=='Pre') %>% 
  select(uni_id, visual_representation:text_statement)  %>% 
  column_to_rownames("uni_id")

eco_post <- dat_eco %>% 
  filter(name=='Post') %>% 
  select(uni_id, visual_representation:text_statement) %>% 
  arrange(uni_id)%>% 
  column_to_rownames("uni_id")

# not sure why but the number is 1 higher than it should be (most have 0)
rich_pre <- specnumber(eco_pre, MARGIN = 1) 
rich_post <- specnumber(eco_post, MARGIN = 1)

s_pre <- diversity(eco_pre)
s_post <- diversity(eco_post)

par(mfrow = c(2, 2))
hist(rich_pre)
hist(rich_post)
hist(s_pre)
hist(s_post)

df1 <- as.data.frame(cbind(rich_pre, s_pre))
colnames(df1) <- c('richness', 'shannon')
df2 <- as.data.frame(cbind(rich_post, s_post))
colnames(df2) <- c('richness', 'shannon')
df1$timing <- 'Pre'
df2$timing <- 'Post'

df3 <- full_join(df1, df2)
df3 <- df3 %>% 
  select(timing, richness, shannon) %>% 
  pivot_longer(c(richness, shannon), names_to = 'metric', values_to = 'value')

ggplot(df3, aes(x = fct_rev(timing), y =  value)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = 'free_y')

mean(s_pre)
mean(s_post)
mean(rich_pre)
mean(rich_post)
