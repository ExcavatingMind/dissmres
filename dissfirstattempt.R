library(tidyverse)

# load the data

diss_data <- read_csv("final_data.csv")

# take out the .png from the images column

new_diss <- diss_data %>% separate(images, into = c("images2", "png"), sep = -4)

# a minus number indicates the far right position of the variables. By separating by 4 points on the right
# we can extract the ".png" from the letters and numbers. 
# separate the numbers from the letters in the images2 column

newer <- new_diss %>% separate(images2, into = c("image_n", "condition"), 
                               sep = -1)

newer <- newer %>%
  mutate(condition = recode(condition, A = "nonlinear", B = "linear", C = "inverted", D = "standard"))

newer$r_cat <- cut(newer$my_rs,
                    breaks = c(0, 0.39,  0.59, 1.0),
                    labels = c("weak", "moderate",
                               "strong"))


# as the letters are only one point, we can use -1 to extract them from the numbers.
# Thus, separator | denotes numbers | letter, e.g., 14 | A


head(newer)

literacy_score <- newer %>%
  filter(!is.na(q1_slider.response)) %>%
  rowwise() %>%
  mutate(sum_lit = sum(c(q1_slider.response, 
                         q2_slider.response, 
                         q3_slider.response, 
                         q4_slider.response, 
                         q5_slider.response))) %>%
  select(participant, sum_lit)

newer_cronbach <- newer %>%
  filter(!is.na(q1_slider.response)) %>%
  select(q1_slider.response, 
         q2_slider.response, 
         q3_slider.response, 
         q4_slider.response, 
         q5_slider.response)


cronbachs_alpha(newer_cronbach)
library(ltm)
cronbach.alpha(newer_cronbach, na.rm = T, CI = T)

demographic_score <- newer %>%
  filter(!is.na(gender_slider.response)) %>%
  mutate(gender_slider.response = recode(gender_slider.response,
                                         `1` = "Female",
                                         `2` = "Male",
                                         `3` = "NonBinary")) %>%
  select(matches(c("participant",
                   "age_textbox.text",
                   "gender_slider.response"))) %>%
  rename("age" = "age_textbox.text") %>%
  rename("gender" = "gender_slider.response")


demographic_score %>% 
  summarise(mean_age = mean(age), sd_age = sd(age))

demographic_score %>% count(gender)
# m age = 29.6, sd = 8.54
# f = 75, m = 72, nB = 3


mixed_model_data %>% 
  group_by(condition) %>%
  summarise(mean_rt = mean(rt), sd_rt = sd(rt))

## VT threshold testing...

VT_data <- diss_data %>%
  filter(!is.na(VT_with_labels)) %>%
  select(c("VT_with_labels", "participant", "VT_textbox2.text")) %>%
  mutate(VT_answer = str_replace(VT_with_labels, pattern = "vis_threshold_plots/", replacement = "")) %>%
  mutate(VT_answer = str_replace(VT_answer, pattern = "_VT.png", replacement = "")) %>%
  mutate(correct_VT = case_when(
    VT_answer == VT_textbox2.text ~ "y",
    VT_answer != VT_textbox2.text ~ "n",
    is.na(VT_answer) ~ "n", TRUE ~ as.character(VT_answer))) %>%
  group_by(participant) %>% 
  summarise(sum_VT = sum(correct_VT == "y")) %>%
  select("participant", "sum_VT")

VT_data %>%
  summarise(mean_VT = mean(sum_VT), sd_VT = sd(sum_VT))
# 5.95 0.225


## DOT PITCH

mixed_model_data %>% 
  group_by(condition) %>%
  summarise(mean_rt = mean(rt), sd_rt = sd(rt))


diss_data %>%
  filter(!is.na(res_width)) %>%
  summarise(mean_resw = mean(res_width), mean_resh = mean(res_height),
            heightm = mean(height), rheight = range(height))

## dot pitch...

dot_pitch_data <- diss_data %>%
  filter(!is.na(res_width)) %>%
  mutate(res_height = res_width*0.5625,
         width_precursor = height*1.777778,
         width = width_precursor/2.54,
         dot_pitch = sqrt(height^2 + width^2)/sqrt(res_height^2 + res_width^2) * 25.4) %>%
  select(participant, dot_pitch)

### my rationale, we have screen height and res_width so, first
# we calculate res_height on 16:9 aspect by multipling width by .5625
# then we calculate width by multiplying height by 1.7.. which is the inverse calculation
# then we divide width by 2.54 so we get inches rather than cm/mm as the formula requires inches.
# ik screen scaler and the psychopy code infers cm/mm, e.g., 8.54 is credit card in cm. so calculated height is in cm
# then we do the dot pitch formula, then multiply by 25.4 to get the mm value. 

dot_pitch_data %>%
  summarise(mean_dot = mean(dot_pitch), sd_dot = sd(dot_pitch))
# 0.354 m ,  0.0623 sd
# could be wrong but I think this is correct.

## experimental conditions...




## absolute value of reaction time. 

response_score <- newer %>%
  filter(!is.na(my_rs)) %>%
  mutate(response = abs(my_rs - slider.response)) %>%
  mutate(un_response = my_rs - slider.response) %>%
  select(participant, response, image_n, condition, r_cat, un_response,
         trials.thisN) %>%
  mutate(condition = factor(condition)) %>%
  filter(!is.na(r_cat))

response_score$training <- cut(response_score$trials.thisN,
                   breaks = c(0, 92,  185),
                   labels = c("first",
                              "second")) 
response_score <- response_score %>%
  filter(!is.na(training))

  
lmm_model_data <- merge(response_score, literacy_score, by.x = "participant")


lmm_model_data <- merge(lmm_model_data, dot_pitch_data, by.x = "participant")

descriptive_mod_data <- merge(response_score, demographic_score, by.x = "participant")

# pretty much done here, could do some basic analyses, but also I want to add some stuff
# thinking....
  
### problem here possibly

lmm_model_data %>%
  filter(!is.na(un_response)) %>%
  group_by(condition) %>%
  summarise(mean_score = mean(un_response))

## sum stats better

library(rstatix)

summary_q1 <- lmm_model_data %>%
  group_by(condition) %>%
  get_summary_stats(un_response, type = "common")

summary_q2 <- lmm_model_data %>%
  group_by(r_cat) %>%
  get_summary_stats(un_response, type = "common")

summary_q1
summary_q2

# SET CONTRASTS

contrasts(lmm_model_data$r_cat) <- contr.sum(3)
contrasts(lmm_model_data$condition) <- contr.sum(4)

contrasts(lmm_model_data$r_cat)
contrasts(lmm_model_data$condition)




# MODELING


library(lme4)
library(performance)
library(buildmer)
library(afex)

des_mod <- descriptive_mod_data %>% filter(!is.na(gender))




# descriptive stats

gender_age <- lmer(response ~ age + (1 | participant), data= des_mod)


summary(gender_age)

# first model

# full_model <- buildmer(response ~ condition + (! + condition | image_n) 
#                       + (1 + condition | participant),
#                       data = lmm_model_data)

# formula(full_model@model)

final_model <- lmerTest::lmer(response ~ condition + (1|image_n) + (1|participant), 
                    data = lmm_model_data, REML = TRUE)

check_model(final_model)


null_model <- lmer(response ~ (1|image_n) + (1|participant), data = lmm_model_data)

anova(final_model, training_mod)


library(easystats)

summary(final_model)
cohens_d(final_model)


fin_mod <- lmerTest::as_lmerModLmerTest(final_model)
### posthoc tests
summary(fin_mod, ddf= "Kenward-Roger")



library(emmeans)

# contrasts for final model with effect sizes

contrasts1 <- emmeans(final_model, pairwise ~ condition)

contrasts1

eff_size(contrasts1, sigma = sigma(final_model), edf = df.residual(final_model))


# Additional Models


r_mod <- lmerTest::lmer(response ~ condition * r_cat + (1|image_n) + (1|participant), 
                                     data = lmm_model_data, REML = TRUE)
lit_mod <- lmerTest::lmer(response ~ condition * sum_lit + (1|image_n) + (1|participant), 
                   data = lmm_model_data, REML = T)

dot_mod <- lmerTest::lmer(response ~ condition:dot_pitch + condition  + 
                            (1|image_n) + (1|participant), 
                          data = lmm_model_data, REML = T)

lit_mod2 <- lmerTest::lmer(response ~ condition * sum_lit + (1|image_n) + (1|participant), 
                          data = lmm_model_data, REML = T)

training_mod <- lmerTest::lmer(response ~ condition * training + (1|image_n) + (1|participant), 
                               data = lmm_model_data, REML = T)
summary(training_mod)

summary(r_mod)
anova(final_model, lit_mod)


anova(final_model, training_mod)

tibble(lmm_model_data)

anova(fin)

r_mod
lmtest::lrtest(final_model, lit_mod)

library(lmtest)

lmerTest::anova(final_model, lit_mod)

anova(lit_mod, dot_mod)

 
### what we would do here is to make a big ass table.
### we could also make https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9299307/

contrast_r_cat <- emmeans(lit_mod, pairwise ~ r_cat)



summary(r_mod)



library(lmerTest)


# contrasts for literacy model with effect sizes

VarCorr(r_mod)

tot_s_lit <- sqrt(0.071079 + 0.027771 + 0.128391)




contrastsLit <- emtrends(lit_mod, pairwise ~ condition, var = "sum_lit")

contrastsLit

eff_size(contrastsLit, edf = Inf, sigma = tot_s_lit)


# contrasts for dot model with effect sizes

contrastD <- emtrends(dot_mod, pairwise ~ condition, var = "dot_pitch")

contrastD

eff_size(contrastD, sigma = tot_sd, edf = Inf)

# contrasts for r value.

VarCorr(dot_mod)

tot_sd <- sqrt(0.071263 + 0.027771 + 0.128393)

tot_sd
contrast_r <- emmeans(r_mod, pairwise ~ condition:r_cat, adjust = "Bonferroni")
contrast_r

eff_size(contrast_r, sigma(r_mod), edf = df.residual(r_mod))




summary(lit_mod)

# plots 



  
violin_data <- lmm_model_data %>%
  group_by(condition, participant, r_cat) %>%
  filter(!is.na(response)) %>%
  filter(!is.na(condition)) %>%
  summarise(
    mean = mean(response),
    lci = t.test(response, conf.level = 0.95)$conf.int[1],
    hci = t.test(response, conf.level = 0.95)$conf.int[2],
  )  

violin_plot <- ggplot(violin_data, aes(x = condition, y = mean, fill = condition)) +
  geom_violin() +
  ggforce::geom_sina() +
  geom_boxplot(alpha = .5, width = .3, height = .5, fill = "white") +
  theme_minimal() +
theme(legend.position = "none") +
  scale_fill_grey()

violin_plot

ggsave("violin.png", plot = violin_plot)


violin_plot_interact <- ggplot(violin_data, aes(x = condition, y = mean, fill = condition)) +
  geom_violin() +
  ggforce::geom_sina() +
  geom_boxplot(alpha = .5, width = .3, fill = "white") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_grey() +
  facet_wrap(.~r_cat, ncol = 2)

violin_plot_interact
ggsave("vinter.png", plot=violin_plot_interact)



lit_plot.df <- emmip(lit_mod, condition ~ sum_lit, cov.reduce = range, plotit = F)

lit_plot_2.df <- lit_plot.df %>%
  rename(Condition = tvar) %>%
  rename(Literacy = xvar) %>%
  rename(Error = yvar)


lit_plot <- lit_plot_2.df %>%
  ggplot(aes(x = Literacy, y = Error, group = Condition)) +
  geom_line(aes(linetype = Condition, colour = Condition), size = 1.5) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid", "dotdash")) +
  scale_colour_grey(start = .1, end = .4) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 15)))
  
  
lit_plot

ggsave("litplot.png", plot = lit_plot)

dot_plot.df <- emmip(dot_mod, condition ~ dot_pitch, cov.reduce = range, plotit = F)

dot_plot_2.df <- dot_plot.df %>%
  rename(Condition = tvar) %>%
  rename(Dotpitch = xvar) %>%
  rename(Error = yvar)


dot_plot <- dot_plot_2.df %>%
  ggplot(aes(x = Dotpitch, y = Error, group = Condition)) +
  geom_line(aes(linetype = Condition, colour = Condition), size = 1.5) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid", "dotdash")) +
  scale_colour_grey(start = .1, end = .4) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 15)))


dot_plot

ggsave("dotplot.png", plot=dot_plot)



# get the slopes of the fitted lines

emt2 <- emtrends(dot_mod, "condition", var = "dot_pitch")
emt2

# get the comparisons between said fitted lines...

pairs(emt2)
pairs

anova(dot_mod, lit_mod, test = "Chisq")




# data visualisation 

literacy_plot <- ggplot(lmm_model_data,
                        aes(x= sum_lit, y = response, group = condition)) +
  geom_line(aes(linetype = condition))+
  theme(legend.position="top")

print(literacy_plot)







# buildmer add.terms demonstration

model_tryout <- lmer(add.terms(formula(final_model), "r_cat"),
                     data= lmm_model_data)



summary(model_tryout)


