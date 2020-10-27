
# load packages
library(tidyverse)
library(broom)
library(ggcarly)
library(coefplot)
library(brglm)
library(gridExtra)

# set seed
set.seed(12)


# load data
prof.data <- read.csv("data/germany-pretest-v1.csv",
                       na.strings = "-99")
glimpse(prof.data)
# remove unfinished surveys
prof.data <- filter(prof.data, Finished == TRUE)
# create missing
prof.data[prof.data == ""] <- NA

# manipulation checks
table(prof.data$Q31)
table(prof.data$FL_12_DO)
# seems like disproportionate is the most common response 
table(prof.data$Q31, prof.data$FL_12_DO)
prof.data$fail.ca <- ifelse(prof.data$FL_12_DO == "FL_45" &
                                  prof.data$Q31 != "The United States provides a disproportionate share of collective security for alliance members.",
                                1, 0)
table(prof.data$fail.ca)

prof.data$fail.ex <- ifelse(prof.data$FL_12_DO == "FL_49" &
                                  prof.data$Q31 != "NATO members support US foreign policy interests and priorities in exchange for protection.",
                                1, 0)
table(prof.data$fail.ex)

# Failed manipulation dummy
prof.data$fail.manip <- ifelse(prof.data$fail.ex == 1 |
                                  prof.data$fail.ca == 1,
                                1, 0)
table(prof.data$fail.manip)


### Set up control variables
# partisanship
table(prof.data$Q11)
prof.data$greens = ifelse(prof.data$Q11 == "BÃ¼ndnis 90/Die GrÃ¼nen",
                               1, 0)
table(prof.data$greens)


# Ideology: 7 is very conservative 
table(prof.data$Q15_1)
prof.data$ideology <- prof.data$Q15_1

# tabulate ideology and partisanship
table(prof.data$ideology, prof.data$greens)

# FP information
table(prof.data$Q22)
prof.data$nato.cor <- ifelse(prof.data$Q22 == "25-30",
                              1, 0)
table(prof.data$nato.cor)


prof.data$unsc.cor <- ifelse(prof.data$Q21 == "China,France,Russia,The United Kingdom,The United States",
                              1, 0)
table(prof.data$unsc.cor)

prof.data$unsc.num <- str_count(prof.data$Q21, "China") +
  str_count(prof.data$Q21, "France") +
  str_count(prof.data$Q21, "Russia") +
  str_count(prof.data$Q21, "The United Kingdom") +
  str_count(prof.data$Q21, "The United States") -
  str_count(prof.data$Q21, "Brazil") -
  str_count(prof.data$Q21, "Germany") -
  str_count(prof.data$Q21, "Japan") -
  str_count(prof.data$Q21, "Israel") 
table(prof.data$unsc.num)

# National Pride
prof.data$natl.pride <- prof.data$Q23_1
table(prof.data$natl.pride)

# military service 
prof.data$mil.serv <- ifelse(prof.data$Q20 == "Yes", 1, 0)
table(prof.data$mil.serv)

# education
table(prof.data$Q7)
prof.data$college.grad <- ifelse(prof.data$Q7 == "Bachelor's degree in college (4-year)" |
                                    prof.data$Q7 == "Master's degree" |
                                    prof.data$Q7 == "Doctoral degree",
                                  1, 0
)
table(prof.data$college.grad)


# Age
prof.data$age <- prof.data$Q6
summary(prof.data$age)
prof.data$age[prof.data$age == 231] <- 23 # fix typo

# Gender
prof.data$gender <- ifelse(prof.data$Q9 == "Male", 1, 2)
table(prof.data$gender)

# income
table(prof.data$Q10)
prof.data$income.num <- as.numeric(factor(prof.data$Q10))
table(prof.data$income.num)



### Treatment assigment dummies
# Treatment assignment
table(prof.data$FL_12_DO)
prof.data$treat.neut <- ifelse(prof.data$FL_12_DO == "FL_43",
                                1, 0)
table(prof.data$treat.neut)
prof.data$treat.ca <- ifelse(prof.data$FL_12_DO == "FL_45",
                              1, 0)
table(prof.data$treat.ca)
prof.data$treat.ex <- ifelse(prof.data$FL_12_DO == "FL_49",
                              1, 0)
table(prof.data$treat.ex)


### Code outcome variables
table(prof.data$Q26)
table(prof.data$Q26, prof.data$FL_12_DO)
prof.data$force <- ifelse(prof.data$Q26 == "Yes, should use force.",
                           1, 0)
table(prof.data$force)

# withdrawal
table(prof.data$Q27)
table(prof.data$Q27, prof.data$FL_12_DO)
prof.data$spend <- ifelse(prof.data$Q27 == "Yes, should spend more.",
                              1, 0)
table(prof.data$spend)

# favorability
table(prof.data$Q25)
table(prof.data$Q25, prof.data$FL_12_DO)
prof.data$nato.favor <- factor(prof.data$Q25, levels =
                                  c("Very unfavorable",
                                    "Somewhat unfavorable",
                                    "Neither favorable nor unfavorable",
                                    "Somewhat favorable",
                                    "Very favorable"),
                                ordered = TRUE)
table(prof.data$nato.favor)
prof.data$nato.favor.num <- as.numeric(prof.data$nato.favor)
table(prof.data$nato.favor.num)

# dummy variable for alternative analyses
prof.data$favor.dum <- ifelse(prof.data$nato.favor.num > 3, 1, 0)
table(prof.data$favor.dum)


### Relationship between the outcome vars
table(prof.data$nato.favor, prof.data$Q26)
table(prof.data$nato.favor, prof.data$Q27)
table(prof.data$Q26, prof.data$Q27)



# Plot the raw by treatments
fv.raw.ge <- ggplot(prof.data, aes(x = nato.favor.num,
                                 group = FL_12_DO,
                                 fill = FL_12_DO)) +
  geom_bar(position = position_dodge()) +
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_43", "FL_45", "FL_49"),
                      labels=c("Neutral", "CA", "EX")) +
  labs(x = "NATO Favorability") +
  theme_bw()
fv.raw.ge 
# calculate averages by group
prof.data %>%
  group_by(FL_12_DO) %>%
  summarize(
    mean.favor = mean(nato.favor.num, na.rm = TRUE),
    se.favor = sqrt(var(nato.favor.num, na.rm = TRUE)),
    .groups = "keep"
  )



fr.raw.ge <- ggplot(prof.data, aes(x = Q26,
                                 group = FL_12_DO,
                                 fill = FL_12_DO)) +
  geom_bar(position = position_dodge()) +
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_43", "FL_45", "FL_49"),
                      labels=c("Neutral", "CA", "EX")) +
  labs(x = "Support Intervention") +
  theme_bw()
fr.raw.ge

sp.raw <- filter(prof.data, !is.na(Q27)) %>% # drop one missing
  ggplot(aes(x = Q27,
             group = FL_12_DO,
             fill = FL_12_DO)) +
  geom_bar(position = position_dodge()) +
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_43", "FL_45", "FL_49"),
                      labels=c("Neutral", "CA", "EX")) +
  labs(x = "Spend More on Military") +
  theme_bw()
sp.raw

# Arrange plots
grid.arrange(fv.raw.ge, fr.raw.ge, sp.raw, nrow = 3)
raw.plots.ge <- arrangeGrob(fv.raw, fr.raw, wd.raw, nrow = 3)
ggsave("figures/raw-data-ge.png", raw.plots.ge, height = 8, width = 8)



# Check failed manip: filter out these obs
prof.data <- filter(prof.data, fail.manip == 0)



### quick ANOVA analysis
favor.anova.ge <- aov(nato.favor.num ~ factor(FL_12_DO), data = prof.data)
summary(favor.anova.ge)
plot(favor.anova.ge, 1)
TukeyHSD(favor.anova.ge)
kruskal.test(nato.favor.num ~ factor(FL_12_DO), data = prof.data)


# rescale variables
prof.data.rs <- select(prof.data,
                        nato.favor.num, favor.dum,
                        spend, force,
                        treat.ex, treat.ca,
                        greens, ideology, natl.pride,
                        unsc.num, mil.serv, age, gender,
                        college.grad)
prof.data.rs[, 5:ncol(prof.data.rs)] <- lapply(prof.data.rs[, 5:ncol(prof.data.rs)], 
                                                 function(x) arm::rescale(x, binary.inputs = "0/1"))

# regression with controls
favor.reg.ge <- lm(nato.favor.num ~ treat.ex + treat.ca +
                  greens + ideology + natl.pride +
                  unsc.num + mil.serv + age + gender +
                  college.grad, 
                data = prof.data.rs)
summary(favor.reg.ge)

# dummy for favorable
table(prof.data$favor.dum, prof.data$FL_12_DO)
favor.glm <- brglm(favor.dum ~ treat.ex + treat.ca +
                     greens + ideology + natl.pride +
                     unsc.num + mil.serv + age + gender +
                     college.grad, 
                   family = binomial(link = "logit"),
                   data = prof.data.rs)
summary(favor.glm)

# logit of support for withdrawal
spend.glm <- brglm(spend ~ treat.ex + treat.ca + 
                        greens + ideology + natl.pride +
                        unsc.num + mil.serv + age + gender +
                        college.grad, 
                      family = binomial(link = "logit"),
                      data = prof.data.rs)
summary(spend.glm)


# logit of support for intervention
force.glm.ge <- brglm(force ~ treat.ex + treat.ca +
                     greens + ideology + natl.pride +
                     unsc.num + mil.serv + age + gender +
                     college.grad, 
                   family = binomial(link = "logit"),
                   data = prof.data.rs)
summary(force.glm.ge)


# plot the coefficients
results.ge <- bind_rows(tidy(favor.reg.ge),
                     tidy(spend.glm),
                     tidy(force.glm.ge)
) %>%
  mutate(
    model = c(rep("Favorability", nrow(tidy(favor.reg.ge))),
              rep("Higher Spending", nrow(tidy(favor.reg.ge))),
              rep("Intervention", nrow(tidy(favor.reg.ge)))
    )
  ) %>% 
  filter(term != "(Intercept)") 

results.ge$term <- recode(results$term,
                       treat.ex = "Exchange", treat.ca = "Collective Action",
                       greens = "Partisan ID", ideology = "Ideology",
                       natl.pride = "Natl. Pride", unsc.num = "FP Knowledge",
                       age = "Age", gender = "Gender",
                       mil.serv = "Military Service", college.grad = "College Grad")
# order terms factor for plotting
results.ge$term <- factor(results.ge$term, levels = unique(results.ge$term),
                       ordered = TRUE)

ggplot(results.ge, aes(y = term, x = estimate)) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_errorbarh(aes(
    xmin = estimate - 1.96*std.error,
    xmax = estimate + 1.96*std.error   
  ), height = .1, size = 1) +
  scale_y_discrete(limits = rev(levels(results$term))) +
  labs(x = "Estimate", y = "Variable") +
  theme_bw()
ggsave("figures/prof-res-both.png", height = 6, width = 8)



### gjrm model of all three
library(GJRM)

favor.formula.ge <- formula(favor.dum ~ treat.ex + treat.ca +
                           greens + ideology + natl.pride +
                           age + college.grad +
                           unsc.cor + mil.serv)
spend.formula <- formula(spend ~ treat.ex + treat.ca +
                              greens + ideology + natl.pride +
                              age + college.grad +
                              unsc.cor + mil.serv)
force.formula.ge <- formula(force ~ treat.ex + treat.ca +
                           greens + ideology + natl.pride +
                           age + college.grad +
                           unsc.cor + mil.serv)

joint.model.ge <- gjrm(list(favor.formula.ge, spend.formula,
                         force.formula.ge),
                    margins = c("probit", "probit", "probit"),
                    BivD = "N",
                    Model = "T",
                    data = prof.data
)
conv.check(joint.model.ge)
AIC(joint.model.ge)
summary(joint.model.ge)


### mooted mechanism from neutral category
table(prof.data$Q29)
table(prof.data$Q29, prof.data$treat.neut)
prof.data$mechanism <- recode(prof.data$Q29,
                               "The United States provides a disproportionate share of collective security for alliance members." =
                                 "Collective Action",
                               "NATO members support US foreign policy interests and priorities in exchange for protection." =
                                 "Exchange")
table(prof.data$mechanism, prof.data$treat.neut)

filter(prof.data, treat.neut == 1) %>% 
  ggplot(aes(x = mechanism)) +
  geom_bar() +
  labs(x = "Mechanism", y = "Count") +
  theme_bw()
