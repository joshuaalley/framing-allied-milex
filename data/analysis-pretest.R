# Joshua Alley
# Analyze pre-test data from Mturk


# load packages
library(tidyverse)
library(broom)
library(rIP)
library(ggcarly)
library(coefplot)
library(brglm)
library(gridExtra)

# set seed
set.seed(12)


# load data
mturk.data <- read.csv("data/mturk-pretest-v3.csv",
                       na.strings = "-99")
# remove unfinished surveys
mturk.data <- filter(mturk.data, Finished == TRUE)
# create missing
mturk.data[mturk.data == ""] <- NA

# check IP addresses
length(unique(mturk.data$IPAddress))
iphub.key <- "MTAwNzE6NHc1akdaT0hzaGNON050SjdaZ1pRSU9tMWUwZHdIM0g="

ip.data <- getIPinfo(mturk.data, "IPAddress", iphub_key = iphub.key)
ip.data
table(ip.data$IP_Hub_recommend_block)

mturk.data <- left_join(mturk.data, ip.data, by = "IPAddress")

# filter out failed IP checks
mturk.data <- filter(mturk.data, IP_Hub_recommend_block != 1)


# attention checks
table(mturk.data$Q31)
table(mturk.data$FL_12_DO)
# seems like disproportionate is the most common response 
table(mturk.data$Q31, mturk.data$FL_12_DO)
mturk.data$fail.ca <- ifelse(mturk.data$FL_12_DO == "FL_62" &
                               mturk.data$Q31 != "The United States provides a disproportionate share of collective security for alliance members.",
                             1, 0)
table(mturk.data$fail.ca)

mturk.data$fail.ex <- ifelse(mturk.data$FL_12_DO == "FL_63" &
                               mturk.data$Q31 != "NATO members support US foreign policy interests and priorities in exchange for protection.",
                             1, 0)
table(mturk.data$fail.ex)

# Failed manipulation dummy
mturk.data$fail.manip <- ifelse(mturk.data$fail.ex == 1 |
                                 mturk.data$fail.ca == 1,
                               1, 0)
table(mturk.data$fail.manip)

# Check failed manip: filter out these obs
mturk.data <- filter(mturk.data, fail.manip == 0)


### Set up control variables
# partisanship
table(mturk.data$Q11)
mturk.data$republican = ifelse(mturk.data$Q11 == "Republican",
                               1, 0)

# Gradations of partisanship
mturk.data$partisanship <- NA
mturk.data$partisanship[is.na(mturk.data$partisanship) &
                      !is.na(mturk.data$Q12)] <- as.character(mturk.data$Q12)[!is.na(mturk.data$Q12)]
mturk.data$partisanship[is.na(mturk.data$partisanship) &
                          !is.na(mturk.data$Q13)] <- as.character(mturk.data$Q13)[!is.na(mturk.data$Q13)]
mturk.data$partisanship[is.na(mturk.data$partisanship) &
                          !is.na(mturk.data$Q14)] <- as.character(mturk.data$Q14)[!is.na(mturk.data$Q14)]
table(mturk.data$partisanship)
mturk.data$partisanship <- factor(mturk.data$partisanship,
                                  levels = c("Strong Democrat",
                                  "Moderate Democrat",
                                  "Lean Democrat",
                                  "Don't Lean",
                                  "Lean Republican",
                                  "Moderate Republican",
                                  "Strong Republican"),
                                  ordered = TRUE
                              ) 
table(mturk.data$partisanship)
mturk.data$partisanship.num <- as.numeric(mturk.data$partisanship)
table(mturk.data$partisanship.num)

# Ideology: 7 is very conservative 
table(mturk.data$Q15_1)
mturk.data$ideology <- mturk.data$Q15_1

# tabulate ideology and partisanship
table(mturk.data$ideology, mturk.data$partisanship)

# FP information
table(mturk.data$Q22)
mturk.data$nato.cor <- ifelse(mturk.data$Q22 == "25-30",
                              1, 0)
table(mturk.data$nato.cor)


mturk.data$unsc.cor <- ifelse(mturk.data$Q21 == "China,France,Russia,The United Kingdom,The United States",
                              1, 0)
table(mturk.data$unsc.cor)

mturk.data$unsc.num <- str_count(mturk.data$Q21, "China") +
  str_count(mturk.data$Q21, "France") +
  str_count(mturk.data$Q21, "Russia") +
  str_count(mturk.data$Q21, "The United Kingdom") +
  str_count(mturk.data$Q21, "The United States") -
  str_count(mturk.data$Q21, "Brazil") -
  str_count(mturk.data$Q21, "Germany") -
  str_count(mturk.data$Q21, "Japan") -
  str_count(mturk.data$Q21, "Israel") 
table(mturk.data$unsc.num)

# National Pride
mturk.data$natl.pride <- mturk.data$Q23_1
table(mturk.data$natl.pride)

# military service 
mturk.data$mil.serv <- ifelse(mturk.data$Q20 == "Yes", 1, 0)
table(mturk.data$mil.serv)

# education
table(mturk.data$Q7)
mturk.data$college.grad <- ifelse(mturk.data$Q7 == "Bachelor's degree in college (4-year)" |
                                  mturk.data$Q7 == "Master's degree" |
                                  mturk.data$Q7 == "Doctoral degree" |
                                    mturk.data$Q7 == "Professional degree (JD, MD)",
                                  1, 0
                                    )
table(mturk.data$college.grad)


# Age
mturk.data$age <- mturk.data$Q6

# Gender
mturk.data$gender <- ifelse(mturk.data$Q9 == "Male", 1, 2)
table(mturk.data$gender)

# income
table(mturk.data$Q10)
mturk.data$income.num <- as.numeric(factor(mturk.data$Q10))
table(mturk.data$income.num)



### Treatment assigment dummies
# Treatment assignment
table(mturk.data$FL_12_DO)
mturk.data$treat.neut <- ifelse(mturk.data$FL_12_DO == "FL_61",
                                1, 0)
table(mturk.data$treat.neut)
mturk.data$treat.ca <- ifelse(mturk.data$FL_12_DO == "FL_62",
                                1, 0)
table(mturk.data$treat.ca)
mturk.data$treat.ex <- ifelse(mturk.data$FL_12_DO == "FL_63",
                              1, 0)
table(mturk.data$treat.ex)


### Code outcome variables
table(mturk.data$Q26)
table(mturk.data$Q26, mturk.data$FL_12_DO)
mturk.data$force <- ifelse(mturk.data$Q26 == "Yes, should use force.",
                             1, 0)
table(mturk.data$force)

# withdrawal
table(mturk.data$Q27)
table(mturk.data$Q27, mturk.data$FL_12_DO)
mturk.data$withdraw <- ifelse(mturk.data$Q27 == "Yes, should withdraw.",
                              1, 0)
table(mturk.data$withdraw)

# favorability
table(mturk.data$Q25)
table(mturk.data$Q25, mturk.data$FL_12_DO)
mturk.data$nato.favor <- factor(mturk.data$Q25, levels =
                                  c("Very unfavorable",
                                     "Somewhat unfavorable",
                                    "Neither favorable nor unfavorable",
                                    "Somewhat favorable",
                                    "Very favorable"),
                                ordered = TRUE)
table(mturk.data$nato.favor)
mturk.data$nato.favor.num <- as.numeric(mturk.data$nato.favor)
table(mturk.data$nato.favor.num)

# dummy variable for alternative analyses
mturk.data$favor.dum <- ifelse(mturk.data$nato.favor.num > 3, 1, 0)
table(mturk.data$favor.dum)


### Relationship between the outcome vars
table(mturk.data$Q25, mturk.data$Q26)
table(mturk.data$Q25, mturk.data$Q27)
table(mturk.data$Q26, mturk.data$Q27)



# Plot the raw by treatments
fv.raw <- ggplot(mturk.data, aes(x = nato.favor.num,
                       group = FL_12_DO,
                       fill = FL_12_DO)) +
           geom_bar(position = position_dodge()) +
           scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_61", "FL_62", "FL_63"),
                      labels=c("Neutral", "CA", "EX")) +
           labs(x = "NATO Favorability") +
           theme_bw()
fv.raw 
# calculate averages by group
mturk.data %>%
  group_by(FL_12_DO) %>%
   summarize(
     mean.favor = mean(nato.favor.num, na.rm = TRUE),
     se.favor = sqrt(var(nato.favor.num, na.rm = TRUE)),
     .groups = "keep"
   )



fr.raw <- ggplot(mturk.data, aes(x = Q26,
                       group = FL_12_DO,
                       fill = FL_12_DO)) +
           geom_bar(position = position_dodge()) +
           scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_61", "FL_62", "FL_63"),
                      labels=c("Neutral", "CA", "EX")) +
           labs(x = "Support Intervention") +
           theme_bw()
fr.raw

wd.raw <- ggplot(mturk.data, aes(x = Q27,
                       group = FL_12_DO,
                       fill = FL_12_DO)) +
           geom_bar(position = position_dodge()) +
           scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("FL_61", "FL_62", "FL_63"),
                      labels=c("Neutral", "CA", "EX")) +
           labs(x = "Support Withdrawal") +
          theme_bw()
wd.raw

# Arrange plots
grid.arrange(fv.raw, fr.raw, wd.raw, nrow = 3)
raw.plots <- arrangeGrob(fv.raw, fr.raw, wd.raw, nrow = 3)
ggsave("figures/raw-data.png", raw.plots, height = 8, width = 8)


### quick ANOVA analysis
favor.anova <- aov(nato.favor.num ~ factor(FL_12_DO), data = mturk.data)
summary(favor.anova)
plot(favor.anova, 1)
TukeyHSD(favor.anova)
kruskal.test(nato.favor.num ~ factor(FL_12_DO), data = mturk.data)

# regression with controls
favor.reg <- lm(nato.favor.num ~ treat.ex + treat.ca +
                  partisanship.num + ideology + natl.pride +
                  unsc.num + mil.serv + age + gender +
                  college.grad, 
                data = mturk.data)
summary(favor.reg)

# dummy for favorable
table(mturk.data$favor.dum, mturk.data$FL_12_DO)
favor.glm <- brglm(favor.dum ~ treat.ex + treat.ca +
                      partisanship.num + ideology + natl.pride +
                      unsc.num + mil.serv + age + gender +
                      college.grad, 
                    family = binomial(link = "logit"),
                    data = mturk.data)
summary(favor.glm)

# logit of support for withdrawal
withdraw.glm <- brglm(withdraw ~ treat.ex + treat.ca + 
                      partisanship.num + ideology + natl.pride +
                      unsc.num + mil.serv + age + gender +
                      college.grad, 
                    family = binomial(link = "logit"),
                    data = mturk.data)
summary(withdraw.glm)


# logit of support for intervention
force.glm <- brglm(force ~ treat.ex + treat.ca +
                     partisanship.num + ideology + natl.pride +
                     unsc.num + mil.serv + age + gender +
                     college.grad, 
                    family = binomial(link = "logit"),
                    data = mturk.data)
summary(force.glm)


# plot the coefficients
results <- bind_rows(tidy(favor.reg),
                     tidy(withdraw.glm),
                     tidy(force.glm)
                    ) %>%
                mutate(
                  model = c(rep("Favorability", nrow(tidy(favor.reg))),
                            rep("Withdrawal", nrow(tidy(favor.reg))),
                            rep("Intervention", nrow(tidy(favor.reg)))
                  )
                ) %>% 
               filter(term != "(Intercept)") 
               
results$term <- recode(results$term,
                      treat.ex = "Exchange", treat.ca = "Collective Action",
                      partisanship.num = "Partisan ID", ideology = "Ideology",
                      natl.pride = "Natl. Pride", unsc.cor = "FP Knowledge",
                      age = "Age", gender = "Gender",
                      mil.serv = "Military Service", college.grad = "College Grad")
# order terms factor for plotting
results$term <- factor(results$term, levels = unique(results$term),
                       ordered = TRUE)

ggplot(results, aes(y = term, x = estimate)) +
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
ggsave("figures/mturk-res-both.png", height = 6, width = 8)



### repeat the process, but use exchange treatment only
# regression with controls
favor.reg.ex <- lm(nato.favor.num ~ treat.ex + 
                     partisanship.num + ideology + natl.pride +
                     unsc.cor + mil.serv + age +
                     college.grad, 
                data = mturk.data)
summary(favor.reg.ex)


# logit of support for withdrawal
withdraw.glm.ex <- brglm(withdraw ~ treat.ex +  
                           partisanship.num + ideology + natl.pride +
                           unsc.cor + mil.serv + age +
                           college.grad, 
                    family = binomial(link = "logit"),
                    data = mturk.data)
summary(withdraw.glm.ex)


# logit of support for intervention
force.glm.ex <- glm(force ~ treat.ex +
                        partisanship.num + ideology + natl.pride +
                        unsc.cor + mil.serv + age +
                        college.grad, 
                   family = binomial(link = "logit"),
                   data = mturk.data)
summary(force.glm.ex)


# plot the coefficients
results.ex <- bind_rows(tidy(favor.reg.ex),
                     tidy(withdraw.glm.ex),
                     tidy(force.glm.ex)
) %>%
  mutate(
    model = c(rep("Favorability", nrow(tidy(favor.reg.ex))),
              rep("Withdrawal", nrow(tidy(favor.reg.ex))),
              rep("Intervention", nrow(tidy(favor.reg.ex)))
    )
  ) %>% 
  filter(term != "(Intercept)") 

results.ex$term <- recode(results.ex$term,
                       treat.ex = "Exchange", treat.ca = "Collective Action",
                       partisanship.num = "Partisan ID", ideology = "Ideology",
                       natl.pride = "Natl. Pride", unsc.cor = "FP Knowledge",
                       mil.serv = "Military Service", college.grad = "College Grad")
# order terms factor for plotting
results.ex$term <- factor(results.ex$term, levels = unique(results.ex$term),
                       ordered = TRUE)

ggplot(results.ex, aes(y = term, x = estimate)) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_errorbarh(aes(
    xmin = estimate - 1.96*std.error,
    xmax = estimate + 1.96*std.error   
  ), height = .1, size = 1) +
  scale_y_discrete(limits = rev(levels(results.ex$term))) +
  labs(x = "Estimate", y = "Variable") +
  theme_bw()
ggsave("figures/mturk-res-ex.png", height = 6, width = 8)


# exchange treatment across groups
multiplot(favor.reg, favor.reg.ex, 
          withdraw.glm, withdraw.glm.ex,
          force.glm, force.glm.ex,
          coefficients = "treat.ex",
          lwdOuter = 1,
          color = "black",
          secret.weapon = TRUE)


### gjrm model of all three
library(GJRM)

favor.formula <- formula(favor.dum ~ treat.ex + treat.ca +
                           partisanship.num + ideology + natl.pride +
                           age + college.grad +
                           unsc.cor + mil.serv)
withdraw.formula <- formula(withdraw ~ treat.ex + treat.ca +
                           partisanship.num + ideology + natl.pride +
                          age + college.grad +
                           unsc.cor + mil.serv)
force.formula <- formula(force ~ treat.ex + treat.ca +
                              partisanship.num + ideology + natl.pride +
                           age + college.grad +
                              unsc.cor + mil.serv)

joint.model <- gjrm(list(favor.formula, withdraw.formula,
                    force.formula),
  margins = c("probit", "probit", "probit"),
  BivD = "N",
  Model = "T",
  data = mturk.data
)
conv.check(joint.model)
AIC(joint.model)
summary(joint.model)
joint.model.sum <- summary(joint.model)


# summarize results
joint.res <- rbind.data.frame(joint.model.sum$tableP1[, 1:2],
                   joint.model.sum$tableP2[, 1:2],
                   joint.model.sum$tableP3[, 1:2])

colnames(joint.res) <- c("estimate", "std.error")
  
joint.res$term <- rep(recode(rownames(joint.model.sum$tableP1), 
    X.Intercept. = "(Intercept)",                         
    treat.ex = "Exchange", treat.ca = "Collective Action",
    partisanship.num = "Partisan ID", ideology = "Ideology",
    natl.pride = "Natl. Pride", unsc.cor = "FP Knowledge",
    mil.serv = "Military Service", college.grad = "College Grad"),
    3)


# label models
joint.res$model <- c(rep("Favorability", nrow(joint.model.sum$tableP1)),
            rep("Withdrawal", nrow(joint.model.sum$tableP1)),
            rep("Nonintervention", nrow(joint.model.sum$tableP1))
            )
# plot terms w/o intercept
joint.res <- filter(joint.res, term != "(Intercept)")

joint.res$term <- factor(joint.res$term, levels = unique(joint.res$term),
                          ordered = TRUE)

ggplot(joint.res, aes(y = term, x = estimate)) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_errorbarh(aes(
    xmin = estimate - 1.96*std.error,
    xmax = estimate + 1.96*std.error   
  ), height = .1, size = 1) +
  scale_y_discrete(limits = rev(levels(joint.res$term))) +
  labs(x = "Estimate", y = "Variable") +
  theme_bw()
#ggsave("figures/gjrm-res.png",
#       height = 6, width = 8)




### Estimate heterogeneous treatment effects


# Partisanship and responses
ggplot(mturk.data, aes(x = nato.favor.num,
                       group = factor(republican),
                       fill = factor(republican))) +
  facet_wrap(~ FL_12_DO) +
  geom_bar(position = position_dodge())

table(mturk.data$Q26, mturk.data$republican)
ggplot(mturk.data, aes(x = Q26,
                       group = factor(republican),
                       fill = factor(republican))) +
  facet_wrap(~ FL_12_DO) +
  geom_bar(position = position_dodge())

table(mturk.data$Q27, mturk.data$republican)
ggplot(mturk.data, aes(x = Q27,
                       group = factor(republican),
                       fill = factor(republican))) +
  facet_wrap(~ FL_12_DO) +
  geom_bar(position = position_dodge())



# hierarchical model with brms
library(rstanarm)
options(mc.cores = parallel::detectCores())

het.favor.ml.ex <- stan_lmer(nato.favor.num ~ 
                              age + natl.pride + 
                               treat.ex*unsc.cor +
                              # treat.ca*unsc.cor +
                               treat.ex*republican + 
                              # treat.ca*republican +
                               treat.ex*college.grad + 
                              # treat.ca*college.grad +
                             (1 + treat.ex | ideology),
                    data = mturk.data,
                    prior = normal(location = 0, 
                                    scale = .25,
                                    autoscale = FALSE),
                    prior_intercept = normal(location = 0, 
                                              scale = .75, 
                                              autoscale = FALSE),
                    adapt_delta = .99
                    )

summary(het.favor.ml.ex, digits = 3)
het.favor.ml.ex


# Findit algoritm is struggling
library(FindIt)
het.favor <- FindIt(model.treat = nato.favor.num ~ treat.ex,
            model.main= ~   partisanship.num + ideology + natl.pride +
              unsc.cor + 
              college.grad,
            model.int= ~ partisanship.num + ideology + natl.pride +
              unsc.cor + 
              college.grad,
            data = mturk.data,
            type = "continuous",
            treat.type = "single")
summary(het.favor)
pred.het.favor <- predict(het.favor)
head(pred.het.favor$data, n=10)
plot(pred.het.favor)

ggplot(pred.het.favor$data, aes(x = factor(ideology),
                                y = Treatment.effect)) +
       geom_violin()


### mooted mechanism from neutral category
table(mturk.data$Q29)
table(mturk.data$Q29, mturk.data$treat.neut)
mturk.data$mechanism <- recode(mturk.data$Q29,
                               "The United States provides a disproportionate share of collective security for alliance members." =
                                 "Collective Action",
                               "NATO members support US foreign policy interests and priorities in exchange for protection." =
                                 "Exchange")
table(mturk.data$mechanism, mturk.data$treat.neut)

filter(mturk.data, treat.neut == 1) %>% 
ggplot(aes(x = mechanism,
           fill = factor(republican))) +
  geom_bar() +
  labs(x = "Mechanism", y = "Count") +
  theme_bw()
ggsave("figures/neutral-mech.png",
       height = 4, width = 6)


# Map of respondents
library(ggmap)
library(maps)
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us.map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 
us.map  

# Data without Hawaii and plot
mturk.data.nohi <- filter(mturk.data, LocationLongitude >= -125)
mturk.data.nohi$long <- mturk.data.nohi$LocationLongitude
mturk.data.nohi$lat <- mturk.data.nohi$LocationLatitude


states <- map_data("state")
states <- left_join(states, mturk.data.nohi)

# favorability
qmplot(x = LocationLongitude,
       y = LocationLatitude, 
       #maptype = "toner-background",
       mapcolor = "bw",
       force = TRUE,
       color = ordered(nato.favor.num),
       data = mturk.data.nohi) 

# intervention
qmplot(x = LocationLongitude,
       y = LocationLatitude, 
       #maptype = "toner-background",
       mapcolor = "bw",
       force = TRUE,
       color = ordered(force),
       data = mturk.data.nohi) 



# summaries by area
us.map +
  stat_summary_2d(
    aes(x = LocationLongitude,
        y = LocationLatitude,
        z = nato.favor.num),
    fun = "mean",
    size = .5, bins = 10, alpha = .75,
    data = mturk.data.nohi
  ) +
  scale_fill_viridis_b("Avg. Favor")
ggsave("presentation/favor-map.png", height = 6, width = 8)

us.map +
  stat_summary_2d(
    aes(x = LocationLongitude,
        y = LocationLatitude,
        z = force),
    fun = "mean",
    size = .5, bins = 10, alpha = .75,
    data = mturk.data.nohi
  ) +
  scale_fill_viridis_b(name = "% No Interv.")
ggsave("presentation/noforce-map.png", height = 6, width = 8)





### STM of topic model res
library(stm)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

mturk.text <- select(mturk.data, Q28,
       treat.ex, treat.ca,
       partisanship.num, 
       unsc.cor, natl.pride,
       mil.serv, ideology, college.grad) %>%
  drop_na()

mturk.open <- textProcessor(mturk.text$Q28,
                            metadata = mturk.text[, 2:ncol(mturk.text)])

# associate text and metadata
out.mturk <- prepDocuments(mturk.open$documents, mturk.open$vocab,
                      mturk.open$meta, lower.thresh = 3)

# Search likely number of topics
storage <- searchK(out.mturk$documents, out.mturk$vocab, K = c(2, 10),
                   prevalence = ~  treat.ex + treat.ca +
                     partisanship.num + ideology + natl.pride +
                     unsc.cor + mil.serv + college.grad, 
                   data = out.mturk$meta)
plot(storage)


# estimate 
stm.mturk <- stm(documents = out.mturk$documents, vocab = out.mturk$vocab,
                       K = 2, 
                     prevalence = ~ treat.ex + treat.ca +
                     partisanship.num + ideology + natl.pride +
                     unsc.cor + mil.serv + college.grad,
                       max.em.its = 125, data = out.mturk$meta,
                       init.type = "Spectral")
plot(stm.mturk)
labelTopics(stm.mturk, c(1, 2))


# Look at treatment effects
est.topics <- estimateEffect(c(1:2) ~ treat.ex + treat.ca +
                  partisanship.num + ideology + natl.pride +
                  unsc.cor + mil.serv + college.grad,
                  stjmob <- stm.mturk,
                  metadata = out.mturk$meta)
# CA differences
plot.estimateEffect(est.topics, model = stm.mturk, 
                    covariate = "treat.ca",
                    method = "difference",
                    cov.value1=1,cov.value2=0)
# Ex differences
plot.estimateEffect(est.topics, model = stm.mturk, 
                    covariate = "treat.ex",
                    method = "difference",
                    cov.value1=1,cov.value2=0)


# Example docs TODO(JOSH): finish this. 
#docs.used <- as.character(mturk.text$Q25[out.mturk$docs.removed])
# Topic 1
thoughts1 <- findThoughts(stm.mturk, texts = mturk.text$Q28,
                          n = 2, topics = 1)$docs[[1]]
plotQuote(thoughts1, width = 30, main = "Topic 1")
