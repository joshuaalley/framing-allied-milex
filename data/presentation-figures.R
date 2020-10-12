# Joshua Alley



### What to present 

# Lots of raw data
ggplot(mturk.data, aes(x = nato.favor.num, 
                       fill = FL_12_DO)) +
  facet_wrap(~ FL_12_DO) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(show.legend = FALSE) +
  labs(x = "NATO Favorability", y = "Count") +
  theme_carly_presents()
ggsave("presentation/raw-favor.png", height = 6, width = 8)


# withdrawal/other policy response
ggplot(mturk.data, aes(x = policy.res.num, 
                       fill = FL_12_DO)) +
  facet_wrap(~ FL_12_DO) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(show.legend = FALSE) +
  labs(x = "Preferred Policy", y = "Count") +
  theme_carly_presents()
ggsave("presentation/raw-policy.png", height = 6, width = 8)


ggplot(mturk.data, aes(x = factor(withdraw), 
                       fill = FL_12_DO)) +
  facet_wrap(~ FL_12_DO) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(show.legend = FALSE) +
  labs(x = "Support Withdrawal", y = "Count") +
  theme_carly_presents()
ggsave("presentation/raw-withdraw.png", height = 6, width = 8)


# Nonintervention 
drop_na(mturk.data, noforce) %>%
  ggplot(aes(x = factor(noforce), 
             fill = FL_12_DO)) +
  facet_wrap(~ FL_12_DO) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(show.legend = FALSE) +
  labs(x = "Do Not Support Intervention", y = "Count") +
  theme_carly_presents()
ggsave("presentation/raw-interv.png", height = 6, width = 8)



# Treatment effects from regressions
# exchange treatment across groups
multiplot(favor.reg, 
          withdraw.glm,
          noforce.glm,
          coefficients = "treat.ex",
          names = c("Favor", "Withdraw", "No Interv."),
          lwdOuter = 1,
          color = "black",
          title = "Exchange",
          ylab = "Treatment Effect",
          secret.weapon = TRUE) +
  theme_carly_presents()
ggsave("presentation/ex-treat.png", height = 6, width = 8)


# CA treatment across groups
multiplot(favor.reg, 
          withdraw.glm,
          noforce.glm,
          coefficients = "treat.ca",
          names = c("Favor", "Withdraw", "No Interv."),
          lwdOuter = 1,
          color = "black",
          title = "Collective Action",
          ylab = "Treatment Effect",
          secret.weapon = TRUE) +
  theme_carly_presents()
ggsave("presentation/ca-treat.png", height = 6, width = 8)


# share of democrats
ggplot(mturk.data, aes(x = partisanship.num)) + geom_bar() +
  labs(x = "Partisan ID", y = "Count") + theme_carly_presents()

ggplot(mturk.data, aes(x = partisanship.num, 
                       fill = FL_12_DO)) +
  facet_wrap(~ FL_12_DO) +
  geom_bar(show.legend = FALSE) +
  labs(x = "Partisan ID", y = "Count") +
  theme_carly_presents()
ggsave("presentation/partisan-group.png", height = 6, width = 8)


# partisanship overall 
ggplot(mturk.data, aes(x = factor(partisanship.num), y = nato.favor.num)) +
  geom_boxplot() + 
  labs(x = "Partisanship", y = "NATO Favorability") +
  theme_carly_presents()

# Mechanism check Q for neutral
filter(mturk.data, treat.neut == 1) %>% 
  ggplot(aes(x = mechanism)) +
  geom_bar() +
  labs(x = "Mechanism", y = "Count") +
  theme_carly_presents()
ggsave("presentation/neutral-mech.png",
       height = 6, width = 8)

