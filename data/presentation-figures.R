# Joshua Alley



### What to present 

# US raw data
fv.raw.plot <- fv.raw + 
                scale_fill_brewer(name="Experimental\nCondition",
                                  breaks=c("FL_61", "FL_62", "FL_63"),
                                  labels=c("Neutral", "CA", "EX"),
                                  palette = "Dark2") +
                theme_carly_presents()
fv.raw.plot
# force
fr.raw.plot <- fr.raw + 
                scale_fill_brewer(name="Experimental\nCondition",
                    breaks=c("FL_61", "FL_62", "FL_63"),
                    labels=c("Neutral", "CA", "EX"),
                    palette = "Dark2") +
                scale_x_discrete(
                 labels=c("No, should not use force." = "No",
                  "Yes, should use force." = "Yes")
                 ) + theme_carly_presents()
fr.raw.plot
# withdrawal
wd.raw.plot <- wd.raw + 
                scale_fill_brewer(name="Experimental\nCondition",
                    breaks=c("FL_61", "FL_62", "FL_63"),
                    labels=c("Neutral", "CA", "EX"),
                    palette = "Dark2") +
                 scale_x_discrete(
                   labels=c("No, should not withdraw." = "No",
                            "Yes, should withdraw." = "Yes")) +
                theme_carly_presents()
wd.raw.plot

# Arrange plots
grid.arrange(fv.raw.plot, fr.raw.plot, wd.raw.plot, nrow = 3)
raw.us.pres <- arrangeGrob(fv.raw.plot, fr.raw.plot, wd.raw.plot, nrow = 3)
ggsave("presentation/raw-us-pres.png", raw.us.pres, height = 8, width = 8)



# Treatment effects from regressions
# exchange treatment across groups
ex.treat.us <- multiplot(favor.reg, 
                withdraw.glm,
                force.glm,
                coefficients = "treat.ex",
                names = c("Favor", "Withdraw", "Intervention"),
                lwdOuter = 1,
                color = "black",
                title = "Exchange",
                xlab = "Treatment\n Effect",
                ylab = "Outcome",
                secret.weapon = TRUE) +
         theme_carly_presents()
ex.treat.us


# CA treatment across groups
ca.treat.us <- multiplot(favor.reg, 
                         withdraw.glm,
                         force.glm,
                         coefficients = "treat.ca",
                         names = c("Favor", "Withdraw", "Intervention"),
                         lwdOuter = 1,
                         color = "black",
                         title = "Collective Action",
                         xlab = "Treatment\n Effect",
                         ylab = "Outcome",
                         secret.weapon = TRUE) +
                  theme_carly_presents()
ca.treat.us

# combine treatments
grid.arrange(ex.treat.us, ca.treat.us, nrow = 2)
us.te.pres <- arrangeGrob(ex.treat.us, ca.treat.us, nrow = 2)
ggsave("presentation/us-te-pres.png", us.te.pres, height = 6, width = 8)


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





### Germany results



# German raw data
fv.raw.plot.ge <- fv.raw.ge + 
  scale_fill_brewer(name="Experimental\nCondition",
                    breaks=c("FL_43", "FL_45", "FL_49"),
                    labels=c("Neutral", "CA", "EX"),
                    palette = "Dark2") +
  theme_carly_presents()
fv.raw.plot.ge
# force
fr.raw.plot.ge <- fr.raw.ge + 
  scale_fill_brewer(name="Experimental\nCondition",
                    breaks=c("FL_43", "FL_45", "FL_49"),
                    labels=c("Neutral", "CA", "EX"),
                    palette = "Dark2") +
  scale_x_discrete(
    labels=c("No, should not use force." = "No",
             "Yes, should use force." = "Yes")
  ) + theme_carly_presents()
fr.raw.plot.ge
# withdrawal
sp.raw.plot <- sp.raw + 
  scale_fill_brewer(name="Experimental\nCondition",
                    breaks=c("FL_43", "FL_45", "FL_49"),
                    labels=c("Neutral", "CA", "EX"),
                    palette = "Dark2") +
  scale_x_discrete(
    labels=c("No, should not spend more." = "No",
             "Yes, should spend more." = "Yes")
    ) + theme_carly_presents()
sp.raw.plot


# Arrange plots
grid.arrange(fv.raw.plot.ge, fr.raw.plot.ge, sp.raw.plot, nrow = 3)
raw.plots.ge <- arrangeGrob(fv.raw.plot.ge, fr.raw.plot.ge, sp.raw.plot, nrow = 3)
ggsave("presentation/raw-german-pres.png", raw.plots.ge, height = 8, width = 8)




# Treatment effects from regressions
# exchange treatment across groups
ex.treat.ge <- multiplot(favor.reg.ge, 
                         spend.glm,
                         force.glm.ge,
                         coefficients = "treat.ex",
                         names = c("Favor", "Spend", "Intervention"),
                         lwdOuter = 1,
                         color = "black",
                         title = "Exchange",
                         xlab = "Treatment\n Effect",
                         ylab = "Outcome",
                         secret.weapon = TRUE) +
  theme_carly_presents()
ex.treat.ge


# CA treatment across groups
ca.treat.ge <- multiplot(favor.reg.ge, 
                         spend.glm,
                         force.glm.ge,
                         coefficients = "treat.ca",
                         names = c("Favor", "Spend", "Intervention"),
                         lwdOuter = 1,
                         color = "black",
                         title = "Collective Action",
                         xlab = "Treatment\n Effect",
                         ylab = "Outcome",
                         secret.weapon = TRUE) +
  theme_carly_presents()
ca.treat.ge

# combine treatments
grid.arrange(ex.treat.ge, ca.treat.ge, nrow = 2)
ge.te.pres <- arrangeGrob(ex.treat.ge, ca.treat.ge, nrow = 2)
ggsave("presentation/ge-te-pres.png", ge.te.pres, height = 6, width = 8)
