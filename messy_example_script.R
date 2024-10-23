# Load packages -----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(forcats)
library(gtsummary)


# Load LOS_data ---------------------------------------------------------------

LOS_data <- read_csv("LOS.csv") # nolint

View(LOS_data)

# plot age
hist(LOS_data$Age) # don't use this
ggplot(LOS_data) +
  geom_histogram(aes(Age))

ggplot(LOS_data) +
  geom_histogram(aes(LOS))

ggplot(LOS_data) +
  geom_histogram(aes(Age)) +
  facet_wrap(~Organisation)


LOS_data <- LOS_data |> mutate(Organisation = factor(Organisation), Organisation = fct_relevel(Organisation, "Trust10", after = Inf))
ggplot(LOS_data) +
  geom_histogram(aes(Age)) +
  facet_wrap(~Organisation)

mean(LOS_data$Age) # 50.65667
sd(LOS_data$Age)
mean(LOS_data$LOS) # 4.936667
mean(LOS_data$Death) # 0.1766667
table(LOS_data$organisation)
# error - don't know why

age1 <- LOS_data |>
  filter(Age <= 50)
age2 <- LOS_data |>
  filter(Age > 50)
t.test(age1$LOS, age2$LOS, var.equal = T)
t.test(age1$LOS, age2$LOS, var.equal = T)$p.value

# models
mod1 <- glm(Death ~ Age, data = LOS_data, family = "binomial")
summary(mod1)

mod2 <- glm(Death ~ Age + LOS, data = LOS_data, family = "binomial")
summary(mod2)

mod3 <- glm(Death ~ Age + LOS + Organisation, data = LOS_data, family = "binomial")
summary(mod3)

mod4 <- glm(Death ~ LOS, data = LOS_data, family = "binomial")
summary(mod4)


# Results -----------------------------------------------------------------

modelTable <- tbl_regression(mod3)

modelTable |>
  as_gt() |>
  gtsave("table.docx")
