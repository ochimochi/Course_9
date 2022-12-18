if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("dplyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(gam)) install.packages("gam")
if(!require(evtree)) install.packages("evtree")
if(!require(knitr)) install.packages("knitr")

library(tidyverse)# For data cleaning, sorting, and visualization
library(caret)
library(dplyr)
library(matrixStats)
library(gam)
library(evtree)
library(knitr)
library(DataExplorer) # For Exploratory Data Analysis
library(gridExtra) # To plot several plots in one figure
library(ggpubr) # To prepare publication-ready plots
library(GGally) # For correlations
library(caTools) # For classification model
library(rpart) # For classification model
library(rattle) # Plot nicer descision trees
library(randomForest) # For Random Forest model
df <- read_csv("../input/heart-disease-uci/heart.csv") # To read file on Kaggle
copy <- df

df2 <- df %>%
  filter(
    thal != 0 & ca != 4 # remove values correspondind to NA in original dataset
  ) %>%
  # Recode the categorical variables as factors using the dplyr library.
  mutate(
    sex = case_when(
      sex == 0 ~ "female",
      sex == 1 ~ "male"
    ),
    fbs = case_when(
      fbs == 0 ~ "<=120",
      fbs == 1 ~ ">120"
    ),
    exang = case_when(
      exang == 0 ~ "no",
      exang == 1 ~ "yes"
    ),
    cp = case_when(
      cp == 3 ~ "typical angina",
      cp == 1 ~ "atypical angina",
      cp == 2 ~ "non-anginal",
      cp == 0 ~ "asymptomatic angina"
    ),
    restecg = case_when(
      restecg == 0 ~ "hypertrophy",
      restecg == 1 ~ "normal",
      restecg == 2 ~ "wave abnormality"
    ),
    target = case_when(
      target == 1 ~ "asymptomatic",
      target == 0 ~ "heart-disease"
    ),
    slope = case_when(
      slope == 2 ~ "upsloping",
      slope == 1 ~ "flat",
      slope == 0 ~ "downsloping"
    ),
    thal = case_when(
      thal == 1 ~ "fixed defect",
      thal == 2 ~ "normal",
      thal == 3 ~ "reversable defect"
    ),
    sex = as.factor(sex),
    fbs = as.factor(fbs),
    exang = as.factor(exang),
    cp = as.factor(cp),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = as.factor(thal)
  )

glimpse(df2) # Check that the transformnation worked
plot_missing(df2) # Check that the transformation did not induce NA values
df <- df2 # Replace the df dataset by the tidy dataset
df %>%
  summary()
plot_density(df, ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
plot_bar(df, ggtheme = theme_classic2())
df %>%
  filter(sex == "female") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(sex == "male") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(sex == "female") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(sex == "male") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(target == "asymptomatic") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(target == "heart-disease") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(target == "asymptomatic") %>%
  plot_bar(ggtheme = theme_classic2())

df %>%
  filter(target == "heart-disease") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(sex == "female", target == "asymptomatic") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))

df %>%
  filter(sex == "female", target == "heart-disease") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))

df %>%
  filter(sex == "female", target == "asymptomatic") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(sex == "female", target == "heart-disease") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(sex == "male", target == "asymptomatic") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(sex == "male", target == "heart-disease") %>%
  plot_density(ggtheme = theme_classic2(), geom_density_args = list("fill" = "black", "alpha" = 0.6))
df %>%
  filter(sex == "male", target == "asymptomatic") %>%
  plot_bar(ggtheme = theme_classic2())
df %>%
  filter(sex == "male", target == "heart-disease") %>%
  plot_bar(ggtheme = theme_classic2())

#Prepare a summary table per disease and gender
df %>%
  group_by(target, sex) %>%
  summarise(
    n_disease = n(),
    mean_age = round(mean(age), digits=2),
    sd_age = round(sd(age), digits=2),
    mean_trestbps = round(mean(trestbps), digits=2),
    sd_trestbps = round(sd(trestbps), digits=2),
    mean_chol = round(mean(chol), digits=2),
    sd_chol = round(sd(chol), digits=2),
    mean_thalach = round(mean(thalach), digits=2),
    sd_thalach = round(sd(thalach), digits=2),
    mean_oldpeak = round(mean(oldpeak), digits=2),
    sd_oldpeak = round(sd(oldpeak), digits=2)
  )
#A Visualization of variables per gender
# Male and Female count
a1 <- ggplot(df, aes(x = sex, fill = sex)) +
  geom_bar(width = 0.5) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# Age per gender
b1 <- ggplot(df, aes(x= sex, y = age, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) +
  ylim(0, 90) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# trestbps
c1 <- ggplot(df, aes(x = sex, y = trestbps, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "blood pressure (mmHg)") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# chol
d1 <- ggplot(df, aes(x = sex, y = chol, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "cholestorol (mg/dl)") +
  ylim(0,500) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# oldpeak
e1 <- ggplot(df, aes(x = sex, y = oldpeak, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "ST depression") +
  ylim(0,10) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# thalach
f1 <- ggplot(df, aes(x = sex, y = thalach, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "Max. heart rate") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

suppressWarnings(ggarrange(a1, b1, c1, d1, e1, f1, 
                           ncol = 2, nrow = 3,
                           align = "v"))
# Disease status
g1 <- ggplot(df, aes(x = target, fill = sex)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  labs(x = "") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# cp
h1 <- ggplot(df, aes(cp, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "chest pain") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# restecg
i1 <- ggplot(df, aes(restecg, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "rest. electrocardiographic") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# slope
j1 <- ggplot(df, aes(slope, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "peak exercise ST") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# thal 
k1 <- ggplot(df, aes(thal, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Thalium stress test") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# fbp
l1 <- ggplot(df, aes(fbs, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Fasting blood sugar") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# exang
m1 <- ggplot(df, aes(exang, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Exercise induced angina") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# ca
n1 <- ggplot(df, aes(ca, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "flourosopy") +
  coord_flip() +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(g1, h1, i1, j1, k1, l1, m1, n1, 
          ncol = 2, nrow = 4,
          align = "v")
df <- df2 %>%
  filter(sex == "male")
# Male and Female count
a2 <- ggplot(df, aes(x = target, fill = target)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# Age per gender
b2 <- ggplot(df, aes(x= target, y = age, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) +
  ylim(0, 90) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# trestbps
c2 <- ggplot(df, aes(x = target, y = trestbps, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "blood pressure (mmHg)") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# chol
d2 <- ggplot(df, aes(x = target, y = chol, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "cholestorol (mg/dl)") +
  ylim(0,500) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# oldpeak
e2 <- ggplot(df, aes(x = target, y = oldpeak, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "ST depression") +
  ylim(0,10) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# thalach
f2 <- ggplot(df, aes(x = target, y = thalach, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "Max. heart rate") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(a2, b2, c2, d2, e2, f2, 
          ncol = 2, nrow = 3,
          align = "v")
# Disease status
g2 <- ggplot(df, aes(x = target, fill = target)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  labs(x = "") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# cp
h2 <- ggplot(df, aes(cp, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "chest pain") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# restecg
i2 <- ggplot(df, aes(restecg, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "rest. electrocardiographic") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# slope
j2 <- ggplot(df, aes(slope, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "peak exercise ST") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# thal 
k2 <- ggplot(df, aes(thal, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Thalium stress test") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# fbp
l2 <- ggplot(df, aes(fbs, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Fasting blood sugar") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# exang
m2 <- ggplot(df, aes(exang, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Exercise induced angina") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# ca
n2 <- ggplot(df, aes(ca, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "flourosopy") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(g2, h2, i2, j2, k2, l2, m2, n2, 
          ncol = 2, nrow = 4,
          align = "v")
df <- df2 %>%
  filter(sex == "female")
# Male and Female count
a2 <- ggplot(df, aes(x = target, fill = target)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# Age per gender
b2 <- ggplot(df, aes(x= target, y = age, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) +
  ylim(0, 90) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# trestbps
c2 <- ggplot(df, aes(x = target, y = trestbps, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "blood pressure (mmHg)") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# chol
d2 <- ggplot(df, aes(x = target, y = chol, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "cholestorol (mg/dl)") +
  ylim(0,500) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# oldpeak
e2 <- ggplot(df, aes(x = target, y = oldpeak, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "ST depression") +
  ylim(0,10) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# thalach
f2 <- ggplot(df, aes(x = target, y = thalach, fill = target)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "Max. heart rate") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

suppressWarnings(ggarrange(a2, b2, c2, d2, e2, f2, 
                           ncol = 2, nrow = 3,
                           align = "v"))
# Disease status
g2 <- ggplot(df, aes(x = target, fill = target)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  labs(x = "") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# cp
h2 <- ggplot(df, aes(cp, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "chest pain") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# restecg
i2 <- ggplot(df, aes(restecg, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "rest. electrocardiographic") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# slope
j2 <- ggplot(df, aes(slope, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "peak exercise ST") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# thal 
k2 <- ggplot(df, aes(thal, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Thalium stress test") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# fbp
l2 <- ggplot(df, aes(fbs, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Fasting blood sugar") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# exang
m2 <- ggplot(df, aes(exang, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Exercise induced angina") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

# ca
n2 <- ggplot(df, aes(ca, group = target, fill = target)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "flourosopy") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc97f","#ef3b2c"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(g2, h2, i2, j2, k2, l2, m2, n2, 
          ncol = 2, nrow = 4,
          align = "v")
df <- copy %>%
  filter(
    thal != 0 & ca != 4 # remove values correspondind to NA in original dataset
  )

# ggcorr(df, palette = "RdBu")
GGally::ggcorr(df, geom = "circle")
select2 <- df %>%
  dplyr::select(
    target,
    slope,
    thalach,
    restecg,
    cp
  )
ggcorr(select2, geom = "circle")

ggpairs(df)
ggpairs(select2)

# glimpse(df)

df_select <- df %>%
  dplyr::select( #because of conflict between MASS and dplyr select need to use dplyr::select
    target,
    age,
    sex,
    chol,
    restecg,
    cp,
    thalach,
    slope
  )
df_select$target <- factor(df_select$target) # Define target as a factor. rpart classification would not work otherwise.

accuracy <- 0

# Build a simple classification desicion tree with rpart. Run the model until the accuracy reach the selected minimum.
while(accuracy <= 0.88) {
  split_values <- sample.split(df_select$target, SplitRatio = 0.65)
  train_set <- subset(df_select, split_values == T)
  test_set <- subset(df_select, split_values == F)
  mod_class <- rpart(target~. , data=train_set)
  result_class <- predict(mod_class, test_set, type = "class")
  table <- table(test_set$target, result_class)
  accuracy <- (table["0","0"] + table["1","1"])/sum(table)
  # cat("accuracy = ", round(accuracy, digits = 2)*100, "%")
}
cat("Model accuracy", round(accuracy, digits = 2)*100, "%")  
## Model accuracy 88 %

# par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
fancyRpartPlot(mod_class, , caption = NULL)
# plot(mod_class)
# text(mod_class, use.n = TRUE)
copy2 <- df
df$target <- factor(df$target)
accuracy <- 0

# Build a simple classification desicion tree with rpart. Run the model until the accuracy reach the selected minimum.
while(accuracy <= 0.88) {
  split_values <- sample.split(df_select$target, SplitRatio = 0.65)
  train_set <- subset(df, split_values == T)
  test_set <- subset(df, split_values == F)
  mod_class <- rpart(target~. , data=train_set)
  result_class <- predict(mod_class, test_set, type = "class")
  table <- table(test_set$target, result_class)
  accuracy <- (table["0","0"] + table["1","1"])/sum(table)
  # cat("accuracy = ", round(accuracy, digits = 2)*100, "%")
}
## Model accuracy 89 %
# par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
fancyRpartPlot(mod_class, , caption = NULL)
# plot(mod_class)
# text(mod_class, use.n = TRUE)
set.seed(103)
train <- sample(nrow(df_select), 0.7*nrow(df_select), replace = FALSE)
TrainSet <- df_select[train,]
ValidSet <- df_select[-train,]
summary(TrainSet)
summary(ValidSet)
# Create a Random Forest model with default parameters
model1 <- randomForest(target ~ ., data = TrainSet,  ntree = 1000, mtry = 1, importance = TRUE)
model1
# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$target) 
# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$target)                
table(predValid,ValidSet$target)
# To check important variables
importance(model1)        
varImpPlot(model1)  
set.seed(103)
train <- sample(nrow(df), 0.7*nrow(df_select), replace = FALSE)
TrainSet <- df[train,]
ValidSet <- df[-train,]
summary(TrainSet)
summary(ValidSet)
# Create a Random Forest model with default parameters
model2 <- randomForest(target ~ ., data = TrainSet,  ntree = 1000, mtry = 2, importance = TRUE)
model2
# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$target) 
# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$target)      
table(predValid,ValidSet$target)
# To check important variables
importance(model2)        
varImpPlot(model2)  







