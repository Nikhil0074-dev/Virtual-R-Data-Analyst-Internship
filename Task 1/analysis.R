# ==============================================================================
# Week 1 - Data Cleaning and Preliminary Analysis with R
# Dataset : Titanic passenger data (891 passengers, 12 variables)
# Source  : https://github.com/datasciencedojo/datasets (titanic.csv)
# ==============================================================================

# ---- setup ----
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(corrplot); library(gridExtra); library(scales)
})
options(width = 90, digits = 5)
theme_set(theme_minimal(base_size = 12) +
          theme(plot.background = element_rect(fill = "white", colour = NA)))
pal <- c(Died = "#D1495B", Survived = "#2E86AB")
dir.create("figures", showWarnings = FALSE)
dir.create("output",  showWarnings = FALSE)
cat(R.version.string, "\n")
cat("ggplot2", as.character(packageVersion("ggplot2")),
    "| dplyr", as.character(packageVersion("dplyr")), "\n")

# ---- load ----
raw <- read.csv("titanic.csv", stringsAsFactors = FALSE,
                na.strings = c("", "NA"), strip.white = TRUE)
df <- raw
cat("Rows:", nrow(df), "  Columns:", ncol(df), "\n\n")
head(df[, c("PassengerId", "Survived", "Pclass", "Sex", "Age",
            "SibSp", "Parch", "Fare", "Cabin", "Embarked")], 6)

# ---- str_raw ----
str(df)

# ---- summary_raw ----
summary(df[, c("Survived", "Pclass", "Age", "SibSp", "Parch", "Fare")])
cat("\nUnique values in text columns:\n")
sapply(df[sapply(df, is.character)], function(x) length(unique(x)))

# ---- missing ----
# Blank strings hidden in the raw file (read.csv does not treat them as NA by default)
blank_raw <- sapply(read.csv("titanic.csv", na.strings = "NA", stringsAsFactors = FALSE),
                    function(x) sum(x == "", na.rm = TRUE))
cat("Blank strings in the raw file:\n")
print(blank_raw[blank_raw > 0])

miss <- data.frame(Variable = names(df),
                   Missing  = colSums(is.na(df)),
                   Percent  = round(100 * colSums(is.na(df)) / nrow(df), 2),
                   row.names = NULL)
miss <- miss[order(-miss$Missing), ]
cat("\nMissing values per variable:\n")
print(miss)
cat("\nComplete cases:", sum(complete.cases(df)), "of", nrow(df), "\n")

# ---- missing_plot ----
p_miss <- ggplot(miss[miss$Missing > 0, ],
                 aes(x = reorder(Variable, Percent), y = Percent)) +
  geom_col(fill = "#2E86AB", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%  (n=", Missing, ")")), hjust = -0.05) +
  coord_flip() + ylim(0, 110) +
  labs(title = "Missing values by variable", x = NULL, y = "% of rows missing")
ggsave("figures/fig02_missing.png", p_miss, width = 7, height = 3, dpi = 150)

# ---- missing_mech ----
# Is Age missing completely at random? Test against Pclass and Survived
df %>% group_by(Pclass) %>%
  summarise(n = n(), age_missing = sum(is.na(Age)),
            pct_missing = round(100 * mean(is.na(Age)), 1))
chisq.test(table(is.na(df$Age), df$Pclass))
chisq.test(table(is.na(df$Age), df$Survived))

# ---- dup_valid ----
cat("Duplicate rows          :", sum(duplicated(df)), "\n")
cat("Duplicate PassengerId   :", sum(duplicated(df$PassengerId)), "\n")
cat("Duplicate passenger name:", sum(duplicated(df$Name)), "\n")
cat("Age range               :", range(df$Age, na.rm = TRUE), "\n")
cat("Fare range              :", range(df$Fare), "\n")
cat("Passengers with Fare = 0:", sum(df$Fare == 0), "\n")
cat("Negative Age/Fare values:", sum(df$Age < 0, na.rm = TRUE) + sum(df$Fare < 0), "\n\n")
lapply(df[c("Survived", "Pclass", "Sex", "Embarked")], table, useNA = "ifany")

# ---- types ----
df$SurvivedNum <- df$Survived                       # keep a 0/1 copy for modelling
df$Survived <- factor(df$Survived, levels = c(0, 1), labels = c("Died", "Survived"))
df$Pclass   <- factor(df$Pclass, levels = 1:3,
                      labels = c("1st", "2nd", "3rd"), ordered = TRUE)
df$Sex      <- factor(df$Sex)
df$Embarked <- factor(df$Embarked, levels = c("C", "Q", "S"),
                      labels = c("Cherbourg", "Queenstown", "Southampton"))
str(df[, c("Survived", "SurvivedNum", "Pclass", "Sex", "Embarked")])

# ---- features ----
# Extract the title (Mr, Mrs, ...) from the Name column
df$Title <- trimws(gsub("(.*, )|(\\..*)", "", df$Name))
cat("Raw titles:\n"); print(table(df$Title))

rare_titles <- c("Dr", "Rev", "Col", "Major", "Capt", "Don",
                 "Jonkheer", "Lady", "Sir", "the Countess")
df$Title[df$Title %in% c("Mlle", "Ms")] <- "Miss"
df$Title[df$Title == "Mme"]             <- "Mrs"
df$Title[df$Title %in% rare_titles]     <- "Rare"
df$Title <- factor(df$Title)
cat("\nConsolidated titles by Sex:\n"); print(table(df$Title, df$Sex))

# Family and cabin based features
df$FamilySize  <- df$SibSp + df$Parch + 1
df$IsAlone     <- as.integer(df$FamilySize == 1)
df$HasCabin    <- as.integer(!is.na(df$Cabin))
df$Deck        <- substr(df$Cabin, 1, 1)
df$Age_missing <- as.integer(is.na(df$Age))       # keep a flag before imputing
df$Fare_zero   <- as.integer(df$Fare == 0)
cat("\nNew columns created: Title, FamilySize, IsAlone, HasCabin, Deck, Age_missing, Fare_zero\n")

# ---- impute ----
age_before <- df$Age

# 1) Embarked (2 NAs): mode imputation
mode_emb <- names(which.max(table(df$Embarked)))
df$Embarked[is.na(df$Embarked)] <- mode_emb
cat("Embarked imputed with the mode:", mode_emb, "\n")

# 2) Age (177 NAs): median of the passenger's Title x Pclass group
grp_med <- ave(df$Age, df$Title, df$Pclass, FUN = function(x) median(x, na.rm = TRUE))
df$Age  <- ifelse(is.na(df$Age), grp_med, df$Age)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)   # safety net
cat("Age imputed with group medians. Remaining NAs:", sum(is.na(df$Age)), "\n")

# 3) Fare: 15 zero fares are implausible for paying passengers -> treat as NA,
#    impute with the median fare of the same class
df$Fare[df$Fare == 0] <- NA
df$Fare <- ifelse(is.na(df$Fare),
                  ave(df$Fare, df$Pclass, FUN = function(x) median(x, na.rm = TRUE)),
                  df$Fare)

# 4) Cabin (77% missing): keep only the deck letter and label the rest "Unknown"
df$Deck[is.na(df$Deck)] <- "Unknown"
df$Deck <- factor(df$Deck)
cat("\nDeck distribution:\n"); print(table(df$Deck))

cat("\nMissing values after imputation (excluding raw Cabin):\n")
print(colSums(is.na(df[, setdiff(names(df), "Cabin")])))

# ---- impute_plot ----
age_cmp <- rbind(
  data.frame(Age = age_before[!is.na(age_before)], Stage = "Before (714 observed)"),
  data.frame(Age = df$Age,                          Stage = "After imputation (891)"))
p_imp <- ggplot(age_cmp, aes(Age, fill = Stage)) +
  geom_density(alpha = 0.5, colour = NA) +
  scale_fill_manual(values = c("#D1495B", "#2E86AB")) +
  labs(title = "Age distribution before vs after imputation", y = "Density") +
  theme(legend.position = "bottom")
ggsave("figures/fig03_age_imputation.png", p_imp, width = 7, height = 3.6, dpi = 150)
cat("Mean Age before:", round(mean(age_before, na.rm = TRUE), 2),
    "| after:", round(mean(df$Age), 2), "\n")
cat("Median Age before:", median(age_before, na.rm = TRUE),
    "| after:", median(df$Age), "\n")
cat("SD Age before:", round(sd(age_before, na.rm = TRUE), 2),
    "| after:", round(sd(df$Age), 2), "\n")

# ---- outlier_iqr ----
iqr_bounds <- function(x) {
  q <- quantile(x, c(.25, .75)); i <- diff(q)
  c(lower = unname(q[1] - 1.5 * i), upper = unname(q[2] + 1.5 * i))
}
num_vars <- c("Age", "Fare", "SibSp", "Parch", "FamilySize")
outlier_tbl <- do.call(rbind, lapply(num_vars, function(v) {
  b <- iqr_bounds(df[[v]])
  data.frame(Variable = v,
             Lower = round(b["lower"], 2), Upper = round(b["upper"], 2),
             IQR_outliers = sum(df[[v]] < b["lower"] | df[[v]] > b["upper"]),
             Z_gt_3 = sum(abs(scale(df[[v]])) > 3),
             row.names = NULL)
}))
outlier_tbl$Pct_IQR <- round(100 * outlier_tbl$IQR_outliers / nrow(df), 1)
print(outlier_tbl)
cat("\nTop 8 fares:", head(sort(df$Fare, decreasing = TRUE), 8), "\n")

# ---- outlier_plot ----
df_long <- df %>% select(Age, Fare, FamilySize) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")
p_box <- ggplot(df_long, aes(x = "", y = Value)) +
  geom_boxplot(fill = "#9CC5DB", outlier.colour = "#D1495B", outlier.alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Boxplots used for outlier detection (red = outside 1.5 x IQR)",
       x = NULL, y = NULL)
ggsave("figures/fig04_boxplots.png", p_box, width = 8, height = 3.6, dpi = 150)

# ---- outlier_treat ----
skew <- function(x) mean(((x - mean(x)) / sd(x))^3)      # sample skewness

cap_99 <- quantile(df$Fare, 0.99)
cat("99th percentile of Fare (cap):", round(cap_99, 2), "\n")
df$Fare_capped <- pmin(df$Fare, cap_99)      # winsorisation
df$Fare_log    <- log1p(df$Fare)             # log transformation

data.frame(Version  = c("Original Fare", "Capped at 99th pct", "log1p(Fare)"),
           Max      = round(c(max(df$Fare), max(df$Fare_capped), max(df$Fare_log)), 2),
           Skewness = round(c(skew(df$Fare), skew(df$Fare_capped), skew(df$Fare_log)), 3),
           IQR_outliers = c(sum(df$Fare > iqr_bounds(df$Fare)["upper"]),
                            sum(df$Fare_capped > iqr_bounds(df$Fare_capped)["upper"]),
                            sum(df$Fare_log > iqr_bounds(df$Fare_log)["upper"] |
                                df$Fare_log < iqr_bounds(df$Fare_log)["lower"])))

# ---- outlier_treat_plot ----
h <- function(x, title, fill) ggplot(df, aes(x = .data[[x]])) +
  geom_histogram(bins = 30, fill = fill, colour = "white") +
  labs(title = title, x = NULL, y = "Count") + theme(plot.title = element_text(size = 10))
g <- arrangeGrob(h("Fare", "Original Fare", "#D1495B"),
                 h("Fare_capped", "Capped at 99th percentile", "#E8A33D"),
                 h("Fare_log", "log1p(Fare)", "#2E86AB"), ncol = 3)
ggsave("figures/fig05_fare_treatment.png", g, width = 9, height = 3, dpi = 150)

# ---- normalise ----
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
df$Age_mm        <- minmax(df$Age)
df$Fare_mm       <- minmax(df$Fare_log)
df$FamilySize_mm <- minmax(df$FamilySize)
df$Age_z         <- as.numeric(scale(df$Age))
df$Fare_z        <- as.numeric(scale(df$Fare_log))

round(sapply(df[c("Age", "Age_mm", "Age_z", "Fare_log", "Fare_mm", "Fare_z")],
             function(x) c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))), 3)

# ---- normalise_plot ----
h2 <- function(x, title, fill) ggplot(df, aes(x = .data[[x]])) +
  geom_histogram(bins = 25, fill = fill, colour = "white") +
  labs(title = title, x = NULL, y = NULL) + theme(plot.title = element_text(size = 10))
g2 <- arrangeGrob(h2("Age", "Age (raw)", "#D1495B"),
                  h2("Age_mm", "Age (min-max, 0 to 1)", "#E8A33D"),
                  h2("Age_z", "Age (z-score)", "#2E86AB"),
                  h2("Fare_log", "log1p(Fare) (raw)", "#D1495B"),
                  h2("Fare_mm", "log1p(Fare) (min-max)", "#E8A33D"),
                  h2("Fare_z", "log1p(Fare) (z-score)", "#2E86AB"), ncol = 3)
ggsave("figures/fig06_normalisation.png", g2, width = 9, height = 5, dpi = 150)

# ---- encode ----
df$Sex_bin    <- as.integer(df$Sex == "female")     # binary encoding
df$Pclass_ord <- as.integer(df$Pclass)              # ordinal encoding (1 < 2 < 3)

emb <- model.matrix(~ Embarked - 1, df); colnames(emb) <- sub("Embarked", "Emb_",   colnames(emb))
ttl <- model.matrix(~ Title    - 1, df); colnames(ttl) <- sub("Title",    "Title_", colnames(ttl))
dk  <- model.matrix(~ Deck     - 1, df); colnames(dk)  <- sub("Deck",     "Deck_",  colnames(dk))

df_encoded <- data.frame(SurvivedNum = df$SurvivedNum, Pclass_ord = df$Pclass_ord,
                         Sex_bin = df$Sex_bin, Age_z = df$Age_z, Fare_z = df$Fare_z,
                         SibSp = df$SibSp, Parch = df$Parch, FamilySize = df$FamilySize,
                         IsAlone = df$IsAlone, HasCabin = df$HasCabin,
                         Age_missing = df$Age_missing,
                         as.data.frame(emb), as.data.frame(ttl), as.data.frame(dk))
cat("Encoded data frame:", nrow(df_encoded), "rows x", ncol(df_encoded), "columns\n")
cat("Any NA left?", anyNA(df_encoded), "\n\n")
print(names(df_encoded))
cat("\n")
head(df_encoded[, c("SurvivedNum", "Sex_bin", "Pclass_ord", "Emb_Cherbourg", "Emb_Queenstown",
                    "Emb_Southampton", "Title_Mr", "Title_Mrs", "Title_Miss")], 6)

# ---- save ----
df_clean <- df[, !(names(df) %in% c("Cabin", "Ticket"))]
write.csv(df_clean,   "output/titanic_clean.csv",   row.names = FALSE)
write.csv(df_encoded, "output/titanic_encoded.csv", row.names = FALSE)
cat("Saved: output/titanic_clean.csv   (", nrow(df_clean),   "x", ncol(df_clean),   ")\n")
cat("Saved: output/titanic_encoded.csv (", nrow(df_encoded), "x", ncol(df_encoded), ")\n")

# ---- eda_str ----
str(df_clean[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare",
                 "Embarked", "Title", "FamilySize", "IsAlone", "HasCabin", "Deck")])

# ---- eda_summary ----
summary(df_clean[, c("Survived", "Pclass", "Sex", "Age", "Fare", "FamilySize",
                     "Embarked", "Title")])

# ---- eda_desc ----
desc_vars <- c("Age", "Fare", "SibSp", "Parch", "FamilySize")
desc <- t(sapply(df[desc_vars], function(x)
  c(Mean = mean(x), Median = median(x), SD = sd(x), Min = min(x),
    Q1 = unname(quantile(x, .25)), Q3 = unname(quantile(x, .75)),
    Max = max(x), IQR = IQR(x), Skew = skew(x))))
round(desc, 2)

# ---- eda_freq ----
for (v in c("Survived", "Sex", "Pclass", "Embarked", "Title")) {
  tb <- table(df[[v]])
  cat("\n", v, "\n", sep = "")
  print(cbind(Count = tb, Percent = round(100 * prop.table(tb), 1)))
}

# ---- eda_group ----
df %>% group_by(Survived) %>%
  summarise(n = n(), mean_age = round(mean(Age), 1), median_age = median(Age),
            mean_fare = round(mean(Fare), 1), median_fare = round(median(Fare), 1),
            mean_family = round(mean(FamilySize), 2))
df %>% group_by(Pclass) %>%
  summarise(n = n(), mean_age = round(mean(Age), 1),
            median_fare = round(median(Fare), 1), pct_with_cabin = round(100 * mean(HasCabin), 1))

# ---- eda_univar ----
p1 <- ggplot(df, aes(Age)) + geom_histogram(bins = 30, fill = "#2E86AB", colour = "white") +
  labs(title = "Age distribution", y = "Passengers")
p2 <- ggplot(df, aes(Fare_log)) + geom_histogram(bins = 30, fill = "#E8A33D", colour = "white") +
  labs(title = "Fare distribution (log1p)", x = "log1p(Fare)", y = "Passengers")
p3 <- ggplot(df, aes(Pclass, fill = Pclass)) + geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("#2E86AB", "#9CC5DB", "#D1495B")) +
  labs(title = "Passengers by class", x = NULL, y = "Passengers")
p4 <- ggplot(df, aes(Sex, fill = Sex)) + geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("#D1495B", "#2E86AB")) +
  labs(title = "Passengers by sex", x = NULL, y = "Passengers")
ggsave("figures/fig07_univariate.png", arrangeGrob(p1, p2, p3, p4, ncol = 2),
       width = 9, height = 6, dpi = 150)

# ---- eda_surv_rates ----
surv_rate <- function(v) df %>% group_by(.data[[v]]) %>%
  summarise(n = n(), survived = sum(SurvivedNum), survival_pct = round(100 * mean(SurvivedNum), 1))
for (v in c("Sex", "Pclass", "Embarked", "Title")) { print(surv_rate(v)); cat("\n") }
cat("Survival % by Sex x Pclass:\n")
round(100 * tapply(df$SurvivedNum, list(Sex = df$Sex, Pclass = df$Pclass), mean), 1)

# ---- eda_surv_plot ----
rate_plot <- function(v, title) {
  d <- surv_rate(v)
  ggplot(d, aes(x = .data[[v]], y = survival_pct)) +
    geom_col(fill = "#2E86AB", width = 0.65) +
    geom_text(aes(label = paste0(survival_pct, "%")), vjust = -0.4, size = 3.3) +
    geom_hline(yintercept = 100 * mean(df$SurvivedNum), linetype = "dashed", colour = "#D1495B") +
    ylim(0, 105) + labs(title = title, x = NULL, y = "Survival rate (%)") +
    theme(plot.title = element_text(size = 11))
}
ggsave("figures/fig08_survival_rates.png",
       arrangeGrob(rate_plot("Sex", "Survival by sex"), rate_plot("Pclass", "Survival by class"),
                   rate_plot("Embarked", "Survival by port"), rate_plot("Title", "Survival by title"),
                   ncol = 2), width = 9, height = 6.5, dpi = 150)

# ---- eda_bivar ----
p_a <- ggplot(df, aes(Survived, Age, fill = Survived)) + geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = pal) + labs(title = "Age by survival", x = NULL)
p_b <- ggplot(df, aes(Age, fill = Survived)) + geom_density(alpha = 0.5, colour = NA) +
  scale_fill_manual(values = pal) + labs(title = "Age density by survival", y = "Density") +
  theme(legend.position = "bottom")
p_c <- ggplot(df, aes(Pclass, Fare, fill = Survived)) + geom_boxplot() + scale_y_log10() +
  scale_fill_manual(values = pal) + labs(title = "Fare (log scale) by class", x = NULL) +
  theme(legend.position = "bottom")
fs <- df %>% group_by(FamilySize) %>%
  summarise(rate = 100 * mean(SurvivedNum), n = n())
p_d <- ggplot(fs, aes(FamilySize, rate)) + geom_line(colour = "#2E86AB") +
  geom_point(aes(size = n), colour = "#D1495B") + scale_x_continuous(breaks = 1:11) +
  labs(title = "Survival rate by family size", y = "Survival rate (%)", size = "Passengers") +
  theme(legend.position = "bottom")
ggsave("figures/fig09_bivariate.png", arrangeGrob(p_a, p_b, p_c, p_d, ncol = 2),
       width = 9, height = 7, dpi = 150)

# ---- eda_sexclass ----
p_sc <- ggplot(df, aes(Pclass, fill = Survived)) +
  geom_bar(position = "fill") + facet_wrap(~ Sex) +
  scale_fill_manual(values = pal) + scale_y_continuous(labels = percent) +
  labs(title = "Survival proportion by class, split by sex", x = NULL, y = NULL)
ggsave("figures/fig10_sex_class.png", p_sc, width = 8, height = 3.8, dpi = 150)

# ---- eda_corr ----
cor_vars <- df_encoded[, c("SurvivedNum", "Pclass_ord", "Sex_bin", "Age_z", "Fare_z",
                           "SibSp", "Parch", "FamilySize", "IsAlone", "HasCabin")]
cor_mat <- cor(cor_vars)
round(cor_mat, 2)
cat("\nCorrelation with Survival (sorted):\n")
sort(round(cor_mat[, "SurvivedNum"], 3), decreasing = TRUE)[-1]

# ---- eda_corr_plot ----
png("figures/fig11_correlation.png", width = 1100, height = 1000, res = 150)
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black",
         number.cex = 0.65, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#D1495B", "white", "#2E86AB"))(200))
invisible(dev.off())

# ---- eda_tests ----
ct_sex <- chisq.test(table(df$Sex, df$Survived))
ct_cls <- chisq.test(table(df$Pclass, df$Survived))
ct_emb <- chisq.test(table(df$Embarked, df$Survived))
wt_age <- wilcox.test(Age ~ Survived, data = df)
tt_far <- t.test(Fare_log ~ Survived, data = df)
cramer <- function(ct) sqrt(unname(ct$statistic) / sum(ct$observed))
tests <- data.frame(
  Test = c("Chi-square: Sex vs Survived", "Chi-square: Pclass vs Survived",
           "Chi-square: Embarked vs Survived", "Wilcoxon: Age by Survived",
           "Welch t-test: log Fare by Survived"),
  Statistic = round(c(ct_sex$statistic, ct_cls$statistic, ct_emb$statistic,
                      wt_age$statistic, tt_far$statistic), 2),
  P_value = format.pval(c(ct_sex$p.value, ct_cls$p.value, ct_emb$p.value,
                          wt_age$p.value, tt_far$p.value), digits = 3, eps = 1e-16),
  Effect = c(paste("Cramer's V =", round(cramer(ct_sex), 2)),
             paste("Cramer's V =", round(cramer(ct_cls), 2)),
             paste("Cramer's V =", round(cramer(ct_emb), 2)), "-", "-"))
print(tests, row.names = FALSE)

# ---- eda_model ----
model_df <- df
model_df$Pclass <- factor(as.character(model_df$Pclass), levels = c("1st", "2nd", "3rd"))
model_df$Sex    <- relevel(model_df$Sex, ref = "male")
fit <- glm(SurvivedNum ~ Pclass + Sex + Age + FamilySize + Fare_log + Embarked,
           data = model_df, family = binomial)
ci  <- exp(confint.default(fit))
or_tbl <- data.frame(Odds_Ratio = round(exp(coef(fit)), 3),
                     CI_low = round(ci[, 1], 3), CI_high = round(ci[, 2], 3),
                     P_value = format.pval(summary(fit)$coefficients[, 4], digits = 3, eps = 1e-16))
print(or_tbl)
cat("\nNull deviance:", round(fit$null.deviance, 1), " Residual deviance:", round(fit$deviance, 1),
    " AIC:", round(AIC(fit), 1), "\n")
pred <- ifelse(predict(fit, type = "response") > 0.5, 1, 0)
cat("In-sample accuracy:", round(mean(pred == model_df$SurvivedNum), 3), "\n")
print(table(Predicted = pred, Actual = model_df$SurvivedNum))

# ---- before_after ----
data.frame(Aspect = c("Rows", "Columns", "Variables with missing values",
                      "Missing cells (excl. raw Cabin column)", "Columns added or derived",
                      "Variables stored as factors", "Fare skewness (raw vs log1p)"),
           Before = as.character(c(nrow(raw), ncol(raw), sum(colSums(is.na(raw)) > 0),
                      sum(is.na(raw[, setdiff(names(raw), "Cabin")])), 0, 0,
                      round(skew(raw$Fare), 2))),
           After  = as.character(c(nrow(df_clean), ncol(df_clean),
                      sum(colSums(is.na(df_clean)) > 0), sum(is.na(df_clean)),
                      ncol(df_clean) - (ncol(raw) - 2),
                      sum(sapply(df_clean, is.factor)), round(skew(df$Fare_log), 2))))
