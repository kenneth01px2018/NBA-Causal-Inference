library(RSQLite)
library(tidyverse)
library(tableone)
library(Matching)
library(ipw) 
library(survey)
library(MatchIt)
library(imputeTS)
library(caret)
library(VIM)
library(laeken)
library(gbm)
library(sensitivityfull)

# Connect to db
con <- dbConnect(drv = RSQLite::SQLite(), dbname = "/Users/kenneth01px2018/Desktop/Stats Courses/Winter 22/STATS 199/Causal Inference/Data/archive/basketball.sqlite")

# List all tables
tables <- dbListTables(con)

# Exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

# Create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

# Label each data frame
names(lDataFrames) <- tables

# Obtain relevant data sets
combine <- lDataFrames$Draft_Combine
dim(combine)
player_attributes <- lDataFrames$Player_Attributes
dim(player_attributes)
# Convert start year to numeric
from_yrs <- as.numeric(player_attributes$FROM_YEAR)
sum(from_yrs >= 2001)

# Check for missing values
sum(is.na(player_attributes$SEASON_EXP))
sum(is.na(player_attributes$DRAFT_ROUND))

# Keep columns of interest, and players that started after 2001
temp <- player_attributes[from_yrs > 2000, c("SEASON_EXP", "DRAFT_ROUND")]
temp$namePlayer <- player_attributes$DISPLAY_FIRST_LAST[from_yrs > 2000]
dim(temp)

# Find number of combine players that were drafted
sum(combine$namePlayer %in% temp$namePlayer)

# Semi-join data frames (some combine players may never play in the NBA)
joined <- inner_join(combine, temp, by = "namePlayer")

# Check for missing values
sum(is.na(joined$SEASON_EXP))
sum(is.na(joined$DRAFT_ROUND))

# Calculate missing value proportions
missing_prop <- sapply(joined, function(x) {sum(is.na(x)) / nrow(joined)})
hist(missing_prop, "Proportion of Missing Values", xlab = "Proportion of Missing Values in a Variable")
# Find number of features with missingness < 40%
sum(missing_prop < 0.4)

# Remove columns with missingness >= 40%
df <- joined[, missing_prop < 0.4]
dim(df)

# Remove irrelevant columns
df <- df %>% 
  dplyr::select(-nameFirst, -namePlayer, -nameLast, -idPlayer, -heightWOShoes, -wingspan, -reachStandingO, -heightWShoes)
dim(df)

# Convert draft round column to binary treatment variable (1 = 1st round, 0 = 2nd round/undrafted)
df$DRAFT_ROUND <- ifelse(df$DRAFT_ROUND == "1", 1, 0)

# Reevaluate missingness
missing_prop <- sapply(df, function(x) {sum(is.na(x)) / nrow(df)})
missing_prop

# Find smd values
xvars <- colnames(df)[-c(1, 14, 15)]
smdTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = df, test = F)
print(smdTable, smd = T)

# Notes for missingness:
# heightWShoesInches (16%) - 128 consecutive missing values to start
statsNA(df$heightWShoesInches)
# pctBodyFat (13.3%) - seems fine, just not too many did the test in the beginning
statsNA(df$pctBodyFat)
# repsBenchPress135 (21.9%) - seems fine
statsNA(df$repsBenchPress135)
# timeThreeQuarterCourtSprint (12.3%) - seems fine
statsNA(df$timeThreeQuarterCourtSprint)
# timeLaneAgility (12.9%) - seems fine
statsNA(df$timeLaneAgility)
# verticalLeapMaxInches (12%) - seems fine
statsNA(df$verticalLeapMaxInches)
# verticalLeapStandingInches (11.7%) - seems fine
statsNA(df$verticalLeapStandingInches)

# Check missingness of repsBenchPress135 by position
count_na <- function(x) {sum(is.na(x))}
df %>%
  group_by(slugPosition) %>%
  summarize(count_na(repsBenchPress135))

# Ordinal encode positions
position <- as.numeric(as.factor(df$slugPosition))
df$position <- position

# Save and remove columns from df
yearCombine <- df$yearCombine
slugPosition <- df$slugPosition
heightWShoesInches <- df$heightWShoesInches
df$yearCombine <- NULL
df$slugPosition <- NULL
df$heightWShoesInches <- NULL

# Reevaluate missingness
missing_prop <- sapply(df, function(x) {sum(is.na(x)) / nrow(df)})
missing_prop

# Impute missing values with KNN
imputed <- kNN(df, k = 3, numFun = weightedMean, weightDist=TRUE)
dim(imputed)

# Check results of imputation for repsBenchPress135
summary(df$repsBenchPress135)
summary(imputed$repsBenchPress135)
par(mfrow = c(1, 2))
hist(df$repsBenchPress135)
hist(imputed$repsBenchPress135)
# Check results of imputation for pctBodyFat
summary(df$pctBodyFat)
summary(imputed$pctBodyFat)
par(mfrow = c(1, 2))
hist(df$pctBodyFat)
hist(imputed$pctBodyFat)
# Check results of imputation for timeThreeQuarterCourtSprint
summary(df$timeThreeQuarterCourtSprint)
summary(imputed$timeThreeQuarterCourtSprint)
par(mfrow = c(1, 2))
hist(df$timeThreeQuarterCourtSprint)
hist(imputed$timeThreeQuarterCourtSprint)
# Check results of imputation for timeLaneAgility
summary(df$timeLaneAgility)
summary(imputed$timeLaneAgility)
par(mfrow = c(1, 2))
hist(df$timeLaneAgility)
hist(imputed$timeLaneAgility)
# Check results of imputation for verticalLeapMaxInches
summary(df$verticalLeapMaxInches)
summary(imputed$verticalLeapMaxInches)
par(mfrow = c(1, 2))
hist(df$verticalLeapMaxInches)
hist(imputed$verticalLeapMaxInches)
# Check results of imputation for verticalLeapStandingInches
summary(df$verticalLeapStandingInches)
summary(imputed$verticalLeapStandingInches)
par(mfrow = c(1, 2))
hist(df$verticalLeapStandingInches)
hist(imputed$verticalLeapStandingInches)

# Save cleaned data set
clean_df <- imputed[, 1:13]
dim(clean_df)

# Find smd values
xvars <- colnames(clean_df)[-c(1, 11, 12)]
smdTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = clean_df, test = F)
print(smdTable, smd = T)

# Baseline pscore model
psmodel <- glm(DRAFT_ROUND ~ . - SEASON_EXP, data = clean_df, family = binomial())
summary(baseline)

# Obtain propensity scores
pscores <- psmodel$fitted.values
pscores

# Set seed before matching
set.seed(005544529)
# Propensity score, nearest neighbor (aka paired) matching
matched <- Match(Tr = clean_df$DRAFT_ROUND, M = 1, X = pscores, replace = F)
# Obtain matching indices
match_idx <- unlist(matched[c("index.treated", "index.control")])
match_df <- clean_df[match_idx, ]
# Find SMD scores for matched data
psTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = match_df, test = F)
print(psTable, smd = T)
# Compare with unmatched data
psTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = clean_df, test = F)
print(psTable, smd = T)

# Plot Overlap
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
par(mfrow = c(1, 2))
hgA <- hist(pscores[clean_df$DRAFT_ROUND == 1], plot = FALSE)
hgB <- hist(pscores[clean_df$DRAFT_ROUND == 0], plot = FALSE)
plot(hgA, col = c1, main = "Overlap Plot (Before Matching)", xlab = "Propensity Scores")
plot(hgB, col = c2, add = TRUE)
lbls <- c("Treatment", "Control")
legend("topleft", inset=.02,
       lbls, fill = c(c1, c2), cex = 1, text.width = strwidth(lbls)[1]*.5)
ps_matched <- pscores[match_idx]
hgA <- hist(ps_matched[match_df$DRAFT_ROUND == 1], plot = FALSE)
hgB <- hist(ps_matched[match_df$DRAFT_ROUND == 0], plot = FALSE)
plot(hgA, col = c1, main = "Overlap Plot (After Matching)", xlab = "Propensity Scores")
plot(hgB, col = c2, add = TRUE)

# Find grouped means of outcome variable (before matching)
clean_df %>%
  group_by(DRAFT_ROUND) %>%
  summarize(mean = mean(SEASON_EXP))
# Find grouped means of outcome variable (after matching)
match_df %>%
  group_by(DRAFT_ROUND) %>%
  summarize(mean = mean(SEASON_EXP))
# Paired t-test
treated <- match_df$SEASON_EXP[match_df$DRAFT_ROUND == 1]
control <- match_df$SEASON_EXP[match_df$DRAFT_ROUND == 0]
t.test(treated, control, paired = T, alternative = "greater")

# Sensitivity Analysis
y <- rbind(clean_df[matched$index.treated, "SEASON_EXP"], clean_df[matched$index.control, "SEASON_EXP"])
treat1 <- rep(TRUE, nrow(y))
gammas <- seq(1, 2, length = 11)
pvals <- numeric(11)
for (i in 1:11) {
  sens_test <- senfm(y, treat1, gamma = gammas[i], alternative="greater")
  pvals[i] <- sens_test$pval
}
data.frame(Gamma = gammas, pvals = pvals)
# Our conclusions change once Gamma > 1.7
senfm(y, treat1, gamma = 1.7, alternative="greater")
senfm(y, treat1, gamma = 1.71, alternative="greater")

# Calculate caliper
logit <- log(pscores / (1 - pscores))
calip <- .2 * sd(logit)

# Set seed before matching
set.seed(005544529)
# Propensity score, nearest neighbor (aka paired) matching with a caliper
matched <- Match(Tr = clean_df$DRAFT_ROUND, M = 1, X = pscores, replace = F, caliper = calip)
# Obtain matching indices
match_idx <- unlist(matched[c("index.treated", "index.control")])
match_df <- clean_df[match_idx, ]
# Find SMD scores for matched data
psTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = match_df, test = F)
print(psTable, smd = T)
# Compare with unmatched data
psTable <- CreateTableOne(vars = xvars, strata = "DRAFT_ROUND", data = clean_df, test = F)
print(psTable, smd = T)

# Plot Overlap
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
par(mfrow = c(1, 2))
hgA <- hist(pscores[clean_df$DRAFT_ROUND == 1], plot = FALSE)
hgB <- hist(pscores[clean_df$DRAFT_ROUND == 0], plot = FALSE)
plot(hgA, col = c1, main = "Overlap Plot (Before Matching - Caliper)", xlab = "Propensity Scores", ylim = c(0, 80))
plot(hgB, col = c2, add = TRUE)
lbls <- c("Treatment", "Control")
legend("topleft", inset=.02,
       lbls, fill = c(c1, c2), cex = 1, text.width = strwidth(lbls)[1]*.5)
ps_matched <- pscores[match_idx]
hgA <- hist(ps_matched[match_df$DRAFT_ROUND == 1], plot = FALSE)
hgB <- hist(ps_matched[match_df$DRAFT_ROUND == 0], plot = FALSE)
plot(hgA, col = c1, main = "Overlap Plot (After Matching - Caliper)", xlab = "Propensity Scores", ylim = c(0, 80))
plot(hgB, col = c2, add = TRUE)
# Find grouped means of outcome variable (before matching)
clean_df %>%
  group_by(DRAFT_ROUND) %>%
  summarize(mean = mean(SEASON_EXP))
# Find grouped means of outcome variable (after matching)
match_df %>%
  group_by(DRAFT_ROUND) %>%
  summarize(mean = mean(SEASON_EXP))
# Paired t-test
treated <- match_df$SEASON_EXP[match_df$DRAFT_ROUND == 1]
control <- match_df$SEASON_EXP[match_df$DRAFT_ROUND == 0]
t.test(treated, control, paired = T, alternative = "greater")

# Sensitivity Analysis
y <- rbind(clean_df[matched$index.treated, "SEASON_EXP"], clean_df[matched$index.control, "SEASON_EXP"])
treat1 <- rep(TRUE, nrow(y))
gammas <- seq(1, 2, length = 11)
pvals <- numeric(11)
for (i in 1:11) {
  sens_test <- senfm(y, treat1, gamma = gammas[i], alternative="greater")
  pvals[i] <- sens_test$pval
}
data.frame(Gamma = gammas, pvals = pvals)
