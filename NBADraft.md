How the NBA Draft Impacts the Length of NBA Careers
========================================================
author: Kenneth (Hsuan An) Chen
date: February 13, 2022
autosize: true

Introduction: The NBA Draft
========================================================

![NBA Draft 2021](./Images/nbadraft.jpeg)

***
- Introduced in 1947
- A way to induct new players into the NBA
- Since 1989, the draft consists of two round (30 picks each)
- Each team takes turns picking a player to join their team
- Each player may only enter the draft once
- The earlier the pick, (usually) the better the player

The NBA Combine
========================================================

![NBA Combine](./Images/nbacombine.jpeg)

***
- Not sure when it was introduced, but data starts in 2001
- Before the draft, prospective players are invited to the combine
- Invitations are determined by votes from each NBA team
- Combine consists of various physical tests and skill tests
- Allows for prospects to showcase their abilities prior to the draft

The Causal DAG
========================================================

![Causal DAG](./Images/CausalDAG.jpg)

***
- Covariates: the results from the combine tests
- Treatment: NBA Draft round (1 - first round, 0 - second round or undrafted)
- Outcome: Number of years played in the NBA
- It should be noted that hidden bias is very likely here

Research Question
========================================================

Do the players that are drafted in the first round play longer in the NBA?

![Meme](./Images/meme.jpeg)

The Data: Kaggle
========================================================

![Raiden](./Images/raiden.png)

***
- Downloaded from Kaggle
- Collection of data sets wrangled from stats.nba.com
- sqlite file
- Data sets used: Player_Attributes and DRAFT_COMBINE
- Player_Attributes: country, school, weight, seasons played, draft round/year, pts, etc
- DRAFT_COMBINE: test results from the draft combine


Draft Combine Data Set
========================================================




```r
dim(combine)
```

```
[1] 1395  116
```

```r
combine[1:5, 4:8]
```

```
   nameLast       namePlayer slugPosition heightWOShoesInches heightWOShoes
1     Allen      Malik Allen         PF-C               80.25     6' 8.25''
2 Arceneaux Harold Arceneaux        SG-SF               76.50      6' 4.5''
3    Barnes    Lamont Barnes         PF-C               80.50      6' 8.5''
4     Bland      Mario Bland           PF               77.50      6' 5.5''
5    Brezec    Primoz Brezec            C               84.75     7' 0.75''
```

Player Attributes Data Set
========================================================


```r
dim(player_attributes)
```

```
[1] 4500   37
```

```r
player_attributes[1:5, desired_cols]
```

```
  FIRST_NAME    LAST_NAME  DISPLAY_FIRST_LAST SEASON_EXP DRAFT_ROUND
1       Alaa    Abdelnaby      Alaa Abdelnaby          4           1
2       Zaid   Abdul-Aziz     Zaid Abdul-Aziz          9           1
3     Kareem Abdul-Jabbar Kareem Abdul-Jabbar         19           1
4    Mahmoud   Abdul-Rauf  Mahmoud Abdul-Rauf          8           1
5      Tariq  Abdul-Wahad   Tariq Abdul-Wahad          6           1
```

Pre-joining Analysis (Part 1)
========================================================


```r
# Convert start year to numeric and find number of players that started after 2000
from_yrs <- as.numeric(player_attributes$FROM_YEAR)
sum(from_yrs > 2000)
```

```
[1] 1517
```

```r
# Check for missing values in treatment and outcome variables
!sum(is.na(player_attributes$SEASON_EXP)) & !sum(is.na(player_attributes$DRAFT_ROUND))
```

```
[1] TRUE
```

Pre-joining Analysis (Part 2)
========================================================


```r
# Keep columns of interest, and players that started after 2000
temp <- player_attributes[from_yrs > 2000, c("SEASON_EXP", "DRAFT_ROUND")]
temp$namePlayer <- player_attributes$DISPLAY_FIRST_LAST[from_yrs > 2000]
dim(temp)
```

```
[1] 1517    3
```

```r
sum(combine$namePlayer %in% temp$namePlayer)
```

```
[1] 849
```

```r
sum(temp$namePlayer %in% combine$namePlayer)
```

```
[1] 838
```

Pre-joining Analysis (Summary)
========================================================

- 1517 players started playing after 2000 (1395 players in combine data)
- No missing values in treatment and outcome variables
- Only keep players that started after 2000 to minimize chances of duplicate names when joining data sets
- Example: Mike Dunleavy Sr. and Mike Dunleavy Jr. (father and son)
- 849 players that participated in the combine played in the NBA
- 838 NBA players participated in the combine

Joining the Data Sets
========================================================

- Players could participate in the combine without ever playing in the NBA
- Players could play in the NBA without ever participating in the combine
- We want players that have participated in the combine AND played in the NBA
- Solution: inner-join
- Result: 852 observations (due to duplicates in both data sets)
- Quality checked by randomly drawing sample from joined data set

Joining the Data Sets (R Code)
========================================================


```r
# Inner-join data frames
joined <- inner_join(combine, temp, by = "namePlayer")
dim(joined)
```

```
[1] 852 118
```

```r
# Check for missing values
sum(is.na(joined$SEASON_EXP))
```

```
[1] 0
```

```r
sum(is.na(joined$DRAFT_ROUND))
```

```
[1] 0
```

Missing Values
========================================================

![plot of chunk unnamed-chunk-7](NBADraft-figure/unnamed-chunk-7-1.png)

```
[1] "Number of variables with over 40% missing: 95"
```

***
- 95 have over 40% of their values missing (out of 118)
- May be due to the fact most tests are not even attempted by the players
- Example: setOffDribTopKeyCollegeMade (> 97.5% missing) 
- Some variables are redundant (i.e. heightWOShoes and heightWOShoesInches)
- Some variables provide no information (i.e. IDs and names)

Missing Values (R Code)
========================================================


```r
# Remove columns with missingness >= 40%
df <- joined[, missing_prop < 0.4]
dim(df)
```

```
[1] 852  23
```

```r
# Remove irrelevant columns
df <- df %>% 
  dplyr::select(-nameFirst, -namePlayer, -nameLast, -idPlayer, -heightWOShoes, -wingspan, -reachStandingO, -heightWShoes)
dim(df)
```

```
[1] 852  15
```

Feature Engineering
========================================================

- Binary encode DRAFT_ROUND (1 = 1st round, 0 = 2nd round or undrafted)
- Ordinal encode slugPosition as new column position
- heightWShoesInches has 16% of its values missing (128 consecutive missing values to start)
- heightWShoesInches highly correlated with heightWOShoesInches (0.9% missing)
- Remove heightWShoesInches along with yearCombine and slugPosition

Feature Engineering (R Code)
========================================================


```r
# Convert draft round column to binary treatment variable (1 = 1st round, 0 = 2nd round/undrafted)
df$DRAFT_ROUND <- ifelse(df$DRAFT_ROUND == "1", 1, 0)
# Ordinal encode positions
position <- as.numeric(as.factor(df$slugPosition))
df$position <- position
# Remove columns from df
df$yearCombine <- NULL
df$slugPosition <- NULL
df$heightWShoesInches <- NULL
dim(df)
```

```
[1] 852  13
```

Impute with KNN
========================================================


```r
# Impute missing values with KNN
imputed <- kNN(
  df, k = 3, numFun = weightedMean, 
  weightDist=TRUE
)
dim(imputed)
```

```
[1] 852  26
```

```r
# Save cleaned data set
clean_df <- imputed[, 1:13]
dim(clean_df)
```

```
[1] 852  13
```

***
- Nearest neighbors tend to have similar physical traits and abilities
- Note that the kNN package outputs an extra logical column for each original variable
- The rows of each extra logical column indicates which observations were imputed for a given variable

TableOne (Prior to Matching - Part 1)
========================================================


```
                                        Stratified by DRAFT_ROUND
                                         0              1              SMD   
  n                                         451            401               
  heightWOShoesInches (mean (SD))         77.16 (3.41)   77.95 (3.32)   0.235
  weightLBS (mean (SD))                  213.85 (25.83) 217.09 (25.25)  0.127
  wingspanInches (mean (SD))              82.17 (4.02)   82.84 (3.93)   0.167
  reachStandingInches (mean (SD))        102.87 (5.06)  104.03 (4.81)   0.235
  verticalLeapStandingInches (mean (SD))  29.28 (3.01)   29.33 (2.86)   0.018
  verticalLeapMaxInches (mean (SD))       34.60 (3.58)   34.57 (3.48)   0.011
```

TableOne (Prior to Matching - Part 2)
========================================================


```
                                         Stratified by DRAFT_ROUND
                                          0            1            SMD   
  n                                         451          401              
  timeLaneAgility (mean (SD))             11.37 (0.57) 11.41 (0.56)  0.063
  timeThreeQuarterCourtSprint (mean (SD))  3.29 (0.13)  3.27 (0.12)  0.138
  repsBenchPress135 (mean (SD))           10.30 (5.01)  9.88 (4.89)  0.086
  pctBodyFat (mean (SD))                   7.24 (2.67)  7.23 (2.50)  0.004
  position (mean (SD))                     7.91 (3.59)  7.58 (3.58)  0.092
```

Propensity Score Matching
========================================================

![Raiden and Ganyu](./Images/raidenganyu.png)

***

- Fit a logistic regression model to obtain propensity scores
- DRAFT_ROUND regressed on covariates
- Paired (nearest neighbor) matching: 401 pairs

Propensity Score Matching (R Code)
========================================================


```r
# Fite propensity score model
psmodel <- glm(DRAFT_ROUND ~ . - SEASON_EXP, data = clean_df, family = binomial())
# Obtain propensity scores
pscores <- psmodel$fitted.values
# Set seed before matching
set.seed(005544529)
# Propensity score, nearest neighbor (aka paired) matching
matched <- Match(Tr = clean_df$DRAFT_ROUND, M = 1, X = pscores, replace = F)
# Obtain matching indices
match_idx <- unlist(matched[c("index.treated", "index.control")])
match_df <- clean_df[match_idx, ]
dim(match_df)
```

```
[1] 802  13
```

TableOne (After Matching - Part 1)
========================================================


```
                                        Stratified by DRAFT_ROUND
                                         0              1              SMD   
  n                                         401            401               
  heightWOShoesInches (mean (SD))         77.50 (3.25)   77.95 (3.32)   0.136
  weightLBS (mean (SD))                  215.15 (25.69) 217.09 (25.25)  0.076
  wingspanInches (mean (SD))              82.48 (3.92)   82.84 (3.93)   0.092
  reachStandingInches (mean (SD))        103.42 (4.84)  104.03 (4.81)   0.126
  verticalLeapStandingInches (mean (SD))  29.26 (3.00)   29.33 (2.86)   0.024
  verticalLeapMaxInches (mean (SD))       34.51 (3.56)   34.57 (3.48)   0.016
```

TableOne (After Matching - Part 2)
========================================================


```
                                         Stratified by DRAFT_ROUND
                                          0            1            SMD   
  n                                         401          401              
  timeLaneAgility (mean (SD))             11.39 (0.58) 11.41 (0.56)  0.030
  timeThreeQuarterCourtSprint (mean (SD))  3.28 (0.13)  3.27 (0.12)  0.085
  repsBenchPress135 (mean (SD))           10.13 (5.10)  9.88 (4.89)  0.050
  pctBodyFat (mean (SD))                   7.25 (2.72)  7.23 (2.50)  0.007
  position (mean (SD))                     7.81 (3.64)  7.58 (3.58)  0.064
```

Overlap Plot (Before Matching)
========================================================

![plot of chunk unnamed-chunk-16](NBADraft-figure/unnamed-chunk-16-1.png)

Overlap Plot (After Matching)
========================================================

![plot of chunk unnamed-chunk-17](NBADraft-figure/unnamed-chunk-17-1.png)

Comparison of Grouped Means
========================================================


```
[1] "Before Matching"
```

```
# A tibble: 2 × 2
  DRAFT_ROUND  mean
        <dbl> <dbl>
1           0  3.33
2           1  6.68
```

```
[1] "After Matching"
```

```
# A tibble: 2 × 2
  DRAFT_ROUND  mean
        <dbl> <dbl>
1           0  3.32
2           1  6.68
```

Paired t-test
========================================================

- $H_0: \mu_1 = \mu_0$ vs $H_a: \mu_1 > \mu_0$ where $\mu_1 \text{ and } \mu_0$ are the true means of the treatment and control group, respectively.


```

	Paired t-test

data:  treated and control
t = 13.235, df = 400, p-value < 2.2e-16
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 2.936297      Inf
sample estimates:
mean of the differences 
               3.354115 
```

Sensitivity Analysis
========================================================


| Gamma|     pvals|
|-----:|---------:|
|   1.0| 0.0199286|
|   1.1| 0.0239263|
|   1.2| 0.0279577|
|   1.3| 0.0327914|
|   1.4| 0.0371167|
|   1.5| 0.0414037|
|   1.6| 0.0456312|
|   1.7| 0.0497840|
|   1.8| 0.0553527|
|   1.9| 0.0596228|
|   2.0| 0.0638116|

***
- General rule: conclusion changes at $\Gamma = 1.1 \implies$ highly sensitive to hidden bias
- General rule: conclusion changes at $\Gamma = 5 \implies$ robust to hidden bias
- We see that conclusions change after $\Gamma > 1.7 \implies$ not very robust to hidden bias

Matching With a Caliper - Part 1
========================================================


```
                                        Stratified by DRAFT_ROUND
                                         0              1              SMD   
  n                                         340            340               
  heightWOShoesInches (mean (SD))         77.83 (3.15)   77.77 (3.37)   0.018
  weightLBS (mean (SD))                  216.59 (25.55) 216.42 (25.71)  0.007
  wingspanInches (mean (SD))              82.89 (3.78)   82.73 (4.07)   0.043
  reachStandingInches (mean (SD))        103.91 (4.67)  103.85 (4.90)   0.013
  verticalLeapStandingInches (mean (SD))  29.34 (2.98)   29.31 (2.84)   0.011
  verticalLeapMaxInches (mean (SD))       34.54 (3.56)   34.45 (3.47)   0.026
```

Matching With a Caliper - Part 2
========================================================


```
                                         Stratified by DRAFT_ROUND
                                          0            1            SMD   
  n                                         340          340              
  timeLaneAgility (mean (SD))             11.41 (0.59) 11.40 (0.56)  0.021
  timeThreeQuarterCourtSprint (mean (SD))  3.28 (0.12)  3.28 (0.13)  0.003
  repsBenchPress135 (mean (SD))           10.03 (5.17) 10.09 (4.96)  0.010
  pctBodyFat (mean (SD))                   7.29 (2.75)  7.28 (2.52)  0.003
  position (mean (SD))                     7.66 (3.68)  7.65 (3.56)  0.002
```

- We see a much more balanced distribution with a caliper
- $\text{Caliper} = 0.2 \times \text{sd(logit(propensity scores))}$

Overlap Plot (Before Caliper)
========================================================

![plot of chunk unnamed-chunk-23](NBADraft-figure/unnamed-chunk-23-1.png)

Overlap Plot (After Caliper)
========================================================

![plot of chunk unnamed-chunk-24](NBADraft-figure/unnamed-chunk-24-1.png)

Comparison of Grouped Means
========================================================


```
[1] "Before Matching (No Caliper)"
```

```
# A tibble: 2 × 2
  DRAFT_ROUND  mean
        <dbl> <dbl>
1           0  3.33
2           1  6.68
```

```
[1] "After Matching (Caliper)"
```

```
# A tibble: 2 × 2
  DRAFT_ROUND  mean
        <dbl> <dbl>
1           0  3.33
2           1  7.29
```

Paired t-test
========================================================


```

	Paired t-test

data:  treated and control
t = 14.302, df = 339, p-value < 2.2e-16
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 3.497065      Inf
sample estimates:
mean of the differences 
               3.952941 
```

Sensitivity Analysis
========================================================


| Gamma|     pvals|
|-----:|---------:|
|   1.0| 0.0269184|
|   1.1| 0.0319745|
|   1.2| 0.0370406|
|   1.3| 0.0420695|
|   1.4| 0.0470280|
|   1.5| 0.0518930|
|   1.6| 0.0584344|
|   1.7| 0.0647325|
|   1.8| 0.0699198|
|   1.9| 0.0750043|
|   2.0| 0.0799803|

***
- We see that conclusions change after $\Gamma > 1.4 \implies$ even more sensitive to hidden bias
- This is a bias-variance trade-off with a caliper
- By using a caliper, we lower bias but increase the variance

Conclusion
========================================================

![Chubby Raiden](./Images/raidenchubby.png)

***
- We see that, on average, players that are drafted in the first round play longer than players that are not drafted in the first round
- It should be noted that this conclusion is not very robust to hidden bias due to unaccounted confounders
