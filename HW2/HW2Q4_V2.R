### Part A ###
### Loading the Data & identify all teams and sort by name
games <- read.csv("http://www.stanford.edu/~wfithian/games.csv",as.is=TRUE)
teams <- read.csv("http://www.stanford.edu/~wfithian/teams.csv",as.is=TRUE)
all.teams <- sort(unique(c(teams$team,games$home,games$away)))

### Assign score win or loss to vector z. This will be used a response in the model.
z <- with(games, ifelse(homeScore>awayScore,1,0))

### Initialise a data frame of the games(rows) and teams (columns), 
### indicating for each game which teams were home or away
X0 <- as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0) <- all.teams

### Assign to the data frame for each game which team was home or away
for(tm in all.teams) {
  X0[[tm]] <- 1*(games$home==tm) - 1*(games$away==tm)
}

### Remove stanford's column to make it the baseline team against which other teams will be compared
X <- X0[,names(X0) != "stanford-cardinal"]
reg.season.games <- which(games$gameType=="REG")

### Assign a homeadvantage coefficient which is common to all games
homeAdv <- 1 - games$neutralLocation

### Perform a the logistic regression
logrega.mod <- glm(z ~ 0 + homeAdv + ., data=X, family=binomial, subset=reg.season.games)

### Idenfity the top 25 teams
margin.top25 <- order(coef(summary(logrega.mod))[,1],decreasing=TRUE)[1:25]
coef(summary(logrega.mod))[margin.top25,1]

### Draw up a table of the two top teams, how many games they were in as home and away
### It can be seen that both team only played 1 away game, where they won.
table(X[,c("saint-mary-saint-mary","st.-thomas-(tx)-celts")])

### Look at the performance in those games, both teams won (Sevvandi's code)
bool.condition.1=games$home=="saint-mary-saint-mary"|games$away=="saint-mary-saint-mary"
games[bool.condition.1,]

bool.condition.2=games$home=="st.-thomas-(tx)-celts"|games$away=="st.-thomas-(tx)-celts"
games[bool.condition.2,]


### Part B ###
### Add to table "games" column of the minimum number of games played by either team 
### Excluding teams with less than 5 games we have two choices. Exclude the teams (columns), but leave their games.
### Or exclude the games they played in. I have chosen the latter for two reasons. 1) Standofrd was our baseline
### by removing the team columns; this baseline will no longer be valid if many other team columns are excluded.
### 2) If low game teams are excluded, hoever their games are left in - the opposition's result (win or lose)
### will still be available as a predictor; however the strength of the low playing team they played will be lost. 
### In the team ranging using logistic regression, there are minor differences seen in either approach once 
### the logistic regression ranking is made.

teamplays <- table(c(games$home, games$away))
games$mingames[reg.season.games] <- vapply(reg.season.games, function(i) {
  min(
    teamplays[names(teamplays)==games$away[i]],
    teamplays[names(teamplays)==games$home[i]]
  ) 
}, 1)  

### Create subset index on both regular season games and minimum games
subset.vector <- which(games$gameType=="REG" & games$mingames >= 5)


### Perform the logistic regression excluding teams with 5 games or less and non regular season games
logregb.mod <- glm(z ~ 0 + homeAdv + ., data=X, family=binomial, subset=subset.vector)
logregb.coef <- coef(logregb.mod)[paste("`",teams$team,"`",sep="")]
names(logregb.coef) <- teams$team

### Calculate the Linear Model from class, change the variable names
y <- with(games, homeScore-awayScore)
homeAdvlm <- 1 - games$neutralLocation
lmb.mod <- lm(y ~ 0 + homeAdvlm + ., data=X, subset=subset.vector)
lmb.coef <- coef(lmb.mod)[paste("`",teams$team,"`",sep="")]
names(lmb.coef) <- teams$team

### Create the ranking table
rank.table <- cbind(
#                   "LinReg Model Score" = lmb.coef,
                    "LinReg Model Rank"  = rank(-lmb.coef,ties="min"),
#                   "LogReg Model Score" = logregb.coef,
                    "LogReg Model Rank"  = rank(-logregb.coef,ties="min"),
                    "AP Rank"     = teams$apRank,
                    "USAT Rank"   = teams$usaTodayRank)
rank.table[order(logregb.coef,decreasing=TRUE)[1:25],]


### Part C ###

par(mfrow=c(1,2))
plot(coef(summary(lmb.mod))[-1,1], coef(summary(lmb.mod))[-1,4], xlab="Slope Estimate (Linear Model)", ylab= "p-value of performance", col="blue")
abline(h = .05, col = "red")
abline(v = 0, lty=2)
text(-30,.1, "p-value = 0.05", col = "red")
plot(coef(summary(logregb.mod))[-1,1], coef(summary(logregb.mod))[-1,4],xlab="Slope Estimate (Logistic Reg.)", ylab= "p-value of performance", col="blue")
abline(h = .05, col = "red")
abline(v = 0, lty=2)


### Find the number of p-values greater or less than .05
linmod_pvalue <- (ifelse(coef(summary(lmb.mod))[,4]<.05,"Y", "N"))
logreg_pvalue <- (ifelse(coef(summary(logregb.mod))[,4]<.05,"Y", "N"))

### For logistic regression, we do not pick up score differentials in the mode.
### Therefore, we would have less confidence in the individual game wins.
### Linear model uses score differentials so gives more confidence of wins.
table(linmod_pvalue)/length(linmod_pvalue)
table(logreg_pvalue)/length(logreg_pvalue)  

### Part D
### Use ten-fold cross-validation to estimate the test error rate for these predictions,
### Make a 2*2 contingency table of four possible outcomes: both models are right in their prediction, both are wrong, 
### only logistic regression is right, or only linear regression is right. 

### Define a matrix to hold predicted coeffients per team.
set.seed(1)
coefficients = matrix(,nrow=nrow(games), ncol=4) 

### Create an index vector to identify train & test sets within the games of interest only 
### ie. For consistency with previous questions, include only regular season games, and teams which played over 5 games
index <- as.numeric(rownames(X[subset.vector,]))

### Break the games into ten folds. Use the caret library to make the split
### For numeric y, the sample is split into groups sections based on percentiles 
### and sampling is done within these subgroups. For smaller samples sizes, these two functions 
### may not do stratified splitting and, at most, will split the data into quartiles. So in our case,
### there will be a proportional split of teams into train and test (we will not have all home games of 
### one team grouped in one test fold).
library(caret)
folds <- createFolds(y=games$home[index], k=10, list=TRUE, returnTrain=TRUE)

### loop through each fold to model with train and store the results for test
for(i in 1:10){
  
### For ease of use, store the test and train indices for each loop
        indtest  = index[-folds[[i]]]
        indtrain = index[folds[[i]]]
  
### Using the train fold only, fit log_reg and lin_mod
        CVlog.mod <- glm(z ~ 0 + homeAdv + ., data=X, family=binomial, subset=indtrain)
        CVlm.mod  <- lm(y ~ 0 + homeAdvlm + ., data=X, subset=indtrain)

### Assign the predicted coefficients per team to the test fold
        coefficients[indtest,1] <- coef(CVlog.mod)[paste("`",games$home[indtest],"`",sep="")]
        coefficients[indtest,2] <- coef(CVlog.mod)[paste("`",games$away[indtest],"`",sep="")]
        coefficients[indtest,3] <- coef(CVlm.mod)[paste("`",games$home[indtest],"`",sep="")]
        coefficients[indtest,4] <- coef(CVlm.mod)[paste("`",games$away[indtest],"`",sep="")]
        }

### create vectors to hold success of the logistic regression and linear model
winnerlog <- rep(NA, nrow(games))
winnerlm  <- rep(NA, nrow(games))

winnerlog[(games$homeScore>games$awayScore) == (coefficients[,1]>coefficients[,2])] <- "logistic right"
winnerlog[(games$homeScore>games$awayScore) != (coefficients[,1]>coefficients[,2])] <- "logistic wrong"
winnerlm[(games$homeScore>games$awayScore) == (coefficients[,3]>coefficients[,4])] <- "linear right"
winnerlm[(games$homeScore>games$awayScore) != (coefficients[,3]>coefficients[,4])] <- "linear wrong"

### Create contingency table of actual result, linear model, logistic regression
table(winnerlm,winnerlog)

### Check if all the indexed games of interest (5 or more plays and regular season) are in the contingency table. 
length(index) - sum(table(winnerlm,winnerlog))

### Part E
### Use the normal approximation to carry out a test of the hypothesis that both models are equally good at 
### predicting games. What is the conclusion of your test?
### For the Hypothesis test we use a 95% confidence interval, mean ± 2 · SE, where D = sum of test games in which
### the two models disagreed, mean = D/2 and SE = Sqrt(D)/2

### this function will take the contingency table as input and calculate the confidence interval for the
### errors given of D value.
hypTest <- function(contingency){
  D = contingency[1,2] + contingency[2,1]
  
  lower_int = D/2 - 1.96*(sqrt(D)/2)
  upper_int = D/2 + 1.96*(sqrt(D)/2)
  cat('The confidence interval of D :', c(lower_int, upper_int))
  
}

hypTest(table(winnerlm,winnerlog))

### We next perform an Exact McNemar test (given the large samples), using R package 'exact2x2', which calculates
### the exact McNemar's test with appropriate matching confidence intervals. A p-value of .05534 given the contingency table.
### With this, there is insufficient evidence to reject the zero hypothesis, that both models are qually good 
### at predicting games. 
  
library(exact2x2)
mcnemar.exact(as.matrix(table(winnerlm,winnerlog)),y=NULL, conf.level=.95)
