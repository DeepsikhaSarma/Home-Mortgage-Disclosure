rm(list=ls())


library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(DT)
library(export)
library(data.table)
library(h2o)
library(randomForest)
library(caret)


fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

homeMortgage = read_csv("ny_hmda_2015.csv")

homeMortgageStatus = homeMortgage %>% group_by(action_taken_name) %>%
  summarise(CountOfActionTaken = n()) %>%
  mutate(PercentageActionTaken = CountOfActionTaken /sum(CountOfActionTaken) * 100) %>%
  arrange(desc(PercentageActionTaken))

ggplot(homeMortgageStatus, aes(x = reorder(action_taken_name, PercentageActionTaken), 
                               y = PercentageActionTaken)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = action_taken_name, y = 1, label = paste0("( ",round(PercentageActionTaken,2),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'action_taken_name', y = '%age Count Of Action Taken', title = 'Actions in Loans') +
  coord_flip() + 
  theme_bw()
rm(homeMortgageStatus)

homeMortgageStatus_ethnicity = homeMortgage %>% group_by(action_taken_name,applicant_ethnicity_name) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))

homeMortgage_ethnicity = homeMortgage %>% group_by(applicant_ethnicity_name) %>%
  summarise(CountOfEthnicity = n()) %>%
  arrange(desc(CountOfEthnicity))

ggplot(homeMortgage_ethnicity, aes(x = reorder(applicant_ethnicity_name, CountOfEthnicity), 
                                   y = CountOfEthnicity)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = applicant_ethnicity_name, y = 1, label = paste0("(",round(CountOfEthnicity),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'applicant_ethnicity_name', y = '%age Count Of Action Taken', title = 'Actions in Loans') +
  coord_flip() + 
  theme_bw()


homeMortgageStatus_ethnicity2 = inner_join(homeMortgageStatus_ethnicity,homeMortgage_ethnicity) %>%
  mutate(percentage = (CountOfActionTaken/CountOfEthnicity) * 100 ) 


ggplot(homeMortgageStatus_ethnicity2, aes(x = reorder(action_taken_name, percentage), 
                                          y = percentage)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  facet_wrap(~ applicant_ethnicity_name) +
  geom_text(aes(x = action_taken_name, y = 1, label = paste0("(",round(percentage),"%)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'action_taken_name', y = '%age Count Of Action Taken', title = 'Actions in Loans') +
  coord_flip() + 
  theme_bw()
graph2ppt(file = "one.pptx")

homeMortgageStatus_applicant_race1 = homeMortgage %>% group_by(action_taken_name,applicant_race_name_1) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))

homeMortgage_applicant_race1 = homeMortgage %>% group_by(applicant_race_name_1) %>%
  summarise(CountOfRace1 = n()) %>%
  arrange(desc(CountOfRace1))

ggplot(homeMortgage_applicant_race1, aes(x = reorder(applicant_race_name_1, CountOfRace1), 
                                         y = CountOfRace1)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = applicant_race_name_1, y = 1, label = paste0("(",round(CountOfRace1),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Race Name', y = 'Count Of Action Taken', title = 'Actions in Loans by Race') +
  coord_flip() + 
  theme_bw()


homeMortgageStatus_race = inner_join(homeMortgageStatus_applicant_race1,homeMortgage_applicant_race1) %>%
  mutate(percentage = (CountOfActionTaken/CountOfRace1) * 100 ) 


ggplot(homeMortgageStatus_race, aes(x = reorder(action_taken_name, percentage), 
                                    y = percentage)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  facet_wrap(~ applicant_race_name_1) +
  geom_text(aes(x = action_taken_name, y = 1, label = paste0("(",round(percentage),"%)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Action', y = '%age Count Of Action Taken', title = 'Actions in Loans by Race') +
  coord_flip() + 
  theme_bw()

graph2ppt(file = "two.pptx")


actionStatus = "Loan originated"
breaks = seq(0,400,50)

homeMortgage %>%
  filter(action_taken_name == actionStatus ) %>%
  ggplot(aes(applicant_income_000s)) +
  scale_x_continuous(limits = c(0, 400),breaks=breaks ) + 
  geom_histogram(binwidth = 10,fill = c("Orange")) +
  labs(x = 'Income in Thousands', y = 'Count', title = 'Loan Originated Applicant Income distribution') +  theme_bw()

graph2ppt(file = "three.pptx")



homeMortgage %>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(loan_purpose_name = reorder(loan_purpose_name, percentage)) %>%
  
  ggplot(aes(x = loan_purpose_name,y = percentage)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = loan_purpose_name, y = 1, label = paste0("( ",round(percentage),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types') +
  coord_flip() + 
  theme_bw()

graph2ppt(file = "four.pptx")

homeMortgage %>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name,action_taken_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  
  ggplot(aes(x = loan_purpose_name,y = CountLoanPurpose,fill =(action_taken_name))) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types Distribution with Action Types') +
  theme_bw()
graph2ppt(file = "five.pptx")
selectedCols = c("action_taken","applicant_ethnicity",        
                 "applicant_income_000s","applicant_race_1","co_applicant_ethnicity",
                 "co_applicant_sex", "county_name","hoepa_status","lien_status",
                 "loan_purpose","loan_type","msamd",                                              
                 "owner_occupancy","preapproval",
                 "property_type","purchaser_type","loan_amount_000s")

homeMortgage_selectedCols = homeMortgage %>% select(selectedCols) %>%
  mutate(isLoanOriginated = FALSE)  %>%
  mutate(isLoanOriginated = replace(isLoanOriginated, action_taken == 1, TRUE)) %>%
  select(-action_taken)


homeMortgage %>%
  filter(!is.na(county_name)) %>%
  group_by(county_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(county_name = reorder(county_name, percentage)) %>%
  arrange(desc(percentage)) %>%
  head(10) %>%
  
  ggplot(aes(x = county_name,y = percentage)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = county_name, y = 1, label = paste0("( ",round(percentage,2),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'County Name', y = 'Count', title = 'County and Loans') +
  coord_flip() + 
  theme_bw()

df<-homeMortgage_selectedCols

## Modelling Code 
# Grouping the Countries 
county <-c("Suffolk County","Queens County","Nassau County","Kings County","Westchester County",
       "Erie County","Monroe County","New York County","Onondaga County","Bronx County",
       "Richmond County", "Orange County","Albany County","Saratoga County","Rockland County",
       "Dutchess County","Oneida County","Niagara County","Schenectady County","Rensselaer County",
       "Ulster County","Broome County")                                                                                                                              
                                                                        
df$Countygroup <- with(df, ifelse(county_name %in% county, df$county_name,"Others"))
df <- select(df, -county_name)
names(df)
# % Good & Bad.
prop.table(table(df$isLoanOriginated))
cols = c("applicant_ethnicity","Countygroup",
         "applicant_race_1","co_applicant_ethnicity",
         "co_applicant_sex", "hoepa_status","lien_status",
         "loan_purpose","loan_type","msamd",                                              
         "owner_occupancy","preapproval",
         "property_type","purchaser_type","isLoanOriginated")
setDT(df)[, (cols):= lapply(.SD, factor), .SDcols=cols]
str(df)
df[complete.cases(df), ]
rm(list=setdiff(ls(), "df"))


# Writing function to order columns 
df<- as.data.frame(df)
shuffle_columns <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first",
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

df <- df[shuffle_columns(names(df), "isLoanOriginated before applicant_ethnicity")]

# Creating a Random forest model to predict Approval and declines

h2o.init(
  nthreads=-1)    
df.hex <- as.h2o(df)

splits <- h2o.splitFrame(
  df.hex,c(0.6,0.2),    
  seed=1234)  
train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex")   
test <- h2o.assign(splits[[3]], "test.hex")    

rf1 <- h2o.randomForest(        
  training_frame = train,        
  validation_frame = valid,      
  x=2:17,                      
  y=1,                         
  model_id = "rf_covType_v1",    
  ntrees = 200,                 
  stopping_rounds = 2,          
  score_each_iteration = T,     
  seed = 1000000)  
summary(rf1) 
rf1@model$validation_metrics
h2o.shutdown(prompt=FALSE)
rm(list=setdiff(ls(), "df"))


set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7,0.3))
train <- df[ind==1,]
test <- df[ind==2,]
rf <- randomForest(isLoanOriginated~., data = train,
                   ntree = 300,
                   mtry = 8,
                   na.action = na.omit,
                   importance = TRUE)
print(rf)
attributes(rf)
rf$confusion
#load library caret
library(caret)

#prediction and confusion matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$isLoanOriginated)

#prediction and confusion matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$isLoanOriginated)

#error rate of random forest
plot(rf)

importance    <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
print("Variable importance after second iteration")
library(ggthemes)
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'Dark Green') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
graph2ppt(file = "six.pptx")
