library(tidyverse)
library(fs)

dirIAPS <- '../../../IAPS/IAPS Data/' #set the directory for IAPS data
dirData <- '../../../IAPS/Subject Data/' #set the directory for subject data
dirImageOrder <- '../../../IAPS/Subject Image Order/' #set the directory for image order
filesIAPS <- dir_ls(dirIAPS) #list the files in the IAPS directory
filesData <- dir_ls(dirData) #list the files in the subject data directory
filesImageOrder <- dir_ls(dirImageOrder) #list the files in the image order directory


#Import the IAPS key data set
keyIAPS <- filesIAPS %>%
  map_df(~read_csv(.x, na = c("", ".", NA),
                   col_types = cols(IAPS = col_character())) %>%
           select(desc:dom1sd) %>%
           drop_na(),
         .id = "form") %>%
  mutate(form = case_when(str_detect(form, "1_IAPS") ~ 1,
                          str_detect(form, "2_IAPS") ~ 2,
                          str_detect(form, "3_IAPS") ~ 3,
                          str_detect(form, "4_IAPS") ~ 4),
         form = as.factor(form))

#Read in subject rating
subjectData <- filesData %>%
  map_df(~read_csv(.x) %>%
           rename(time = Timestamp,
                  subject = `Subject ID:`),
         .id = "form") %>%
  mutate(form = case_when(str_detect(form, "1_SFA") ~ 1,
                          str_detect(form, "2_SFA") ~ 2,
                          str_detect(form, "3_SFA") ~ 3,
                          str_detect(form, "4_SFA") ~ 4),
           form = as.factor(form)) %>%
  #pivot the data so all ratings are in a column "rating" instead of in their own rows.
  pivot_longer(cols = -c(form, time, subject), values_to = "rating") %>%
  select(-name) %>%
  #have a running order for each subject
  group_by(subject) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  #make this subjectID string match that of imageOrder
  mutate(subject = str_replace(subject, "\\-", "\\_")) %>%
  select(-time) %>%
  #arrange by each subject and the form (so one subject goes through all four
  #forms before moving to next subject)
  arrange(subject, form)


  imageOrder <- filesImageOrder %>%
  map_df(~read_csv(.x),
         .id = "form") %>%
  mutate(form = case_when(str_detect(form, "1_pictureID") ~ 1,
                          str_detect(form, "2_pictureID") ~ 2,
                          str_detect(form, "3_pictureID") ~ 3,
                          str_detect(form, "4_pictureID") ~ 4),
         form = as.factor(form)) %>%
  #pivot the data so all image values (IDs) are in a column "value"
  pivot_longer(cols = -c(X1, form),
               names_to = "subject") %>%
  #remvoe subject prefix
  mutate(subject = str_remove(.$subject, "\\d\\_")) %>%
  #arrange by subject
  arrange(subject) %>%
  #remove NAs that appear because of how the data was read in where each form's
  #image ID were basically in a new column for the subject with the prefix
  #1_SFA_..."
  drop_na() %>%
  #have a running order for each subject, and remove the X1 column.
  group_by(subject) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(-X1)

#join the imageOrder and subject data by subject, form, and order.
orderedSubjectData <- full_join(imageOrder, subjectData, by = c("form", "subject", "order")) %>%
  rename(picID = value)

#Label the orderedSubjectData with a column that has the type of question asked
#based on the picID's P or N. Then rename the picID column as IAPS, removing any
#"P" or "N" at the end, so the names are the same as in the keyIAPS dataframe.
labeledOrderedSubjectData <- orderedSubjectData %>%
  mutate(question = case_when(str_detect(picID, "P") ~ "positive",
                              str_detect(picID, "N") ~ "negative",
                              TRUE ~ "Oops"),
         picID = str_remove_all(picID, "P|N")) %>%
  rename(IAPS = picID)

#create positiveRatings as a dataframe that has IAPS data from the rounds where
#it was asked how positive the image made them feel. Then combine the rating and
#question data into one column that has the ratings for when the question was
#about positive feelings. I could've done this using the following code instead.
#Same thing.

#labeledOrderedSubjectData %>%
#  filter(question == "positive") %>%
#  select(-question) %>%
#  rename(positive = rating))

positiveRatings <- labeledOrderedSubjectData %>%
  filter(question == "positive") %>%
  pivot_wider(names_from = question, values_from = rating)

#create negativeRatings as a dataframe that has IAPS data from the rounds where
#it was asked how negative the image made them feel. Then combine the
#rating and question data into one column that has the ratings for when the
#question was about negative feelings.
negativeRatings <- labeledOrderedSubjectData %>%
  filter(question == "negative") %>%
  pivot_wider(names_from = question, values_from = rating)

#combine the positive and negative ratings data by IAPS, subject, and form.
#Could've just used IAPS but just more specificity. Then remove miscellaneous
#order column and join that with the IAPS key. Then remove NAs since there are
#some images participants saw that are not standardized in the IAPS key. The
#final data has 12 columns and 5760 rows and consists of 120 positive and
#negative ratings from 48 subjects.
finalData <- full_join(positiveRatings, negativeRatings, by = c("IAPS", "subject", "form")) %>%
  select(-c(order.x, order.y)) %>%
  full_join(keyIAPS, by = c("IAPS", "form")) %>%
  drop_na()



# Model Stuff -------------------------------------------------------------

library(tidymodels)
library(tictoc)

set.seed(18)

train_cv <- vfold_cv(finalData, v = 6)

netRec <- recipe(valmn ~ positive + negative, data = finalData)

netGrid <- expand_grid(penalty = seq(0,5),
                       mixture = seq(0,1, by = 0.01))

netSpec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(netRec) %>%
  add_model(netSpec)

tic("tune")
netTuned1 <- tune_grid(wf,
                      resamples = train_cv,
                      grid = netGrid)
toc()

netTuned %>% collect_metrics() %>%
ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~.metric, scales = "free", nrow = 2)



netGrid2 <- expand_grid(penalty = seq(0,10, by = 0.01),
                       mixture = seq(0,1, by = 0.01))
tic("tune grid 2")
netTuned <- tune_grid(wf,
                      resamples = train_cv,
                      grid = netGrid2)
toc()



netTuned %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  arrange(mean)




netTuned1 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  arrange(desc(penalty), mean)

OGRegression <- finalData %>%
  nest(data = -c(form, subject, IAPS)) %>%
  mutate(fit = map(data, ~ lm(valmn ~ positive + negative, data = .x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance),
         augmented = map(fit, augment))

OGRegression %>% select(glanced) %>% unnest(glanced)


#pull best metrics
bestMetrics <- select_best(netTuned1, metric = "rmse")
#finalize workflow
finalWF <- finalize_workflow(wf, parameters = bestMetrics)

#fit the workflow with the best penalty
library(vip)
finalWF %>%
  fit(finalData) %>%
  pull_workflow_fit() %>%
  vi(lambda = bestMetrics$penalty)

split <- initial_split(finalData)
training <- training(split)
CVtraining <- vfold_cv(training, v = 4)


netGrid3 <- expand_grid(penalty = seq(0,1, by = 0.001),
                        mixture = seq(0,1, by = 0.01))

tic("splitNetGrid")
splitNetGrid <- tune_grid(wf,
                          resamples = CVtraining,
                          grid = netGrid3)
toc()



bestSplitMetrics <- splitNetGrid %>% select_best(metric = "rmse")

finalWFSplit <- finalize_workflow(wf, bestSplitMetrics)

finalSplitFit <- finalWFSplit %>% last_fit(split = split)

finalSplitFitMetrics <- finalSplitFit %>% collect_metrics()

finalSplitFitPreds <- finalSplitFit %>%
  select(.predictions) %>%
  unnest(.predictions)


lm(valmn ~ positive + negative, training) %>%
  predict(testing(split)) %>%
  bind_cols(testing(split)) %>%
  rename(.pred = `...1`) %>% select(.pred) %>% bind_cols(justPredsSplitFit) %>% View()

bind_cols()





