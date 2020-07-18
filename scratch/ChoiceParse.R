# # install packages
# #install.packages('gridExtra')
# #install.packages("ggplot2")
# #install.packages("stringr")
#
# #load packages
# library(gridExtra)
# library(stringr)
# library(ggplot2)
# library(R.matlab)
#
# subjectFolder<- "ECT_8000"  #!!!Change for each subject, then select all and control+enter to run
# labDrivePath <- "/Volumes/KishidaLab/ECT/Subject Data/"
#
# setwd(paste(labDrivePath,subjectFolder,sep=""))
#
# matlabFileName <- paste(subjectFolder,'.mat',sep="")
#
# # Reads mgt text file
# filename = list.files(path = ".", pattern = '*.iaps_choice.*\\d.txt') #lists files inside folder, automatically finds imgt file in folder.
# # ^ only works if there is just one imgt txt file in the folder. Otherwise, change filename (below) to the actual filename
# output <- read.table(filename, #"control.imgt3.IMGT-4014.nosessionid.2018-12-20T11_11_18.txt",
#                      header = FALSE,
#                      sep = "\t",
#                      stringsAsFactors = FALSE)
#
# # selects only column with data (other column is time stamp)
# outputText <- output[,2]
#
# #game start time
# startTime <- output$V1[grep("starting game", outputText)]
#
# #game end time, before final rating
# trialEndTime<-output$V1[grep("FINAL RATING SELECTION", outputText)]
#
# gameDuration<- trialEndTime-startTime #not including final rating screen
# gameDurationMinutes<- gameDuration/60/1000
#
# # finds lines where round ends and begins
# roundEndLines <- grep("CLEAR SCREEN", outputText)
# roundBeginsLines <- grep("SHOW: OPTION SELECTION", outputText)
#
# # information about game
# introLines <- roundBeginsLines[1] - 1
# numberRounds <- length(roundEndLines)
#
# # finds lines and times where answer was picked and question was presented
# answerPickedLines<- grep("HIGHLIGHT", outputText)
# timeAnswerPicked<- output$V1[answerPickedLines]
# answerPickedTimeFromStart <- timeAnswerPicked-startTime
# optionsPresentedLines<- grep("SHOW: OPTION SELECTION", outputText)
# timeQuestionPresented<- output$V1[optionsPresentedLines]
# timeQuestionPresentedFromStart<- timeQuestionPresented-startTime
#
# # Show groups displayed
# iconMappingLine<-  grep("ICON TO GROUP MAPPING", outputText)
# icons <- str_extract_all(outputText[iconMappingLine], "....\\d\\d")
#
# # group to icon mapping
# icon1Group <- sub(":","",str_extract(outputText[iconMappingLine],":\\d"))
# icon2Group <- sub("icon02:","",str_extract(outputText[iconMappingLine], "icon02:\\d"))
# icon3Group <- sub("icon03:","",str_extract(outputText[iconMappingLine], "icon03:\\d"))
# icon4Group <- sub("icon04:","",str_extract(outputText[iconMappingLine], "icon04:\\d"))
# icon5Group <- sub("icon05:","",str_extract(outputText[iconMappingLine], "icon05:\\d"))
# icon6Group <- sub("icon06:","",str_extract(outputText[iconMappingLine], "icon06:\\d"))
#
# #icons and corresponding groups displayed
# iconLeft<- sub("1:","",str_extract(outputText[optionsPresentedLines], "1:icon0\\d"))
# iconRight<- sub("2:","", str_extract(outputText[optionsPresentedLines], "2:icon0\\d"))
# groupLeft <- iconLeft
# groupLeft[iconLeft=="icon01"]<- icon1Group
# groupLeft[iconLeft=="icon02"]<- icon2Group
# groupLeft[iconLeft=="icon03"]<- icon3Group
# groupLeft[iconLeft=="icon04"]<- icon4Group
# groupLeft[iconLeft=="icon05"]<- icon5Group
# groupLeft[iconLeft=="icon06"]<- icon6Group
# groupLeft <- as.numeric(groupLeft)
# groupRight <- iconRight
# groupRight[iconRight=="icon01"]<- icon1Group
# groupRight[iconRight=="icon02"]<- icon2Group
# groupRight[iconRight=="icon03"]<- icon3Group
# groupRight[iconRight=="icon04"]<- icon4Group
# groupRight[iconRight=="icon05"]<- icon5Group
# groupRight[iconRight=="icon06"]<- icon6Group
# groupRight<- as.numeric(groupRight)
# optionsDisplayed<- data.frame(timeQuestionPresentedFromStart, iconLeft, iconRight, groupLeft, groupRight)
#
# # check random distribution of groups during phase 3
# totalPhase3Groups <- c(groupLeft[76:150],groupRight[76:150])
# hist(totalPhase3Groups)
#
# #gets rating data (lines, time, ratings)
# showRatingLines <- grep("SHOW: RATING SELECTION", outputText)
# showRatingTime <- output$V1[showRatingLines]
# ratingSubmitLines <- grep("SUBMIT rating value", outputText)
# ratingSubmitTime <- output$V1[ratingSubmitLines]
# ratingSubmitTime<- ratingSubmitTime[-length(ratingSubmitTime)]
# ratingLines <- grep("SUBMIT rating value", outputText)
# ratings <- str_extract(outputText[ratingLines], "\\d")
# finalRating <- ratings[length(ratings)]#str_extract(outputText[finalRatingLine], "\\d")
# ratingLines <- ratingLines[-length(ratingLines)]  # removes last rating which is the final rating
# ratings<- ratings[-length(ratings)]
# ratingTime <- output$V1[ratingLines]
# ratingTimeFromStart<- ratingTime-startTime
# ratingdf<- data.frame(ratings, showRatingTime, ratingTime)
#
# #slider movement data
# sliderDecreaseLines<- grep("SLIDER decreased", outputText)
# sliderIncreaseLines<- grep("SLIDER increased", outputText)
# sliderDecreaseTime<-output$V1[sliderDecreaseLines]
# sliderIncreaseTime<-output$V1[sliderIncreaseLines]
# sliderDecreaseValue<- str_extract(outputText[sliderDecreaseLines], "\\d")
# sliderIncreaseValue<- str_extract(outputText[sliderIncreaseLines], "\\d")
#
# sliderDecreaseDf<- data.frame(sliderDecreaseTime, sliderDecreaseValue)
# sliderIncreaseDf<- data.frame(sliderIncreaseTime, sliderIncreaseValue)
#
# rounds<- as.integer(sub("ROUND\\s","",str_extract(outputText[answerPickedLines], "(ROUND\\s\\d\\d\\d)|(ROUND\\s\\d\\d)|(ROUND\\s\\d)")))
# roundBeginsTimes<- output$V1[roundBeginsLines]
# roundEndsTimes<- output$V1[roundEndLines]
#
# roundInfo<- data.frame(rounds, roundBeginsTimes, roundEndsTimes)
#
#
# #TTL pulse
# ttlLines<- grep("TR", outputText)
# ttlTime <- output$V1[ttlLines]
# ttlNumber<- str_extract(outputText[ttlLines], "\\d\\d\\d\\d|\\d\\d\\d|\\d\\d|\\d")
#
# #times for choice presentation screen
# showChoiceLines <- grep("SHOW: OPTION SELECTION", outputText)
# showChoiceTime <- output$V1[showChoiceLines]
# choicePickedLines <- grep("HIGHLIGHT CHOICE:", outputText)
# choicePickedTime <- output$V1[choicePickedLines]
#
# #times for keypress
# choiceKeypressLines <- grep("KEYPRESS: \\d, OPTION", outputText)
# choiceKeypressTime <- output$V1[choiceKeypressLines]
# showChoiceEndLines <- grep("fixed feedback duration", outputText)
# showChoiceEndTime <- output$V1[showChoiceEndLines]
#
# #times for outcome screen
# feedbackShowLines <- grep("fixed feedback duration", outputText)
# feedbackShowTime <- output$V1[feedbackShowLines]
# endFeedbackShowLines <- grep("CLEAR SCREEN", outputText)
# endFeedbackShowTime <- output$V1[endFeedbackShowLines]
#
# #shows icons and groups picked for each round
# iconsPicked <- as.integer(str_extract(outputText[answerPickedLines], "0\\d"))
# groupsPicked2 <- str_extract(outputText[answerPickedLines], "GROUP:\\s\\d")
# groupsPicked <- as.numeric(sub("GROUP:\\s","", groupsPicked2))
# groupData <- data.frame(timeAnswerPicked, groupsPicked, iconsPicked, rounds)
# optionsDisplayed<- data.frame(optionsDisplayed, groupsPicked)
#
# #Finds if groups picked were on the right or left
# groupLR<- data.frame(groupsPicked, groupRight, groupLeft)
# groupLR$sidePicked[groupLR$groupsPicked == groupLR$groupRight]<- 'R'
# groupLR$sidePicked[groupLR$groupsPicked == groupLR$groupLeft]<- 'L'
# sideGroupPicked <- groupLR$sidePicked
#
#
# hist(groupsPicked[1:25], main="Frequency of Groups Picked in Phase 1", xlab="groups picked")
# hist(groupsPicked[26:75], main="Frequency of Groups Picked in Phase 2", xlab="groups picked")
# #hist(groupsPicked[1:75], main="Frequency of Groups Picked in Phase 1 and 2", xlab="groups picked")
# hist(groupsPicked[76:150], main = "Frequency of Groups Picked in Phase 3", xlab="groups picked")
#
# #group picked Expected value
# expectedValuePhase12 <- groupsPicked[1:75]
# expectedValuePhase12[expectedValuePhase12 == 1] <- 0.25
# expectedValuePhase12[expectedValuePhase12 == 2] <- 0.50
# expectedValuePhase12[expectedValuePhase12 == 3] <- 0.75
# expectedValuePhase12[expectedValuePhase12 == 4] <- -0.25
# expectedValuePhase12[expectedValuePhase12 == 5] <- -0.5
# expectedValuePhase12[expectedValuePhase12 == 6] <- -0.75
#
# expectedValuePhase3 <- groupsPicked[76:150]
# expectedValuePhase3[expectedValuePhase3 == 1] <- 0.625
# expectedValuePhase3[expectedValuePhase3 == 2] <- 0.75
# expectedValuePhase3[expectedValuePhase3 == 3] <- 0.375
# expectedValuePhase3[expectedValuePhase3 == 4] <- -0.3125
# expectedValuePhase3[expectedValuePhase3 == 5] <- -0.375
# expectedValuePhase3[expectedValuePhase3 == 6] <- -0.1875
#
# expectedValue <- c(expectedValuePhase12, expectedValuePhase3)
#
# #groupRight Expected value
# groupRightexpectedValuePhase12 <- groupRight[1:75]
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 1] <- 0.25
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 2] <- 0.50
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 3] <- 0.75
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 4] <- -0.25
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 5] <- -0.5
# groupRightexpectedValuePhase12[groupRightexpectedValuePhase12 == 6] <- -0.75
#
# groupRightexpectedValuePhase3 <- groupRight[76:150]
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 1] <- 0.625
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 2] <- 0.75
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 3] <- 0.375
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 4] <- -0.3125
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 5] <- -0.375
# groupRightexpectedValuePhase3[groupRightexpectedValuePhase3 == 6] <- -0.1875
#
# expectedValueRight <- c(groupRightexpectedValuePhase12, groupRightexpectedValuePhase3)
#
# #groupLeft Expected value
# groupLeftexpectedValuePhase12 <- groupLeft[1:75]
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 1] <- 0.25
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 2] <- 0.50
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 3] <- 0.75
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 4] <- -0.25
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 5] <- -0.5
# groupLeftexpectedValuePhase12[groupLeftexpectedValuePhase12 == 6] <- -0.75
#
# groupLeftexpectedValuePhase3 <- groupLeft[76:150]
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 1] <- 0.625
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 2] <- 0.75
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 3] <- 0.375
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 4] <- -0.3125
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 5] <- -0.375
# groupLeftexpectedValuePhase3[groupLeftexpectedValuePhase3 == 6] <- -0.1875
#
# expectedValueLeft <- c(groupLeftexpectedValuePhase12, groupLeftexpectedValuePhase3)
#
#
# #times for intertrial interval
# intertrialLines <- grep("inter-trial delay", outputText)
# #interTrialDelay <-
#
# rewardLines <- grep("SHOW: image", outputText)
# rewardedTime <- output$V1[rewardLines]
# rewardedTimeFromStart <- rewardedTime - startTime
# rewarded <- sub(".*image - *(.*?) *.jpg.*", "\\1", outputText[rewardLines]) #as.numeric(str_extract(outputText[rewardLines], "(\\d\\.\\d\\d)|(-\\d\\.\\d\\d)"))
# rewardDf<- data.frame(rewarded, rewardedTime, rewardedTime+1000)
#
# ##Convert rewarded image into equivalent imgt earnings
# groupReward <- data.frame(rewarded, groupsPicked)
# #phase 1
# groupReward$monetaryVal[(groupReward[,1] == 'neutral')] <- 0 # if image is neutral, value is 0
# groupRewardPhaseI <- groupReward[1:25,]
# groupRewardPhaseI$monetaryVal[is.na(groupRewardPhaseI$monetaryVal)]<-1
# #phase 2
# groupRewardPhaseII <- groupReward[26:75,]
# groupRewardPhaseII$monetaryVal[(groupRewardPhaseII$groupsPicked == 4 | groupRewardPhaseII$groupsPicked == 5 |groupRewardPhaseII$groupsPicked == 6 ) & groupRewardPhaseII$rewarded != "neutral"] <- -1
# groupRewardPhaseII$monetaryVal[(groupRewardPhaseII$groupsPicked == 1 | groupRewardPhaseII$groupsPicked == 2 |groupRewardPhaseII$groupsPicked == 3 ) & groupRewardPhaseII$rewarded != "neutral"] <- 1
# #phase 3
# groupRewardPhaseIII <- groupReward[76:length(groupReward$rewarded),]
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 1 & groupRewardPhaseIII$rewarded != "neutral"] <- 2.5
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 2 & groupRewardPhaseIII$rewarded != "neutral"] <- 1.5
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 3 & groupRewardPhaseIII$rewarded != "neutral"] <- 0.5
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 4 & groupRewardPhaseIII$rewarded != "neutral"] <- -1.25
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 5 & groupRewardPhaseIII$rewarded != "neutral"] <- -0.75
# groupRewardPhaseIII$monetaryVal[groupRewardPhaseIII$groupsPicked == 6 & groupRewardPhaseIII$rewarded != "neutral"] <- -0.25
#
# groupReward = rbind(groupRewardPhaseI, groupRewardPhaseII, groupRewardPhaseIII)
#
#
# finalRatingLine <- grep("FINAL RATING SELECTION", outputText)
# finalRatingStartTime <- output$V1[finalRatingLine]
# totalEarningsLines <- grep("Experiment Complete", outputText)
# totalEarnings <- str_extract(outputText[totalEarningsLines], "(\\d\\d.\\d)|(-\\d\\d.\\d)|(-\\d.\\d)|(\\d.\\d)|(\\d\\d\\d)|(\\d\\d)|(\\d)")
# endTime<- output$V1[totalEarningsLines]
#
# #Use below lines for incomplete runs
# #totalEarningsLines <- grep("Experiment Aborted", outputText)
# #totalEarnings <- str_extract(outputText[totalEarningsLines], "(\\d\\d.\\d)|(-\\d\\d.\\d)|(-\\d.\\d)|(\\d.\\d)|(\\d\\d\\d)|(\\d\\d)|(\\d)")
# #endTime<- output$V1[totalEarningsLines]
#
# par(new=TRUE) # disables scientific notation, for time stamps
#
# #writes variables to matlab .mat
# writeMat( matlabFileName ,
#           gameOnset = as.matrix(startTime),
#           choiceOnset = as.matrix(showChoiceTime), choiceGroupLeft = as.matrix(groupLeft), choiceGroupRight = as.matrix(groupRight), choiceIconLeft = as.matrix(iconLeft), choiceIconRight=as.matrix(iconRight), choiceEVLeft = as.matrix(expectedValueLeft), choiceEVRight = as.matrix(expectedValueRight),
#           decisionOnset = as.matrix(choicePickedTime), decisionGroupValue = as.matrix(groupsPicked), decisionIconValue = as.matrix(iconsPicked), decisionExpectedValue= as.matrix(expectedValue), decisionSideOfGroupValue= sideGroupPicked,
#           outcomeOnset = as.matrix(feedbackShowTime), outcomeValue = as.matrix(groupReward$monetaryVal), outcomeOffset = as.matrix(endFeedbackShowTime),
#           subratingAdjustmentDecrease = as.matrix(sliderDecreaseDf), subratingAdjustmentIncrease = as.matrix(sliderIncreaseDf),
#           subratingOnset = as.matrix(showRatingTime), subratingOutcome = as.matrix(ratings), subratingOffset = as.matrix(ratingTime),
#           TTLPulseTime = as.matrix(ttlTime), TTLNumber = as.matrix(ttlNumber))
#
#
#
#
#
#
# makeGraph <- 0 # set to 1 to make below graphs
#
# if (makeGraph == 1)
#
# {
#   #Graphs to plot rewards and feedback
#   # plots rewards and ratings, labels with group picked
#   ggplot(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))+
#     labs(title="Ratings and Rewards")+
#     geom_segment(aes(x = showRatingTime,y = ratings,xend = ratingTime), data=ratingdf, yend= 0) +geom_segment(aes(x = rewardedTime,y = groupReward$monetaryVal/3,xend = rewardedTime),data=rewardDf, yend= 0, color='blue') + geom_text(data=groupData, mapping=aes(x=timeAnswerPicked, y=0, label=substr(groupsPicked, 1, 1)), size=3)
#
#
#   #plots ratings and rewards on the same graph. Rewards are red, ratings are black
#   rewardedModified <- groupReward$monetaryVal#rewarded
#   rewardedModified[groupReward$monetaryVal == 0] <- 0.05 # Changes 0.00 values in rewarded to 0.05 so 0 values can be seen on graph
#   ggplot() + labs(title="Ratings and Rewards", x = "Time(ms)") + geom_rect(aes(xmin = showRatingTime, ymin = rep(0, (length(showRatingTime))), xmax= ratingTime,
#                                                                                ymax = as.numeric(ratings), fill="ratings"))+ geom_rect(aes(xmin = rewardedTime, ymin = rep(0,length(groupReward$monetaryVal)), xmax = rewardedTime+1000, ymax = (rewardedModified), fill='rewards'))#+geom_point(aes(x=showRatingTime[1], y=0))+ geom_point(aes(x=rewardedTime[1],y=0))+
#
#   #Seperate Reward and Rating Plots, using actual time stamps
#   ratingPlot<- ggplot() + labs(title="Ratings", x = "Time (ms)", y="Rating") + xlim(startTime,endTime) + geom_rect(aes(xmin = showRatingTime, ymin = rep(0, (length(ratings))), xmax= ratingTime, ymax = as.numeric(ratings)))
#   rewardPlot<- ggplot() + labs(title="Winnings", y="Amount ($)", x = "Time (ms)") + xlim(startTime, endTime) + geom_rect(aes(xmin = rewardedTime, ymin = rep(0,length(groupReward$monetaryVal)), xmax = rewardedTime+1000, ymax = (groupReward$monetaryVal)))
#   choicePlot<- ggplot() + labs(title="Choices", x = "Time (ms)", y=" ") + xlim(startTime, endTime) + ylim(-1,2) + geom_rect(aes(xmin = showChoiceTime, ymin = rep(0, length(showChoiceTime)), xmax = choicePickedTime, ymax = (1.1))) +
#     geom_text(data = groupData, mapping=aes(x = showChoiceTime, y = 1, label = groupsPicked), nudge_y =0.25, size = 3)#+
#   #theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) #removes y axis labels and ticks
#   grid.arrange(ratingPlot, rewardPlot, choicePlot)
#
#   #Reward and Rating Plots with time adjusted (experiment starts at 0 ms)
#   ###ratingPlot<- ggplot() + labs(title="Ratings", x = "Time (ms)", y="Rating") + xlim(0,endTime-startTime) + geom_rect(aes(xmin = showRatingTime-startTime, ymin = rep(0, (length(ratings))), xmax= ratingTime-startTime, ymax = as.numeric(ratings)))
#   ###rewardPlot<- ggplot() + labs(title="Winnings", y="Amount ($)", x = "Time (ms)") + xlim(0, endTime-startTime) + geom_rect(aes(xmin = rewardedTime-startTime, ymin = rep(0,length(rewarded)), xmax = rewardedTime+1000-startTime, ymax = (rewarded)))
#   ###choicePlot<- ggplot() + labs(title="Choices", x = "Time (ms)", y=" ") + xlim(0, endTime-startTime) + ylim(-1,2) + geom_rect(aes(xmin = showChoiceTime-startTime, ymin = rep(0, length(showChoiceTime)), xmax = choicePickedTime-startTime, ymax = (1.1))) +
#   ###  geom_text(data = groupData, mapping=aes(x = showChoiceTime-startTime, y = 1, label = groupsPicked), nudge_y =0.25, size = 2)
#   ###grid.arrange(ratingPlot, rewardPlot, choicePlot)
#
#   #Reward and Rating Plots with time adjusted to seconds (experiment starts at 0 s)
#   ratingPlot<- ggplot() + labs(title="Ratings", x = "Time (s)", y="Rating") + xlim(0,(endTime-startTime)/1000) + geom_rect(aes(xmin = (showRatingTime-startTime)/1000, ymin = rep(0, (length(ratings))), xmax= (ratingTime-startTime)/1000, ymax = as.numeric(ratings)))
#   rewardPlot<- ggplot() + labs(title="Winnings", y="Amount ($)", x = "Time (s)") + xlim(0, (endTime-startTime)/1000) + geom_rect(aes(xmin = (rewardedTime-startTime)/1000, ymin = rep(0,length(rewardedModified)), xmax = (rewardedTime+1000-startTime)/1000, ymax = (rewardedModified)))
#   choicePlot<- ggplot() + labs(title="Choices", x = "Time (s)", y=" ") + xlim(0, (endTime-startTime)/1000) + ylim(-1,2) + geom_rect(aes(xmin = (showChoiceTime-startTime)/1000, ymin = rep(0, length(showChoiceTime)), xmax = (choicePickedTime-startTime)/1000, ymax = (1.1))) +
#     geom_text(data = groupData, mapping=aes(x = (showChoiceTime-startTime)/1000, y = 1, label = groupsPicked), nudge_y =0.25, size = 3)
#   grid.arrange(ratingPlot, rewardPlot, choicePlot)
#
#   #Reward and Rating Plots with time adjusted to minutes (experiment starts at 0 minutes)
#   ratingPlot<- ggplot() + labs(title="Ratings", x = "Time (min)", y="Rating") + xlim(0,(endTime-startTime)/60000) + geom_rect(aes(xmin = (showRatingTime-startTime)/60000, ymin = rep(0, (length(ratings))), xmax= (ratingTime-startTime)/60000, ymax = as.numeric(ratings)))
#   rewardPlot<- ggplot() + labs(title="Winnings", y="Amount ($)", x = "Time (min)") + xlim(0, (endTime-startTime)/60000) + geom_rect(aes(xmin = (rewardedTime-startTime)/60000, ymin = rep(0,length(rewardedModified)), xmax = (rewardedTime+1000-startTime)/60000, ymax = (rewardedModified)))
#   grid.arrange(ratingPlot, rewardPlot)
#
#   #group picked time line
#   ggplot() + labs(y="groups", x="Time (ms)") + geom_rect( aes(xmin = showChoiceTime, ymin = rep(0, length(showChoiceTime)), xmax = choicePickedTime, ymax = groupsPicked))+#rep(1, length(choicePickedTime))))+
#     geom_text(aes(x = showChoiceTime, y=groupsPicked, label=groupsPicked),hjust=0, vjust=0, size = 2)
#   #option time line with numbers for groups picked
#   ggplot() + labs(y="groups", x="Time (ms)") + geom_rect( aes(xmin = showChoiceTime, ymin = (-1*groupLeft), xmax = choicePickedTime, ymax = groupLeft))+#rep(1, length(choicePickedTime))))+
#     geom_text(aes(x = showChoiceTime, y=groupsPicked, label=groupsPicked),hjust=0, vjust=0, size=2)
#
#   # combined time line of Options screen, chosen screen, outcome screen, and rating screens
#   ggplot() + labs(title="Screen Timeline", x="Time (ms)") + ylim(-1.5,2.5) +
#     geom_rect( size=0, aes(xmin = showChoiceTime, ymin = rep(0, length(showChoiceTime)), xmax = choicePickedTime, ymax = rep(1, length(choicePickedTime)), fill = 'options presented')) +
#     geom_rect( size=0, aes(xmin = choiceKeypressTime, ymin = rep(0, length(choiceKeypressTime)), xmax = showChoiceEndTime, ymax = rep(1, length(showChoiceEndTime)), fill = 'chosen screen')) +
#     geom_rect( size=0, aes(xmin = feedbackShowTime, ymin = rep(0, length(feedbackShowTime)), xmax = endFeedbackShowTime, ymax = rep(1, length(endFeedbackShowTime)), fill = 'outcome')) +
#     geom_rect( size=0, aes(xmin = showRatingTime, ymin = rep(0, length(showRatingTime)), xmax = ratingSubmitTime, ymax = rep(1, length(ratingSubmitTime)), fill = 'rating')) +
#     scale_fill_manual("Duration", values = c("options presented" = "cornflowerblue", "chosen screen" = "tomato","outcome" = "darkgreen", "rating" = "purple"))
#
#   #Timeline split into 3
#   showRatingTime1 <- showRatingTime[showRatingTime<endFeedbackShowTime[length(endFeedbackShowTime)/3]]
#   ratingSubmitTime1 <- ratingSubmitTime[ratingSubmitTime<endFeedbackShowTime[length(endFeedbackShowTime)/3]]
#   showRatingTime2 <- showRatingTime[showRatingTime>=endFeedbackShowTime[length(endFeedbackShowTime)/3] & showRatingTime<endFeedbackShowTime[length(endFeedbackShowTime)/3*2]]
#   ratingSubmitTime2<- ratingSubmitTime[ratingSubmitTime>=endFeedbackShowTime[length(endFeedbackShowTime)/3]&ratingSubmitTime<endFeedbackShowTime[length(endFeedbackShowTime)/3*2]]
#   showRatingTime3 <- showRatingTime[showRatingTime>=endFeedbackShowTime[length(endFeedbackShowTime)/3*2]]
#   ratingSubmitTime3 <- ratingSubmitTime[ratingSubmitTime>=endFeedbackShowTime[length(endFeedbackShowTime)/3*2]]
#
#   optionPlot1<- ggplot()+ylim(-0.5,1.5)+
#     labs(title="Timeline: Trials 1-50", x="time (ms)")+
#     #time for participant to pick a choice
#     geom_rect(size=0, aes(xmin = showChoiceTime[1:(length(showChoiceTime)/3)], ymin = rep(0, (length(showChoiceTime)/3)), xmax = choicePickedTime[1:(length(choicePickedTime)/3)], ymax = rep(1, (length(choicePickedTime)/3)), fill="options presented")) +
#     #choice screen duration
#     geom_rect(size=0, aes(xmin = choiceKeypressTime[1:(length(choiceKeypressTime)/3)], ymin = rep(0, (length(showChoiceTime)/3)), xmax = showChoiceEndTime[1:(length(showChoiceEndTime)/3)], ymax = rep(1, (length(showChoiceEndTime)/3)), fill="chosen screen")) +
#     #outcome screen duration
#     geom_rect(size=0, aes(xmin = feedbackShowTime[1:(length(feedbackShowTime)/3)], ymin = rep(0, (length(feedbackShowTime)/3)), xmax = endFeedbackShowTime[1:(length(endFeedbackShowTime)/3)], ymax = rep(1, (length(endFeedbackShowTime)/3)), fill="outcome")) +
#     #rating screen duration
#     geom_rect(size=0, aes(xmin = showRatingTime1, ymin = rep(0, (length(showRatingTime1))), xmax = ratingSubmitTime1, ymax = rep(1, length(ratingSubmitTime1)), fill="rating"))+
#     scale_fill_manual("Duration", values = c("options presented" = "dodgerblue", "chosen screen" = "chartreuse3","outcome" = "tomato", "rating" = "purple"))
#   optionPlot2<- ggplot()+ylim(-0.5,1.5)+
#     labs(title="Timeline: Trials 51-100", x="time (ms)")+
#     geom_rect(size=0, aes(xmin = showChoiceTime[(length(showChoiceTime)/3+1):(length(showChoiceTime)/3*2)], ymin = rep(0, (length(showChoiceTime)/3)), xmax = choicePickedTime[(length(choicePickedTime)/3+1):(length(choicePickedTime)/3*2)], ymax = rep(1, (length(choicePickedTime)/3)), fill="options presented")) +
#     geom_rect(size=0, aes(xmin = choiceKeypressTime[(length(choiceKeypressTime)/3+1):(length(choiceKeypressTime)/3*2)], ymin = rep(0, (length(showChoiceTime)/3)), xmax = showChoiceEndTime[(length(showChoiceEndTime)/3+1):(length(showChoiceEndTime)/3*2)], ymax = rep(1, (length(showChoiceEndTime)/3)), fill = "chosen screen")) +
#     geom_rect(size=0, aes(xmin = feedbackShowTime[(length(feedbackShowTime)/3+1):(length(feedbackShowTime)/3*2)], ymin = rep(0, (length(feedbackShowTime)/3)), xmax = endFeedbackShowTime[(length(endFeedbackShowTime)/3+1):(length(endFeedbackShowTime)/3*2)], ymax = rep(1, (length(endFeedbackShowTime)/3)), fill = "outcome")) +
#     geom_rect(size=0, aes(xmin = showRatingTime2, ymin = rep(0, (length(showRatingTime2))), xmax = ratingSubmitTime2, ymax = rep(1, length(ratingSubmitTime2)), fill = "rating"))+
#     scale_fill_manual("Duration", values = c("options presented" = "dodgerblue", "chosen screen" = "chartreuse3","outcome" = "tomato", "rating" = "purple"))
#   optionPlot3 <- ggplot()+ylim(-0.5,1.5) +
#     labs(title="Timeline: Trials 101-150", x="time (ms)")+
#     geom_rect(aes(xmin = showChoiceTime[(length(showChoiceTime)/3*2+1):length(showChoiceTime)], ymin = rep(0, (length(showChoiceTime)/3)), xmax = choicePickedTime[(length(choicePickedTime)/3*2+1):length(choicePickedTime)], ymax = rep(1, (length(choicePickedTime)/3)), fill = "options presented")) +
#     geom_rect(aes(xmin = choiceKeypressTime[(length(choiceKeypressTime)/3*2+1):length(choiceKeypressTime)], ymin = rep(0, (length(choiceKeypressTime)/3)), xmax = showChoiceEndTime[(length(showChoiceEndTime)/3*2+1):(length(showChoiceEndTime))], ymax = rep(1, (length(showChoiceEndTime)/3)), fill = "chosen screen")) +
#     geom_rect(size=0, aes(xmin = feedbackShowTime[(length(feedbackShowTime)/3*2+1):length(feedbackShowTime)], ymin = rep(0, (length(feedbackShowTime)/3)), xmax = endFeedbackShowTime[(length(endFeedbackShowTime)/3*2+1):length(endFeedbackShowTime)], ymax = rep(1, (length(endFeedbackShowTime)/3)), fill = "outcome")) +
#     geom_rect(size=0, aes(xmin = showRatingTime3, ymin = rep(0, (length(showRatingTime3))), xmax = ratingSubmitTime3, ymax = rep(1, length(ratingSubmitTime3)), fill = "rating"))+
#     scale_fill_manual("Duration", values = c("options presented" = "dodgerblue", "chosen screen" = "chartreuse3","outcome" = "tomato", "rating" = "purple"))
#   grid.arrange(optionPlot1, optionPlot2, optionPlot3)
#
# }
