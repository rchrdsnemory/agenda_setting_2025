#########################################################################################################################
######################        ASP  Exp 3 - Target Speaks Last            ###############################################
#########################################################################################################################
## Load packages
library(tidyverse)
library(glue)
library(ggeffects)
library(ggh4x)
library(ggtext)
library(broom.mixed)
library(scales)
library(ordinal)
library(viridis)
library(ggbeeswarm)
library(patchwork)
library(janitor)
library(readxl)

############
############ Create clean theme
# theme_cleanPub<-theme_minimal()+theme(axis.text.x = element_text(size=12, lineheight=.9, face="bold",
#                                                                  color="black", family="Palatino",
#                                                                  angle=0, hjust=.5, vjust=0),
#                                       axis.title.x=element_text(size=14, face="bold", color="black", family="Palatino",),
#                                       axis.title.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       axis.text.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       legend.text = element_text(size=13, face="bold", color="black", family="Palatino",),
#                                       legend.position="bottom",
#                                       # legend.title = element_blank(),
#                                       legend.title = element_text(size = 12, face = "bold", family="Palatino"),
#                                       plot.title = element_text(hjust = 0.5, family="Palatino",
#                                                                 size=23, face="bold", colour="black"),
#                                       plot.subtitle=element_text(size=14, hjust=0.5, family="Palatino",
#                                                                  face="italic", color="black"),
#                                       strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
#                                       strip.background = element_rect(fill="gray82", color="black")
# )

#########################################################################################################################
#######################################              IMPORT DATA          ###############################################
#########################################################################################################################

################################################### Import response data
###################################################
ASP_Exp3 <- read_excel("data/24.02.16_ASP_Exp3_data.xlsx", 
                       sheet = "RData")
str(ASP_Exp3)

# Check exclusions:
# 15 ppl failed basic comprehension and weren't allowed to do experiment 
# and 24 failed the post-exp attn check, 
# but only 1 had a definitely bot-like verbal explanation
ASP_Exp3%>%
  group_by(exclude_preScreen, exclude_postScreen, exclude_forAnalysis)%>%
  summarise(Totals=n())%>%
  adorn_totals()

# filter out exclusions
ASP_Exp3<-ASP_Exp3%>%
  filter(exclude_forAnalysis == 0)%>%
  select(-starts_with("Explain"), -Comments)%>%
  relocate(starts_with("Think_"), .after = "Say_Spkr5")

# sample size by condition 
ASP_Exp3%>%
  group_by(PowerLevel)%>%
  tally()

## create this function so that we can use it to compute the sum of Y v B votes:
greaterThan10<-function(x) ifelse(x>10, 1, 0)

##### create variables of interest
ASP_Exp3<-ASP_Exp3%>%
  # first, for each Speaker's Say & Think columns, create a column encoding it 
  # as either Yellow (>10) or Blue (<10)
  mutate(across(Say_Spkr2:Say_Spkr5, ~ifelse(.x>10, "Y", "B"),
                .names = 'Vote_{col}'),
         across(Think_Spkr2:Think_Spkr5, ~ifelse(.x>10, "Y", "B"),
                .names = 'Belief_{col}'))%>%
  # then glue together the values in those columns, and add Spkr1's Yellow Vote/Belief
  # to create the pattern of votes (PublicPattern) and beliefs (PrivatePattern)
  unite("PublicPattern", Vote_Say_Spkr2:Vote_Say_Spkr5, sep = "")%>%
  unite("PrivatePattern", Belief_Think_Spkr2:Belief_Think_Spkr5, sep = "")%>%
  mutate(PublicPattern = glue("B{PublicPattern}"),
         PrivatePattern = glue("B{PrivatePattern}"))%>%
  # save those patterns as factors
  mutate(PublicPattern = factor(PublicPattern, levels = c("BYYYY", "BYYYB", "BYYBY", "BYBYY", "BBYYY",
                                                          "BYYBB", "BYBBY", "BBBYY", "BBYYB", "BBYBY", "BYBYB",
                                                          "BYBBB", "BBBBY", "BBYBB", "BBBYB", "BBBBB")),
         PrivatePattern = factor(PrivatePattern, levels = c("BYYYY", "BYYYB", "BYYBY", "BYBYY", "BBYYY",
                                                            "BYYBB", "BYBBY", "BBBYY", "BBYYB", "BBYBY", "BYBYB",
                                                            "BYBBB", "BBBBY", "BBYBB", "BBBYB", "BBBBB")))%>%
  # then sum the Yellow votes to get the Public and Private degree of consensus,
  # and count how many blues there were before the first yellow
  rowwise()%>%
  mutate(PublicVoteCount = sum(across(Say_Spkr2:Say_Spkr5, greaterThan10)),
         PrivateVoteCount = sum(across(Think_Spkr2:Think_Spkr5, greaterThan10)),
         # it will be useful to know participants' predictions about how many speakers flipped to Yellow
         # before someone stuck to their guns; 
         # this code says "if the search for a Y returns something, return the index of its position in the string,
         # and subtract 1 to account for Spkr1 to find out which Spkr it was; if the search returns nothing,
         # return the number 4 to show that none of the 4 speakers after Spkr1 said Yellow. 
         blueVotesToFirstYellow = ifelse(is.na(str_match(PublicPattern, "Y"))!=T, str_locate(PublicPattern, "Y")-1, 4)[,1],
         blueBeliefsToFirstYellow = ifelse(is.na(str_match(PrivatePattern, "Y"))!=T, str_locate(PrivatePattern, "Y")-1, 4)[,1],
         # in Exp 1, whether or not Spkr2 flipped was useful; we'll keep that in Exp 3 just for comparison,
         # but more useful will be whether or not Spkr5 flipped, since he was the only initial Yellow, and 
         # his flip may differ by condition
         Spkr2_voteFlip = ifelse(Say_Spkr2 > 10, "Spkr2_VotesYellow", "Spkr2_VotesBlue"),
         Spkr5_voteFlip = ifelse(Say_Spkr5 > 10, "Spkr5_VotesYellow", "Spkr5_VotesBlue"),
         # It will be useful to compare patterns for participant who believed that
         # (1) everyone would flip, (2) no one would flip, or (3) some mixed pattern would flip
         all_or_none = ifelse(PublicPattern == "BBBBB" | PublicPattern == "BBBBY", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("BBBBB", "Mixed", "BBBBY")))%>%
  ungroup()

# Pivot and save a long format data
ASP_Exp3_long<-ASP_Exp3%>%
  pivot_longer(cols = Say_Spkr2:Decision_AccurateChoice,
               names_pattern = "(.*)_(.*)", names_to = c("Measure", "SpkrID"),
               values_to = "Values")%>%
  mutate(Measure = factor(Measure, levels = c("Say", "Think", "Decision"),
                          labels = c('PublicVote', 'PrivateBelief', 'Decision')),
         SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5", "TeamChoice", "AccurateChoice")),
         Spkr5vsAll = ifelse(SpkrID == "Spkr5", "Spkr5", "Spkrs1_4"),
         Spkr5vsAll = factor(Spkr5vsAll, levels = c("Spkr5", "Spkrs1_4")))
  

# Export the cleaned data for Isaac in wide and long forms
# write.csv(ASP_Exp3, "ASP_Exp3_cleanedWide.csv")
# write.csv(ASP_Exp3_long, "ASP_Exp3_cleanedLong.csv")

glimpse(ASP_Exp3)

################################################### Import model fit data
###################################################
ASP_Exp3_modPred <- read_excel("data/24.02.16_ASP_Exp3_data.xlsx", 
                                   sheet = "fromMod_modPredictions")
ASP_Exp3_paramFit <- read_excel("data/24.02.16_ASP_Exp3_data.xlsx", 
                                     sheet = "fromMod_paramFits")

####### FIRST fix the subIDs
# obnoxious code issue in model was easiest to fix by assigning new subIDs by row number, so we include a lookup table to match
# the subIDs we assigned to run the model more easily back to the original subIDs assigned to the raw data
ASP_Exp3_subID_reference <- read_excel("data/24.02.16_ASP_Exp3_data.xlsx", 
                                sheet = "subID_reference")

# fix subID for ASP_Exp3_modPred
ASP_Exp3_modPred<-ASP_Exp3_modPred%>%
  left_join(ASP_Exp3_subID_reference, by=join_by(Participant == Participant_fromModRowNumber))%>%
  mutate(Participant = subID_original)%>%
  select(-subID_original)
# fix subID for ASP_Exp3_paramFit
ASP_Exp3_paramFit<-ASP_Exp3_paramFit%>%
  left_join(ASP_Exp3_subID_reference, by=join_by(Participant == Participant_fromModRowNumber))%>%
  mutate(Participant = subID_original)%>%
  select(-subID_original)

# check what's in the dbs
glimpse(ASP_Exp3_modPred)
glimpse(ASP_Exp3_paramFit)

# merge those tables
ASP_Exp3_modFit<-ASP_Exp3_paramFit%>%
  # to rename the parameters, first pivot wider to make that easier
  pivot_wider(names_from = "Param", values_from = c("Val_fixedModel", "Val_varModel"))%>%
  # now rename them all at once
  rename_with(~str_replace(.,"Val_fixedModel", "fittedParam_fixMod"), contains("Val_fixedModel"))%>%
  rename_with(~str_replace(.,"Val_varModel", "fittedParam_varMod"), contains("Val_varModel"))%>%
  # now merge the parameter fits with the model predictions
  left_join(ASP_Exp3_modPred, by=join_by(Participant == Participant, Condition==Condition))%>%
  # to rename those parameters too, first pivot wider
  pivot_wider(names_from = c("Type", "Agent"), values_from = c("Human", "Model_fixed", "Model_var"))%>%
  # now rename them all at once; add an extra _ to make sure that when we pivot longer there's still one there
  rename_with(~str_replace(.,"Human", "human_"), contains("Human"))%>%
  rename_with(~str_replace(.,"Model_fixed", "fixMod_"), contains("Model_fixed"))%>%
  rename_with(~str_replace(.,"Model_var", "varMod_"), contains("Model_var"))%>%
  # now pivot longer - this will make it easier to insert the Spkr number 
  pivot_longer(cols = starts_with(c("human", "fixMod", "varMod")),
               names_to = c(".value", ".value", "SpkrID"),
               names_pattern = "(.*)_(.*)_(.*)")%>%
  # and create a speaker ID
  mutate(SpkrID = factor(str_c("Spkr", SpkrID), levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")))%>%
  # now also give the "vote" and "think" columns the same name as in the original data
  rename_with(~str_replace(.,"Vote", "PublicVote"), contains("Vote"))%>%
  rename_with(~str_replace(.,"Belief", "PrivateBelief"), contains("Belief"))%>%
  # now we need to pivot back
  pivot_wider(names_from = SpkrID,
              values_from = ends_with(c("Vote", "Belief")))%>%
  # and change Participant to subID, and Condition to PowerLevel, along with making the values match the original data
  rename(PowerLevel = Condition, subID = Participant)%>%
  mutate(PowerLevel = case_when(
    PowerLevel == "Anon" ~ "Anonymous",
    PowerLevel == "Pop" ~ "Popular", 
    TRUE ~ PowerLevel))

# glimpse(ASP_Exp3_modFit)

# now join the model fits to the main data and save it as "augmented"
ASP_Exp3_augmented<-ASP_Exp3%>%
  left_join(ASP_Exp3_modFit, by=join_by(subID == subID, PowerLevel == PowerLevel))%>%
  # divide the ratings for Decision_TeamChoice and Decision_AccurateChoice by 20 so they fit within the same model prediction range 
  mutate(across(starts_with("Decision"), .fns = function(x) x / 20))%>%
  mutate(Spkr2_falseFlip = ifelse(Say_Spkr2 > 10 & Think_Spkr2<11, "Spkr2_flip: Vote only", "Spkr2_flip: both or neither"))%>%
  # drop the Say & Think since we scaled it to fall between 0 and 1 for human_values
  select(-starts_with(c("Say", "Think")))


# Now we'd like to set that data up to compare model predictions to human ratings
ASP_Exp3_augmentedLong<-ASP_Exp3_augmented%>%
  # first drop the model predictions, and pivot the human ratings
  select(-starts_with(c("fixMod", "varMod")))%>%
  pivot_longer(cols = starts_with("human"),
               names_pattern = "(.*)_(.*)_(.*)", 
               names_to = c("humanSource","Measure", "SpkrID"),
               values_to = "human_values")%>%
  # now join the model predictions: first, drop everything but subID, PowerLevel, and the model predictions, then 
  # perform the same pivot as above and join by the shared columns
  left_join(select(ASP_Exp3_augmented, starts_with(c("subID","PowerLevel","fixMod", "varMod")))%>%
              pivot_longer(cols = starts_with(c("fixMod", "varMod")),
                           names_pattern = "(.*)_(.*)_(.*)", 
                           names_to = c("modelSource","Measure", "SpkrID"),
                           values_to = "model_values"),
            by=join_by(subID == subID, PowerLevel == PowerLevel,
                       Measure==Measure, SpkrID==SpkrID))%>%
  relocate(modelSource, .after=humanSource)%>%
  mutate(param_socCon = case_when(modelSource=="fixMod"~ fittedParam_fixMod_socCon,
                                  modelSource=="varMod"~ fittedParam_varMod_socCon, TRUE ~ NA),
         param_infoCon = case_when(modelSource=="fixMod"~ fittedParam_fixMod_infoCon,
                                   modelSource=="varMod"~ fittedParam_varMod_infoCon, TRUE ~ NA),
         param_selfWeight = case_when(modelSource=="fixMod"~ fittedParam_fixMod_selfWeight,
                                      modelSource=="varMod"~ fittedParam_varMod_selfWeight, TRUE ~ NA),
         param_weightAccFav = case_when(modelSource=="fixMod"~ fittedParam_fixMod_weightAccFav,
                                        modelSource=="varMod"~ fittedParam_varMod_weightAccFav, TRUE ~ NA),
         param_power = case_when(modelSource=="fixMod"~ fittedParam_fixMod_power,
                                 modelSource=="varMod"~ fittedParam_varMod_power, TRUE ~ NA))

#### save the varPower mod fits for a visualization
ASP_Exp3_varFits<-ASP_Exp3_augmentedLong%>%
  filter(modelSource == "varMod")%>%
  select(-contains("fittedParam"))%>%
  pivot_wider(names_from = "SpkrID", values_from = c(human_values, model_values))%>%
  mutate(across(model_values_Spkr2:model_values_Spkr5, ~ifelse(.x>.5, "Y", "B"),
                .names = 'modelCode_{col}'),
         across(human_values_Spkr2:human_values_Spkr5, ~ifelse(.x>.5, "Y", "B"),
                .names = 'humanCode_{col}'))%>%
  # then glue together the values in those columns, and add Spkr1's Yellow Vote/Belief
  # to create the pattern of votes (PublicPattern) and beliefs (PrivatePattern)
  unite("modelPattern", modelCode_model_values_Spkr2:modelCode_model_values_Spkr5, sep = "")%>%
  unite("humanPattern", humanCode_human_values_Spkr2:humanCode_human_values_Spkr5, sep = "")%>%
  mutate(modelPattern = as.character(glue("B{modelPattern}")),
         humanPattern = as.character(glue("B{humanPattern}")))%>%
  pivot_longer(cols = starts_with(c("human_values", "model_values")), 
               names_pattern = "(.*)_(Spkr\\d+)",
               names_to = c(".value", "SpkrID"))%>%
  relocate(c(Measure, SpkrID, humanPattern), .before="modelPattern")%>%
  mutate(SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")))%>%
  rename(votingRound = SpkrID)


# save averages for a different visualization
ASP_Exp3_counts<-ASP_Exp3_augmented%>%
  rename(scale01_TeamChoice = Decision_TeamChoice, scale01_AccurateChoice = Decision_AccurateChoice)%>%
  rowwise()%>%
  mutate(across(contains("human_PublicVote_Spkr"),
                ~ . - get(sub("PublicVote", "PrivateBelief", cur_column())),
                .names = "diff_SayThink_{col}"))%>%
  select(-starts_with(c("varMod_", "fixMod")))%>%
  mutate(human_avgVote = mean(c_across(human_PublicVote_Spkr2:human_PublicVote_Spkr5)),
         human_avgBelief = mean(c_across(human_PrivateBelief_Spkr2:human_PrivateBelief_Spkr5)),
         # do people on believe what they say?
         human_avgDiff_voteHigh = mean(c_across(diff_SayThink_human_PublicVote_Spkr2:diff_SayThink_human_PublicVote_Spkr5)),
         # we want to know if the participant endorses the same option they think the team will
         decision_BvY = ifelse(scale01_TeamChoice > .5, "predicted team choice: Yellow", "predicted team choice: Blue"),
         rating_TeamChoice = 20*scale01_TeamChoice,
         rating_AccurateChoice = 20*scale01_AccurateChoice,
         rating_avgVote = 20*human_avgVote,
         rating_avgBelief = 20*human_avgBelief,
         rating_avgDiff_voteHigh = 20*human_avgDiff_voteHigh)%>%
  select(-contains(c("_Spkr", "_PublicVote_", "_PrivateBelief_")))%>%
  pivot_longer(cols = contains(c("fixMod_", "varMod_")),
               names_pattern = "(.*)_(.*)_(.*)", 
               names_to = c("discard","modelSource", "param"),
               values_to = "human_values")%>%
  pivot_wider(names_from = "param", values_from = "human_values")%>%
  # for supplementals: also add the parameter bins and the final vote count
  mutate(wAcc_Bins = case_when(weightAccFav <= .5 ~ "wAcc <= .5", 
                               # param_weightAccFav == .5 ~ "wAcc = .5", 
                               weightAccFav > .5 ~ "wAcc > .5"),
         wSelf_vPower = selfWeight - power,
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr1", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr1"),
         finalVote_YvB = paste0({PublicVoteCount}+1, "v", 5-({PublicVoteCount}+1)))%>%
  drop_na(wSelf_vPower)%>%
  filter(modelSource == "varMod")

# glimpse(ASP_Exp3_counts)
################################################### Import simulation data
###################################################
ASP_Exp3_simParams <- read_excel("data/24.02.16_ASP_Exp3_data.xlsx", 
                                 sheet = "fromMod_simParams")
str(ASP_Exp3_simParams)

# rename columns and create bins
ASP_Exp3_simParams<-ASP_Exp3_simParams%>%
  # rename the columns
  rename(votePattern = `Prior Votes`, 
         infoConformity = InfoCon, 
         socConformity = SocCon,
         selfWeight = SelfWeight, 
         wAcc_SocFav = w_acc, 
         pwrSpkr5 = Spkr5_power,
         beliefY_initial = Init_Y_belief, 
         beliefY_updated = Updated_Y_belief, 
         voteY_prob = Y_vote_prob)%>%
  mutate(votingRound = paste0("Spkr", {str_length(votePattern)}+1),
         voteCategories = ifelse(votePattern %in% c("B", "BB", "BY", "BYYYY", "BBBBY"), votePattern,
                                 glue("sumY_{str_count(votePattern, 'Y')}")),
         voteCascade = factor(voteCategories, levels = c("BBBBB", "sumY_3", "sumY_2",  "BB", "B", "BY", "sumY_1", "BBBBY")),
         pwrSpkr5_BINS = case_when(pwrSpkr5 %in% c(0,1) ~ "Power: 0, 1",
                                   pwrSpkr5 %in% c(2,3) ~ "Power: 2,3",
                                   pwrSpkr5 %in% c(4, 5) ~ "Power: 4, 5",
                                   pwrSpkr5 > 5 ~ "Power: 6-10"),
         Spkr2_voteFlip = case_when(str_detect(votePattern, "^.Y") | (votingRound == "Spkr2" & voteY_prob >=.5) ~ "Spkr2_VoteYellow", 
                                    TRUE ~ "Spkr2_VoteBlue"),
         # Spkr5_voteFlip = case_when(str_detect(votePattern, "....Y$") ~ "Spkr2_VoteYellow",
         #                            str_detect(votePattern, "....B$") ~ "Spkr2_VoteBlue",
         #                            TRUE ~ "earlyround"),
         wAcc_Bins = case_when(wAcc_SocFav <= .5 ~ "wAcc <= .5", 
                               # wAcc_SocFav == .5 ~ "wAcc = .5", 
                               wAcc_SocFav > .5 ~ "wAcc > .5"),
         wSelf_vPower = selfWeight - pwrSpkr5,
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr5", 
                                       # wSelf_vPower == 0 ~ "self = Spkr5",
                                       wSelf_vPower > 0 ~ "self > Spkr5"))%>%
  relocate(c(infoConformity, socConformity, pwrSpkr5, pwrSpkr5_BINS, wAcc_SocFav, wAcc_Bins,
             selfWeight, wSelf_vPower, wSelf_vPower_Bins, Spkr2_voteFlip,
             votingRound, votePattern, voteCategories, voteCascade, beliefY_initial, beliefY_updated), 
           .before="voteY_prob")



########### plot sim data in 4 regions
# 
ASP_Exp3_simParams%>%
  mutate(infoConformity = factor(infoConformity),
         socConformity = factor(socConformity))%>%
  # filter(Spkr2_voteFlip == "Spkr2_VoteBlue")%>%
  ggplot(aes(x=beliefY_updated, y=voteY_prob, color=Spkr2_voteFlip))+
  geom_jitter(size=1.5, alpha=.05, width=.01, height=.01,
              aes(color=Spkr2_voteFlip, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_abline(intercept = 0, slope=1, color="black")+
  geom_hline(yintercept=.5, linetype = "dashed")+
  geom_vline(xintercept=.5, linetype = "dashed")+
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), labels = c(0.0, .5, 1.0))+
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c(0.0, .5, 1.0))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  theme_cleanPub+
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        legend.title = element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        legend.title.position = "top")+
  facet_grid2(wSelf_vPower_Bins~wAcc_Bins+votingRound, strip = strip_nested(size = "variable"))+
  ggtitle("Exp 3: simulation data for wAcc & wSelf_vPower",
          subtitle = "model thinks Spkr2 is **guaranteed** to vote Blue. But even if Spkr2 were to vote Yellow, 
          <br> Spkr3 would still be facing a split B-Y vote, and would still be almost certain to vote Blue.
          <br> So, the only reason there's a big Yellow cluster for Spkr4 is that the simulation includes 
          <br> the very unlikely counterfactual where both Spkr2 AND Spkr3 vote Yellow; and even then, a B-Y-Y pattern
          <br> is enough that some Spkr4 will vote Yellow w/o the extra Spkr1 weight, and that means Spkr5 will too")


#################################################################################################
##########################           Summary / Patchwork Figure        ##########################
#################################################################################################


### What do people think the most common patterns are?
ASP_Exp3_PlotA<-ASP_Exp3%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.
  mutate(all_or_none = ifelse(Values == "BBBBB" | Values == "BBBBY", as.character(Values),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("BBBBB", "Mixed", "BBBBY")),
         # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
         Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
         Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()%>%
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count),
                # label=paste0(after_stat(count),"\n(", scales::percent(after_stat(count)/120, accuracy=1),")"),
                y=after_stat(3.0)),
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=121", "Pop\n n=120"), x = c(2.5, 2.5), y = c(90,90),
                         PowerLevel   = c("Anonymous", "Popular"),
                         Measure = c("PublicVote", "PrivateBelief")),
             aes(x = x, y = y, label = label), fill="white")+
  scale_y_continuous("count", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  # scale_y_continuous("count ", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  labs(x="inferred judgment for each speaker")+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  theme_cleanPub+
  scale_x_discrete(drop=F)+
  facet_grid(PowerLevel~"Measure")+
  labs(tag = "5a")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none")+
  ggtitle("Experiment 3: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")


## Display by-subject and averages for votes and beliefs
ASP_Exp3_PlotB<-ASP_Exp3_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "BBBBB" | PublicPattern == "BBBBY", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("BBBBB", "Mixed", "BBBBY")))%>%
  ggplot(aes(x=as.numeric(SpkrID)+1, y=Values, color=Measure))+
  geom_beeswarm(size=3, cex=.75, alpha=.5)+ # dodge.width=1, 
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=interaction(Measure,  SpkrID)), 
               position=position_dodge(width=1), linewidth=1.15, width=.30, color="black")+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(after_stat(y), 2),  y=Values, group=interaction(Measure, SpkrID), fill=Measure), 
               position=position_dodge(width=1), color="black", size=4.5)+
  scale_y_continuous("Inferred Propensity to Endorse Yellow", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_x_continuous("Speaker Number", breaks = c(2,3,4,5), labels = c('Spkr2', 'Spkr3', 'Spkr4', 'Spkr5'))+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~all_or_none, scales = "free_x",
             labeller = labeller(all_or_none = as_labeller(c('BBBBB' = "Public Votes: BBBBB",
                                                             'Mixed' = "Public Votes: Mixed",
                                                             'BBBBY' = "Public Votes: BBBBY"))))+
  theme_cleanPub+
  labs(tag = "5b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom")+
  ggtitle("Experiment 3: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")


######### team choice & accuracy as a function of predicted flips
ASP_Exp3_PlotC<-ASP_Exp3_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, 
                  # shape=Spkr2_voteFlip,
                  shape=Spkr5_voteFlip,
                  # shape=PowerLevel, 
                  stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("best design according to participant",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue\nBetter (1)",  "Uncertain\n(10.5)", "Yellow\nBetter (20)")
  )+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY),
              se = T, alpha=.15, linetype=0)+
  stat_smooth (geom="line", method="lm", alpha=0.5, size=1, span=0.5, aes(color=decision_BvY, group=decision_BvY)) +
  geom_smooth(method="lm", color="black", linetype="dashed")+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"trust majority")+
  theme_cleanPub+
  labs(tag = "5c")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=90, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        # legend.position = "bottom",
        legend.position = "inside",
        legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 3: Target Speaks Last",
          subtitle = "vote-belief gaps versus participant judgment,
          <br>depending on predicted final team choice")


#### Patchwork Plot
(ASP_Exp3_PlotA+ASP_Exp3_PlotB+ASP_Exp3_PlotC)+
  plot_layout(ncol=3, widths = c(1, 3, 1),
              guides = 'collect')+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 3: Target Speaks Last')



#################################################################################################
##########################          Figures for Model Fit              ##########################
#################################################################################################

# This figure is mainly useful as a comparison to the analogous figure for Exp 1, so will only appear in the supplemental materials

##############################################
############################################## Fig 6a: model fits
##############################################
paramFig6A<-ASP_Exp3_augmentedLong%>%
  drop_na(model_values)%>%
  # filter(modelSource == "varMod")%>%
  mutate(SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")),
         wAcc_Bins2 = ifelse(param_weightAccFav > .5, "wAcc more (>.5)", "socFav more (<.5)"),
         modelSource = case_when(modelSource == "fixMod" ~ "fixPower", 
                                 modelSource == "varMod" ~ "varPower"))%>%
  select(subID, modelSource, PowerLevel, Measure, SpkrID, wAcc_Bins2, model_values, human_values)%>%
  ggplot(aes(x=model_values, y=human_values, color=SpkrID))+
  # geom_line(aes(group=subID), color="black", alpha=.3)+
  geom_point(size=1.5, alpha=.5, position=position_jitter(width=.005, height=.005))+
  geom_abline(intercept = 0, slope=1, linetype="dashed")+
  scale_x_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  scale_y_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  geom_hline(yintercept=.5, linetype="dashed")+
  geom_vline(xintercept=.5, linetype="dashed")+
  geom_smooth(method="lm")+
  scale_color_manual(values = c("#00FDFF", "#00A4FF","#0C00FF", "#9600FF"))+
  labs(tag = "A")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"))+
  facet_grid2(Measure~modelSource+PowerLevel, strip = strip_nested(size = "variable"))+
  ggtitle("",
          subtitle = "model fits")
paramFig6A
##############################################
############################################## Fig 6b: model parameter distributions
##############################################
paramFig6B<-ASP_Exp3_augmented%>%
  pivot_longer(cols = starts_with("fittedParam"),
               names_pattern = "(.*)_(.*)_(.*)", names_to = c("discard", "modType", "paramName"),
               values_to = "param_values")%>%
  mutate(paramName = factor(paramName, levels = c("power","weightAccFav", "selfWeight","infoCon", "socCon")),
         modType = case_when(modType == "fixMod" ~ "fixPower", 
                             modType == "varMod" ~ "varPower"))%>%
  select(-discard)%>%
  ggplot(aes(x=modType, y=param_values, fill=paramName,alpha=modType))+
  geom_boxplot()+
  scale_alpha_manual(values=c(.2, 1))+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", position=position_dodge(width=1), size=2, width=.10, color="black",
               aes(y=param_values, group=interaction(modType,  paramName, PowerLevel)))+
  stat_summary(fun ="mean", geom="label", position=position_dodge(width=1), color="black", fill="gray", size=6,
               aes(label=round(..y.., 2),  y=param_values, group=interaction(modType, paramName, PowerLevel)))+
  paletteer::scale_fill_paletteer_d("MetBrewer::Lakota")+
  labs(tag = "B")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        legend.position = "none",
        strip.background = element_rect(fill="gray82", color="black"))+
  facet_grid2(~paramName+PowerLevel, scales="free",independent = "y",
              strip = strip_nested(size = "variable"), 
              labeller = labeller(paramName = as_labeller(c('infoCon' = "infoConformity: \n*higher = consensus implies accuracy*",
                                                            'power' = "PowerLevel: \n*number of 'votes' given to 5th Speaker*",
                                                            'selfWeight' = "selfWeight: \n*higher = trust self more than others*",
                                                            'socCon' = "socConformity: \n*higher = will vote conformist despite beliefs*",
                                                            'weightAccFav' = "socFav_vsAcc: \n*>.5 = values acc more than social favor*"))))+
  facetted_pos_scales(
    y = list(
      paramName == "power" ~ scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)),
      paramName == "weightAccFav" ~ scale_y_continuous(breaks = c(0,.25, .5, .75, 1)),
      paramName == "selfWeight" ~ scale_y_continuous(breaks = c(1,2,3,4,5)),
      paramName == "infoCon" ~ scale_y_continuous(breaks = c(1,2,3,4,5)),
      paramName == "socCon" ~ scale_y_continuous(breaks = c(1,2,3,4,5))))+
  ggtitle("",
          subtitle = "parameter distributions expected to be identical: speaking last nullifies powerful agent's influence")

paramFig6B
##############################################
############################################## Fig 6c: parameter space in 4 regions
##############################################

# we want to shade the lower diagonal of each plot to separate vote_Y > belief_Y & vice versa  
triangle_data_Exp3 <- data.frame(
  x = c(0, 1, 1),
  y = c(0, 0, 1))
facet_data<-expand.grid(
  wSelf_vPower_Bins = c("self <= Spkr5", "self > Spkr5"),
  wAcc_Bins = c("wAcc <= .5", "wAcc > .5"),
  votingRound = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")
)
triangle_data_Exp3 <- merge(facet_data, triangle_data_Exp3)
triangle_data_Exp3

glimpse(ASP_Exp3_varFits)
# now make the figure 
paramFig6C.alt<-ASP_Exp3_varFits%>%
  drop_na(model_values)%>%
  select(-humanPattern, -modelPattern)%>%
  pivot_wider(id_cols = c(subID, Spkr2_voteFlip, votingRound, contains(c("param", "PowerLevel", "_false"))), 
              names_from = Measure, values_from = model_values)%>%
  mutate(wSelf_vPower = param_selfWeight - param_power)%>%
  rename(model_beliefs = PrivateBelief, model_votes = PublicVote)%>%
  mutate(wAcc_Bins = case_when(param_weightAccFav <= .5 ~ "wAcc <= .5", 
                               # param_weightAccFav == .5 ~ "wAcc = .5", 
                               param_weightAccFav > .5 ~ "wAcc > .5"),
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr5", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr5"))%>%
  ggplot(aes(x=model_beliefs, y=model_votes))+
  geom_polygon(data = triangle_data_Exp3, aes(x = x, y = y, group = interaction(wSelf_vPower_Bins, wAcc_Bins, votingRound)), 
               fill = "lightgrey", alpha = 0.5)+
  geom_abline(intercept = 0, slope=1, color="black")+
  geom_hline(yintercept=.5, linetype = "dashed")+
  geom_vline(xintercept=.5, linetype = "dashed")+
  geom_point(stroke=1.5,size=1, alpha=.5, position=position_jitter(width=0.01, height=0.01), 
             # could substitute shape=PowerLevel, but it's visually busy & Spkr2_voteFlip is the more general pattern
             aes(color=Spkr2_voteFlip, shape=Spkr2_voteFlip))+ 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), limits=c(-0.05,1.05), labels = c(0.0, .5, 1.0))+
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), limits=c(-0.05,1.05), labels = c(0.0, .5, 1.0))+
  scale_shape_manual(values=c(4,19))+ 
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        legend.title = element_blank(),
        # legend.title = element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        legend.title.position = "top")+
  facet_grid2(wSelf_vPower_Bins~wAcc_Bins+votingRound, strip = strip_nested(size = "variable"))+
  labs(tag = "C")+
  ggtitle("",
          subtitle = "4 regions of parameter space (for comparison to Exp 1)")

paramFig6C.alt

##############################################
############################################## Patchwork Fig 6
##############################################

(paramFig6A|paramFig6C.alt)/paramFig6B+
  # plot_layout(ncol=5, widths = c(2, 3, 5),
  #             guides = 'collect')
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 3: model fits')


##########################################################################################################
##############################           COUNTS            ###############################################
##########################################################################################################

# how many flips did participants in each condition predict? 
# Anon: n=39, but only 5 expected more than 1 flip 
# Pop: n=57, but only 10 expected more than 1 flip
ASP_Exp3%>%
  group_by(PowerLevel, PublicVoteCount)%>%
  tally()%>%
  pivot_wider(names_from = "PublicVoteCount", values_from = "n", 
              names_prefix="flip_", values_fill=0, names_sort=T)%>%
  mutate(Total_flips = sum(c_across(flip_1:flip_4)),
         Total_cond=sum(c_across(flip_0:flip_4)))%>%
  adorn_totals(c("row"))

# very few Spkr2 flips, obviously
ASP_Exp3%>%
  rowwise()%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicPattern, starts_with("Say"))%>%
  group_by(PowerLevel, Spkr2_voteFlip)%>%
  tally()%>%
  pivot_wider(id_cols = PowerLevel, 
              names_from = "Spkr2_voteFlip",
              values_from = "n", values_fill = 0)%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  adorn_totals(c("row"))%>%
  mutate(percentYellow = Spkr2_VotesYellow / Total)

# more Spkr5 Vote flips in Anon than Pop
# Anon: 68% flips
# Pop: 53% flips
ASP_Exp3%>%
  rowwise()%>%
  select(subID, PowerLevel, Spkr5_voteFlip, PublicPattern, starts_with("Say"))%>%
  group_by(PowerLevel, Spkr5_voteFlip)%>%
  tally()%>%
  pivot_wider(id_cols = PowerLevel, 
              names_from = "Spkr5_voteFlip",
              values_from = "n", values_fill = 0)%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  adorn_totals(c("row"))%>%
  mutate(percentBlue = 1-(Spkr5_VotesYellow / Total))


# almost no one expects the **team** to flip to Yellow
# Anon: 5 of 121
# Pop: 18 of 120
ASP_Exp3%>%
  mutate(decision_BvY = ifelse(Decision_TeamChoice > 10, "predicted choice: Yellow", "predicted choice: Blue"))%>%
  group_by(PowerLevel, decision_BvY)%>%
  tally()

# what percentage expected ALL informants to continue to privately believe Blue? 
# ANS: 90.9%
ASP_Exp3%>%
  rowwise()%>%
  select(subID, PowerLevel, PrivatePattern)%>%
  mutate(thinkYellow_count = str_count(str_remove(PrivatePattern, "Y$"), "Y"))%>%
  group_by(PowerLevel, thinkYellow_count)%>%
  tally()%>%
  pivot_wider(id_cols = PowerLevel, 
              names_from = "thinkYellow_count",
              values_from = "n", names_prefix = "thinkY_")%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  adorn_totals(c("row"))%>%
  mutate(percentYellow = thinkY_0 / Total)


#################################################################################################
################################           STATISTICS             ###############################
#################################################################################################

#### were Spkr2-4's public & private judgments equally strong for Blue? 
#### and did Spkr5's judgments differ by PowerLevel or public vs private differ?
ASP_Exp3_resultMain<-ASP_Exp3_long%>%
  filter(Measure %in% c("PublicVote", "PrivateBelief"))%>%
  mutate(Measure = fct_relevel(Measure, "PrivateBelief", "PublicVote"),
         PowerLevel = fct_relevel(PowerLevel, "Popular", "Anonymous"))%>%
  group_by(Spkr5vsAll)%>%
  nest()%>%
  mutate(mod1 = map(data, ~tidy(lmer(Values-10.5~Measure*PowerLevel+(1|subID:SpkrID), data=.), conf.int=T)))%>%
  unnest(mod1)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))%>%
  select(-data, -effect, -group)

ASP_Exp3_resultMain


#### how were predictions about public & private judgments related to 
#### inferences about (A) which design the team would choose and (B) which was better?

# first wrangle data shape for the analysis
ASP_Exp3_df1_choiceTrust<-ASP_Exp3_counts%>%
  mutate(PublicVoteCount_maj = PublicVoteCount-2,
         scale01_TeamChoice_center = scale01_TeamChoice-.5,
         scale01_AccChoice_center = scale01_AccurateChoice-.5,
         rating_TeamChoice_center = rating_TeamChoice-10.5,
         rating_AccChoice_center = rating_AccurateChoice-10.5,
         decision_BvY = fct_relevel(decision_BvY, "predicted team choice: Blue", "predicted team choice: Yellow"),
         decision_BvY = fct_relabel(decision_BvY, ~gsub("predicted team choice: ", "choice:", .x)))%>%
  pivot_longer(cols = c("rating_TeamChoice_center", "rating_AccChoice_center"), 
               names_to = "measure", values_to = "ratings_center")%>%
  mutate(testGroup = case_when(str_detect(measure, "TeamChoice")==T & PowerLevel == "Popular" ~ "teamChoice_Pop",
                               str_detect(measure, "TeamChoice")==T & PowerLevel == "Anonymous" ~ "teamChoice_Anon",
                               str_detect(measure, "AccChoice")==T ~ "accChoice_collapse", 
                               TRUE~ "other"),
         testGroup = factor(testGroup, levels = c("teamChoice_Pop", "teamChoice_Anon", "accChoice_collapse")),
         PowerLevel = factor(PowerLevel, levels = c("Anonymous", "Popular")))%>%
  rename(avgDiff=rating_avgDiff_voteHigh)

# and nest it 
ASP_Exp3_result_choiceTrust<-ASP_Exp3_df1_choiceTrust%>%
  group_by(testGroup, PowerLevel)%>%
  nest()%>%
  mutate(mod_diff = case_when(str_detect(testGroup, "teamChoice") ~ 
                                map(data, 
                                    ~tidy(lm(ratings_center~scale(PublicVoteCount_maj), data=.), conf.int=T)),
                              str_detect(testGroup, "accChoice") ~ 
                                map(data, "no test, will collapse PowerLevel")),
         # for accAchoice, check which design they trust in each condition
         # for teamChoice, confirm that ttest gives same result as intercept for mod_diff
         mod_ttest = case_when(str_detect(testGroup, "teamChoice") ~ 
                                 map(data, ~tidy(t.test(ratings_center ~ 1, data=.), conf.int=T)),
                               str_detect(testGroup, "accChoice") ~ 
                                 map(data, ~tidy(t.test(ratings_center ~ 1, data=.), conf.int=T))))

#### now show the choice-trust analyses:

## team choice related to majority vote
ASP_Exp3_result_choiceTrust%>%
  unnest(mod_diff)%>%
  filter(testGroup %in% c("teamChoice_Pop", "teamChoice_Anon"))%>%
  select(-data, -mod_ttest)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))

## but people trust the pre-sequence consensus 
ASP_Exp3_result_choiceTrust%>%
  unnest(mod_ttest)%>%
  filter(testGroup=="accChoice_collapse")%>%
  select(-data, -mod_diff)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))

# But very few in either PowerLevel condition expected the team's final choice to be Yellow 
# Pop: 102 of 120
# Anon: 116 of 121  
ASP_Exp3_df1_choiceTrust%>%
  filter(testGroup=="accChoice_collapse")%>%
  group_by(PowerLevel, decision_BvY)%>%
  tally()

# since there was no effect of PowerLevel on trust & so few expected a Yellow final decision, 
# collapse across PowerLevel before testing greater gaps in avgDiff affected trust for either decision_BvY group
ASP_Exp3_df1_choiceTrust%>%
  lm(ratings_center ~ avgDiff*decision_BvY, data=.)%>%
  tidy(conf.int=T)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))


























