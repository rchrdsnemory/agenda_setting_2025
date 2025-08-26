#########################################################################################################################
######################        ASP  Exp 1 - Target Speaks First            ###############################################
#########################################################################################################################
## Load packages
library(tidyverse)
library(glue)
library(ggeffects)
library(ggh4x)
library(ggtext)
library(lmerTest)
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
#                                       )

#########################################################################################################################
#######################################              IMPORT DATA          ###############################################
#########################################################################################################################

################################################### Import response data
###################################################
ASP_Exp1 <- read_excel("data/24.02.12_ASP_Exp1_data.xlsx", 
                       sheet = "RData")
str(ASP_Exp1)


# Check exclusions:
# 40 ppl failed basic comprehension and weren't allowed to do experiment 
# but only 3 others failed post-attn check and / or had bot-like verbal explanations 
ASP_Exp1%>%
  group_by(exclude_preScreen, exclude_forAnalysis)%>%
  summarise(Totals=n())%>%
  adorn_totals()

# filter out exclusions
ASP_Exp1<-ASP_Exp1%>%
  filter(exclude_forAnalysis == 0)%>%
  select(-starts_with("Explain"), -Comments)%>%
  relocate(starts_with("Think_"), .after = "Say_Spkr5")

# sample size by condition after exclusions: Anon = 120, Pop=118 
ASP_Exp1%>%
  group_by(PowerLevel)%>%
  tally()

## create this function so that we can use it to compute the sum of Y v B votes:
greaterThan10<-function(x) ifelse(x>10, 1, 0)

##### create variables of interest
ASP_Exp1<-ASP_Exp1%>%
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
  mutate(PublicPattern = glue("Y{PublicPattern}"),
         PrivatePattern = glue("Y{PrivatePattern}"))%>%
  # save those patterns as factors
  mutate(PublicPattern = factor(PublicPattern, levels = c("YYYYY", "YYYYB", "YYYBY", "YYBYY", "YBYYY",
                                                          "YYYBB","YYBYB", "YYBBY", "YBYYB", "YBYBY", "YBBYY",
                                                          "YYBBB", "YBYBB", "YBBYB", "YBBBY", "YBBBB")),
         PrivatePattern = factor(PrivatePattern, levels = c("YYYYY", "YYYYB", "YYYBY", "YYBYY", "YBYYY",
                                                            "YYYBB","YYBYB", "YYBBY", "YBYYB", "YBYBY", "YBBYY",
                                                            "YYBBB", "YBYBB", "YBBYB", "YBBBY", "YBBBB")))%>%
  # then sum the Yellow votes to get the Public and Private degree of consensus,
  # and count how many blues there were before the first yellow
  rowwise()%>%
  mutate(PublicVoteCount = sum(across(Say_Spkr2:Say_Spkr5, greaterThan10)),
         PrivateVoteCount = sum(across(Think_Spkr2:Think_Spkr5, greaterThan10)),
         # it will be useful to know participants' predictions about how many speakers flipped to Yellow
         # before someone stuck to their guns
         yellowVotesToFirstBlue = ifelse(is.na(str_match(PublicPattern, "B"))!=T, str_locate(PublicPattern, "B")-1, 4)[,1],
         yellowBeliefsToFirstBlue = ifelse(is.na(str_match(PrivatePattern, "B"))!=T, str_locate(PrivatePattern, "B")-1, 4)[,1],
         # It will be useful to compare cases where participants predicted that Spkr2 would flip
         # to cases where they predicted Spkr2 would not, regardless of how many subsequent voters flipped
         Spkr2_voteFlip = ifelse(yellowVotesToFirstBlue == 1, "Spkr2_VotesBlue", "Spkr2_VotesYellow"),
         # It will be useful to compare patterns for participant who believed that
         # (1) everyone would flip, (2) no one would flip, or (3) some mixed pattern would flip
         all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YYYYY", "Mixed", "YBBBB")))%>%
  ungroup()
# check output
ASP_Exp1

# Pivot and save a long format data
ASP_Exp1_long<-ASP_Exp1%>%
  pivot_longer(cols = Say_Spkr2:Decision_AccurateChoice,
               names_pattern = "(.*)_(.*)", names_to = c("Measure", "SpkrID"),
               values_to = "Values")%>%
  mutate(Measure = factor(Measure, levels = c("Say", "Think", "Decision"),
                          labels = c('PublicVote', 'PrivateBelief', 'Decision')),
         SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5", "TeamChoice", "AccurateChoice")))
  
glimpse(ASP_Exp1)
# Export the cleaned data for modeling in wide and long forms
# write.csv(ASP_Exp1, "ASP_Exp1_cleanedWide.csv")
# write.csv(ASP_Exp1_long, "ASP_Exp1_cleanedLong.csv")


################################################### Import model fit data
###################################################
ASP_Exp1_modPred <- read_excel("data/24.02.12_ASP_Exp1_data.xlsx", 
                                   sheet = "fromMod_modPredictions")
ASP_Exp1_paramFit <- read_excel("data/24.02.12_ASP_Exp1_data.xlsx", 
                                     sheet = "fromMod_paramFits")

# check what's in the files
glimpse(ASP_Exp1_modPred)
glimpse(ASP_Exp1_paramFit)

# standardize column names and merge those two datasheets
ASP_Exp1_modFit<-ASP_Exp1_paramFit%>%
  # to rename the parameters, first pivot wider to make that easier
  pivot_wider(names_from = "Param", values_from = c("Val_fixedModel", "Val_varModel"))%>%
  # now rename them all at once
  rename_with(~str_replace(.,"Val_fixedModel", "fittedParam_fixMod"), contains("Val_fixedModel"))%>%
  rename_with(~str_replace(.,"Val_varModel", "fittedParam_varMod"), contains("Val_varModel"))%>%
  # now merge the parameter fits with the model predictions
  left_join(ASP_Exp1_modPred, by=join_by(Participant == Participant, Condition==Condition))%>%
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
  rename_with(~str_replace(.,"vote", "PublicVote"), contains("vote"))%>%
  rename_with(~str_replace(.,"think", "PrivateBelief"), contains("think"))%>%
  # now we need to pivot back
  pivot_wider(names_from = SpkrID,
              values_from = ends_with(c("Vote", "Belief")))%>%
  # and change Participant to subID, and Condition to PowerLevel, along with making the values match the original data
  rename(PowerLevel = Condition, subID = Participant)%>%
  mutate(PowerLevel = case_when(
    PowerLevel == "anon" ~ "Anonymous",
    PowerLevel == "pop" ~ "Popular", 
    TRUE ~ PowerLevel))

# now join the model fits to the main data and save it as "augmented"
ASP_Exp1_augmented<-ASP_Exp1%>%
  left_join(ASP_Exp1_modFit, by=join_by(subID == subID, PowerLevel == PowerLevel))%>%
  # divide the ratings for Decision_TeamChoice and Decision_AccurateChoice by 20 so they fit within the same model prediction range 
  mutate(across(starts_with("Decision"), .fns = function(x) x / 20))%>%
  mutate(Spkr2_falseFlip = ifelse(Say_Spkr2 > 10 & Think_Spkr2<11, "Spkr2_flip: Vote only", "Spkr2_flip: both or neither"))%>%
  # drop the Say & Think since we scaled it to fall between 0 and 1 for human_values
  select(-starts_with(c("Say", "Think")))
  
# glimpse(ASP_Exp1_augmented)

# Now we'd like to set that data up to compare model predictions to human ratings
ASP_Exp1_augmentedLong<-ASP_Exp1_augmented%>%
  # first drop the model predictions, and pivot the human ratings
  select(-starts_with(c("fixMod", "varMod")))%>%
  pivot_longer(cols = starts_with("human"),
               names_pattern = "(.*)_(.*)_(.*)", 
               names_to = c("humanSource","Measure", "SpkrID"),
               values_to = "human_values")%>%
  # now join the model predictions: first, drop everything but subID, PowerLevel, and the model predictions, then 
  # perform the same pivot as above and join by the shared columns
  left_join(select(ASP_Exp1_augmented, starts_with(c("subID","PowerLevel","fixMod", "varMod")))%>%
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
ASP_Exp1_varFits<-ASP_Exp1_augmentedLong%>%
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
  mutate(modelPattern = as.character(glue("Y{modelPattern}")),
         humanPattern = as.character(glue("Y{humanPattern}")))%>%
  pivot_longer(cols = starts_with(c("human_values", "model_values")), 
               names_pattern = "(.*)_(Spkr\\d+)",
               names_to = c(".value", "SpkrID"))%>%
  relocate(c(Measure, SpkrID, humanPattern), .before="modelPattern")%>%
  mutate(SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")))%>%
  rename(votingRound = SpkrID)
str(ASP_Exp1_varFits)
glimpse(ASP_Exp1_augmented)

# For a visualization: save average informant judgments as both 0-1 and 1-20, and save parameter bins
ASP_Exp1_counts<-ASP_Exp1_augmented%>%
  rowwise()%>%
  rename(scale01_TeamChoice = Decision_TeamChoice, scale01_AccurateChoice = Decision_AccurateChoice)%>%
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

################################################### Import simulation data
###################################################
ASP_Exp1_simParams <- read_excel("data/24.02.12_ASP_Exp1_data.xlsx", 
                               sheet = "fromMod_simParams")
str(ASP_Exp1_simParams)

# rename columns and create bins
ASP_Exp1_simParams<-ASP_Exp1_simParams%>%
  # rename the columns
  rename(votePattern = `Prior Votes`, 
         infoConformity = InfoCon, 
         socConformity = SocCon,
         selfWeight = SelfWeight, 
         wAcc_SocFav = w_acc, 
         pwrSpkr1 = Spkr1_power,
         beliefY_initial = Init_Y_belief, 
         beliefY_updated = Updated_Y_belief, 
         voteY_prob = Y_vote_prob)%>%
  mutate(votingRound = paste0("Spkr", {str_length(votePattern)}+1),
         voteCategories = ifelse(votePattern %in% c("Y", "YY", "YB", "YYYYY", "YBBBB"), votePattern,
                                 glue("sumY_{str_count(votePattern, 'Y')}")),
         voteCascade = factor(voteCategories, levels = c("YYYYY", "sumY_3", "sumY_2",  "YY", "Y", "YB", "sumY_1", "YBBBB")),
         pwrSpkr1_BINS = case_when(pwrSpkr1 %in% c(0,1) ~ "Power: 0, 1",
                                   pwrSpkr1 %in% c(2,3) ~ "Power: 2,3",
                                   pwrSpkr1 %in% c(4, 5) ~ "Power: 4, 5",
                                   pwrSpkr1 > 5 ~ "Power: 6-10"),
         Spkr2_voteFlip = case_when(str_detect(votePattern, "^.Y") | (votingRound == "Spkr2" & voteY_prob >=.5) ~ "Spkr2_VoteYellow", 
                                    TRUE ~ "Spkr2_VoteBlue"),
         wAcc_Bins = case_when(wAcc_SocFav <= .5 ~ "wAcc <= .5", 
                               # wAcc_SocFav == .5 ~ "wAcc = .5", 
                               wAcc_SocFav > .5 ~ "wAcc > .5"),
         wSelf_vPower = selfWeight - pwrSpkr1,
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr1", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr1"))%>%
  relocate(c(infoConformity, socConformity, pwrSpkr1, pwrSpkr1_BINS, wAcc_SocFav, wAcc_Bins,
             selfWeight, wSelf_vPower, wSelf_vPower_Bins,  Spkr2_voteFlip,
             votingRound, votePattern, voteCategories, voteCascade, beliefY_initial, beliefY_updated), 
           .before="voteY_prob")


########### plot sim data in 4 regions
# 
ASP_Exp1_simParams%>%
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
        strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
        legend.title = element_blank())+
  facet_grid2(wSelf_vPower_Bins~wAcc_Bins+votingRound, strip = strip_nested(size = "variable"))+
  ggtitle("Exp 1: 'counterfactual' simulation of parameter space when dissenter speaks last, in 4 key regions",
          subtitle = "NOTE: all vote patterns are simulated each round for each combination of parameters,  even if they didn't occur under that combination of parameters
          <br> (e.g., Spkr2 never votes Blue when wAcc<=.5 & self<=Spkr1, but the simulation still includes a case to estimate Spkr3's vote 'had Spkr2 voted Blue')")


#################################################################################################
##########################           Summary / Patchwork Figure        ##########################
#################################################################################################

### What do people think the most common patterns are?
ASP_Exp1_PlotA<-ASP_Exp1%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.  
  mutate(all_or_none = ifelse(Values == "YYYYY" | Values == "YBBBB", as.character(Values),  "Mixed"),
       all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")),
       # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
       Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
       Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count),
                # label=paste0(after_stat(count),"\n(", scales::percent(after_stat(count)/120, accuracy=1),")"),
                y=after_stat(3.0)),
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=120", "Pop\n n=118"), x = c(3, 3), y = c(90,90),
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
  labs(tag = "2a")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none")+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")

## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotB<-ASP_Exp1_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")))%>%
  ggplot(aes(x=as.numeric(SpkrID)+1, y=Values, color=Measure))+
  geom_beeswarm(size=3, cex=.75, alpha=.5)+ # dodge.width=1, 
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=interaction(Measure,  SpkrID)), 
               position=position_dodge(width=1), size=1.15, width=.30, color="black")+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(..y.., 2),  y=Values, group=interaction(Measure, SpkrID), fill=Measure), 
               position=position_dodge(width=1), color="black", size=4.5)+
  scale_y_continuous("Inferred Propensity to Endorse Yellow", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_x_continuous("Speaker Number", breaks = c(2,3,4,5), labels = c('Spkr2', 'Spkr3', 'Spkr4', 'Spkr5'))+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~all_or_none, scales = "free_x",
             labeller = labeller(all_or_none = as_labeller(c('YBBBB' = "Public Votes: YBBBB",
                                                           'Mixed' = "Public Votes: Mixed",
                                                           'YYYYY' = "Public Votes: YYYYY"))))+
  theme_cleanPub+
  labs(tag = "2b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")


  
######### team choice & accuracy as a function of predicted flips
ASP_Exp1_PlotC<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
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
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"trust majority")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=90, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        # legend.position = "bottom",
        legend.position = "inside",
        legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "vote-belief gaps versus participant judgment,
          <br>depending on predicted final team choice")



#### Patchwork Plot
(ASP_Exp1_PlotA+ASP_Exp1_PlotB+ASP_Exp1_PlotC)+
  plot_layout(ncol=3, widths = c(1, 3, 1),
              guides = 'collect')+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Target Speaks First')


#################################################################################################
##########################          Figures for Model Fit              ##########################
#################################################################################################


##############################################
############################################## Fig 3a: model fits
##############################################
paramFig3A<-ASP_Exp1_augmentedLong%>%
  drop_na(model_values)%>%
  # filter(modelSource == "varMod")%>%
  mutate(SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")),
         wAcc_Bins2 = ifelse(param_weightAccFav > .5, "wAcc more (>.5)", "socFav more (<.5)"),
         modelSource = case_when(modelSource == "fixMod" ~ "fixPower", 
                                 modelSource == "varMod" ~ "varPower"))%>%
  select(subID, modelSource, PowerLevel, Measure, SpkrID, wAcc_Bins2, model_values, human_values)%>%
  ggplot(aes(x=model_values, y=human_values, color=SpkrID))+
  # geom_line(aes(group=subID), color="black", alpha=.3)+
  geom_point(size=1.5, alpha=.5, position=position_jitter(width=.01, height=.01))+
  geom_abline(intercept = 0, slope=1, linetype="dashed")+
  scale_x_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  scale_y_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  geom_hline(yintercept=.5, linetype="dashed")+
  geom_vline(xintercept=.5, linetype="dashed")+
  geom_smooth(method="lm")+
  scale_color_manual(values = c("#00FDFF", "#00A4FF","#0C00FF", "#9600FF"))+
  labs(tag = "3a")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"))+
  facet_grid2(Measure~modelSource+PowerLevel, strip = strip_nested(size = "variable"))+
  ggtitle("",
          subtitle = "model fits")

##############################################
############################################## Fig 3b: model parameter distributions
##############################################
paramFig3B<-ASP_Exp1_augmented%>%
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
  labs(tag = "3b")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        strip.text = element_text(size=10, face="bold", color="black", family="Palatino"),
        legend.position = "none",
        strip.background = element_rect(fill="gray82", color="black"))+
  facet_grid2(~paramName+PowerLevel, scales="free",independent = "y",
              strip = strip_nested(size = "variable"), 
              labeller = labeller(paramName = as_labeller(c('infoCon' = "infoConformity: \n*higher = consensus implies accuracy*",
                                                            'power' = "PowerLevel: \n*number of 'votes' given to 1st Speaker*",
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
          subtitle = "parameter distributions")


##############################################
############################################## Fig 3c: parameter space in 4 regions
##############################################

# we want to shade the lower diagonal of each plot to separate vote_Y > belief_Y & vice versa  
triangle_data <- data.frame(
  x = c(0, 1, 1),
  y = c(0, 0, 1))
facet_data<-expand.grid(
  wSelf_vPower_Bins = c("self <= Spkr1", "self > Spkr1"),
  wAcc_Bins = c("wAcc <= .5", "wAcc > .5"),
  votingRound = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")
)
triangle_data <- merge(facet_data, triangle_data)
triangle_data

# now make the figure 
paramFig3C.alt<-ASP_Exp1_varFits%>%
  drop_na(model_values)%>%
  select(-humanPattern, -modelPattern)%>%
  pivot_wider(id_cols = c(subID, Spkr2_voteFlip, votingRound, contains(c("param", "PowerLevel", "_false"))), 
              names_from = Measure, values_from = model_values)%>%
  mutate(wSelf_vPower = param_selfWeight - param_power)%>%
  rename(model_beliefs = PrivateBelief, model_votes = PublicVote)%>%
  mutate(wAcc_Bins = case_when(param_weightAccFav <= .5 ~ "wAcc <= .5", 
                               # param_weightAccFav == .5 ~ "wAcc = .5", 
                               param_weightAccFav > .5 ~ "wAcc > .5"),
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr1", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr1"))%>%
  ggplot(aes(x=model_beliefs, y=model_votes))+
  geom_polygon(data = triangle_data, aes(x = x, y = y, group = interaction(wSelf_vPower_Bins, wAcc_Bins, votingRound)), 
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
  labs(tag = "3c")+
  ggtitle("",
          subtitle = "4 regions of parameter space")

paramFig3C.alt

##############################################
############################################## Patchwork Fig 3
##############################################

(paramFig3A|paramFig3C.alt)/paramFig3B+
  # plot_layout(ncol=5, widths = c(2, 3, 5),
  #             guides = 'collect')
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: model fits')


##########

ASP_Exp1_varFits%>%
  drop_na(model_values)%>%
  select(-humanPattern, -modelPattern)%>%
  pivot_wider(id_cols = c(subID, Spkr2_voteFlip, votingRound, contains(c("param", "PowerLevel", "_false"))), 
              names_from = Measure, values_from = model_values)%>%
  mutate(wSelf_vPower = param_selfWeight - param_power)%>%
  rename(model_beliefs = PrivateBelief, model_votes = PublicVote)%>%
  mutate(powerBins = case_when(param_power %in% c(0,1) ~ "Power: 0, 1",
                               param_power %in% c(2,3) ~ "Power: 2,3",
                               param_power %in% c(4, 5) ~ "Power: 4, 5",
                               param_power > 5 ~ "Power: 6-10"),
         wAcc_Bins = case_when(param_weightAccFav <= .5 ~ "wAcc <= .5", 
                               # param_weightAccFav == .5 ~ "wAcc = .5", 
                               param_weightAccFav > .5 ~ "wAcc > .5"),
         selfWeight_Bins = case_when(param_selfWeight < 5 ~ "SW < 5", param_selfWeight == 5 ~ "SW = 5"),
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr1", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr1"))%>%
  mutate(voteCategory = ifelse(model_votes < .5, "voteBlue", "voteYellow"))%>%
  # Uncomment these lines to show crosstabs of Yellow votes depending on wAcc AND Spkr2's vote
  group_by(wAcc_Bins,
           # wSelf_vPower_Bins,
           Spkr2_voteFlip,
           voteCategory
           )%>%
  tally()%>%
  pivot_wider(id_cols = wAcc_Bins:Spkr2_voteFlip,
              names_from = c("voteCategory"),
              values_from = "n", values_fill = 0)%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  adorn_totals(c("row"))%>%
  mutate(percentYellow = voteYellow / Total)
  # Uncomment these lines to show crosstabs of Spkr2 voting Yellow depending on self_vs_power
  # group_by(# wAcc_Bins,
  #   wSelf_vPower_Bins,
  #   Spkr2_voteFlip,
  #   # voteCategory
  # )%>%
  # tally()%>%
  # pivot_wider(id_cols = wSelf_vPower_Bins,
  #             names_from = c("Spkr2_voteFlip"),
  #             values_from = "n", values_fill = 0)%>%
  # mutate(Total = sum(c_across(where(is.numeric))))%>%
  # adorn_totals(c("row"))%>%
  # mutate(percentYellow = Spkr2_VotesYellow / Total)
  
  

##########################################################################################################
##############################           COUNTS            ###############################################
##########################################################################################################

# how many flips did participants in each condition predict?
# Pop: n=88 (=118-30) expected at least one vote to flip, and 52 expected all 4 to flip
# Anon: n=46 (=120-74) expected at least one vote to flip, but nearly equal expected only one (n=15) vs all four (n=19) 
ASP_Exp1%>%
  group_by(PowerLevel, PublicVoteCount)%>%
  tally()%>%
  pivot_wider(names_from = "PublicVoteCount", values_from = "n")%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  adorn_totals(c("row"))

# Spkr2's vote depends on condition: 
# Pop: 64% Yellow
# Anon: 29% Yellow
ASP_Exp1%>%
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

# predicted flips by both condition and Spkr2 vote
ASP_Exp1%>%
  group_by(PowerLevel, Spkr2_voteFlip, PublicVoteCount)%>%
  tally()%>%
  pivot_wider(id_cols = PowerLevel:Spkr2_voteFlip, 
              names_from = "PublicVoteCount", names_prefix = "flip_",
              values_from = "n", values_fill = 0)%>%
  mutate(Total = sum(c_across(where(is.numeric))))

# what percentage expected ALL informants to continue to privately believe Blue? 
# ANS: 76.4%
ASP_Exp1%>%
  rowwise()%>%
  select(subID, PowerLevel, PrivatePattern)%>%
  mutate(thinkYellow_count = str_count(str_remove(PrivatePattern, "^Y"), "Y"))%>%
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


#### was the shift toward Yellow greater for (1) PublicVotes than Private Beliefs, AND for (2) Popular than Anonymous?
ASP_Exp1_resultMain<-lmer(Values-10.5~Measure*PowerLevel+(1|subID:SpkrID), 
                    # filter out the decision ratings; also, flip the reference level in order to see intercepts for each
                    data=ASP_Exp1_long%>%
                      filter(Measure %in% c("PublicVote", "PrivateBelief"))%>%
                      mutate(Measure = fct_relevel(Measure, "PrivateBelief", "PublicVote"),
                             PowerLevel = fct_relevel(PowerLevel, "Popular", "Anonymous")))
summary(ASP_Exp1_resultMain)

#### Did we find pluralistic ignorance (vote =/= beliefs) only in the Popular condition?
ASP_Exp1_result_PluralIg<-ASP_Exp1_counts%>%
  mutate(avgVote_Centered = human_avgVote -0.5,
         avgBelief_Centered = human_avgBelief-0.5)%>%
  pivot_longer(cols = ends_with("_Centered"), names_to = "judgmentType", values_to = "avgRating_Centered")%>%
  group_by(PowerLevel, judgmentType)%>%
  nest()%>%
  mutate(mod_pluralIgnorance = map(data, ~tidy(t.test(avgRating_Centered ~ 1, data=.), conf.int=T)))%>%
  unnest(mod_pluralIgnorance)%>%
  select(-alternative)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))

ASP_Exp1_result_PluralIg

#### Did participants expect public and private judgments to shift across the sequence depending on previous speakers' votes? 
ASP_Exp1_result_SpkrOrder<-ASP_Exp1_long%>%
  filter(Measure %in% c("PublicVote", "PrivateBelief"))%>%
  mutate(Measure = fct_relevel(Measure, "PublicVote", "PrivateBelief"),
         PowerLevel = fct_relevel(PowerLevel, "Popular", "Anonymous"),
         SpkrOrder = as.numeric(SpkrID)-1)%>%
  group_by(all_or_none,PowerLevel, Measure)%>%
  nest()%>%
  mutate(mod_infoCascade = map(data, ~tidy(lmer(Values-10.5~SpkrOrder+(1|subID), data=.), conf.int=T)))%>%
  unnest(mod_infoCascade)%>%
  select(-data, -effect, -group)%>%
  filter(str_detect(term, "sd_")==F)%>%
  arrange(all_or_none)
# check output
ASP_Exp1_result_SpkrOrder

# UNCOMMENT: to see visualization of coefficients
ASP_Exp1_result_SpkrOrder%>%
  mutate(alphaSig = ifelse(p.value<.05, "sig", "notSig"),
         term = ifelse(term=="SpkrOrder", "coefficient for speaker order: <br>*later speakers are more...*", "coefficient for intercept: <br>*estimate for Spkr2*"),
         Measure = ifelse(Measure == "PrivateBelief", "Private\nBelief", "Public\nVote"),
         PowerLevel = fct_relevel(PowerLevel, "Anonymous", "Popular"),
         est_direction = ifelse(estimate < 0, "estimate trend: Blue", "estimate trend: Yellow"))%>%
  mutate(Measure = factor(Measure, levels = c("Public\nVote", "Private\nBelief")))%>%
  ggplot(aes(x=Measure, y=estimate, fill=Measure))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group=interaction(Measure, term)),
                position=position_dodge(width=1),linewidth=1.5, color="black", width = 0.1)+
  stat_summary(fun ="mean", geom="label",
               aes(label=round(..y.., 2),  y=estimate, group=interaction(Measure, term), fill=Measure, alpha=alphaSig),
               position=position_dodge(width=1), color="black", size=4.5)+
  scale_alpha_manual(values = c(.15, 1))+
  geom_hline(yintercept=0, linetype="dashed")+
  # scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_y_continuous("coefficient values <br> *(negative=trending blue, positive =trending yellow)*")+
  facet_grid2(PowerLevel~all_or_none+term, strip = strip_nested(size = "variable"))+
  # coord_flip()+
  theme_cleanPub+
  theme(axis.text.y = element_text(size=11),
        strip.text = element_markdown(),
        axis.title.y = element_markdown())+
  ggtitle("Experiment 1: coefficients for speaker order model visualized to show trends in public & private judgments",
          subtitle = "model: lmer(Values-10.5~SpkrOrder+(1|subID)")


#### how were predictions about public & private judgments related to 
#### inferences about (A) which design the team would choose and (B) which was better
ASP_Exp1_result_choiceTrust<-ASP_Exp1_counts%>%
  mutate(PublicVoteCount_maj = PublicVoteCount-2,
         scale01_TeamChoice_center = scale01_TeamChoice-.5,
         scale01_AccChoice_center = scale01_AccurateChoice-.5,
         rating_TeamChoice_center = rating_TeamChoice-10.5,
         rating_AccChoice_center = rating_AccurateChoice-10.5,
         decision_BvY = fct_relevel(decision_BvY, "predicted team choice: Yellow", "predicted team choice: Blue"))%>%
  pivot_longer(cols = c("rating_TeamChoice_center", "rating_AccChoice_center"), 
               names_to = "measure", values_to = "ratings_center")%>%
  group_by(PowerLevel, measure)%>%
  nest()%>%
  mutate(mod_diff = case_when(measure=="rating_TeamChoice_center" ~ 
                            map(data, 
                                ~tidy(lm(ratings_center~scale(PublicVoteCount_maj), data=.), 
                                      conf.int=T)),
                          measure=="rating_AccChoice_center" ~ 
                            map(data, 
                                ~tidy(lm(ratings_center ~ rating_avgDiff_voteHigh*decision_BvY, data=.), 
                                      conf.int=T))),
         # for accAchoice, we'll to know which they trust
         # for teamChoice, ttest gives same result as intercept for mod_diff; 
         mod_ttest = case_when(measure=="rating_TeamChoice_center" ~ 
                                map(data, ~tidy(t.test(ratings_center ~ 1, data=.), conf.int=T)),
                              measure == "rating_AccChoice_center" ~ 
                                map(data, ~tidy(t.test(ratings_center ~ 1, data=.), conf.int=T))))

## team choice related to majority vote
ASP_Exp1_result_choiceTrust%>%
  unnest(mod_diff)%>%
  filter(measure=="rating_TeamChoice_center")%>%
  select(-data, -mod_ttest)%>%
  arrange(PowerLevel)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))
  
## but people trust the pre-sequence consensus 
ASP_Exp1_result_choiceTrust%>%
  unnest(mod_ttest)%>%
  filter(measure=="rating_AccChoice_center")%>%
  select(-data, -mod_diff)%>%
  arrange(PowerLevel)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))

## AND, if team choice contradicts pre-sequence consensus, then larger gaps = less trust
## note: equal trust in pre-sequence consensus from ttest above can also be seen below: conf.int for Anon & Pop overlap
ASP_Exp1_result_choiceTrust%>%
  unnest(mod_diff)%>%
  filter(measure=="rating_AccChoice_center")%>%
  select(-data, -mod_ttest)%>%
  arrange(PowerLevel)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))



######## parameter distributions: do they differ by condition?
ASP_Exp1_augmented%>%
  select(subID,PowerLevel, starts_with("fittedParam"))%>%
  pivot_longer(cols = starts_with("fittedParam"), 
               names_pattern = "(.*)_(.*)_(.*)", names_to = c("discard","modelSource", "paramName"),
               values_to = "param_fits")%>%
  select(-discard)%>%
  group_by(modelSource, paramName)%>%
  nest()%>%
  # filter out the row for PowerLevel in the fixPower model, since those fits are identical by definition
  filter(!(paramName == "power" & modelSource == "fixMod"))%>%
  mutate(ParamDiff = map(data, ~tidy(t.test(param_fits ~ PowerLevel, data=.), conf.int=T)))%>%
  unnest(ParamDiff)%>%
  select(-data, -method, -alternative)%>%
  # add the filter to show only the sig diffs
  # filter(p.value < .05)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))
  # split(.$paramName)


######## parameter distributions: do they differ by model or interact?
ASP_Exp1_augmented%>%
  select(subID,PowerLevel, starts_with("fittedParam"))%>%
  pivot_longer(cols = starts_with("fittedParam"), 
               names_pattern = "(.*)_(.*)_(.*)", names_to = c("discard","modelSource", "paramName"),
               values_to = "param_fits")%>%
  select(-discard)%>%
  group_by(paramName)%>%
  nest()%>%
  # filter(!(paramName == "power" & modelSource == "fixMod"))%>%
  mutate(param_anova = map(data, ~tidy(anova(lmer(param_fits ~ PowerLevel*modelSource+(1|subID), data=.)), conf.int=T)))%>%
  unnest(param_anova)%>%
  select(-data)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))
  # filter(term != "PowerLevel" & p.value < .05)
  # split(.$paramName)


################## For supplemental
######### team choice & accuracy as a function of predicted flips
ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
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
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid2(wSelf_vPower_Bins~"trust majority"+wAcc_Bins, strip = strip_nested(size = "variable"))+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=90, hjust=.5),
        # plot.tag.position = c(.02, 1),
        # plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        # plot.title = element_blank(),
        # legend.position = "bottom",
        legend.position = "inside",
        legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: vote-belief gaps & trust in majority across 4 regions of parameter space",
          subtitle = "the greater the vote-belief gap participants expect during the voting sequence, the more they distrust post-sequence
          <br>team choices that contradict pre-sequence private consensus; and vote-belief gaps are largest when wAcc < .5")



