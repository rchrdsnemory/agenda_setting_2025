#########################################################################################################################
#######################        ASP  Model Parameter Space Simulation            #########################################
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
#                                       )


################################################### Import simulation data
###################################################

ASP_Exp1_simParams <- read_excel("~INSERT_PATHNAME/24.11.01_simParams.xlsx", 
                             sheet = "simParam_SpeaksFirstExp1")
ASP_Exp3_simParams <- read_excel("~INSERT_PATHNAME/24.11.01_simParams.xlsx", 
                                 sheet = "simParam_SpeaksLastExp3")
str(ASP_Exp1_simParams)
str(ASP_Exp3_simParams)

###### Experiment 1 simulation
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

# now generate a list of unique parameter combos
lookup_paramID_Exp1<-ASP_Exp1_simParams%>%
  select(infoConformity, socConformity, pwrSpkr1, wAcc_SocFav, selfWeight)%>%
  group_by(infoConformity, socConformity, pwrSpkr1, wAcc_SocFav, selfWeight)%>%
  tally()%>%
  ungroup()%>%
  mutate(paramComboID = row_number())%>%
  select(-n)

# ensure we have the right number of parameter combos (unique combos of parameters: 4*4*11*11*9 = 17424)
length(unique(lookup_paramID_Exp1$paramComboID))

# and use that list to append the paramComboID to the tibble of simulation 
ASP_Exp1_simParams<-ASP_Exp1_simParams%>%
  left_join(lookup_paramID_Exp1, by = c("infoConformity", "socConformity", "pwrSpkr1", "wAcc_SocFav", "selfWeight"))%>%
  relocate(paramComboID, .before=infoConformity)

glimpse(ASP_Exp1_simParams)

###### Experiment 3 simulation
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

# now generate a list of unique parameter combos
lookup_paramID_Exp3<-ASP_Exp3_simParams%>%
  select(infoConformity, socConformity, pwrSpkr5, wAcc_SocFav, selfWeight)%>%
  group_by(infoConformity, socConformity, pwrSpkr5, wAcc_SocFav, selfWeight)%>%
  tally()%>%
  ungroup()%>%
  mutate(paramComboID = row_number())%>%
  select(-n)

# ensure we have the right number of parameter combos (unique combos of parameters: 4*4*11*11*9 = 17424)
length(unique(lookup_paramID_Exp3$paramComboID))

# and use that list to append the paramComboID to the tibble of simulation 
ASP_Exp3_simParams<-ASP_Exp3_simParams%>%
  left_join(lookup_paramID_Exp3, by = c("infoConformity", "socConformity", "pwrSpkr5", "wAcc_SocFav", "selfWeight"))%>%
  relocate(paramComboID, .before=infoConformity)

glimpse(ASP_Exp3_simParams)

# write.csv(ASP_Exp1_simParams, "ASP_Exp1_simParams.csv")
# write.csv(ASP_Exp3_simParams, "ASP_Exp3_simParams.csv")


# in plotting, we want to shade the lower diagonal of each facet to separate vote_Y > belief_Y & vice versa  
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

# and for Exp3, replace the "Spkr1" with "Spkr5"
triangle_data_Exp3<-triangle_data%>%
  mutate(wSelf_vPower_Bins = str_replace(wSelf_vPower_Bins, "Spkr1", "Spkr5"))


#########################################################################################################################
############################        ASP  Experiment 1 (Speaks First)            #########################################
#########################################################################################################################

ASP_Exp1_simParams%>%
  group_by(votingRound, votePattern, Spkr2_voteFlip)%>%
  tally()

ASP_Exp1_simParams%>%
  group_by(votePattern, wAcc_Bins, wSelf_vPower_Bins)%>%
  tally()


########### plot sim data in 4 regions
# 
ASP_Exp1_simParams%>%
  mutate(infoConformity = factor(infoConformity),
         socConformity = factor(socConformity))%>%
  # filter(Spkr2_voteFlip == "Spkr2_VoteBlue")%>%
  ggplot(aes(x=beliefY_updated, y=voteY_prob))+
  geom_polygon(data = triangle_data, aes(x = x, y = y, group = interaction(wSelf_vPower_Bins, wAcc_Bins, votingRound)),
               fill = "lightgrey", alpha = 0.5)+
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
  ggtitle("Exp 1: 'counterfactual' simulation of parameter space when dissenter speaks first, in 4 key regions",
          subtitle = "NOTE: all vote patterns are simulated each round for each combination of parameters,  even if they didn't occur under that combination of parameters
          <br> (e.g., Spkr2 never votes Blue when wAcc<=.5 & self<=Spkr1, but the simulation still includes a case to estimate Spkr3's vote 'had Spkr2 voted Blue')")


# a Yellow vote from Spkr2 almost guarantees a 5v0 Yellow consensus regardless of region;
# <br> a Blue vote from Spkr2 makes a Blue majority the most common outcome, except in Region 1 (top left)
# <br> the most common is still a 5v0 Yellow consensus; 

#########################################################################################################################
############################        ASP  Experiment 3 (Speaks Last)            #########################################
#########################################################################################################################


########### plot sim data in 4 regions
# 
ASP_Exp3_simParams%>%
  mutate(infoConformity = factor(infoConformity),
         socConformity = factor(socConformity))%>%
  # filter(Spkr2_voteFlip == "Spkr2_VoteBlue")%>%
  ggplot(aes(x=beliefY_updated, y=voteY_prob))+
  geom_polygon(data = triangle_data_Exp3, aes(x = x, y = y, group = interaction(wSelf_vPower_Bins, wAcc_Bins, votingRound)),
               fill = "lightgrey", alpha = 0.5)+
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
  ggtitle("Exp 3: 'counterfactual' simulation of parameter space when dissenter speaks last, in 4 key regions",
          subtitle = "NOTE: all vote patterns are simulated each round for each combination of parameters,  even if they didn't occur under that combination of parameters
          <br> (e.g., Spkr2 never votes Blue, but the simulation still includes a case to estimate Spkr3's vote 'had Spkr2 voted Blue')")



