#October 2020 US Sample

#Package Libraries####
library(rio)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggsci)
library(ggthemes)
library(car)
ibrary(ggpubr)
library(showtext)
library(prediction)
#font_add_google("Merriweather", "M", regular.wt = 400)
#font_add_google("Josefin Sans", "JS", regular.wt = 400)
#font_add_google("EB Garamond", "G", regular.wt = 400)
#font_add_google("Raleway", "Rw", regular.wt = 400)
font_add(family = "G", regular="C:/Users/pdjup/AppData/Local/Microsoft/Windows/Fonts/EBGaramond-Regular.ttf")
font_add(family = "JS", regular="C:/Users/pdjup/AppData/Local/Microsoft/Windows/Fonts/JosefinSans-Regular.ttf")
font_add(family = "Rw", regular="C:/Users/pdjup/AppData/Local/Microsoft/Windows/Fonts/Raleway-Regular.ttf")
#font_add_google("Averia Sans Libre", "Ave", regular.wt = 400)
#font_add_google("Nova Round", "Nova", regular.wt = 400)
#font_add_google("Carrois Gothic", "CG", regular.wt=400)
font_add_google("Jost", "Jost", regular.wt=400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
#install.packages("devtools")
#library(devtools)
#install_github("ryanburge/socsci")
library(socsci)
library(paletteer)
#install.packages("jtools")
library(jtools)
#install.packages("interactions")
library(interactions)
#install.packages("patchwork")
library(patchwork)
library(psych)
library(MASS)
#install.packages("ragg")
#library(ragg)
#library(systemfonts)

#rm(list=ls(pattern="ip"))

#Data Import
#Data import####
O20dir <- "C:/Users/pdjup/Dropbox (Personal)/Burge-Djupe-Work/October 2020/"
O20 <- import(paste0(O20dir, "Data/October_2020_US_2528.csv"))
#2739
O20 <- O20 %>% rename_all(tolower) 
O20 <- O20 %>% filter(gc==1)

O20_with_weights <- import(paste0(O20dir, "Data/O20_with_weights.csv"))

O20 <- left_join(O20, O20_with_weights, by="responseid")

rm(O20_with_weights)
rm(O20_weights)
#write.csv(O20, paste0(O20dir, "October_2020_1790.csv"))
#library(foreign)
#write.dta(O20, file=paste0(O20dir, "October_2020_1790.dta"), version = 8,
#          convert.dates = TRUE)
#Recodes####

O20 <- O20 %>% mutate(female=q2-1)
O20 <- O20 %>% mutate(femlab=frcode(female==0 ~ "Men",
                                    female==1 ~ "Women"))

O20 <- O20 %>% mutate(region=frcode(q5==1 ~ "Northeast",
                                    q5==2 ~ "South",
                                    q5==3 ~ "Midwest",
                                    q5==4 ~ "West"))

#Race
O20 <- O20 %>% mutate(white=car::recode(q142_1, "NA=0"),
                      hispanic=car::recode(q142_2, "NA=0"),
                      black=car::recode(q142_3, "NA=0"),
                      asian=car::recode(q142_4, "NA=0"),
                      other=car::recode(q142_5, "NA=0"))

#A single race variable (e.g., white only)
O20 <- O20 %>% mutate(race=case_when(black==1 ~ "Black",
                                     asian==1 & (black!=1 & hispanic!=1) ~ "Asian",
                                     hispanic==1 ~ "Latino",
                                     other==1 & (black!=1 & hispanic!=1 & asian!=1)~ "Other",
                                     white==1 & (black!=1 & hispanic!=1 & asian!=1 & other!=1) ~ "White"))
O20 <- O20 %>% mutate(racew=frcode(race=="White" ~ 1,
                                   race=="Black" ~ 2,
                                   race=="Latino" ~ 3,
                                   race=="Asian" ~ 4,
                                   race=="Other" ~ 5))
O20 <- O20 %>% mutate(agew = car::recode())

O20 <- O20 %>% mutate(whitef=frcode(white==0 ~ "Non-White",
                     white==1 ~ "White"))

#Age & Stuff - simple new naming
O20 <- O20 %>% mutate(age=2020-q3_1,
                      birthyr=q3_1,
                      employment=q139,
                      ed=q143,
                      sexualo=q144,
                      marstat=q145,
                      faminc=q146,
                      masculine=q138_1,
                      feminine=q138_2,
                      polint=6-q33,
                      pid7=q55)

O20 <- O20 %>% mutate(pid7f=frcode(pid7==1 ~ "Strong\nDemocrat",
                                   pid7==2 ~ "Democrat",
                                   pid7==3 ~ "Lean\nDemocrat",
                                   pid7==4 ~ "Independent",
                                   pid7==5 ~ "Lean\nRepublican",
                                   pid7==6 ~ "Republican",
                                   pid7==7 ~ "Strong\nRepublican"))

O20 <- O20 %>% mutate(pidst=car::recode(pid7, "1=4; 2=3;3=4;4=1;5=2;6=3;7=4"))

#Generations
O20 <- O20 %>%  mutate(age4=car::recode(age, "18:24=1; 25:44=2; 45:64=3; 65:100=4; 101:120=NA"))

O20 <- O20 %>% mutate(age4f=frcode(age4==1 ~ '18-24',
                                   age4==2 ~ '25-44',
                                   age4==3 ~ '45-64',
                                   age4==4 ~ '65-100'))

O20 <- O20 %>% mutate(cohorts = frcode(birthyr>= 1901 & birthyr <= 1924 ~ 'Greatest Generation',
                                       birthyr>= 1925 & birthyr <= 1945 ~ 'Silent\nGeneration',
                                       birthyr>= 1946 & birthyr <= 1964 ~ 'Boomers',
                                       birthyr>= 1965 & birthyr <= 1976 ~ 'Gen X',
                                       birthyr>= 1977 & birthyr <= 1995 ~ 'Millennials',
                                       birthyr>= 1996 & birthyr <= 2020 ~ 'Gen Z'))

#More Partisanship
O20 <- O20 %>% mutate(pid3=frcode(pid7<4 ~ "Democrat",
                                  pid7==4 ~ "Independent",
                                  pid7>4 ~ "Republican"),
                      pidst=car::recode(pid7, "1=4; 2=3; 3=2; 4=1; 5=2; 6=3; 7=4"))

O20 <- O20 %>% mutate(rep=car::recode(pid7, "1:4=0; 5:7=1"),
                      rep=frcode(rep==0 ~ "Other",
                                 rep==1 ~ "Republicans"))

#Attendance
O20 <- O20 %>% mutate(q6r=as.factor(q6))
O20 <- O20 %>% mutate(attend=frcode(q6==1 ~ "More than\nonce a week",
                                    q6==2 ~ "Once a week",
                                    q6==3 ~ "A few times\na month",
                                    q6==4 ~ "A few times\nayear",
                                    q6==5 ~ "Seldom",
                                    q6==6 ~ "Never"))
                                   
O20 <- O20 %>% mutate(attend5=car::recode(q6, "1:2=5; 3=4; 4=3; 5=2; 6=1"))
O20 <- O20 %>% mutate(hiatt=car::recode(attend5, "4:5=1; else=0"))
O20 <- O20 %>% mutate(hiattf=frcode(hiatt==0 ~ "Low Attender",
                                    hiatt==1 ~ "High Attender"))

#Covid church stuff
O20 <- O20 %>% mutate(q40_1c=car::recode(q40_1, "2=0;NA=0"),
                      q40_2c=car::recode(q40_2, "2=0;NA=0"),
                      q40_3c=car::recode(q40_3, "2=0;NA=0"),
                      q40_4c=car::recode(q40_4, "2=0;NA=0"),
                      q40_5c=car::recode(q40_5, "2=0;NA=0"),
                      q40_6c=car::recode(q40_6, "2=0;NA=0"),
                      q40_7c=car::recode(q40_7, "2=0;NA=0"))

#Prosperity Gospel
O20 %>% dplyr::select(q36_1, q36_2, q36_3) %>% psych::alpha(.) #a=.92

O20 <- O20 %>% mutate(prosperity1=((5-q36_1) + (5-q36_2) + (5-q36_3))/12)

O20 <- O20 %>% mutate(prosperity1c=car::recode(prosperity1, "0:.5=0; .50001:1=1"),
                      prosperity1c=frcode(prosperity1c==0 ~ "Not PG",
                                          prosperity1c==1 ~ "Prosperity Gospel"))

#Religious activity
O20 <- O20 %>% mutate(involve=q62_4)

#Evil
O20 %>% dplyr::select(q44_1, q44_2, q44_3) %>% psych::alpha(.) #a=.63
O20 <- O20 %>% mutate(evil=((5-q44_1)+(5-q44_2)+(5-q44_3))/12)

#end times
O20 <- O20 %>% mutate(end=5-q41_7)

#Left the church
O20 <- O20 %>% mutate(left=car::recode(q45, "1=1; 2:3=0; else=NA"))

#Literalism
O20 <- O20 %>% mutate(literal=5-q53)

#Inclusive/Exclusive Values
O20 <- O20 %>% mutate(inclusive=((6-q43_1)+(6-q43_2))/2)
O20 <- O20 %>% mutate(exclusive=((6-q43_3)+(6-q43_4))/2)

#Christian Nationalism
#O20 %>% dplyr::select(q64_1, q64_2, q64_3, q64_4, q64_5) %>% psych::alpha(., check.keys = TRUE) #a=.8
#O20 %>% dplyr::select(q69_1, q69_2, q69_3, q69_4, q69_5) %>% psych::alpha(., check.keys = TRUE) #a=.81
#O20 %>% dplyr::select(q73_1, q73_2, q73_3, q73_4, q73_5) %>% psych::alpha(., check.keys = TRUE) #a=.81

O20 <- O20 %>% mutate(q64_1r=6-q64_1,
                      q64_2r=6-q64_2,
                      q64_3r=6-q64_3,
                      q64_4r=6-q64_4,
                      q64_5r=6-q64_5,
                      q69_1r=6-q69_1,
                      q69_2r=6-q69_2,
                      q69_3r=6-q69_3,
                      q69_4r=6-q69_4,
                      q69_5r=6-q69_5,
                      q73_1r=6-q73_1,
                      q73_2r=6-q73_2,
                      q73_3r=6-q73_3,
                      q73_4r=6-q73_4,
                      q73_5r=6-q73_5)
O20 <- O20 %>% gather(key="t_cn", value="cn1", q64_1r, q69_1r, q73_1r, na.rm=TRUE)
O20 <- O20 %>% gather(key="t_cn", value="cn2", q64_2r, q69_2r, q73_2r, na.rm=TRUE)
O20 <- O20 %>% gather(key="t_cn", value="cn3", q64_3r, q69_3r, q73_3r, na.rm=TRUE)
O20 <- O20 %>% gather(key="t_cn", value="cn4", q64_4r, q69_4r, q73_4r, na.rm=TRUE)
O20 <- O20 %>% gather(key="t_cn", value="cn5", q64_5r, q69_5r, q73_5r, na.rm=TRUE)
O20 <- O20 %>% mutate(cn=(((cn1+cn2+(6-cn3)+cn4+cn5)-5)/20))
O20 <- O20 %>% mutate(cntot=(((cn1+cn2+(6-cn3)+cn4+cn5)-5)))

O20 %>% dplyr::select(cn1,cn2,cn3,cn4,cn5) %>% alpha(., check.keys = TRUE)

#library(gtools)
O20 <- O20 %>% mutate(cntot4=quantcut(cntot, 4))
O20 <- O20 %>% mutate(cntot2=quantcut(cntot, 2))

O20 <- O20 %>% mutate(t_cn=frcode(t_cn=="q64_5r" ~ "Control",
                                  t_cn=="q69_5r" ~ "Minority",
                                  t_cn=="q73_5r" ~ "Majority"))

#Christian Persecution####
O20 <- O20 %>% gather(key="cp_vers", value="cp1", q67_1, q80_1, na.rm=TRUE)
O20 <- O20 %>% gather(key="cp_vers", value="cp2", q67_2, q80_2, na.rm=TRUE)
O20 <- O20 %>% gather(key="cp_vers", value="cp3", q67_3, q80_3, na.rm=TRUE)
O20 <- O20 %>% gather(key="cp_vers", value="cp4", q67_4, q80_4, na.rm=TRUE)
O20 <- O20 %>% gather(key="cp_vers", value="cp5", q67_5, q80_5, na.rm=TRUE)
O20 <- O20 %>% gather(key="cp_vers", value="cp6", q67_6, q80_6, na.rm=TRUE)

O20 %>% dplyr::select(cp1, cp2, cp3, cp4, cp5, cp6) %>% alpha(.) #a=.95
O20 %>% dplyr::select(cph1, cph2, cph3, cph4, cph5, cph6) %>% alpha(.) #a=.81

  #higher is more persecution
O20 <- O20 %>% mutate(cpavg=(1-((cp1+cp2+cp3+cp4+cp5+cp6)-6)/24))

O20 <- O20 %>% mutate(cp4avg=(1-((cp1+cp2+cp3+cp4)-4)/16))
library(gtools)
O20 <- O20 %>% mutate(cpavg2=quantcut(cpavg, 2))
O20 <- O20 %>% mutate(cpavg2=frcode(cpavg2=="[0,0.5]" ~ "Low",
                                    cpavg2=="(0.5,1]" ~ "High"))

#Agree/Disagree
O20 <- O20 %>% mutate(cp1c=car::recode(cp1, "1:2=1; 3:5=0"),
                      cp2c=car::recode(cp2, "1:2=1; 3:5=0"),
                      cp3c=car::recode(cp3, "1:2=1; 3:5=0"),
                      cp4c=car::recode(cp4, "1:2=1; 3:5=0"))

#Heard about Christian Persecution
#cph_vers is q68 when CP came before CN; in q81, CN comes well before
O20 <- O20 %>% gather(key="cph_vers", value="cph1", q68_1, q81_1, na.rm=TRUE)
O20 <- O20 %>% gather(key="cph_vers", value="cph2", q68_2, q81_2, na.rm=TRUE)
O20 <- O20 %>% gather(key="cph_vers", value="cph3", q68_3, q81_3, na.rm=TRUE)
O20 <- O20 %>% gather(key="cph_vers", value="cph4", q68_4, q81_4, na.rm=TRUE)
O20 <- O20 %>% gather(key="cph_vers", value="cph5", q68_5, q81_5, na.rm=TRUE)
O20 <- O20 %>% gather(key="cph_vers", value="cph6", q68_6, q81_6, na.rm=TRUE)

O20 <- O20 %>% mutate(cph1=car::recode(cph1, "2=0"),
                      cph2=car::recode(cph2, "2=0"),
                      cph3=car::recode(cph3, "2=0"),
                      cph4=car::recode(cph4, "2=0"),
                      cph5=car::recode(cph5, "2=0"),
                      cph6=car::recode(cph6, "2=0"))
O20 %>% dplyr::select(cph1, cph2, cph3, cph4, cph5, cph6) %>% alpha(.) #a=.81

O20 <- O20 %>% mutate(cphavg=(cph1+cph2+cph3+cph4+cph5+cph6))
O20 <- O20 %>% mutate(cph4avg=(cph1+cph2+cph3+cph4))

O20 <- O20 %>% mutate(cphavg2=quantcut(cphavg, 2))
O20 <- O20 %>% mutate(cphavg2a=car::recode(cphavg, "0:2=0; 3:6=1"))

O20 <- O20 %>% mutate(cphavg2=frcode(cphavg2=="[0,3]" ~ "Low",
                                    cphavg2=="(3,6]" ~ "High"))
O20 <- O20 %>% mutate(cphavg2a=frcode(cphavg2a==0 ~ "Low",
                                     cphavg2a==1 ~ "High"))


#Ideology coding####
O20 <- O20 %>% mutate(q56_1=car::recode(q56_1, "NA=0"),
                      q56_2=car::recode(q56_2, "NA=0"),
                      q56_3=car::recode(q56_3, "NA=0"),
                      q56_4=car::recode(q56_4, "NA=0"),
                      q56_5=car::recode(q56_5, "NA=0"),
                      q56_6=car::recode(q56_6, "NA=0"))

O20 <- O20 %>% mutate(proglib=frcode(q56_5==1 & q56_4==0 ~ "Progressive Only",
                                     q56_5==0 & q56_4==1 ~ "Liberal Only",
                                     q56_5==1 & q56_4==1 ~ "Both Prog & Lib"))


O20 <- O20 %>% mutate(ideology_id=frcode(q56_5==1 & q56_4==0 ~ "Progressive Only",
                                         q56_5==0 & q56_4==1 ~ "Liberal Only",
                                         q56_5==1 & q56_4==1 ~ "Both Prog & Lib",
                                         q56_3==1 ~ "Moderate",
                                         q56_1==1 ~ "Conservative",
                                         q56_2==1 ~ "Alt-Right",
                                         q56_6==1 ~ "Libertarian",
                                         q56_7==1 ~ "Socialist"))
O20 %>% ct(ideology_id, wt=wgt)
O20 <- O20 %>% mutate(ideology_id3=frcode(q56_5==1 ~ "Progressive",
                                          q56_4==1 ~ "Liberal Only",
                                          q56_3==1 ~ "Moderate",
                                          q56_1==1 ~ "Conservative",
                                          q56_2==1 ~ "Alt-Right",
                                          q56_6==1 ~ "Libertarian",
                                          q56_7==1 ~ "Socialist"))

O20 <- O20 %>% mutate(ideology_id2=frcode(q56_5==1 & q56_4==0 ~ "Progressive Only",
                                          q56_5==0 & q56_4==1 ~ "Liberal Only",
                                          q56_5==1 & q56_4==1 ~ "Both Prog & Lib",
                                          q56_3==1 | q56_4==1  | q56_5==1  | q56_6==1 ~ "Other",
                                          TRUE ~ "NA"))

#Anti-Semitic Tropes####
O20 <- O20 %>% mutate(q107_1r=car::recode(q107_1, "2=0"),
                      q107_2r=car::recode(q107_2, "2=0"),
                      q107_3r=car::recode(q107_3, "2=0"),
                      q107_4r=car::recode(q107_4, "2=0"),
                      q107_5r=car::recode(q107_5, "2=0"),
                      q107_6r=car::recode(q107_6, "2=0"),
                      q107_7r=car::recode(q107_7, "2=0"),
                      q107_8r=car::recode(q107_8, "2=0"))
O20 <- O20 %>% mutate(tropes=q107_1r+q107_2r+q107_3r+q107_4r+q107_5r+q107_6r+q107_7r+q107_8r)

#Reciprocity####
#1=will respect my rights, 0=will not.
O20 <- O20 %>% mutate(q39_1c=car::recode(q39_1, "1:3=1;4:6=0"),
                      q39_2c=car::recode(q39_2, "1:3=1;4:6=0"),
                      q39_3c=car::recode(q39_3, "1:3=1;4:6=0"),
                      q39_4c=car::recode(q39_4, "1:3=1;4:6=0"))
#6=will respect my rights, 1=will not.
O20 <- O20 %>% mutate(q39_1r=7-q39_1,
                      q39_2r=7-q39_2,
                      q39_3r=7-q39_3,
                      q39_4r=7-q39_4)

#Q41s####
O20 <- O20 %>% mutate(q41_1r=6-q41_1,
                      q41_2r=6-q41_2,
                      q41_3r=6-q41_3,
                      q41_4r=6-q41_4,
                      q41_5r=6-q41_5,
                      q41_6r=6-q41_6,
                      q41_7r=6-q41_7,
                      q41_8r=6-q41_8,
                      q41_9r=6-q41_9)

#Qanon stuff####
O20 <- O20 %>%  mutate(q47_14c=car::recode(q47_14, "0:49=0; 50:100=1")) 

O20 <- O20 %>% mutate(q38_10rc=car::recode(q38_10r, "1:3=0; 4:5=1"))

#Civic Skills Experiment####
O20 <- O20 %>% mutate(q51_1=car::recode(q51_1, "NA=0"),
                      q51_2=car::recode(q51_2, "NA=0"),
                      q51_3=car::recode(q51_3, "NA=0"),
                      q51_4=car::recode(q51_4, "NA=0"),
                      womenlead = q51_1+q51_2+q51_3+q51_4)
O20 <- O20 %>% mutate(q52_1=car::recode(q52_1, "NA=0"),
                      q52_2=car::recode(q52_2, "NA=0"),
                      q52_3=car::recode(q52_3, "NA=0"),
                      q52_4=car::recode(q52_4, "NA=0"),
                      q52_5=car::recode(q52_5, "NA=0"),
                      skills = q52_1+q52_2+q52_3+q52_4,
                      skillsc=car::recode(skills, "1:4=1"))
O20 <- O20 %>% mutate(skills_order=frcode(civicskillsexperiment_do_q51==1 ~ "Definition First",
                                          civicskillsexperiment_do_q51==2 ~ "Skills First"))

#Political Participation####
O20 <- O20 %>% mutate(q54_1=car::recode(q54_1,"1=0; 2=1"),
                      q54_2=car::recode(q54_2,"1=0; 2=1"),
                      q54_3=car::recode(q54_3,"1=0; 2=1"),
                      q54_4=car::recode(q54_4,"1=0; 2=1"),
                      q54_5=car::recode(q54_5,"1=0; 2=1"),
                      q54_6=car::recode(q54_6,"1=0; 2=1"),
                      q54_8=car::recode(q54_8,"1=0; 2=1"),
                      polact = q54_1 + q54_2 + q54_3 + q54_4 + q54_5 + q54_6 + q54_8)

#Fox News
O20 <- O20 %>% mutate(fox=car::recode(q58, "4=1; else=0"))

#SDO
O20 %>% dplyr::select(q37_1, q37_2, q37_3, q37_4) %>% alpha(., check.keys = TRUE) #a=.6
O20 <- O20 %>% mutate(sdo=((5-q37_1)+(5-q37_2)+(q37_3-1)+(q37_4-1))/16)

#Church size
O20 <- O20 %>% mutate(churchsize=q61)

#Born-again
O20 <- O20 %>% mutate(ba=car::recode(q28, "2=0"))
O20 <- O20 %>% mutate(baf=frcode(ba==0 ~ "Non-evangelical",
                                 ba==1 ~ "Evangelical"))

#Anointed
O20 <- O20 %>% gather(key="t_anoint", value="anoint1", q48_1, q49_1, na.rm=TRUE)
O20 <- O20 %>% gather(key="t_anoint", value="anoint2", q48_2, q49_2, na.rm=TRUE)

O20 <- O20 %>% mutate(anoint1c=car::recode(anoint1, "1:2=1; 3:5=0"))

#q74, q70, q65
O20 <- O20 %>% gather(key="dnq", value="dna1", q65_1, q70_1, q74_1, na.rm=TRUE)
O20 <- O20 %>% gather(key="dnq", value="dna2", q65_2, q70_2, q74_2, na.rm=TRUE)
O20 <- O20 %>% gather(key="dnq", value="dna3", q65_3, q70_3, q74_3, na.rm=TRUE)
O20 <- O20 %>% gather(key="dnq", value="dna4", q65_4, q70_4, q74_4, na.rm=TRUE)
O20 <- O20 %>% gather(key="dnq", value="dna5", q65_5, q70_5, q74_5, na.rm=TRUE)
O20 <- O20 %>% gather(key="dnq", value="dna6", q65_6, q70_6, q74_6, na.rm=TRUE)

O20 <- O20 %>% mutate(dnq=frcode(dnq=="q65_6" ~ "Control",
                                 dnq=="q70_6" ~ "Minority Treatment",
                                 dnq=="q74_6" ~ "Majority Treatment"))

O20 %>% dplyr::select(dna1, dna2, dna3, dna4, dna5, dna6) %>% alpha(., check.keys=TRUE) #a=.76
O20 %>% dplyr::select(dna1, dna2) %>% alpha(., check.keys=TRUE) #a=.67
O20 %>% dplyr::select(dna3, dna4, dna5, dna6) %>% alpha(., check.keys=TRUE) #a=.8

O20 <- O20 %>% mutate(dnafull=((dna1+dna2+dna3+dna4+dna5+dna6)/60),
                      dnamaj=((dna1+dna2)/20),
                      dnaeq=(dna3+dna4+dna5+dna6)/40)

#Combining the how democratic is the US questions
O20 <- O20 %>% gather(key="hd_item", value="howdem", q75_1, q71_1, q66_1, na.rm=TRUE)
O20 <- O20 %>% mutate(hd_item=frcode(hd_item=="q66_1" ~ "Control",
                                     hd_item=="q71_1" ~ "Minority Treatment",
                                     hd_item=="q75_1" ~ "Majority Treatment"))

#Nationalism
O20 %>% dplyr::select(q78_1, q78_2, q79_1, q79_2, q79_3, q79_4, q79_5) %>% 
  alpha(., check.keys = TRUE) #a=.85

O20 <- O20 %>% mutate(natl=(((6-q78_1)+(6-q78_2)+q79_1+q79_2+q79_3+q79_4+q79_5)-7)/28)

O20 <- O20 %>% mutate(natl1=(((6-q78_1)+(6-q78_2))-2)/2)
O20 <- O20 %>% mutate(natl2=((q79_1+q79_2+q79_3+q79_4+q79_5)-5)/23)

#Christian place in the world stuff -- Q37s
O20 <- O20 %>% mutate(q38_1r = 6-q38_1,
                      q38_2r = 6-q38_2,
                      q38_3r = 6-q38_3,
                      q38_4r = 6-q38_4,
                      q38_5r = 6-q38_5,
                      q38_6r = 6-q38_6,
                      q38_7r = 6-q38_7,
                      q38_8r = 6-q38_8,
                      q38_9r = 6-q38_9,
                      q38_10r = 6-q38_10)
O20 <- O20 %>% mutate(q38_1rc=car::recode(q38_1r, "1:3=0; 4:5=1"),
                      q38_2rc=car::recode(q38_2r, "1:3=0; 4:5=1"),
                      q38_3rc=car::recode(q38_3r, "1:3=0; 4:5=1"),
                      q38_4rc=car::recode(q38_4r, "1:3=0; 4:5=1"),
                      q38_5rc=car::recode(q38_5r, "1:3=0; 4:5=1"),
                      q38_6rc=car::recode(q38_6r, "1:3=0; 4:5=1"),
                      q38_7rc=car::recode(q38_7r, "1:3=0; 4:5=1"),
                      q38_8rc=car::recode(q38_8r, "1:3=0; 4:5=1"),
                      q38_9rc=car::recode(q38_9r, "1:3=0; 4:5=1"),
                      q38_10rc=car::recode(q38_10r, "1:3=0; 4:5=1"))

#Clergy Speech
O20 <- O20 %>% mutate(q59_1=car::recode(q59_1, "NA=0"),
                      q59_2=car::recode(q59_2, "NA=0"),
                      q59_3=car::recode(q59_3, "NA=0"),
                      q59_4=car::recode(q59_4, "NA=0"),
                      q59_5=car::recode(q59_5, "NA=0"),
                      q59_6=car::recode(q59_6, "NA=0"),
                      q59_7=car::recode(q59_7, "NA=0"),
                      q59_8=car::recode(q59_8, "NA=0"),
                      q59_9=car::recode(q59_9, "NA=0"),
                      q59_10=car::recode(q59_10, "NA=0"),
                      q59_11=car::recode(q59_11, "NA=0"),
                      q59_12=car::recode(q59_12, "NA=0"),
                      q59_13=car::recode(q59_13, "NA=0"),
                      q59_14=car::recode(q59_14, "NA=0"),
                      q59_15=car::recode(q59_15, "NA=0"),
                      q59_16=car::recode(q59_16, "NA=0"))

O20 <- O20 %>% mutate(speech=q59_1+q59_2+q59_3+q59_4+q59_5+q59_6+q59_7+q59_8+q59_9+q59_10+
                        q59_11+q59_12+q59_14+q59_15+q59_16)
O20 <- O20 %>% mutate(speech10=car::recode(speech,"10:16=10"))


#RELTRAD Stuff####
O20 <- O20 %>% mutate(white=car::recode(q142_1, "NA=0"),
                      hispanic=car::recode(q142_2, "NA=0"),
                      black=car::recode(q142_3, "NA=0"),
                      asian=car::recode(q142_4, "NA=0"),
                      other=car::recode(q142_5, "NA=0"))

O20 <- O20 %>% mutate(race=case_when(white==1 & (black!=1 & hispanic!=1 & asian!=1 & other!=1) ~ 1,
                                     black==1 ~ 2,
                                     asian==1 & (black!=1 & hispanic!=1) ~ 4,
                                     hispanic==1 ~ 3,
                                     other==1 & (black!=1 & hispanic!=1 & asian!=1)~ 5))
O20 <- O20 %>% mutate(race=frcode(race=="1" ~ "White",
                                  race=="2" ~ "Black",
                                  race=="3" ~ "Hispanic",
                                  race=="4" ~ "Asian",
                                  race=="5" ~ "Other"))

#RELTRAD Stuff####
O20 <- O20 %>% 
  mutate(religpew = q7) %>%  
  mutate(religpew_protestant = car::recode(q8, "14=90; 15=90")) %>% 
  mutate(religpew_baptist = q9) %>% 
  mutate(religpew_methodist = q10) %>% 
  mutate(religpew_nondenom = q11) %>% 
  mutate(religpew_lutheran = q12) %>% 
  mutate(religpew_presby = q13) %>% 
  mutate(religpew_pentecost = q14) %>% 
  mutate(religpew_episcop = q15) %>% 
  mutate(religpew_christian = q16) %>% 
  mutate(religpew_congreg = q17) %>% 
  mutate(religpew_holiness = q18) %>% 
  mutate(religpew_reformed = q19) %>% 
  mutate(religpew_advent = q20)

O20 <- O20 %>% 
  mutate(religpew_baptist = car::recode(religpew_baptist,"11=90")) %>% 
  mutate(religpew_methodist = car::recode(religpew_methodist,"6=90")) %>% 
  mutate(religpew_nondenom = car::recode(religpew_nondenom,"6=90")) %>% 
  mutate(religpew_presby = car::recode(religpew_presby,"7=90")) %>% 
  mutate(religpew_pentecost = car::recode(religpew_pentecost,"10=90")) %>% 
  mutate(religpew_episcop = car::recode(religpew_episcop,"5=90")) %>% 
  mutate(religpew_christian = car::recode(religpew_christian,"4=90")) %>% 
  mutate(religpew_congreg = car::recode(religpew_congreg,"4=90")) %>% 
  mutate(religpew_holiness = car::recode(religpew_holiness,"7=90")) %>% 
  mutate(religpew_reformed = car::recode(religpew_reformed,"3=90")) %>% 
  mutate(religpew_advent = car::recode(religpew_advent,"4=90")) 

## Baptist

O20 <- O20 %>%
  mutate(sbc = car::recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = car::recode(sbc, "2=1; else=0"))

O20 <- O20 %>%
  mutate(abc = car::recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = car::recode(abc, "2=1; else=0"))

O20 <- O20 %>%
  mutate(ibc = car::recode(religpew_baptist, "5=1; else=0")) 

O20 <- O20 %>%
  mutate(bgc = car::recode(religpew_baptist, "6=1; else=0")) 

O20 <- O20 %>%
  mutate(mbc = car::recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = car::recode(mbc, "2=1; else=0"))

O20 <- O20 %>%
  mutate(cb = car::recode(religpew_baptist, "8=1; else=0")) 

O20 <- O20 %>%
  mutate(fwb = car::recode(O20$religpew_baptist, "9=1; else=0")) 

O20 <- O20 %>%
  mutate(gabb = car::recode(O20$religpew_baptist, "10=1; else=0")) 

O20 <- O20 %>%
  mutate(obc = car::recode(O20$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = car::recode(obc, "2=1; else=0"))

O20 <- O20 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

##Christian
O20 <- O20 %>% 
  mutate(evanchr = car::recode(religpew_christian, "1:3=1; else=0"))

## Methodist
O20 <- O20 %>%
  mutate(fmc = car::recode(religpew_methodist, "2=1; else=0")) 

O20 <- O20 %>%
  mutate(omc = car::recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = car::recode(omc, "2=1; else=0"))

O20 <- O20 %>% 
  mutate(evanmeth = fmc + omc)

#Reformed
O20 <- O20 %>% 
  mutate(evanref=car::recode(religpew_reformed, "2=1; else=0"))

##Non-Denom
O20 <- O20 %>% 
  mutate(nd = car::recode(religpew_nondenom, "1:90=1; else=0"))

## Lutheran 
O20 <- O20 %>% 
  mutate(mz = car::recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = car::recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(nalc = car::recode(religpew_lutheran, "5=1; else=0")) %>% 
  mutate(evanluth = mz + wi + nalc)

## Presbyterian

O20 <- O20 %>% 
  mutate(evanpres = car::recode(religpew_presby, "2:6=1; else=0"))

## Pentecostal 
O20 <- O20 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
O20 <- O20 %>% mutate(acna = car::recode(religpew_episcop, "2=1; else=0"),
                      aoc = car::recode(religpew_episcop, "3=1; else=0"),
                      evanepis = acna + aoc)

## None

## Congregregational
O20 <- O20 %>% 
  mutate(evancong = recode(religpew_congreg, "2:3=1; else=0"))

## Holiness
O20 <- O20 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
O20 <- O20 %>% 
  mutate(evanadv = recode(religpew_advent, "1:90=1; else=0"))

## None 

## Totaling Up

O20 <- O20 %>% 
  mutate(evangelical = evanbap + evanmeth  + evanluth + evanpres + evanpent + 
           evancong + evanholy + evanepis + evanadv + evanchr + evanref) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

O20 <- O20 %>% 
  mutate(abc = car::recode(religpew_baptist, "2=1; 4=1; else=0"))

O20 <- O20 %>% 
  mutate(epis = car::recode(religpew_episcop, "1=1; else=0"))

O20 <- O20 %>% 
  mutate(luth = car::recode(religpew_lutheran, "1=1; 4=1; else=0"))

O20 <- O20 %>% 
  mutate(meth = car::recode(religpew_methodist, "1=1; 90=1; else=0"))

O20 <- O20 %>% 
  mutate(pres = car::recode(religpew_presby, "1=1; 90=1; else=0"))

O20 <- O20 %>% 
  mutate(cong = car::recode(religpew_congreg, "1=1; 3=1; 90=1; else=0"))

O20 <- O20 %>% 
  mutate(reform = car::recode(religpew_reformed, "1=1; 3=1; else=0"))

O20 <- O20 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong  + reform) %>% 
  mutate(mainline = car::recode(mainline, "1:5=1; else=0"))

## Black Protestant 

O20 <- O20 %>% 
  mutate(meth = car::recode(religpew_methodist, "3:4=1; else=0"))

O20 <- O20 %>%
  mutate(sbc = car::recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = car::recode(sbc, "2=1; else=0"))

O20 <- O20 %>% 
  mutate(nbap = car::recode(religpew_baptist, "3=1; else=0"))

O20 <- O20 %>%
  mutate(bpabc = car::recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(bpabc = black + bpabc) %>% 
  mutate(bpabc = car::recode(bpabc, "2=1; else=0"))

O20 <- O20 %>%
  mutate(miss = car::recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = car::recode(miss, "2=1; else=0"))

O20 <- O20 %>%
  mutate(obap = car::recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = car::recode(obap, "2=1; else=0"))

O20 <- O20 %>%
  mutate(ometh = car::recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = car::recode(ometh, "2=1; else=0"))

O20 <- O20 %>% 
  mutate(apos = car::recode(religpew_pentecost, "6=1; 7=1; else=0"))

O20 <- O20 %>%
  mutate(open = car::recode(religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = car::recode(open, "2=1; else=0"))

O20 <- O20 %>%
  mutate(holy = car::recode(religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = car::recode(holy, "2=1; else=0"))


O20 <- O20 %>% 
  mutate(bprot = meth + sbc + nbap + bpabc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = car::recode(bprot, "1:2=1; else=0"))

## Everything Else

O20 <- O20 %>% 
  mutate(catholic = car::recode(religpew, "2=1; else=0"))

O20 <- O20 %>% 
  mutate(jewish = recode(religpew, "6=1; else=0"))

O20 <- O20 %>% 
  mutate(other = recode(religpew, "3=1; 7:10=1;  else=0"))  #maybe also "something else"?

O20 <- O20 %>% 
  mutate(none = recode(religpew, "11:13=1; else=0"))

O20 <- O20 %>% 
  mutate(reltrad = frcode(evangelical == 1 ~ "Evangelical",
                          mainline == 1 ~ "Mainline",
                          bprot == 1 ~ "Black Prot.",
                          catholic == 1 ~ "Catholic",
                          jewish == 1 ~ "Jewish",
                          other == 1 ~ "Other Faith",
                          none == 1 ~ "No Religion", 
                          nd == 1 ~ "Non-Denominational",
                          TRUE ~ "Unclassified"))

O20 <- O20 %>% mutate(christian=case_when(reltrad=="Evangelical" | 
                                            reltrad=="Mainline" | 
                                            reltrad=="Catholic" | 
                                            reltrad=="Black Prot." | 
                                            reltrad=="Non-Denominational" ~ 1,
                                          TRUE ~ 0))

#By hand Open Endeds ####

#O20 %>% filter(reltrad=="Unclassified") %>% ct(q7)
#q7_14_text <- O20 %>% filter(reltrad=="Unclassified") %>% ct(q7_14_text)
#write.csv(q7_14_text, paste0(O20dir, "q7_14_text.csv"))
#O20 %>% filter(reltrad=="Unclassified") %>% group_by(ba, black) %>% ct(q7_14_text) %>% as.data.frame()
#q8_16_text <- O20 %>% filter(reltrad=="Unclassified") %>% group_by(ba, black) %>% ct(q8_16_text) %>% as.data.frame()
#write.csv(q8_16_text, paste0(O20dir, "q8_16_text.csv"))

#library(stringr)
#O20 <- O20 %>% mutate(q7_14_text=str_replace(q7_14_text, "'", ""))
#O20 <- O20 %>% mutate(q7_14_text=str_replace(q7_14_text, "????T", ""))
#O20 <- O20 %>% mutate(q8_16_text=str_replace(q7_14_text, "'", ""))


O20 <- O20 %>% mutate(reltrad2=frcode(reltrad=="Evangelical" | 
                                           q7_14_text=="Babptist" |
                                           q7_14_text=="baptist" |
                                           q7_14_text=="Baptist" |
                                           q7_14_text=="BAPTIST" |
                                           q7_14_text=="Born Again Christian" |
                                           q7_14_text=="christian" |
                                           q7_14_text=="Christian" |
                                           q7_14_text=="Crishtian" |
                                           q7_14_text=="cristian" |
                                           q7_14_text=="Evangelical Christian" |
                                           q7_14_text=="kistan" |
                                           q7_14_text=="pentecostal" |
                                           q8_16_text=="advertist" |
                                           q8_16_text=="Bethany Church of the Nazarene" |
                                           q8_16_text=="Born again" |
                                           q8_16_text=="christian" |
                                           q8_16_text=="Christian" |
                                           q8_16_text=="Church of chris" |
                                           q8_16_text=="Church of the Nazarine" |
                                           q8_16_text=="Evangelical Free" |
                                           q8_16_text=="Evangelical Free Church" |
                                           q8_16_text=="Hope EFC" |
                                           q8_16_text=="Nazarene" |
                                           q8_16_text=="other christain" ~ "Evangelical",
                                         reltrad=="Mainline" | 
                                           q7_14_text=='Chriastain' |
                                           q7_14_text=='Christan' |
                                           q7_14_text=='Lutheran' |
                                           q7_14_text=='United methodist' |
                                           q8_16_text=='quaker' |
                                           q8_16_text=='Wesleyan' ~ "Mainline",
                                         reltrad=="Black Prot." |
                                           q8_16_text=="Christian" & black==1 |
                                           q7_14_text=="Christian" & black==1 |
                                           q7_14_text=="Baptist" & black==1 |
                                           q7_14_text=="BAPTIST" & black==1 |
                                           q7_14_text=="Christian" & black==1 ~ "Black Prot.",
                                         reltrad=="Catholic" | 
                                           q8_16_text=='catholik' ~ "Catholic",
                                         reltrad=="Jewish" | q7_14_text=="Hebrew" ~ "Jewish",
                                         reltrad=="Other Faith" | 
                                           q7_14_text=="Bahai" |
                                           q7_14_text=="Holy Faith" |
                                           q7_14_text=="Jehovahs Witness" |
                                           q7_14_text=="Jehovah witness" |
                                           q7_14_text=="Jehovahs Witness" |
                                           q7_14_text=="Jehovahs Witness" |
                                           q7_14_text=="spiritual" |
                                           q7_14_text=="Spiritual" |
                                           q7_14_text=="Spiritual Christian" |
                                           q7_14_text=="spirituality" |
                                           q7_14_text=="Spirualist" |
                                           q7_14_text=="Taoism" |
                                           q7_14_text=="unitarian universalist" |
                                           q7_14_text=="Unitarian Universalist" |
                                           q7_14_text=="Wicca" |
                                           q7_14_text=="wiccan" |
                                           q7_14_text=="Wiccan" |
                                           q7_14_text=="Zoroastrianism" ~ "Other Faith",
                                         reltrad=="Non-Denominational" | 
                                           q7_14_text=="Nondeniminational Christian" |
                                           q8_16_text=="non-denominational" |
                                           q8_16_text=="Red letter Christian" ~ "Non-Denominational",
                                         reltrad=="No Religion" | 
                                           q7_14_text=="Atheist" |
                                           q7_14_text=="n" |
                                           q7_14_text=="No religion" |
                                           q7_14_text=="None" |
                                           q7_14_text=="Nothing" |
                                           q7_14_text=="Nuthinh" |
                                           q7_14_text=="Pagan" |
                                           q8_16_text=="dont belong to any specific one" |
                                           q8_16_text=="I belong to no church" |
                                           q8_16_text=="I do not belong to any particular church group" |
                                           q8_16_text=="No particular  church" |
                                           q8_16_text=="none" |
                                           q8_16_text=="None" |
                                           q8_16_text=="Not a member" ~ "No Religion",
                                         TRUE ~ "Unclassified"))
                                           
O20 %>% ct(reltrad2)
O20 %>% ct(reltrad2, wt=wgt) %>% 
  ggplot(., aes(x=reltrad2, y=pct)) + geom_col() +coord_flip() +
  geom_text(aes(label=pct*100), nudge_y = .02)

O20 <- O20 %>% mutate(evan=frcode(reltrad2=="Evangelical" ~ "Evangelical",
                                  TRUE ~ "Non-Evangelical"))

#Antisemitic Tropes####
O20 <- O20 %>% mutate(q107_1r=car::recode(q107_1, "2=0"),
                      q107_2r=car::recode(q107_2, "2=0"),
                      q107_3r=car::recode(q107_3, "2=0"),
                      q107_4r=car::recode(q107_4, "2=0"),
                      q107_5r=car::recode(q107_5, "2=0"),
                      q107_6r=car::recode(q107_6, "2=0"),
                      q107_7r=car::recode(q107_7, "2=0"),
                      q107_8r=car::recode(q107_8, "2=0"))
O20 <- O20 %>% mutate(tropes=q107_1r+q107_2r+q107_3r+q107_4r+q107_5r+q107_6r+q107_7r+q107_8r)
O20 <- O20 %>% mutate(tropes_factor=as.factor(tropes))


#Difference with Church over Trump
O20 <- O20 %>% mutate(congdiff=abs(q60_2-q47_9))
O20 <- O20 %>% mutate(clergydiff=abs(q60_1-q47_9))

#Affective polarization####
O20 <- O20 %>% mutate(polar=abs(q47_7-q47_8))

#Identifying Duplicate IP cases####
IPs <- O20 %>% ct(ipaddress)
IPs <- IPs %>% mutate(duplicate=car::recode(n, "1=0;2:5=1"))
O20 <- left_join(O20, IPs, by="ipaddress")
rm(IPs)
#rm(list=ls(pattern="f"))

#write.csv(O20, file=paste0(O20dir, "O20_with_weights.csv"))