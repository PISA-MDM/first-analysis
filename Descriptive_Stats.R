

library(EdSurvey)
library(lme4)
library(WeMix)
#library(flexplot) not working
library(tidyverse)
library(nlme)
library(merTools)
library(irr)


#####################################
# Read data 
###################################




#read in data
#downloadPISA("C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data", year=2018) #only forst time


sdf <- readPISA(path = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",countries="DEU")

Sys.time() - start_time # runtime for reading in data


#data preparation

global.scales <- c("GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                   "GCAWARE",#Student's awareness of global issues (WLE)
                   "PERSPECT",#Perspective-taking (WLE)
                   "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                   "AWACOM",#Awareness of intercultural communication (WLE)
                   "INTCULT",#Student's interest in learning about other cultures (WLE)
                   "RESPECT",#Respect for people from other cultures (WLE)
                   "GLOBMIND",#Global-mindedness (WLE)
                   "ATTIMM")
global.scales <- str_to_lower(global.scales)

pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")
pv <- str_to_lower(pv)


id.vars <- c("cntschid","cntstuid")


wt.vars <- c("w_fstuwt", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "w_schgrnrabwt", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "w_fstuwt_sch_sum") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                  "repeatgrade",
                  "ST003D03T", #year of birth
                  "progn") # School classification %>% 

control.vars <- str_to_lower(control.vars)

# Make variables lower case


#overwiew on the variables
student <- c("ST001D01T",#Grade
             "ST004D01T",#Student (Standardized) Gender
             "HISCED",#Highest Education of parents (ISCED)
             "HISEI",#Highest In-ternational Socio-Economic Index of Occupational Status
             "PARED",#Index highest parental education in years of schooling
             "IMMIG",#Index Immigration status
             "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
             "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
             #"REPEAT",# Grade repetition
             "repeatgrade",
             "GCSELFEFF",#Self-efficacy regarding global issues (WLE)
             "GCAWARE",#Student's awareness of global issues (WLE)
             "PERSPECT",#Perspective-taking (WLE)
             "COGFLEX",#Cognitive flexibility/adaptability (WLE)
             "AWACOM",#Awareness of intercultural communication (WLE)
             "INTCULT",#Student's interest in learning about other cultures (WLE)
             "RESPECT",#Respect for people from other cultures (WLE)
             "GLOBMIND",#Global-mindedness (WLE)
             "ATTIMM", "ST003D03T")#Student's attitudes towards immigrants (WLE) and student year of birth

student_lower <- str_to_lower(student)

school <- c(
  "SC013Q01TA",#Is your school a public or a private school?
            "SC042Q01TA",#School's policy for <national modal grade for 15-year-olds>: Students are grouped by ability into different classes.
          "SC042Q02TA",#School's policy for <national modal grade for 15-year-olds>: Students are grouped by ability within their classes.
            "SCMCEG",#School principal's view on teachers' multicultural and egalitarian beliefs (WLE)
            "STUBEHA",#Student behaviour hindering learning (WLE)
            "TEACHBEHA",#Teacher behaviour hindering learning (WLE)
            "SC048Q01NA",#Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language>
            "SC048Q02NA",#Percentage <national modal grade for 15-year-olds>: Students with special needs
            "SC048Q03NA",#Percentage <national modal grade for 15-year-olds>: Students from socioeconomically disadvantaged homes
  "SCHSIZE",#School Size (Sum)
  "SCHLTYPE")# School Ownership

school_lower <- str_to_lower(school)


# Remove NA and omitted Levels
#om <- c("Invalid","N/A","Missing","Miss",NA,"(Missing)")

#om2018 <- getAttributes(sdf,'omittedLevels')


### Get Data

pisa.sel <- EdSurvey::getData(data = sdf,
                              varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = F) # don?t return replicate weights



pisa.sel2 <- EdSurvey::getData(data = sdf,
                               varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                               omittedLevels = F,
                               returnJKreplicates = TRUE, # Necessary to make functions work
                               addAttributes = T) # dataframe can be used for EdSurvey functions

############### variables #############################

# "st001d01t"   "st004d01t"   "hisced"      "hisei"       "pared"       "immig"       "st127q01ta"  "st127q02ta"  "repeatgrade" "gcselfeff"   "gcaware"    
# "perspect"    "cogflex"     "awacom"      "intcult"     "respect"     "globmind"    "attimm"    "schsize"  "schltype" 


### new variables
### PROGN with German school names


pisa.sel<- pisa.sel%>%
  mutate(progn_de = factor(case_when(progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL" ~ "Berufsschule", #7
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY (SPECIAL EDUCATION)" ~ "Förderschule", #1
                                     progn == "GERMANY: UPPER SECONDARY (VOCATIONAL), QUALIFYING FOR SUBJECT-SPECIFIC TERTIARY EDUCATIO" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY; BASIC GENERAL EDUCATION" ~ "Hauptschule", # 2
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY NO ACCESS TO UPPER; BASIC GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; EXTENSIVE GENERAL EDUCATION" ~ "Realschule", # 3
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION (EXCLUSIVELY STU" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (STUDENTS OF DIFFE" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: UPPER SECONDARY (EXCLUSIVELY STUDENTS OF THE SAME TRACK [CF. KEY 4])" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION (STUDENTS OF DIFF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACHIEVEMENT-BASED ACCESS TO UPPER SECONDARY (WIT" ~ "Integrierte Gesamtschule", # 5
                                     progn == "GERMANY: LOWER SECONDARY WITH ACCESS TO UPPER (WALDORF SCHOOL)" ~ "Integrierte Gesamtschule", #5
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (DIF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: PRE-VOCATIONAL TRAINING YEAR UPPER SECONDARY LEVEL" ~ "Berufsschule", # 7
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL UPPER SECONDARY LEVEL" ~ "Berufsschule"))) # 7


# Relevel to baseline Hauptschule
pisa.sel$progn_de <- relevel(pisa.sel$progn_de, ref="Hauptschule")



####### mutate ST001D01T",#Grade  -  Tatjana #########
pisa.sel<- pisa.sel%>%
  mutate(#st001d01t = as.numeric(st001d01t),
    st001d01t_ad = factor(case_when(st001d01t <= 9 ~ "Grade 7-9",
                                    st001d01t >= 10 ~ "Grade 10-12")))

# Relevel to baseline Hauptschule
pisa.sel$st001d01t_ad <- relevel(pisa.sel$st001d01t_ad, ref="Grade 7-9")


# calculate school hisei
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei, na.rm = TRUE)) %>% ungroup()


########################## Explore variables UNWEIGHTED ##################################################

# NA sensitive analysis bu gender - Immig
summary2(pisa.sel2, "gcselfeff")


pisa.sel2M <- subset(pisa.sel2, st004d01t %in% "MALE")
summary2(pisa.sel2M, "gcselfeff")

pisa.sel2F <- subset(pisa.sel2, st004d01t %in% "FEMALE")
summary2(pisa.sel2F, "gcselfeff")

pisa.sel2 <- pisa.sel2  %>%
  mutate(age = 2018 - st003d03t)

# NA by Immig
edsurveyTable(gcselfeff ~ immig, data=pisa.sel2)
summary2("immig", data=pisa.sel2)

NATIVE <- subset(pisa.sel2, immig %in% "NATIVE")
summary2(NATIVE, "gcselfeff")

summary2(pisa.sel2, "immig")

SECONDGENERATION <- subset(pisa.sel2, immig %in% "SECOND-GENERATION")
summary2(SECONDGENERATION, "gcselfeff")

FIRSTGENERATION <- subset(pisa.sel2, immig %in% "FIRST-GENERATION")
summary2(FIRSTGENERATION, "gcselfeff")

# type of school 
levelsSDF(varnames = "schltype", data = sdf)
summary2(pisa.sel2, "st001d01t")
PRIVATE_INDEPENDEN <- subset(pisa.sel2, schltype %in% "PRIVATE_INDEPENDENT")

#new school type variable
#summary2(pisa.sel2, "progn_de")
summary(pisa.sel$progn_de)


pisa.sel %>% # Count NA by group
  group_by(st004d01t) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))))

pisa.sel %>% 
  group_by(immig) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))))

pisa.sel %>% 
  group_by(progn_de) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))))


#Grade
pisa.sel %>% 
  group_by(st001d01t_ad) %>% 
  summarize_all(.funs = funs('NA' = sum(is.na(.))))


#Grade
summary2(pisa.sel2, "st001d01t")
# hisei
summary2(data = pisa.sel2, "hisei")

######## Plot with EdSurvey##########################################################################

# Boxplot for pv1 read by sex : used in master thesis
box1 <- ggplot(pisa.sel, aes(x=st004d01t, y=pv1read)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  coord_flip() +
  labs(title = "")  +
  ggplot2::labs (y = "Reading literacy PV1read",
                 x = "", 
                 title ="")
box1

#summary statistics
summary2(sdf, 'immig')
edsurveyTable(pv1read ~ st004d01t, data = sdf)

# beeswarm plot with flexplot
#flexplot(pv1read ~ st004d01t, data=pisa.sel)


ggplot(pisa.sel2, aes(x = gcselfeff , fill = st004d01t)) + geom_histogram(bins =) +
  ggplot2::labs (x = "SelfGlobal",
                 color = "ST004D01T", 
                 title ="title") + theme_bw()


# global competence by gender : : used in master thesis
label=c("Female","Male")   
boxplot(gcselfeff ~ st004d01t, data=pisa.sel2, ylab="Self-efficacy", xlab= "", names = label)
boxplot(pv1read ~ st004d01t, data=pisa.sel2, ylab="Reading literacy", xlab= "", names = label)

# reading by immigration ?????????
boxplot(pv1read ~ immig, data=pisa.sel2, ylab="Reading literacy", xlab= "")

# not used in master thesis
ggplot(data = pisa.sel2, aes(x = pv1read, fill = st004d01t)) + 
  geom_boxplot(size = .75) +   facet_grid( ~ immig , margins = FALSE) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  +
  ggplot2::labs (x = "Reading literacy",
                 color = "Gender") 
#title ="Change in sad by group: CAPI and mixed mode") + #caption = "Data: pairfam wave 11 and 12")

ggplot(data = pisa.sel, aes(x = pv1read)) + 
  geom_boxplot(size = .75) +   facet_grid(~ immig , margins = FALSE) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

ggplot(pisa.sel2, aes(x= pv1read , y =st004d01t)) +
  geom_point() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip()

ggplot(pisa.sel2, aes(y= pv1read, fill = immig)) +
  geom_histogram()  +
  coord_flip() 

ggplot(pisa.sel2, aes(x= pv1read , y =st004d01t)) +
  geom_point() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip()  +   facet_grid(~ immig , margins = FALSE)

# Histogram for hisei
hist.hisei <- ggplot(pisa.sel, aes(x=hisei)) +
  geom_histogram() +
  labs(title = "hisei")
hist.hisei
