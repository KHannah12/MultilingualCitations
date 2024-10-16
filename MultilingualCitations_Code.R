#### Chapter 2 Code - Second Edition ####

library(readr)
library(dplyr)
library(ggplot2)
library(MASS)
library(scales)
library(performance)
library(car)
library(stringr)
library(circlize)
library(tidyverse)

alldat <- read.csv("https://raw.githubusercontent.com/KHannah12/MultilingualCitations/main/ch2Combined.csv") 

alldat <- alldat %>% mutate(Language=as.factor(Language),
                            Script=as.factor(Script),
                            LangFamily=as.factor(LangFamily),
                            StudyDesign=as.factor(StudyDesign),
                            Category=as.factor(Category),
                            TorNT=as.factor(TorNT),
                            English_Abstract=as.factor(English_Abstract),
                            IUCN=as.factor(IUCN),
                            StudyDesign_cat=as.factor(StudyDesign_cat))

alldat <- within(alldat, Language <- relevel(Language, ref = 'English'))
alldat <- within(alldat, English_Abstract <- relevel(English_Abstract, ref = 'No'))
alldat <- within(alldat, StudyDesign <- relevel(StudyDesign, ref = 'After'))
alldat <- within(alldat, IUCN <- relevel(IUCN, ref = 'Least Concern'))
alldat <- within(alldat, Category <- relevel(Category, ref = 'Birds'))
alldat <- within(alldat, TorNT <- relevel(TorNT, ref = 'Not Threatened'))
alldat <- within(alldat, StudyDesign_cat <- relevel(StudyDesign_cat, ref = 'Less complex'))

CE_dat <- alldat[265:437,]
NEL_dat <- alldat[1:264,]

CE_dat <- CE_dat %>% mutate(Language=as.factor(Language),
                            Script=as.factor(Script),
                            LangFamily=as.factor(LangFamily),
                            StudyDesign=as.factor(StudyDesign),
                            Category=as.factor(Category),
                            TorNT=as.factor(TorNT),
                            English_Abstract=as.factor(English_Abstract))

NEL_dat <- NEL_dat %>% mutate(Language=as.factor(Language),
                              Script=as.factor(Script),
                              LangFamily=as.factor(LangFamily),
                              StudyDesign=as.factor(StudyDesign),
                              Category=as.factor(Category),
                              TorNT=as.factor(TorNT),
                              English_Abstract=as.factor(English_Abstract),
                              IUCN)
NEL_dat <- within(NEL_dat, Language <- relevel(Language, ref = 'French'))

## Remove languages with <10 entries per lang for analysis
alldat_reducedlang <- alldat[!(alldat$Language %in% c('Arabic', 'Italian', 'Persian', 'Portuguese', 
                                                      'Traditional Chinese', 'Ukrainian')), ]
NEL_reducedlang <- NEL_dat[!(NEL_dat$Language %in% c('Arabic', 'Italian', 'Persian', 'Portuguese', 
                                                     'Traditional Chinese', 'Ukrainian')), ]

#### Figure 3 - Chord Diagram ####
allchord <- read.csv("https://raw.githubusercontent.com/KHannah12/MultilingualCitations/main/collapsed_10langs.csv") 
allchord <- allchord %>% remove_rownames %>% column_to_rownames(var="Language")
allchord <- as.matrix(allchord)

col = c('English Citation'="#A6CEE3",'French Citation'="#E31A1C",'French Article'="#E31A1C",
        'Dutch Citation'="#CAB2D6",'Russian Citation'="#FF7F00",'Russian Article'="#FF7F00",
        'Spanish Citation'="#B2DF8A",'Spanish Article'="#B2DF8A",'German Citation'="#1F78B4",
        'German Article'="#1F78B4",'Polish Citation'="#FB9A99",'Polish Article'="#FB9A99",
        'Portuguese Citation'="#6A3D9A",'Portuguese Article'="#6A3D9A",'Catalan Citation'="#B15928",
        'Hungarian Article'="#33A02C",'Hungarian Citation'="#33A02C",'Korean Citation'="#FDBF6F",
        'Korean Article'="#FDBF6F",'Japanese Citation'="#FFFF99",'Japanese Article'="#FFFF99",
        'Italian Article'="Light Blue",'Italian Citation'="Light Blue",'Simplified Chinese Citation'="Grey",
        'Simplified Chinese Article'="Grey",'Other Citations'="#4050CB",'Persian Article'="aquamarine1",
        'Persian Citation'="aquamarine1",'Slovenian Citation'="lightpink2",'Czech Citation'="maroon1")

chordDiagram(allchord,grid.col=col,annotationTrack = "grid", preAllocateTracks = 1) # create plot with no labels

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=1.1)
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

### Without Labels ### 
chordDiagram(allchord, grid.col=col, annotationTrack = "grid", preAllocateTracks = 1)

# Customize the track to print only the numbers (excluding category labels)
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  # Print axis with larger numbers
  circos.axis(h = "top", labels.cex = 0.8, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)


#### Figure 4 - Percentage bar chart ####
cross <- read.csv("https://raw.githubusercontent.com/KHannah12/MultilingualCitations/main/Data/Figure1.csv") 
cross <- cross %>% mutate(Language=as.factor(Language))

cross_longer <- pivot_longer(cross,4:6, names_to = "Cit_Type",values_to = "Count")
cross_longer$Language<- str_wrap(cross_longer$Language, width=11)

level_order <- c("French","Italian","Spanish","Portuguese","German","Hungarian","Polish",
                 "Russian","Korean","Japanese","Simplified\nChinese","Traditional\nChinese")

ggplot(cross_longer,aes(x=Language, y=Count, fill=Cit_Type))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab("Proportion of Citations"))+
  scale_x_discrete(limits = level_order)+
  labs(x="Article Language", title = "Rates of cross language citation for each article language", fill = "Citation Type")+
  scale_fill_manual(labels=c("Cross-language citation","Citation by English language papers","Citation from same language"),
                    values = c("darkmagenta","darkorange","darkolivegreen3"))+
  theme_minimal() +
  theme(legend.position = "bottom",legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15))+
  annotate("text",x=1,y=-0.05,label="N=98")+ #French ## N refers to # of citations, not # of articles
  annotate("text",x=2,y=-0.05,label="N=25")+ #Italian
  annotate("text",x=3,y=-0.05,label="N=280")+ #Spanish
  annotate("text",x=4,y=-0.05,label="N=58")+ #Portuguese
  annotate("text",x=5,y=-0.05,label="N=322")+ #German
  annotate("text",x=6,y=-0.05,label="N=22")+ #Hungarian
  annotate("text",x=7,y=-0.05,label="N=28")+ #Polish
  annotate("text",x=8,y=-0.05,label="N=112")+ #Russian
  annotate("text",x=9,y=-0.05,label="N=236")+ #Korean
  annotate("text",x=10,y=-0.05,label="N=36")+ #Japanese
  annotate("text",x=11,y=-0.05,label="N=93")+ #Simp Chinese
  annotate("text",x=12,y=-0.05,label="N=6")  #Trad Chinese

#### Figure 2 - boxplot of English citations ####
m1 <- glm.nb(English ~ Language , data = alldat_reducedlang)
summary(m1) 

m2 <- glm.nb(Tot_Citations ~ Language , data = alldat_reducedlang)
summary(m2)

restructured_data <- alldat_reducedlang %>%
  dplyr::select(Language, English, Tot_Citations) %>%
  pivot_longer(cols = c(English, Tot_Citations), names_to = "Variable", values_to = "Value")

ggplot(restructured_data, aes(x = Language, y = Value, fill = Variable)) +
  geom_boxplot() +
  geom_jitter(width = 0.3, height = 0, alpha = 0.8) +
  scale_fill_manual(values = c("English" = "darkorange", "Tot_Citations" = "lightblue3"), 
                    name = "Citation origin",
                    labels = c("English Citations", "Total Citations")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(1,2,5,10,20,50,100,200,500)) +
  xlab("Article Language") +
  ylab("Number of citations") +
  theme_minimal() +
  theme(legend.position.inside = c(0.85, 0.85), 
        text = element_text(size = 16)) +
  annotate("text",x=1,y=-0.75,label="n = 171")+
  annotate("text",x=2,y=-0.75,label="n = 19")+
  annotate("text",x=3,y=-0.75,label="n = 46")+
  annotate("text",x=4,y=-0.75,label="n = 15")+
  annotate("text",x=5,y=-0.75,label="n = 60")+
  annotate("text",x=6,y=-0.75,label="n = 10")+
  annotate("text",x=7,y=-0.75,label="n = 20")+
  annotate("text",x=8,y=-0.75,label="n = 23")+
  annotate("text",x=9,y=-0.75,label="n = 20")+
  annotate("text",x=10,y=-0.75,label="n = 28")

## Sea stack plot ##
library(devtools)
install_github('Al-Stu/seastackplot')
library(seastackplot)

sea_stack_plot(data = restructured_data,
               data.column = 'Value',
               group.column = 'Language')


#### Figure 5 - English abstract scatterplot ####
m3 <- glm.nb(English ~ Year + English_Abstract + StudyDesign_cat + TorNT + Category + Lang_geneticprox 
             + Tot_samelang_cit,
             data = NEL_dat) 
summary(m3)
vif(m3) 

predat1 <- read.csv("https://raw.githubusercontent.com/KHannah12/MultilingualCitations/main/Data/PredictionDat.csv") 

predat1$predictions <- predict(m3,newdata = predat1,type = 'link')
tmp1 <- predict(m3,newdata = predat1,se.fit=T,type="link")
predat1$lower_ci <- predat1$predictions - (1.96 * tmp1$se.fit)
predat1$upper_ci <- predat1$predictions + (1.96 * tmp1$se.fit)

predat1$predictions2 <- exp(predat1$predictions)
predat1$upper_ci2 <- exp(predat1$upper_ci)
predat1$lower_ci2 <- exp(predat1$lower_ci)

ggplot(predat1, aes(x = Tot_samelang_cit, y = predictions2, color = English_Abstract, fill = English_Abstract)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci2, ymax = upper_ci2), alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.03, height = 0.03), 
             data = NEL_dat[!is.na(NEL_dat$English_Abstract), ], 
             aes(x = English, y = Tot_samelang_cit, color = English_Abstract)) +
  labs(x = "Number of same language citations", y = "Number of English citations") +
  scale_color_manual(values = c("darkorange", "darkblue"), 
                     name = "Article had an English\ntranslated abstract") +
  scale_fill_manual(values = c("darkorange", "darkblue"), 
                    name = "Article had an English\ntranslated abstract", 
                    guide = guide_legend(override.aes = list(
                      color = c("darkorange", "darkblue"),  # Legend point color
                      shape = c(16, 16)  # Legend point shape (circle)
                    ))) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,2,3,5,10,20,30)) +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,2,3,5,10,20,30)) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.9), text = element_text(size = 20)) 

#### Figure 6 - Genetic proximity box plot ####
language_colors <- c("red", "blue", "purple", "orange", "green", "hotpink", "darkred",
                     "navy", "purple4", "grey50", "darkgreen", "deeppink", "darkorange4", 
                     "cyan", "grey10")


ggplot(NEL_dat, aes(x = Lang_geneticprox, y = English)) +
  geom_point(position = position_jitter(width = 0.4, height = 0.1)) +
  labs(x = "Language lexical distance from English", y = "Number of English citations") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,2,3,5,10,20,30,50)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  geom_vline(xintercept = c(31.3, 46.9, 52.4, 52.6, 55.2, 56.5, 56.9, 59.3, 75.6, 84.9, 85.5, 87.2, 87.5, 87.9, 88.4), 
             linetype = "dashed", color = language_colors) +
  annotate("text", x = c(27,43,49,48,50,63.5,60.5,64,71,79.5,81,94,94,92.5,94), 
           y = c(10,15,10,9,4,10,12,4,15,10,7,15,7,4,1), 
           label = c("German", "French", "Italian", "Russian", "Ukrainian", "Portuguese", "Polish", "Spanish",
                     "Persian", "Simplified\nChinese", "Arabic", "Japanese", "Traditional\nChinese", "Korean",
                     "Hungarian"),
           color = language_colors, vjust = -0.5, size = 7) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank()    # Remove minor grid lines
  )



## Supplementary chord diagram
# Set global parameters to rotate the plot by 180 degrees
circos.par(start.degree = 180)

# Plot the chord diagram
chordDiagram(CEchord, grid.col = CEcol, annotationTrack = "grid", preAllocateTracks = 1) # all citations

# Adjust the track for labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  # Ensure the labels are not flipped
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.9)
  
  # Print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

# Reset circos parameters to default after plotting
circos.clear()
circos.par(reset = TRUE)