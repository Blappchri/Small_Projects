library(dplyr)
library(ggplot2)
library(tram)

#source https://github.com/awosoga/peaks-and-primes/blob/master/olympics_data.csv
data <- read.csv("~/Documents/website/olympics_data.csv")

#goal of analysis: How big are performance gaps between placements?
# planned model: result stratified by discipline and adjusted to point in the right direction

#checking distribution of placements
data%>%
  group_by(event)%>%
  slice_max(Place,n=1)%>%
  ungroup()%>%pull(Place)%>%
  table()
table(data$Place)
#decision: Only suppport highest few and bunch rest together

#checking which results aren't just numeric
check<-data%>%
  mutate(num_mark=as.numeric(Mark))%>%
  filter(is.na(num_mark))%>%
  distinct(num_mark,Mark)
table(data$event)
#some cleaning required

#checking gaps in placements
check2<-data%>%
  filter(grepl(x=event_meta,ignore.case = FALSE,pattern = "Final"))%>%
  group_by(event,games)%>%
  arrange(Place)%>%
  summarize(pattern=paste0(Place,collapse = ", "))%>%
  ungroup()
#need to consider qualifications. Otherwise leave ties as is

#first iteration of prep
for_mod<-data%>%
  filter(!grepl("Wheelchair",event))%>%#too rare in data
  filter(!grepl("Mixed",event))%>%#too rare in data
  filter(!grepl("(old)",event))%>%#too rare in data
  filter(event!="Women's 10 Kilometres Race Walk")%>%#too rare in data
  filter(Place!="DNF")%>%
  filter(Place!="DNS")%>%
  filter(Place!="DQ")%>%
  filter(!is.na(Place))%>%
  mutate(
    Type=case_when(#make sure high is always better
      grepl("Metres",event)~"time",
      grepl("Kilometres",event)~"time",
      grepl("Wheel",event)~"time",
      grepl("Marathon",event)~"time",
      TRUE~"other"
    ),
    Mark_cleaned=case_when(#taken from project above
      Type=="time"~difftime(
        lubridate::parse_date_time2(
          Mark, orders = c("%H:%M:%S", "%M:%S:00", "%M:%OS", "%OS"), 
          exact = T
          ),
        lubridate::parse_date_time2("0", orders ="S"),
        units = "secs"
      )%>%as.numeric(),
      TRUE~as.numeric(Mark)
    ),
    Mark_cleaned=case_when(#also from linked code. should cover lubridate cornercases
      is.na(Mark_cleaned)~readr::parse_number(Mark),
      TRUE~Mark_cleaned
    ),
    Performance=case_when(#model is easier if bigger is always better
      Type=="time"~-Mark_cleaned,
      TRUE~Mark_cleaned
    )
    )%>%
  mutate(
    Place=case_when(
      !grepl(x=event_meta,pattern="Final",ignore.case=FALSE)~"Qualifications",
      Place<9~Place%>%as.character(),
      TRUE~"Above8"
    )%>%factor(levels = c(8,1:7,"Above8","Qualifications")),
    Performance=as.numeric(Performance),
    Event=as.factor(event)
  )%>%
  select(Place,Performance,Event)

summary(for_mod)

#Fit a small test model to identify other issues

#rest of preparation
for_mod2<-for_mod%>%
  mutate(
    Gender=case_when(
      grepl("Women",Event)~"f",
      TRUE~"m"
    )%>%factor(),
    Event=stringr::str_remove(Event,"Women's "),
    Event=stringr::str_remove(Event,"Men's "),
    Event=factor(Event)
  )%>%
  group_by(Event,Gender)%>%
  mutate(#move to 0-1 scale
    Performance=Performance-min(Performance),
    Performance=Performance/max(Performance)
    )%>%
  ungroup()
table(for_mod2$Event)
table(for_mod2$Place)

mod<-Lehmann(Performance|Event~Gender+Place,data=for_mod2,bounds=c(0,1))
summary(mod)


for_plot<-data.frame(
  x=c(2,1,3,(4:10)+0.5),
  y=c(exp(c(coef(mod)[2:8],0,coef(mod)[9:10]))),
  col=c("g","s","b",rep("dull",5),"l","l"),
  width=c(1,1,1,rep(0.8,5),0.6,0.6),
  outline=c(rep("none",8),"yes","yes")
  )

fig<-for_plot%>%
  ggplot(aes(x=x,y=y,fill=col,col=outline))+
  geom_hline(yintercept = 1,linewidth=2)+
  geom_bar(stat = "identity",width = for_plot$width)+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill="grey95")
    )+
  scale_x_continuous(
    NULL,
    breaks = for_plot$x,
    labels = c(
      "Gold","Silver","Bronze","4th","5th","6th","7th","8th", 
      "Other Results\nduring Finals","All Results\nbefore Finals")
  )+
  coord_cartesian(ylim=c(0.1,5.1),expand = FALSE,xlim=c(0.3,11))+
  scale_fill_manual(NULL,values=c(g="#d6af36",s="#d7d7d7",b="#a77044",dull="#e9c2d9",l="white"))+
  scale_y_continuous(
    '"Attempt-Ratio"\n(Lehmann Alternative)',
    breaks = (1:10)/2,minor_breaks = NULL,
    labels = c("0.5","1","1.5","2","2.5","3","3.5","4","4.5","5")
    )+
  scale_colour_manual(values = c("none"=alpha("black",alpha = 0),yes="black"))+
  annotate("label",x=5.5,y=5, 
           label="Reading Example: With a bit of rounding, athletes winning 
Gold are at 5, meaning we estimate that their first try
is equal to the best result among 5 attempts by athletes
who placed 8th
Data Source: github.com/awosoga/peaks-and-primes",
           hjust=0,vjust=1)+
  ggtitle("How Good is a Gold? - An Analysis of Track-and-Field Results from Atlanta to Tokyo")

ggsave(
  plot = fig,
  device = "png",
  dpi=300,
  units = "cm",
  width = 25,
  height = 14,
  filename = "Gold_Lehmann.png")

#result for use linkedin text
(for_plot$y[3]/for_plot$y[4])%>%MASS::fractions(cycles = 2)
