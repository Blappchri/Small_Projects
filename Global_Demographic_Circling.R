#data from ourworldindata

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readxl)

cont_linking <- read_excel("cont_linking.xlsx") #to link countries to continents
main <- read.csv("./population-and-demography.csv") #actual data

#extract appropiate population differences from original data
main_rel<-main%>%
  filter(Year%in%c(2003,2023))%>%
  filter(Code!="")%>%
  filter(Entity!="World")%>%
  left_join(
    cont_linking%>%
      select(
        cont=Continent,
        Code=`ISO-alpha3 Code`))%>%
  mutate(
    cont=case_when(
      Entity=="Taiwan"~"Asia",
      Entity=="Kosovo"~"Europe",
      TRUE~cont
    ))%>%
  transmute(
      Entity=Entity,
      Continent=cont,
      year=Year,
      #pop_old=X65..years,
      pop_young=X5.14.years+X0.4.years,
      pop_old=X65..years
  )%>%
  tidyr::pivot_wider(names_from = year,values_from = pop_old:pop_young)%>%
  transmute(
    Entity=Entity,
    Continent=Continent,
    Change_o=(pop_old_2023-pop_old_2003)/1000000,
    Change_y=(pop_young_2023-pop_young_2003)/1000000,
    #Change_y=pop_mid_2023-pop_mid_2000
  )%>%
  mutate(
    Continent=case_when(
      Continent%in%c("Oceania","North America","South America")~"Americas and Oceania",
      TRUE~Continent
    )%>%factor(levels=c("Asia","Europe","Americas and Oceania","Africa","Sum"))
  )

#some tresholds for what should get summarized into the 'other' category
tres_x=Inf
tres_y=Inf
tres_line=4.5
tres_weighted=Inf

#final step of data prep
for_fig<-main_rel%>%
  mutate(
    Entity=case_when(
      abs(Change_o)>tres_x~Entity,
      abs(Change_y)>tres_y~Entity,
      sqrt(abs(Change_o)^2+abs(Change_y)^2)>tres_line~Entity,
      abs(Change_o)*2+abs(Change_y)>tres_weighted~Entity,
      TRUE~"Rest of\nGroup"
    )
  )%>%
  group_by(Continent,Entity)%>%
  summarize(
    Change_o=sum(Change_o),
    Change_y=sum(Change_y)
  )%>%
  ungroup()%>%
  mutate(
    order_cat=case_when(
      Change_o>0&Change_y>0~1,
      Change_o>0~2,
      !Change_y>0~3,
      TRUE~4
    ),
    order_size=abs(Change_o)+abs(Change_y),
    order_is_group=Entity=="Rest of\nGroup",
    order_angle=atan(Change_y/Change_o)
    )%>%
  #arrange(Continent,order_is_group,order_cat,-order_size)%>%
  arrange(Continent,-order_angle)%>%
  mutate(
    pos_x=cumsum(Change_o),
    pos_y=cumsum(Change_y),
    start_x=lag(pos_x,default=0),
    start_y=lag(pos_y,default=0)
    )%>%
  select(-order_size,-order_cat)
  
cont_sum<-for_fig%>%
  group_by(Continent)%>%
  summarize(
    pos_x=last(pos_x),
    pos_y=last(pos_y),
    start_x=first(start_x),
    start_y=first(start_y),
    type="b"
  )

world_sum<-for_fig%>%
  summarize(
    pos_x=sum(Change_o),
    pos_y=sum(Change_y),
    start_x=0,
    start_y=0,
    type="b",
    Continent="Sum"
  )

fig<-for_fig%>%
  mutate(type="a")%>%
  bind_rows(
    cont_sum
  )%>%
  bind_rows(
    world_sum
  )%>%
  ggplot(aes(
    x=start_x,xend=pos_x,
    y=start_y,yend=pos_y,
    col=Continent,linetype=type,label=Entity
  ))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_segment()+
  geom_point(data=for_fig,aes(x=pos_x,y=pos_y,col=Continent),inherit.aes = FALSE)+
  scale_y_continuous("Change in Population 0to14 (millions)")+
  scale_x_continuous("Change in Population 65+ (millions)")+
  coord_fixed()+
  scale_color_manual("Continent(s)",
    values=c(
      "Europe"="royalblue2",
      "Asia"="deeppink",
      "Americas and Oceania"="red4",
      "Africa"="forestgreen",
      "Sum"="black"
      )
  )+
  theme_minimal()+
  scale_linetype_discrete(guide=NULL)+
  geom_label_repel(
    aes(
      x = (start_x+pos_x)/2,
      y = (start_y+pos_y)/2,
    ),
    min.segment.length = 0,
    show.legend = FALSE,nudge_x = -12,nudge_y = -9,segment.color="black"
  )+
  ggtitle("Change in global Demographics 2023 vs 2003")+
  theme(legend.position = "bottom",plot.background = element_rect(fill="white"))+
  annotate("label",x=5,y=150, 
           label="Data Source: ourworldindata.org",
           hjust=0,vjust=1)

ggsave(
  plot = fig,
  device = "png",
  dpi=300,
  units = "cm",
  width = 27,
  height = 18.25,
  filename = "fig_circling.png")
