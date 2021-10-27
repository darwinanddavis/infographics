### My First Year  
# matt malishev
# @darwinanddavis

### devtools::session_info()
# reinstall all packages 
### update.packages(ask = FALSE, dependencies = c('Suggests'))

# pcks --------------------------------------------------------------------
pacman::p_load(purrr,dplyr,ggplot2,readr,lubridate,stringr,here,ggthemes,patchwork,cowplot,ggtext,colorspace,ggdist)
here::set_here() 

# load data ---------------------------------------------------------------
d <- "data" # choose month or total period
data <- paste0("full_record_2020/",d,".csv") %>% read_csv(trim_ws = T) 
colnames(data) <- c("Activity","Trait","Start","Finish","Value")
data[c("Activity", "Trait")] <- sapply(data[c("Activity", "Trait")],as.character)
svg_path <- here::here("/img") # set path for svg/img files 

# convert time to posix ---------------------------------------------------
# convert to posix
data <- data %>% 
  mutate_at(c("Start","Finish"),dmy_hm) %>% 
  # mutate_at(c("Start","Finish"),with_tz,tzone = "Australia/Melbourne") %>%  # convert to AEST-Melb
  mutate(timediff = c(Finish - Start) %>% hms::as_hms()) # create time diff col
# get first year
tmin <- data %>% group_by(Activity) %>% summarise("Min" = Start %>% min) # get min start date
data <- data %>% # add one year and subset below
  mutate(End = as_date(tmin$Min + years(1)) %>% max) %>% 
  filter(as_date(Start) < End)

# subset activities -------------------------------------------------------

growth <- data %>% filter(Activity=="Growth");growth
feeding <- data %>% filter(Activity=="Feeding");feeding
sleeping <- data %>% filter(Activity=="Sleep");sleeping
diaper <- data %>% filter(Activity=="Diapering");diaper
leisure <- data %>% filter(Activity=="Leisure");leisure 
health <- data %>% filter(Activity=="Health"); health

# clean data --------------------------------------------------------------

# growth
growth$Value <- growth$Value %>% str_remove_all("cm|kg") %>% as.numeric() # remove strings 

# feeding 
feeding %>% filter(Trait == "Meal") %>% pull(Value) %>% unique # get unique values 
protein <- "Beef|beef|Lamb|lamb|Chick|chick|Egg|egg|Bang|bang|Lent|lent"
fruits <- "banan|berry|apple|pear|mango|plum|Frui|frui|melon"
veg <- "Advo|advo|Avo|avo|Ava|ava|Zucc|zucc|Potat|potat|parsn|carr|Carr|Pump|pump|Veg|veg"
rice <- "Ric|ric"
cereal <- "Cere|cere|Grai|grai|Porr|porr|Breakf|breakf|Bix|bix|Semo|semo"
bread <- "bread|Bread|toas|Toas|sand"
yoghurt <- "yoghurt|yogurt|yog|Yog|Yogur|Yoghur"
dessert <- "pudd|Pudd|cust|Cust|pie|Pie"
snack <- "Snac|snac"
feeding[str_which(feeding$Value,cereal),"Value"]  # check above strings
feeding$Value[complete.cases(feeding$Value)] %>% length
meal <- feeding %>% filter(Trait == "Meal") %>%  
  mutate_at("Value", funs(
    case_when(Value %>% str_detect(protein) ~ "Protein",
              Value %>% str_detect(fruits) ~ "Fruit",
              Value %>% str_detect(veg) ~ "Vegetable",
              Value %>% str_detect(rice) ~ "Rice",
              Value %>% str_detect(cereal) ~ "Cereal",
              Value %>% str_detect(bread) ~ "Bread",
              Value %>% str_detect(yoghurt) ~ "Dairy",
              Value %>% str_detect(dessert) ~ "Dessert",
              Value %>% str_detect(snack) ~ "Snack",
              TRUE ~ "Other"))) 
feeding <- feeding[feeding$timediff>0,] # use time values > 0 (removes meal)
bottle <- feeding %>% filter(Trait == "Bottle") # get bottle and keep formula and clean value strings
bottle <- bottle %>% 
  filter(!Value %in% str_subset(bottle$Value,"breas|left")) %>% # rm breast milk 
  mutate_at("Value", funs(
  Value %>% str_remove_all(",|formula|ml") %>% str_trim("both") %>% as.numeric() # keep just numeric formula values
)) 
bottle <- bottle[complete.cases(bottle),] # rm empty cells

# health
temp <- health %>% filter(Trait=="Temperature") # %>% # get temp and make daily timeframe 
  # mutate_at("Start", funs(Start %>% hms::as_hms()))
temp$Value <- temp$Value %>% str_remove_all("℃") %>% as.numeric() # remove unnecessary strings 

health %>% filter(Trait == "Medication") %>% pull(Value) %>% unique # get unique values 
nurofen <- "neu|Neu|nuro|Nuro|nf|Nf"
panadol <- "Pan|pan"
laxative <- "colo|Colo|coll"
allergy <- "tel|Tel"
colic <- "Infan|infan"
health <- health %>% filter(Trait == "Medication") %>%  # rename meds 
  mutate_at("Value", funs(
    case_when(Value %>% str_detect(allergy) ~ "Allergy",
              Value %>% str_detect(colic) ~ "Colic",
              Value %>% str_detect(nurofen) ~ "Ibuprofen", # nurofen = ibuprofen
              Value %>% str_detect(panadol) ~ "Paracetamol", # panadol = paracematmol 
              Value %>% str_detect(laxative) ~ "Laxative",
              TRUE ~ "Other"))) 
health$Value %>% table

# check available activity states with traits
dfc <- list(growth,feeding,diaper,leisure,health,sleeping)
dlist <- dfc %>% map("Trait") %>% map(unique) ; dlist
here("data",list.files("data/")) %>% map(readRDS) %>% map(names)

# plots --------------------------------------------------------------------

# plot params
width <- 40
height <- 40
units <- "cm"
cex <- 20 # 10
lw <- 5
blue_am <- "#9BBFDD"
blue_pm <- "#273871"
green_am <- "#69ADA1"
green_pm <- "#175268"
grays <- "#BDBDBD"
family <- "DK Lemon Yellow Sun" 

# plot functions  -----------------------------------------------
# base plot params
my_theme <- function(){
  family <- family
  list(theme_classic(),
       labs(x = xlab, y = ylab),
       theme(
         # axis.ticks = element_line(color = fg), # element_blank(),
         # axis.title = element_text(color = fg),
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks = element_line(size = 1.5, color = "#000000"),
         axis.line = element_line(size = 1.5),
         axis.title = element_text(face = "bold"),
         axis.text = element_text(face = "bold", color = "#000000"), 
         panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         text = element_text(size = 40,  family = family, face = "bold", color  = "#000000"),
         legend.background = element_rect(fill = "transparent"), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA),
         legend.key.size = unit(2, "cm"),
         legend.position = c(0.9,0.9), # xy from bottom left
         legend.title = element_text(size = 22),
         legend.text = element_text(size = 22),
         plot.margin=unit(rep(0.25,4),"cm"),
       panel.ontop = F)
  )
}

# polar hlines 
geom_polar_hline1 <- function(ylim){
    geom_hline(yintercept=seq(0,ylim,ylim/4), linetype="dotted",color = grays, size=0.2)
}
geom_polar_hline2 <- function(ylim){
    geom_hline(yintercept=ylim+0.25, linetype="solid", color = grays, size=0.3) 
}

# save as png with transparent bg 
my_save <- function(fh,ttl) ggsave(here::here("plots","_final",fh %>% str_to_lower()) %>% paste0("/",ttl %>% str_to_lower(),".png"), dpi = "retina", width = width, height = height, bg = "transparent", units = units, limitsize = F)

# breast feeding ------------------------------------------------
colpal <- c(blue_pm,blue_am)
d <- feeding %>% 
  mutate(AM = Start %>% am, # get am/pm
         Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month, 
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) %>% 
  mutate_at("timediff", funs(timediff %>% minute)) # change time to integer 

trait1 <- "Left Breast"
trait2 <- "Right Breast"
ttl <- "breastfeeding"
xlab <- "" #"TIME (MINS)"
ylab <- "" # "DENSITY"
# limits <- c(-0.0045,0.0045)  # for time axis
# xtick <- c(-0.004, -0.002, 0, 0.002, 0.004) # for time axis 
# colpal <- sequential_hcl(30, coln)[c(4,18)]

ggplot() +
  geom_density(data = d %>% filter(Trait %in% trait1), aes(x = timediff, y = ..density.., col = AM, fill = AM)) +
  geom_density(data = d %>% filter(Trait %in% trait2), aes(x = timediff, y = -..density.., col = AM, fill = AM)) +
  geom_hline(yintercept = 0,col = "#FFFFFF", size = 0.5) +
  # geom_segment_func(t1_mean) + geom_segment_func(t2_mean) +
  facet_wrap(~Meridian, nrow = 4) +
  scale_colour_manual(values = colpal,aesthetics = "col", guide = F) + 
  scale_fill_manual(values = adjustcolor(colpal,0.7),aesthetics = "fill", guide = F) + 
  # scale_y_continuous(limits = limits, breaks = xtick,labels = xtick) + # for time axis
                     # labels = seq(limits[1],limits[2],0.00225) %>% format(scientific = T)) + 
  coord_flip() + labs(x = xlab, y = ylab) + my_theme() + 
  theme(strip.background = element_blank(), # rm facet strips
    strip.text.x = element_blank())
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 

# bottle  -----------------------------------
coln <- "Dark Mint" 
d <- bottle %>% 
  filter(Value > min(Value)) %>%  # remove outliers
  mutate(AM = Start %>% am,
         Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month, # get am/pm
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) %>% 
  mutate_at("AM",funs(case_when( # change T/F to AM/PM # use when adding legend (switches order of colours)
    AM == TRUE ~ "AM", T ~ "PM"))) %>%
  mutate_at("timediff", funs(timediff %>% as.POSIXct()))  # convert to posix for scale_x_datetime() (boxplot)

# bottle chord
library(circlize)
# devtools::install_github("jokergoo/circlize")

# circ plot params
png(here::here("plots","_final","feeding","bottle_chord.png"), # save to dir
    width = width, height = height, units = units, bg = "transparent", res = 500)

circos.clear()
par(mar = rep(0, 4))
circos.par(start.degree = -185, 
           gap.degree = 2, track.margin = c(-0.1, 0.1), 
           points.overflow.warning = F,
           track.height = 0.2)
circos.par(cell.padding =c(0.02, 0, 0.02, 0))
circos.initialize(sectors = d$AM,x = d$Value) # set sectors/factors. not needed for table
colpal <- c(blue_am,blue_pm,sequential_hcl(70,coln)[seq(1,70,7)]) # set colpal
dtab <- d %>% select(AM,Value) %>% table()  #%>% t # create table for circ

# plot 
chordDiagram(dtab,
             grid.col = colpal,
             transparency = 0.5,
             directional = 1, # 1 = link origin is from sectors
             diffHeight  = -0.05,
             link.border = "#FFFFFF",
             annotationTrack = c("grid"
                                 # ,"name" # to check name placment
                                 ),
             annotationTrackHeight = c(0.05, 0.1),
             big.gap = 5, small.gap = 2, link.sort = T, 
             link.largest.ontop = T
             # order = union(rownames(dtab) %>% rev,colnames(dtab)), # set link order
             # xmax = 0.2
             )

dev.off() # close plot save
         
# timediff and value  chord 
dtab <- table(d$timediff %>% as.POSIXct() %>% format("%H:%M"),
              d$Value)

# diaper --------------------------------------------------------
colpal <- c(green_pm,green_am)
d <- diaper %>% 
  mutate(Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month,
         AM = Start %>% am,
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric)

limits <- d$Trait %>% unique
xlab <- "HOUR OF DAY"
ylab <- "DENSITY"
ttl <- d$Trait[1] %>% str_to_lower() 

# density
ggplot() +
  geom_density(data = d, aes(Meridian, fill = AM), col = "#FFFFFF", show.legend = F) +
  scale_fill_manual(values = adjustcolor(colpal,0.7),aesthetics = "fill") + 
  facet_wrap(~Trait) +
  scale_x_continuous(breaks = d$Meridian %>% unique) +
  labs(x = xlab, y = ylab) +
  my_theme() +
  theme(strip.background = element_blank(), # rm facet strips
        strip.text.x = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 10)) 
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 


# GREENS --------------------------------------------------------

# growth ------------------------------------------------------------------
d <- growth
xlab <- "TIME"
ylab <- "VALUE"
colpal <- c(green_pm,green_am,blue_pm)
colpal

ggplot(data = d, aes(x = Start, y = Value, col = Trait, fill = Trait)) +
    geom_point(size = 10) + geom_line(size = 3) +
    scale_fill_manual(values = colpal, aesthetics = c("col","fill"), guide = F) +
    my_theme() 
my_save(d$Activity[1],d$Activity[1] %>% paste0("_legend")) # save to dir 


# sleeping ------------------------------------------------------
# plot PM first
colpal <- c("#E8EBE9",green_pm) # "#E0F2E6"
d <- sleeping %>% 
  mutate(AM = Start %>% am,
         Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month,
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) # get 12 hour time

xlab <- "HOUR OF DAY"
ylab <- "MONTH"
leg <- "Sleep time (hours)"
ttl <- d$Activity[1] %>% str_to_lower() 

# heatmap 
ggplot() +
  geom_tile(data = d,aes(x=Meridian,y=Month,fill=timediff %>% as.POSIXct())) + 
  scale_fill_datetime(name = leg, low = colpal[1], high = colpal[2]) +
  facet_wrap(~AM) +
  scale_x_continuous(breaks = d$Meridian %>% unique) +
  scale_y_continuous(breaks = d$Month %>% unique,
                     labels = month(d$Month %>% unique,label = T,abbr = T)) +
  labs(x = xlab, y = ylab) +
  my_theme() +
  theme(legend.position = "bottom",
        strip.background = element_blank(), # rm facet strips
        strip.text = element_blank()) + 
  guides(fill = guide_colorbar(title.position = "bottom",
                               frame.colour = colpal[1],
                               title.hjust = 0.5)) # shift title right 
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 

# temp ----------------------------------------------------------
colpal <- c(blue_pm,green_pm,green_am)
colpal %>% scales::show_col()

d <- temp %>% 
  mutate(Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month,
         "Temp" = case_when(
           Value < 36.5 ~ colpal[2],
           Value > 38 ~ colpal[3],
           T ~ colpal[1])
  ) %>%
  arrange(Value) # arrange temp values

limits <- d$Trait %>% unique
temp_mean <- 37.5 # set mean temp 
xx <- d$Value
yy <- 1:nrow(d)
xlab <- "TEMP (C)"
ylab <- "COUNT"
ttl <- d$Trait[1] %>% str_to_lower() 
labelxy <- c(2,10,23) # label xy
label_df <- data.frame(
  xx = xx[labelxy],
  yy = yy[c(5,10,23)],
  label = c("Below average","Average","Above average") %>% str_to_upper() #%>% str_replace_all(" ","\n")
  ,col = d[labelxy,"Temp"] %>% pull(Temp)
  )

ggplot(data = d) +
  geom_vline(xintercept = temp_mean, linetype = 3) +
  geom_segment(aes(x=temp_mean, xend=Value, y=yy, yend=yy, col = Temp), size = 2) +
  geom_point(aes(x=Value, y=yy, col = Temp), size = 8) +
  scale_fill_manual(values = colpal, aesthetics = "col", guide = F) +
  labs(x = xlab, y = ylab) +
  # geom_text(data = label_df, aes(x = xx, y = yy, label = label, col = col), size = 5, family = family, show.legend = F,
  #           hjust = -1,vjust = 1) + 
  my_theme()
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 

# BLUES ---------------------------------------------------------

# meds -----------------------------------------------------------------
# plot AM first
colpal <- c(blue_am,blue_pm)
d <- health %>% 
  mutate(AM = Start %>% am,
         Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month, # get am/pm
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) %>% 
  mutate_at("Finish",month) %>% 
  mutate_at("AM",funs(case_when(AM == TRUE ~ "AM",T ~ "PM")))  # change T/F to AM/PM

limits <- c(c("Ibuprofen","Paracetamol", "Colic", "Allergy", "Laxative") %>% rev(), "Other")
xlab <- "MEDICATION"
ylab <- "TOTAL COUNT"
ttl <- xlab

# dotplot 
ggplot() + 
  geom_dotplot(data = d, aes(Value,fill = AM), col = colpal[2],
               binwidth = 0.5,
               dotsize = 0.5,
               position = "dodge",
               show.legend = T,
               key_glyph = "point"
  ) +
  scale_x_discrete(limits=limits) + # reorder xaxis
  scale_y_continuous(NULL, breaks = NULL) + # hide y axis
  scale_fill_manual(name = "AM/PM", values = colpal, aesthetics = c("fill","col"), guide = F) +
  my_theme() +
  # guides(fill = guide_legend(override.aes = list(size = 10))) + # override legend point size
  theme(axis.line.y = element_blank()) # rm y axis
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 

ggsave(here::here("plots","tweet",d$Activity[1] %>% str_to_lower()) %>% paste0("/",ttl %>% str_to_lower(),".png"), dpi = "retina", width = width, height = height, units = units, limitsize = F)

# meal ----------------------------------------------------------
# plot AM first 
colpal <- c(blue_am,blue_pm)
d <- meal %>% filter(Value != "Other") %>% 
  mutate(AM = Start %>% am,
         Meridian = Start %>%  # get 12 hour time
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) %>% 
  mutate_at("Value",funs( # group grains 
    case_when(Value %>% str_detect("Bread|Cereal|Rice") ~ "Grain",
              Value %>% str_detect("Snack|Dessert") ~ "Snack",
              T ~ Value))) %>% 
  mutate_at("AM",funs(case_when(AM == TRUE ~ "AM",T ~ "PM"))) # change T/F to AM/PM

ylim <- 15 # max hist count for yy 
cex <- 10  # label text and bg point size 
limits <- seq(0,ylim,5)
xtext <- data.frame("x" = c(3,6,9,12), "y" = ylim + 4) # outer clock margin 
ytext <- data.frame("x" = 3, "y" = limits[-1], "limits" = limits[-1]) # y text markers
xlab <- ""
ylab <- ""
ttl <- d$Trait[1] %>% str_to_lower() 
# d$id <- seq(1, nrow(d)) %>% sqrt # get data for reverse y axis and sqrt tranform

ggplot() +
  # x lines/labels
  geom_hline(yintercept = xtext$y, color = grays, linetype = 1) + # add outer xline
  geom_point(data = xtext, aes(x = x, y = y),  size = cex + 2, col = "#FFFFFF", show.legend = F) + # add empty bg point for xtext
  geom_text(data = xtext, aes(x = x, y = y, label = x), size = cex, col = grays, family = family, show.legend = F) + 
  # y lines/labels
  geom_hline(yintercept = limits, color = grays, size = 0.1, linetype = 1) + # add ylines 
  geom_point(data = ytext, aes(x = x, y = y), size = cex + 2, col = "#FFFFFF", show.legend = F) + # add empty bg point for ytext
  geom_text(data = ytext, aes(x = x, y = y, label = limits), size = cex/2, col = grays, family = family, show.legend = F) + 
  # hist
  geom_histogram(data = d, aes(Meridian, fill = AM), col = "#FFFFFF", size = 0.1, bins = 12, position = "stack", show.legend = F) + # normal polar
  scale_fill_manual(values = colpal,aesthetics = "fill", guide = F) + # normal
  # geom_histogram(data = d, aes(Meridian,id,fill = AM, col = AM), size = 0, position = "stack", stat = "identity", show.legend = F) + # reverse polar
  # scale_fill_manual(values = adjustcolor(colv,1),aesthetics = c("col","fill")) + # reverse
  facet_wrap(~Value) +
  coord_polar(start = 0.25) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(limits = c(ylim*-0.5,NA), breaks = limits, labels = limits) + # set negative ylim to get bigger inner circle 
  # scale_y_sqrt(limits = c(-10,NA), breaks = seq(0,ylim,ylim/4), labels = seq(0,ylim,ylim/4)) +
  labs(x = xlab, y = ylab) +
  # geom_segment(aes(x=days*0.25-0.5,y=ymax+ybuff,xend=days*0.25+0.5,yend=ymax+ybuff),color=grays) +
  my_theme() + 
  theme(strip.background = element_blank(), # rm strips
        strip.text.x = element_blank(),
        axis.line = element_blank(), # rm y axis stuff 
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.background = element_blank())
my_save(d$Trait[1],ttl %>% paste0("_final")) # save to dir 

# leisure -------------------------------------------------------
colpal <-  c(blue_am,green_am,blue_pm)
d <- leisure %>% 
  # filter(timediff != timediff %>% max) %>% # remove outlier for boxplot plot
  mutate(AM = Start %>% am,
         Hour = Start %>% hour,
         Day = Start %>% day,
         Month = Start %>% month, # get am/pm
         Meridian = Start %>% 
           round_date(unit = "hour") %>% # round off hour
           format("%I") %>% as.numeric) %>% 
  mutate_at("Trait",funs( # group grains 
    case_when(Trait %>% str_detect("Play|Tummy") ~ "Play time",
              T ~ Trait))
  )

limits <- c("Bath time","Play time","Outdoors") %>% rev
xlab <- "TIME (HOURS)"
ylab <- "DENSITY"
ttl <- d$Trait[1] %>% str_to_lower() 
limits <- c(0,0.0012)  
ytick <- c(0,limits[2]/2,limits[2])
labels <- c(0,"6e-04","12e-03")

# density 
ggplot() +
  geom_density(data = d, aes(x = timediff, fill = Trait), col = "#FFFFFF", show.legend = F) +
  scale_fill_manual(values = adjustcolor(colpal,0.8),aesthetics = c("fill")) + 
  scale_y_continuous(limits = limits,breaks = ytick,labels = labels) +
  labs(x = xlab, y = ylab) +
  my_theme()
my_save(d$Activity[1],ttl %>% paste0("_final")) # save to dir 

# final stats ---------------------------------------------------

data_final <- rbind(growth,feeding,sleeping,diaper,leisure,health,meal[meal$Value!="Other",],temp)
data_final %>% nrow  # total data points

# get total time
total_time_func <- function(time) data_final %>% pull(timediff) %>% time %>% sum()
c(total_time_func(hour),total_time_func(minute),total_time_func(second)) %>% 
  period(units = c("hour","minute","second")) %>% 
  as.period("hours")

data_final %>% # unique traits 
  pull(Value) %>% 
  str_replace_all(","," ") %>% 
  str_split_fixed(pattern = " ", n = Inf) %>% # parse notes into separate words
  str_trim("both") %>%
  table %>% attr("dimnames") %>% 
  lengths


