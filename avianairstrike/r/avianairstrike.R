##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                infographics
##                              avian airstrike                   
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
citation <- "ATSB"

# pcks -----------------------------------------------------
pacman::p_load(mapdeck,ggmap,dplyr,sf,sfheaders,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext,ggmap,ggspatial,svglite,readr)

# data ----------------------------------------------------------
fh <- "avianairstrike"
df  <- readRDS(here::here(fh,"data",paste0(fh,".Rda"))) %>% 
  mutate(month = Month %>% lubridate::month(label = T, abbr = T)) # create month name variable 

my_theme <- theme(panel.grid.major = element_line(colour = "transparent"),
                  plot.background = element_rect(fill = "transparent", colour = "transparent"),
                  panel.background = element_rect(fill = "transparent", colour = "transparent"),
                  # plot.margin=unit(c(-10,-10,-10,-10),NULL),
                  panel.ontop = F,
                  text = element_text(colour = col_high, size = 10),
                  axis.ticks = element_line(colour = col_high),
                  axis.text = element_text(colour = col_high),
                  legend.position = "bottom",
                  legend.background = element_rect(fill="transparent") # legend background
)

# summary stats -------------------------------------------------
# collisions per year
# collisions per state
df$`Part damaged` %>% table
df$birds_struck %>% table
df$PhaseOfFlight %>% table
df$AircraftType %>% table
df$AircraftDamageLevel %>% table
df$OperationType %>% table
df$SpeciesFamily %>% table
df$year %>% table
df$bird_size %>% table

df$SpeciesFamily %>% table %>% sort(decreasing = T)

# plots ---------------------------------------------------------
# heatmap of flight phase and bird size   
# heatmap of month and df$SpeciesFamily  
# heatmap of month and bird size   
# regression of bird_mass and Altitude  
# stacked barplot of species collision density 
df$`Species` %>% table %>% sort
# ridgeplot of species densities 
# ridgeplot of bird_mass
# smaller spatial heatmaps of each bird size hit locations   
# month/collision/bird mass relationship    
# boxplot/halfeye plot of bird_mass %>% log10 and OperationType
# greater/lesser plot similar to eli's temp plot  


ggplot() +
  geom_density(data = df, aes(bird_mass, fill = AircraftDamageLevel %>% factor,colour = AircraftDamageLevel %>% factor))

df %>% group_by(state) %>% 
  summarise(birds_struck) %>% count 

df %>% filter(state == "SA") %>% pull(birds_struck) %>% length

# country data -------------------------------------------------------------
get_projection <- F

library(rnaturalearth)
library(rnaturalearthdata)
countries <- c("Australia")
dd <- rnaturalearth::ne_states(countries, returnclass = "sf") # %>% 
  # st_geometry() + c(0, 3)
dd2 <- ne_countries("large","sovereignty",
                    returnclass = "sf") # large file with no visible detail diff to 'sp' class. use with geom_sf(). use 'sovereignty' to get smaller islands)




# google satellite map -------------------------------------------------------

# google map
# base_map <- readRDS(here::here("data","damage","google_base_map.Rda"))
# flood_damage_latlon  <-  readRDS(here::here("data","damage","flood_damage_polygon.Rda"))

# get google map
# devtools::install_github('oswaldosantos/ggsn')
require(ggmap)
require(ggsn)
api <- paste0("~/Documents/Data/gggmap/ayepeeeye.txt") %>% read_lines()
register_google(key = api)
addy <- "Alice Springs, Northern Territory"
extent <- geocode(addy, output = "more") %>% pull(address)
maptype <- "satellite"
zoom <- 4
color <- "bw"
darken <- 0.3


df %>% glimpse
# get google map
base_map <-  get_map(extent, maptype = maptype,color = color, zoom = zoom)
# base_map %>% saveRDS(here::here("data","damage","google_base_map.Rda"))


# proj
if(get_projection){
  plat <- -36.51054457652614
  plon <-  138.42564092426372
  crs <- "moll"
  prj <- paste0("+proj=",crs, " +lat_0=",plat," +lon_0=",plon) # opt1
  prj <- 2163
  # crsn <- 2163  #4340
  # prj <- paste0("+lat_0=",plat," +lon_0=",plon," +init=epsg:",crsn) # opt1
}

bb <- df %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  # st_transform(prj) %>%
  st_bbox() %>% 
  st_as_sfc() 
bb_gg <- bb %>% st_bbox
ttl <- "Avian Airstrike"
var1 <- "SpeciesFamily"
var2 <- "PhaseOfFlight" # bar/frequency plot (small plot)  
var3 <- "birds_struck" # bar plot (small plot)
var4 <- "Part damaged" # airplane graphic highlighting plane parts  
var5 <- "bird_size" # bar plot (small plot)
var6 <- "bird_mass" # bar plot (small plot)
  
df %>% .[[var6]] %>% unique

# colpal  
colv <- sequential_hcl(df %>% .[[var1]]  %>% n_distinct(), palette = "Sunset")
# limits <- df$degdamage %>% unique %>% .[c(3,2,1,4)]
lx <- df$lon[1]
ly <- df$lat[1]


# project data 
if(get_projection){
  df_ <- df %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_transform(prj)
  dd <- dd %>% st_transform(prj)
}

bb_gg <- c(plon,plat) %>% st_point() %>%  
  st_buffer(25) %>% st_bbox()


opac <- 0.5
width <- 940
bins <- 2000
col_low <- sequential_hcl(bins, "Burg")[1]
col_high <- "#F9C883"
height <- width
# colpal <- sequential_hcl(bins, "Burg") # for geom_density2d()
colv <- colorRampPalette(colors = c(col_low, col_high))
colpal <- colv(bins)
sequential_hcl(100, "PinkYl") %>% .[15] %>%  show_col(labels = F)

base_map %>% ggmap(extent = "device", legend = "bottom", darken = c(0.25,colpal[1])) +
  borders(regions = "Australia",colour = colpal[2000], size = 0.2, alpha = 0.5) +
  # geom_sf(data = hex_polygons_china, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0, inherit.aes = F) +
  # geom_hex(data = df, aes(lon,lat), show.legend = T, stat = "binhex", bins = bins, size = 0) +
  # geom_tile(data = df, aes(lon,lat,fill = year %>% factor, col = year %>% factor), show.legend = F) +
  geom_density2d(data=df, aes(lon,lat, col = ..level..), bins=bins, size = 0.1,contour_var = "count", alpha = opac, show.legend = F) +
  scale_fill_gradientn(name = "Density", colours = adjustcolor(colpal,opac), aesthetics = c("col"), na.value = "white"
                       # , guide = "none"
  ) +
  # scale_fill_gradientn(name = "Density", colours = adjustcolor(colpal,1), aesthetics = c("col"), na.value = "white"
#                        # , guide = "none"
# ) +
  # coord_sf(xlim = c(bb_gg[1],bb_gg[3]), ylim = c(bb_gg[2],bb_gg[4])) +
  theme_nothing()
  
  # geom_sf(data = df_, aes(col = .data[[var1]] %>% as.factor(), fill = .data[[var1]] %>% as.factor()),inherit.aes = F, size = 0.1, show.legend = F) +
  # geom_sf(data = dd, aes(geometry = geometry), col = "#FFFFFF", fill = NA, size = 0.1, alpha = opac, show.legend = F, inherit.aes = F) +
  # scale_color_manual(name = ttl, values = adjustcolor(colv,opac), aesthetics = c("col","fill"), na.value = "#FFFFFF"
  #                      # ,limits = limits
  #                    ) +
  # geom_sf(data = march, fill = adjustcolor(colvw %>% lighten(0.2),0.2), col = colvw %>% lighten(0.2), size = 0.2, show.legend = F, inherit.aes = F) +
  # geom_sf(data = case_studies_latlon, col = "red", fill = adjustcolor("red",opac), size = 0.3, show.legend = F, inherit.aes = F) +
  # geom_sf_label(data = case_studies_latlon, aes(centroid_x, centroid_y, label = label),size = 1, col = "red",nudge_x = 0.7, nudge_y = 0.7, fill = NA, label.size = 0, inherit.aes = F) +
  # coord_equal() +
  # scale bar
  # ggsn::scalebar(x.min = bb_gg[1], x.max = bb_gg[1] + 0.005,
  #                y.min = bb_gg[2], y.max = bb_gg[2] + 0.005,
  #                transform = T,
  #                dist = 50, dd2km = TRUE, model = 'WGS84',
  #                box.fill = c("yellow", "white"), st.color = "white"
  #                ) +
  # ggspatial::annotation_scale(location = 'bl',bar_cols = text_col, line_width = 1, pad_x = ggplot2::unit(3, "cm"), pad_y = ggplot2::unit(5, "cm"), text_col = text_col, line_col = text_col, style = "ticks", width_hint = 0.1) +
  # projection 
  # coord_sf(xlim = c(bb_gg[1],bb_gg[3]), ylim = c(bb_gg[2],bb_gg[4])) + # bbox
  # labs(title="", x=lx, y=ly,
  #      # ----- these are the legend key arguments
  #      colour=ttl,
  #      fill=ttl,
  # ) 

# ggsave(here::here("plot","spatial",paste0("flood_damage_",color,"_",height,"_",darken,"_final.svg")),device = "svg", width = width, height = height, units = "px", dpi = "screen", bg = "transparent")
ggsave(here::here("avianairstrike","img",paste0("contour_",darken,".pdf")), width = 20, height = 20, units = "cm", dpi = "retina", bg = "transparent")


# polar plot --------------------------------------------------------------


df_parts <- df %>% filter(`Part damaged` %in% c("Engine","Unknown","None","Nose","Landing gear","Tail","Wing/Rotor")) %>% 
  mutate_at("Part damaged",funs(
    case_when(`Part damaged` == "Wing/Rotor" ~ "Wings", # rename wings
              T ~ `Part damaged`
              ))) 
# df[df$`Part damaged` == "Wing/Rotor","Part damaged"]  

for(i in df_parts$`Part damaged` %>% unique){
  colpal <- colv(df_parts$year %>% n_distinct())
  d <- df_parts %>% filter(`Part damaged` == i)
  y1 <- d %>% group_by(year) %>% count(year) %>% pull(n) %>% max
  ylim <- c(-y1/5,y1)
  ggplot() +
    geom_bar(data = d, aes(`Part damaged`, fill = year %>% factor, col = year %>% factor), position = "dodge", show.legend = F) +
    scale_fill_manual(values = adjustcolor(colpal,1), aesthetics = c( "col")) +
    scale_fill_manual(values = adjustcolor(colpal,0.7), aesthetics = c( "fill")) +
    coord_polar(start = 0) +
    labs(x = NULL, y = NULL) +
    ylim(ylim) +
    my_theme +
    theme(panel.grid.major = element_line(colour = col_high %>% adjustcolor(0.5), size = 0.2))
  
  # show total count for phase of flight for each plane part 
  cat(i,"**********")
  print(
    d %>% 
    group_by(PhaseOfFlight) %>%
    count 
  )
  
  ggsave(here::here("avianairstrike","img","planeparts",paste0("planeparts_",i,".png")), device = "png", width = 10, height = 10, units = "cm", dpi = "retina", bg = "transparent")
}



# heatmap -------------------------------------------

# flight phase vs  bird species and year 
colpal <- colv(df$bird_mass %>% na.omit %>%  max)
ggplot() +
  geom_tile(data = df %>% filter(!PhaseOfFlight %in% c("NULL","Other")),aes(PhaseOfFlight %>% factor, SpeciesFamily,fill=bird_mass),colour = "transparent") +
  scale_fill_gradientn("Bird mass (kg)",colours = colpal %>% adjustcolor(0.7), aesthetics = c("fill"),na.value = "transparent") +  
  # scale_fill_gradientn(colours = colpal %>% adjustcolor(1), aesthetics = c("col")) +  
  labs(x=NULL,y=NULL) +
  # scale_y_discrete(expand=c(7,20)) +
  # expand_limits(y = 0.5) +
  my_theme +
  theme(text = element_text(colour = col_high, size = 10),
        panel.grid.major = element_line(colour = "transparent"),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent") # legend background
  )
ggsave(here::here("avianairstrike","img",paste0("heatmap.png")), device = "png", width = 60, height = 10, units = "cm", dpi = "retina", bg = "transparent")

# year vs month
colpal <- colv(df$bird_mass %>% na.omit %>%  max)
ggplot() +
  geom_tile(data = df %>% filter(!PhaseOfFlight %in% c("NULL","Other")),aes(year%>% factor, month %>% factor,fill=bird_mass),colour = "transparent") + 
  scale_fill_gradientn(NULL,colours = colpal %>% adjustcolor(0.7), aesthetics = c("fill"),na.value = "transparent") +  
  # scale_fill_gradientn(colours = colpal %>% adjustcolor(1), aesthetics = c("col")) +  
  labs(x=NULL,y=NULL) +
  # scale_y_discrete(expand=c(7,20)) +
  # expand_limits(y = 0.5) +
  my_theme

ggsave(here::here("avianairstrike","img",paste0("heatmap_month.png")), device = "png", width = 10, height = 10, units = "cm", dpi = "retina", bg = "transparent")

  

# treemap -----------------------------------------------------------------

# d <- df$OperationType %>% table %>% sort(decreasing = T)
# d <- df %>% 
#   dplyr::select(OperationType,bird_mass) %>% 
#   reshape2::melt() %>%
#   arrange(value) %>% na.omit

var1 <- "SpeciesFamily"
top <- 10 # select number of top ranked results to pull 
st <- df %>% pull(state) %>% unique # loop through all aus states
for(s in st){
  d_ <- df %>%
    filter(state == s) %>% 
    filter(!SpeciesFamily %in% c("Unknown","Other"))
  topspecies <- d_ %>% arrange() %>% pull(SpeciesFamily) %>% 
    unique  %>%
    tail(top) # comment out to include all species
  d_top20 <- d_ %>% filter(d_[[var1]] %in% topspecies)
  d <- d_top20 %>% 
    group_by(d_top20[[var1]]) %>% 
    count %>% 
    arrange(n) 
  colnames(d) <- c(var1,"n")
  colpal <- colv(d[[var1]] %>%  n_distinct())
  d$colpal <- colpal %>% alpha(0.7)
  
  # pacman::p_load(treemap)
  require(treemap)
  width <- 900
  png(here::here("avianairstrike","img","treemap",paste0("top",top,"species"),paste0("treemap_",s,".png")),width = width,height = width,units = "px",bg = "transparent")
  treemap(d,
          index=c(var1),
          # mirror.y = T,
          vSize="n",
          type="color",
          vColor = "colpal",
          # bg.labels="transparent",
          title = "",
          align.labels=list(
            c("left", "bottom")
            #, c("right", "bottom")
          ),
          fontcolor.labels = colpal,
          bg.labels=200,
          fontsize.labels = 30,
          # border.col=c("#c6c6c6","#FFFFFF"), # Color of borders of groups, of subgroups, of subsubgroups ....
          border.col = colpal,
          border.lwds=c(1,0.3) ,
          lowerbound.cex.labels = 0
  )         
  dev.off()
}

# aus state vectors
st <- dd$name %>% unique
for(s in st){
  ggplot() +
    geom_sf(data = dd, fill = NA, size = 0.5, col = colpal[1]) +
    geom_sf(data = dd %>% filter(name == s), fill = colpal[1], size = 0.1, col = bg) +
    theme_nothing()
  ggsave(here::here("avianairstrike","img","treemap","state_vectors",paste0(s,".png")), device = "png", width = 5, height = 5, units = "cm", dpi = "retina", bg = "transparent")
}


# barplot -----------------------------------------------------------------

df$OperationType %>% table
df$PhaseOfFlight %>% table
df$bird_size %>% table


d <- df %>% filter(!PhaseOfFlight %in% c("Unknown", "NULL")) %>%  group_by(PhaseOfFlight) %>% count() %>% arrange(n)
limits <- d$PhaseOfFlight %>%  unique()
colpal <- colv(d$PhaseOfFlight %>% n_distinct())
ggplot() +
  geom_col(data = d, aes(PhaseOfFlight, n, fill = PhaseOfFlight ,colour = PhaseOfFlight)) +
  scale_fill_manual(values = colpal %>% adjustcolor(0.7), aesthetics = c("fill"),na.value = "transparent", limits = limits) +  
  scale_fill_manual(values = colpal %>% adjustcolor(1), aesthetics = c("colour"),na.value = "transparent",limits = limits) +  
  scale_x_discrete(limits = limits) +
  labs(x=NULL,y=NULL) +
  coord_flip() +
  theme_nothing() +
  my_theme +
  theme(legend.position = "none")
ggsave(here::here("avianairstrike","img",paste0("columnplot_flightphase.png")), device = "png", width = 10, height = 10, units = "cm", dpi = "retina", bg = "transparent")


# ridgeplot ---------------------------------------------------------------

require(ggridges)
bg <- "#37262D"
top20species <- df %>%  filter(!SpeciesFamily %in% c("Other","Unknown")) %>% arrange(bird_mass) %>% pull(SpeciesFamily) %>% unique %>% tail(20)
d <- df %>% arrange(bird_mass) %>% 
  filter(SpeciesFamily %in% top20species) 
limits <- d$SpeciesFamily %>% unique
colpal <- colv(d$bird_mass %>% na.omit %>%  n_distinct())
ggplot() +
  geom_density_ridges_gradient(data = d, aes(bird_mass, SpeciesFamily, 
                                    fill = ..x..), alpha = 0.7, col = NA, show.legend = F) +
  scale_fill_gradientn(NULL,colours = colpal, aesthetics = c("fill")) +  
  scale_y_discrete(limits = limits) +
  labs(x=NULL,y=NULL) +
  theme_nothing() + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        text = element_text(colour = col_high, size = 10),
        axis.ticks = element_line(colour = col_high),
        axis.text = element_text(colour = col_high),
        legend.position = "none",
        legend.background = element_rect(fill="transparent") # legend background
  )
ggsave(here::here("avianairstrike","img",paste0("ridgeplot.png")), device = "png", width = 10, height = 10, units = "cm", dpi = "retina", bg = "transparent")




# point/dot plot over time ------------------------------------------------

# year vs month
limits <- d$PhaseOfFlight %>%  unique()
colpal <- colv(df$PhaseOfFlight %>% n_distinct())
ggplot() +
  geom_point(data = df, aes(OccurrenceDate,PhaseOfFlight, col = PhaseOfFlight, fill = PhaseOfFlight, size = bird_mass), shape = 21, show.legend = F) + 
  scale_fill_manual(values = colpal %>% adjustcolor(1), aesthetics = c("col"),na.value = "transparent",limits = limits) +  
  scale_fill_manual(values = colpal %>% adjustcolor(0.7), aesthetics = c("fill"),na.value = "transparent",limits = limits) +  
  scale_y_discrete(limits = limits) +
  labs(x=NULL,y=NULL) +
  my_theme 
ggsave(here::here("avianairstrike","img",paste0("dotplot_birdmass.png")), device = "png", width = 15, height = 10, units = "cm", dpi = "retina", bg = "transparent")




# NOT USED IN FINAL BUT WORKING -------------------------------------------


# hexbin map --------------------------------------------------------------

# hex
dd <- ne_countries("medium",returnclass = "sf")
china_activity_hex <- here::here("data","daily_activity","mmsi-daily-csvs-10-v2-2020") %>% list.files("2020-01-01.csv",full.names = T) %>% read_csv %>% filter(mmsi %in% china$mmsi)  
bins <- 200
colpal <- sequential_hcl(bins, "Burg")
ggplot() +
  # geom_hex(data = df, aes(lon,lat), show.legend = T, stat = "binhex", bins = bins, size = 0) +
  geom_tile(data = df, aes(lon,lat,fill = bird_size %>% factor, col = bird_size %>% factor), show.legend = F) +
  # geom_density2d(data=df, aes(lon,lat, col = ..level..), bins=bins, size = 0.2) +
  scale_fill_gradientn(colours = colpal, aesthetics = c("col","fill")) +  
  # geom_sf(data = dd, aes(geometry = geometry), color = adjustcolor(fg,opac), fill = fg, size = 0.1, inherit.aes = F) +
  theme_map()




# hex 2
# https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/ # hex example
# set new memory allocation for this session https://stackoverflow.com/questions/51248293/error-vector-memory-exhausted-limit-reached-r-3-5-0-macos
# usethis::edit_r_environ()
# usethis::edit_r_environ("project")
# Sys.setenv('R_MAX_VSIZE'=64000000000)

# varc <- "medium" # use df$bird_size %>% unique
cellsize <- 1
df_hex <- df %>% 
  # filter(bird_size == varc) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)
hex_points <- df_hex %>% as_Spatial() %>% spsample(type = "hexagonal", cellsize = cellsize)
hex_polygons <- HexPoints2SpatialPolygons(hex_points) %>%
  st_as_sf(crs = st_crs(df_hex)) # %>% st_intersection(., df_hex)
hex_polygons$fill <- lengths(st_intersects(hex_polygons, df_hex))

# save to dir
hex_polygons %>% saveRDS(here::here("avianairstrike","data",paste0("hex_polygons_cellsize_",cellsize,".Rda")))
# hex_polygons_small <- readRDS(here::here("avianairstrike","data",paste0("hex_polygons_bird_size_small.Rda"))) %>% filter(fill != 0)  # w/o projection
hex_polygons_china <- hex_polygons %>% filter(fill != 0)
colpal <- sequential_hcl(hex_polygons_china$fill %>% n_distinct()  %>% log + 1, "Burg") %>% rev  # colpal for full hex


# hex with buffer decay --------
# ddd <- hex_polygons_aus  %>%  filter(fill != 0)
# hex_buff <- 5*10^4
# hexbuff_outer <- ddd  %>% st_combine %>% st_boundary() %>% st_buffer(hex_buff,endCapStyle = "SQUARE",joinStyle = "MITRE")
# hexbuff_inner <- ddd  %>% st_combine %>% st_boundary() %>% st_buffer(hex_buff/2,endCapStyle = "SQUARE",joinStyle = "MITRE")
# colpal <- sequential_hcl(ddd$fill %>% n_distinct() %>% log + 1, "Purples")  %>% rev # colpal for hex with fill = 0
# full hex --------
# colpal <- sequential_hcl(hex_polygons_china$fill %>% n_distinct()  %>% log + 1, "Dark Mint") %>% rev   # colpal for full hex
# colpal <- sequential_hcl(hex_polygons_china$fill %>% n_distinct()  %>% log + 1, "Burg") %>% rev  # colpal for full hex

ggplot() +
  # hex with buffer decay
  # geom_sf(data =  hexbuff_outer, col = NA, fill = adjustcolor(colpal[1] %>% darken(0.1),opac/2), size = 0.2) +
  # geom_sf(data = hexbuff_inner, col = NA, fill = adjustcolor(colpal[1] %>% darken(0.2),opac/2), size = 0.2) +
  # geom_sf(data = ddd , aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.1) +
  # full hex
  # geom_sf(data = hex_polygons, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  # geom_sf(data = hex_polygons_aus, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  geom_sf(data = hex_polygons_china, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  scale_fill_gradientn(name = "Activity", colours = colpal, aesthetics = c("col"), na.value = "transparent"
                       # , guide = "none"
  ) +
  scale_fill_gradientn(name = "Activity", colours = adjustcolor(colpal,opac), aesthetics = c("fill"), na.value = "transparent", guide = "none") +
  geom_sf(data = dd, aes(geometry = geometry), color = fg %>% lighten(0.8), fill = fg, size = 0.3) +
  # geom_sf(data = bb %>%  st_as_sf(coords = c("long","lat"), crs = 4163) %>% st_transform(prj) %>% st_buffer(5*10^5), col = "black", fill = NA) +
  # coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) +
  theme_map()





# aus state border --------------------------------------------------------

ggplot() +
    geom_sf(data = dd, aes(geometry = geometry), col = "red", fill = NA, size = 0.1, alpha = opac, show.legend = F, inherit.aes = F) +
    theme_nothing() +
    theme(panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = "transparent"),
          axis.text = element_blank(),
          axis.ticks.length=unit(0, "null"),
          # plot.margin=unit(c(-10,-10,-10,-10),NULL),
          panel.ontop = F
    )
ggsave(here::here("avianairstrike","img",paste0("aus_border.png")),device = "png", width = 30, height = 30, units = "cm", dpi = "retina", bg = "transparent")





# crop map using added buffer (can only save to PNG)  
# ggplot() +
#   geom_sf(data = dd, fill = NA, col = "red") +
#   geom_sf(data = dd2 %>% filter(name == "Australia") %>% st_buffer(2), fill = "transparent", col = "blue") +
#   theme_nothing() +
#   theme(panel.grid.major = element_line(colour = "transparent"),
#         plot.background = element_rect(fill = "#FFFFFF", colour = "transparent"),
#         axis.text = element_blank(), 
#         axis.ticks.length=unit(0, "null"),
#         # plot.margin=unit(c(-10,-10,-10,-10),NULL),
#         panel.ontop = F
#   ) 

  



# mapdeck ------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckvxevehj5sui15qp3l80qfll" # style  
my_style_public <- "https://api.mapbox.com/styles/v1/darwinanddavis/ckvxevehj5sui15qp3l80qfll.html?title=view&access_token="
colv <- paste0(sequential_hcl(6,"Red-Purple"))
colvl <- colv[1] # link col

ttl <- "Bird mass (kg)"  
main <- data.frame("Y" = df$lat %>% na.omit() %>% min + 18,
                   "X" = df$lon %>% na.omit() %>% max - 50,
                   "title"= paste0("Avian Airstrike"))
main2 <- data.frame("Y" = main$Y - 2.2,"X"=main$X,
                    "title"= paste0("Aircraft-bird strikes across Australia\n2008-2017"))
main3 <- data.frame("Y" = main2$Y - 3,"X"=main2$X,
                    "title"= paste0("Total collisions: ", df %>% nrow %>% format(big.mark=",",scientific = F,trim = T),
                                    "\nNo. unique species: ", df$SpeciesFamily %>% unique %>% length,
                                    "\n\n\n Year of most collisions: 2012"
                                    ))
  
title_text <- list(title = 
                     paste0("<strong style=color:",colvl,";>Avian airstrikes</strong> <br/>
                            Aircraft-bird collision occurrence <br>
                            Author: <a style=color:",colvl,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                            Github: <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a> <br/>
                            Data source: <a style=color:",colvl,"; href=",citation,"> atsb.gov.au </a> <br/>
                            Map style: <a style=color:",colvl,"; href=", my_style_public,"MAPBOX_ACCESS_TOKEN> Mapbox </a> <br/>
                            Spot an error? <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>"),
                   css = "font-size: 10px; background-color: rgba(255,255,255,0.5);"
                     )

# map ---------------------------------------------------------------------
zoom <- 12
pitch <- 0
bearing <- -30
family <- "Geneva"

mapdeck(
  location = c(df$lon[1],df$lat[1]), 
  zoom = zoom,
  pitch = pitch, bearing = bearing,
  # min_zoom = zoom, max_zoom = zoom,
  # min_pitch = pitch, max_pitch = pitch,
  style = my_style
  # colourvalues::colour_palettes()
) %>%
  add_heatmap(data = df, lat = "lat", lon = "lon",
    weight = "bird_mass",threshold = 0.2,intensity = 5,
    layer_id = "bird_mass", colour_range = colv) %>%
  add_text(data=main,lat = "Y", lon = "X", 
           text = "title", layer_id = "m1",
           size = 40,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_weight = "bold",
           font_family = family
  ) %>% 
  add_text(data=main2,lat = "Y", lon = "X", 
           text = "title", layer_id = "m2",
           size = 25,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_family = family
  ) %>% 
  add_text(data=main3,lat = "Y", lon = "X", 
           text = "title", layer_id = "m3",
           size = 15,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_family = family
  ) %>% 
   add_title(title = title_text, layer_id = "heading") %>% 
htmlwidgets::saveWidget(here::here("30daymap2021",paste0(fh,".html")))
