# Process anonimized results

library(tidyverse)
library(knitr)
library(stringi)

# percentage function
pct <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%") }

# Import public dataset
results <- read_csv("~/Data/CRTC/crtc-anonimized.csv") 

# aggregate by province
results %>% 
  count(Province) %>% 
  arrange(n %>% desc) %>% 
  mutate(n = pct(n/sum(n))) %>% 
  t() %>% 
  kable(align='c') 

# aggregate by First letter of postal code
#https://www.ic.gc.ca/eic/site/bsf-osb.nsf/eng/br03396.html
results %>% 
  count(PC1) %>% 
  arrange(n %>% desc) %>% 
  mutate(n = pct(n/sum(n))) %>% 
  t() %>% 
  kable(align='c') 

# aggregate by cities
agg_cities <- results %>%
  filter (!is.na(City)) %>% 
  select(City, Province) %>% 
  count(City, Province) %>%  
  arrange(n %>% desc)         

agg_cities[1:10, ] %>%   
  kable(align=c('l', 'c', 'c')) 

        
# aggregate and plot by forward sortation area (first three characters of postal code)
fsa_agg <- results %>% 
  select(Postal) %>% 
  filter(!is.na(Postal)) %>%
  mutate(Postal = substr(Postal, 1,3)) %>% 
  count(Postal)


library(rgeos) # prereq: brew install gdal
library(rgdal) # prereq: brew install gdal
library(broom)

# Plot forward sortation areas
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
# https://www150.statcan.gc.ca/n1/pub/92-179-g/92-179-g2016001-eng.htm
canada_fsa <- readOGR(dsn=path.expand("~/Data/geo/fsa/simple.shp"))
# make it work with ggplot
fsa_map <- tidy(canada_fsa, region = "CFSAUID")

ggplot() +
  geom_map(data=fsa_map, map = fsa_map, aes(map_id=id, x = long, y = lat, group=group), 
           fill="white", color="grey80", size=0.2) + # plot boundaries
  geom_map(data=fsa_agg, map = fsa_map, aes(map_id = Postal, fill = n), size=0.25) + # fill in sample data
  coord_map(projection = "ortho", orientation = c(40, -95, 0)) +
  ylim(40, 72) + xlim(-142,-52) + 
  theme_bw() +
  labs(x="", y="") +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.background =element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position=c(0.85, 0.8),
        legend.title = element_blank()) +
  #scale_fill_gradient2(low='lemonchiffon', mid='palegreen', high='lightskyblue', midpoint = 300)
  scale_fill_gradient(low='white', high='grey20')

ggsave('crtc-fsa.png', height=6, width=10, dpi=300)

# Postal codes shapefile  
# https://geocoder.ca/?freedata=1 
# under the Creative Commons Attribution 2.5 Canada License. https://creativecommons.org/licenses/by/2.5/ca/ #
postal_map <- readOGR(dsn = path.expand("~/Data/geo/CanadaPostalCodePolygons/CanadaPostalCodePolygons.shp"))
postal_map$ZIP <- toupper(postal_map$ZIP)
# test postal code format in the shapefile
postal_map[!grepl('^[A-z][0-9][A-z][0-9][A-z][0-9]$', postal_map$ZIP), ]$ZIP

# convert postal shapefile to coordinate file by extracting "labpt"
temp_coords <- postal_map %>% coordinates()
postal_coords <- tibble(Postal = postal_map$ZIP) %>% 
  mutate(x = temp_coords[, 1], y = temp_coords[, 2])
rm(temp_coords, postal_map)
gc()
#postal_coords %>% filter(Postal == 'Y1A5Y9') %>% View()



# postal codes from the dataset missing from the geo shapefile
#missing <- results[!results$Postal %in% postal_coords$Postal & !is.na(results$Postal), ]$Postal  
missing <- results %>% 
  select(Postal) %>% 
  filter(!results$Postal %in% postal_coords$Postal,
         !is.na(results$Postal)) %>% 
  unique() %>% pull(Postal)
  
# deduplicate
#missing <- missing[!duplicated(missing)]

# e.g.
#postal_poly[postal_poly$ZIP %in% "M5V2W9",]$ZIP
#table(substr(missing, 1, 1))


# vary last digit of missing postal codes by ±i in 1:10 to find the closest neighbour present in the shapefile
matched_neighb <- ''
for (i in 1:10) {
  # get last digit and increment
  last_digit <- substr(missing, 6, 6) %>% as.integer() + i 
  last_digit[last_digit > 9] = 9
  # reattach incremented last digit
  missing <- paste(substr(missing, 1, 5), last_digit %>% as.character(), sep = '')
  # try to match
  matched_neighb <- c(matched_neighb, missing[missing %in% postal_coords$Postal])
  # update missing
  missing <- missing[!missing %in% postal_coords$Postal]   
  # get last digit and decrement
  last_digit <- substr(missing, 6, 6) %>% as.integer() - i
  last_digit[last_digit < 0] = 1
  # reattach decremented last digit
  missing <- paste(substr(missing, 1, 5), last_digit %>% as.character(), sep = '')
  # update missing
  matched_neighb <- c(matched_neighb, missing[missing %in% postal_coords$Postal])
  missing <- missing[!missing %in% postal_coords$Postal] 
}

#deduplicate
matched_neighb <- matched_neighb[!duplicated(matched_neighb)]
missing <- missing[!duplicated(missing)]

# reduce postal coordinates database to postal codes in the submissions tibble and matched neighbours
postal_coords_select <- postal_coords %>% 
  filter(Postal %in% c(results$Postal, matched_neighb))

# plot northen provinces and terrs
#postal_map[substr(postal_poly$ZIP, 1, 1) %in% c('A', 'Y', 'X'), ] %>% plot()


# Plot on top of provincial map
# http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
# to simplify the polygons use: 
# ogr2ogr simple.shp "gfsa000b11a_e.shp" -simplify 0.1
prov_map <- readOGR(dsn=path.expand("~/Data/geo/provinces/simple.shp" ))
#prov_names <- prov_map@data[["PRENAME"]]
# convert to tibble format
prov_map <- tidy(prov_map, region = "PRENAME") #install maptools

ggplot() +
  geom_map(data = prov_map, map = prov_map, aes(map_id=id, x = long, y = lat, group=group), 
           fill="white", color="grey80", size=0.2)  + # plot boundaries
  geom_point(data = postal_coords_select, aes(x=x, y=y), 
             colour = "blue", fill = "blue", alpha = 1, size = 0.1) +
  coord_map(projection = "ortho", orientation = c(40, -95, 0)) +
  ylim(40, 72) + xlim(-142,-52) + 
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggsave('crtc-map.png', height=9, width=15, dpi=300)


### Comment stats (CRTC data only) ###
results$com_len <- nchar(results$Comment)            # number of characters
results$com_short <- substr(results$Comment, 1, 100) # first 100 characters in each comment
agg_comments <- results %>%                          # aggregate by comments
  filter(Source == 'CRTC', !is.na(com_short)) %>% 
  count(com_short) %>%        
  arrange(n %>% desc)         
agg_comments %>% filter(n>2) %>% head(10) %>% kable()            # show most popular copy-pasted comments
agg_comments %>% filter(n>2) %>% summarize(sum(n))  # total number of copy-pasted (n>2)

# find out what's in common between submissions with a common comment
#results %>% filter (com_short == agg_comments$com_short[3]) %>% View()


### Submissions over time (CRTC data only)
sub_by_date <- results %>% 
  filter(Source == 'CRTC') %>% 
  group_by(Date) %>% 
  summarize(n = n())  %>%  
  mutate(cumsum = cumsum(n)) 
sub_by_date

ggplot(sub_by_date) + aes(x = Date, y = cumsum) +
  geom_line(alpha = .7, size = 0.7, colour = "cyan3") + 
  theme_light() +
  xlab('Date') +
  ylab('Number of submissions (cumulative)') +
  ggtitle("Number of submissions over time") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('sub-by-time.svg', height=4.3, width=6)



### english vs française (CRTC data only) ###
results$Lang <- ''
results$Lang[!is.na(results$com_short) & results$Source=='CRTC'] <- "EN"
results$Lang[grepl("(é)|([ ]l\')|([ ]la[ ])", results$Comment)] <- "FR"

# contingency table Language-Province exported to markdown
table(results$Lang, results$Province) %>% 
  kable(align='c')

# Languages in QC in and outside of Montréal
results %>% 
  filter(Source == 'CRTC', Province=="QC") %>% 
  group_by(Lang, City=="montreal") %>% 
  summarize(n = n()) %>% # or count()
  kable(align='c')

# Languages in QC in and outside of Gatineau
results %>% 
  filter(Source == 'CRTC', Province=="QC") %>% 
  group_by(Lang, City=="gatineau") %>% 
  summarize(n = n()) %>% # or count()
  kable(align='c')

# Languages in QC in and outside of "Gatineau+Montréal"
results %>% 
  filter(Source == 'CRTC', Province=="QC") %>% 
  group_by(City=="gatineau"|City=="montreal", Lang) %>% 
  summarize(n = n()) %>% # or count()
  kable(align='c')

# Top anglophone places in QC
results %>% 
  filter(Province=="QC", Lang=="EN") %>% 
  select(City) %>% 
  count(City) %>%  
  arrange(n %>% desc) %>% 
  head(10) %>% kable()


# Top francophone places in QC
results %>% 
  filter(Province=="QC", Lang=="FR") %>% 
  select(City) %>% 
  count(City) %>%  
  arrange(n %>% desc)   %>% 
  head(10) %>% kable()

# Use of English in QC outside of Montréal
results %>% 
  filter(Lang == "EN", Province=="QC", City!="montreal") %>% View()


### Organizations ###

results %>% 
  count(is.na(Comp)) %>% kable

results %>% filter(!is.na(Comp)) %>%  View

# Most popular email services
results %>% filter(!is.na(Email)) %>% 
  count(Email) %>% 
  mutate(n = pct(n/sum(n))) %>% 
  arrange(n %>% desc) %>% head(10) %>% kable