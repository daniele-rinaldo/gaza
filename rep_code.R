
remove(list=ls())

library(fixest)
library(AER)
library(panelr)
library(ggplot2)
library(raster)
library(sf)
library(terra)
library(maps)
library(DIDmultiplegtDYN)
library(did) 
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(randomForest)
library(tidyverse)
library(lubridate)
library(mgcViz)
library(mgcv)



###### build datasets #######

dat_ntl <- read.csv("ntl monthly+weekly/ntl_week_gridded_refactor_fulldata.csv")
dat_dam <- read.csv("damages weekly/dpm_ic_masked_new_dates_gridded_refactor_fulldata.csv")
dat_dam2 <- read.csv("~/academica/Gaza/CE lab data/synchronized/damage outputs/damage_monthly_new_cropped_districts_gaza.csv")
dat_cumul <- read.csv("~/academica/Gaza/CE lab data/synchronized/damage outputs/dpm_ic_masked_cumulative_dates_gridded_refactor.csv")

dat_dam_w <- read.csv("~/academica/Gaza/CE lab data/synchronized/damage outputs/dpm_ic_masked_new_dates_gridded_weeks_gridded_refactor.csv")
dat_ntl_w <- read.csv("~/academica/Gaza/CE lab data/synchronized/NTL outputs/ntl_weeks_gridded_refactor.csv")


d_dam <- pivot_longer(dat_dam,
                      col = starts_with("X"),
                      names_to = "week",
                      names_prefix = "",
                      values_to = "damage",
                      values_drop_na = TRUE)
# d_dam <- d_dam[,!colnames(d_dam) %in% c("OBJECTID","GLOBALID")]
d_dam <- d_dam[,!colnames(d_dam) %in% c("system.index",".geo")]





d_dam$weeks <- as.Date(gsub('[.]',"-",str_sub(d_dam$week,start=2,end=11)))
d_dam$id_m <- as.numeric(paste(lubridate::year(d_dam$weeks), ifelse(nchar(lubridate::month(d_dam$weeks))==1,
                                                                    paste(0,lubridate::month(d_dam$weeks),sep=""), lubridate::month(d_dam$weeks)) , sep=""))


d_dam_m <- d_dam %>% group_by(id, id_m)  %>% summarise(damage= sum(damage))



d_dam_c <- pivot_longer(dat_cumul,
                        col = starts_with("X"),
                        names_to = "week",
                        names_prefix = "",
                        values_to = "damage",
                        values_drop_na = TRUE)
d_dam_c <- d_dam_c[,!colnames(d_dam_c) %in% c(".geo","system.index")]

d_dam_c$weeks <- as.Date(gsub('[.]',"-",str_sub(d_dam_c$week,start=2,end=11)))
d_dam_c$id_m <- as.numeric(paste(lubridate::year(d_dam_c$weeks), ifelse(nchar(lubridate::month(d_dam_c$weeks))==1,
                                                                        paste(0,lubridate::month(d_dam_c$weeks),sep=""),
                                                                        lubridate::month(d_dam_c$weeks)) , sep=""))


d_dam_m_c <- d_dam_c %>% group_by(id, id_m)  %>% summarise(damage_cumul = max(damage))


d_ntl = pivot_longer(dat_ntl,
                     col = starts_with("X"),
                     names_to = "week",
                     names_prefix = "",
                     values_to = "NTL",
                     values_drop_na = TRUE)
d_ntl <- d_ntl[,!colnames(d_ntl) %in% c("system.index",".geo")]

dat_ntl_dist <- read.csv("~/academica/Gaza/CE lab data/synchronized/NTL outputs/ntl_grids_distance_to_ISR_border.csv")
dat_dist <- dat_ntl_dist[,c("id","HubDist")]

d_ntl$weeks <- as.Date(gsub('[.]',"-",str_sub(d_ntl$week, start=2,end=11)))
d_ntl$id_m <- as.numeric(paste(lubridate::year(d_ntl$weeks), ifelse(nchar(lubridate::month(d_ntl$weeks))==1,
                                                                    paste(0,lubridate::month(d_ntl$weeks),sep=""), 
                                                                    lubridate::month(d_ntl$weeks)) , sep=""))

d_ntl_m <- d_ntl %>% group_by(id, id_m) %>% summarise(ntl = min(NTL), ntl_m = mean(NTL), ntl_x = max(NTL)) # first of following month, best proxy for NTL (works with mean also)


##### data building 

ihs            <- function(x) log(x + (x^2 + 1) ^ 0.5)

start <-  202309
dat <- left_join(d_ntl_m,d_dam_m)
# dat <- left_join(d_dam_m,d_ntl_m,)
dat <- left_join(dat, d_dam_m_c,by=c("id","id_m"))

dat_mask <- read.csv("~/academica/Gaza/CE lab data/synchronized/damage outputs/builtup_mask_gridded.csv")

dat <- left_join(dat, dat_mask, by ="id")
dat <- left_join(dat,dat_dist)


dat_dam2_fix <- dat_dam2 %>% group_by(id) %>% summarise(Locality_N = first(Locality_N))
# dat_old2 <- left_join(dat, dat_dam2_fix[,c("Locality_N","id")], by="id")
# ### hard code localities that are dropped due to overlapping

unique(dat_dam2$Locality_N)[31] ## check it's Bani Shueila
dat_dam2_fix$Locality_N[which(dat_dam2_fix$id %in% c(406,407,448,449,450,490))] <- unique(dat_dam2$Locality_N)[31]

unique(dat_dam2$Locality_N)[32] ## check it's Jabalya Camp
dat_dam2_fix$Locality_N[which(dat_dam2_fix$id %in% c(1057, 1058, 1099,1100))] <- unique(dat_dam2$Locality_N)[32]

unique(dat_dam2$Locality_N)[23] ## check it's Abasan al Jadida
dat_dam2_fix$Locality_N[which(dat_dam2_fix$id %in% c(491, 532 , 533, 574, 575))] <- unique(dat_dam2$Locality_N)[23]


# dat_old <- dat
dat <- left_join(dat, dat_dam2_fix[,c("Locality_N","id")], by ="id")
dat$Locality_N[is.na(dat$Locality_N)] <- "Boundary"

loc <- read.csv("~/academica/Gaza/CE lab data/localities.csv")
colnames(loc)[2:3] <- c("Locality_N","NS")
dat <- left_join(dat,loc, by="Locality_N")

dat$t <- dat$id_m - start
dat$t <- ifelse(dat$t > 3, dat$t - 88,dat$t )
dat$t <- ifelse(dat$t < -8, dat$t + 88, dat$t )

# dat$t <- dat$id_m - 202310
# dat$t <- ifelse(dat$t > 2, dat$t - 88,dat$t )
# dat$t <- ifelse(dat$t < -9, dat$t + 88, dat$t )


dat$damage_cumul[which(is.na(dat$damage_cumul))] <- 0 
dat$damage[which(is.na(dat$damage))] <- 0 
# dat$damage_m[which(is.na(dat$damage_m))] <- 0 
dat$treatment <- ifelse(dat$damage > 0 ,1 , 0)
dat$post  <- ifelse(dat$id_m > start,1,0)

dat <- dat[-which(is.na(dat$id_m)),]

dat2   <- dat %>% group_by(id) %>% summarise(is_t = ifelse(sum(damage) > 0 , 1 ,0),
                                             is_t_qt = ifelse(sum(damage) > 2500 ,1 ,0)) %>% ungroup()


dat_n <- dat %>% group_by(id) %>% summarise(n_bombed = length(which(damage > 0 )))

dat <- left_join(dat,dat_n)
dat <- left_join(dat,dat2)
dat2 <- dat[which(dat$id_m > 202301),]
dat3 <- dat[which(dat$id_m != 202308),]

dat$damage_norm <- ifelse(is.nan(dat$damage/dat$area_m2),0,dat$damage/dat$area_m2)
# dat$damage_norm <- dat$damage/dat$area_m2

# dat$NS2 <- ifelse(is.na(dat$NS),"N","S")
# dat$is_s <- ifelse(dat$NS=="S",1,0)
# dat$is_s[which(is.na(dat$is_s))] <- 0

dat$camp <- ifelse( dat$Locality_N=="Khan Yunis Camp" |  dat$Locality_N=="Rafah Camp" | dat$Locality_N=="Deir al Balah Camp" |
                      dat$Locality_N=="An Nuseirat Camp" | dat$Locality_N=="Al Maghazi Camp" |
                      dat$Locality_N=="Al Bureij Camp" |   dat$Locality_N=="Ash Shati' Camp" , 1, 0 )

oct <- dat %>% group_by(id) %>% filter(id_m==202409) %>% summarise(treatment = unique(treatment))

dat$treatment[which(dat$id_m==202410)] <- oct$treatment[1:440]

###### stats on data / damages ########


l_ob        <- dat[which(dat$id_m=="202409"),]
l_ob$damage_norm_cumul <- ifelse(is.infinite(
  ifelse(is.nan(l_ob$damage_cumul/l_ob$area_m2),0,l_ob$damage_cumul/l_ob$area_m2)),
  0,
  l_ob$damage_cumul/l_ob$area_m2)
tot_avg_dam <- mean(l_ob$damage_cumul)/1000000
max_dam     <- l_ob[which(l_ob$damage_cumul==max(l_ob$damage_cumul)),]$damage_cumul/1000000
top_dam_loc <- unique(l_ob$Locality_N[which(l_ob$damage_cumul>= quantile(l_ob$damage_cumul,0.99))])
# l_ob[which(l_ob$damage_cumul>= quantile(l_ob$damage_cumul,0.85)),]
avg_num_cells_dam   <- length(last_d[which(last_d !=0)])/length(last_d)

full_dam <- l_ob[which(l_ob$damage_norm_cumul >= 1),]
dim(full_dam)[1]

total_damage_detectable <- sum(l_ob$damage_cumul)/(sum(l_ob$area_m2))

avg_dam_m            <- mean(dat$damage)

avg_dam_max            <- mean(dat$damage_cumul)
avg_dam_w            <- mean(dat_w$damage)
max_norm_dam       <- max(dat$damage_norm)
which_max_norm_dam <- dat$Locality_N[which(dat$damage_norm==max(dat$damage_norm))]
View(dat[which(dat$damage_norm>0.75),])

sum(l_ob$damage_cumul)/(365*1000000)

#### weekly data + effect of ceasefire #### 


d_dam_w  <- pivot_longer(dat_dam_w ,
                         col = starts_with("X"),
                         names_to = "week",
                         names_prefix = "",
                         
                         values_to = "damage",
                         values_drop_na = TRUE)

d_dam_w$weeks <- as.Date(gsub('[.]',"-",str_sub(d_dam_w$week,start=2,end=11)))
d_dam_w <- d_dam_w[,!colnames(d_dam_w) %in% c(".geo","system.index","week")]
d_dam_w <- left_join(d_dam_w, dat_dam2_fix, by = "id")
# d_dam_w$weeks <- d_dam_w$weeks +1

d_ntl_w = pivot_longer(dat_ntl_w,
                       col = starts_with("X"),
                       names_to = "week",
                       names_prefix = "",
                       values_to = "NTL",
                       values_drop_na = TRUE)
d_ntl_w$weeks <- as.Date(gsub('[.]',"-",str_sub(d_ntl_w$week,start=2,end=11)))



d_ntl_w  <- d_ntl_w[,!colnames(d_ntl_w) %in% c("system.index",".geo","week")]
# d_ntl_w$id_m <- as.numeric(paste(lubridate::year(d_ntl_w$weeks),lubridate::month(d_ntl_w$weeks) )

# d_ntl_w$w2 <- d_ntl_w$weeks



dat_w <- left_join(d_ntl_w, d_dam_w, by=c("id", "weeks"))

dat_w$id_w <- as.numeric(paste(lubridate::year(dat_w$weeks),
                               ifelse(nchar(lubridate::month(dat_w$weeks))==1,paste(0,lubridate::month(dat_w$weeks),sep=""),lubridate::month(dat_w$weeks)),
                               ifelse(nchar(lubridate::day(dat_w$weeks))==1,paste(0,lubridate::day(dat_w$weeks),sep=""),lubridate::day(dat_w$weeks)),
                               sep=""))



dat_w$damage[which(is.na(dat_w$damage))] <- 0
dat_w2 <- dat_w %>% group_by(id) %>% summarise(is_t = ifelse(sum(damage) > 0 ,1 ,0)) %>% ungroup()
dat_w  <- left_join(dat_w, dat_w2)

dat_w[which(dat_w$weeks>"2023-11-24" & dat_w$weeks < "2023-12-03"),]

dat_w$post <- ifelse(dat_w$weeks > "2023-10-06",1,0)
dat_w$post <- ifelse(dat_w$weeks > "2023-09-30",1,0)
dat_w$ceasef <- ifelse(dat_w$weeks>"2023-11-24" & dat_w$weeks < "2023-12-03",1,0)


est_cease <- feols(ihs(NTL) ~ is_t:post*factor(ceasef) | id + weeks , data= dat_w)
etable(est_cease ,tex=T)


##### monthly linear DiD specification + maps #######


est_m   <- feols(ihs(ntl) ~  treatment:post | id + id_m + Locality_N^id_m, data=dat,cluster="Locality_N")
est_m_y <- feols(ihs(ntl) ~ treatment:factor(id_m)| id + id_m+ Locality_N^id_m , data=dat,cluster="Locality_N")

est_did <- feols(ihs(ntl) ~ post:is_t | id + id_m , data=dat, cluster="Locality_N")
require(lfe)
est_did_loc <- feols(ihs(ntl) ~ i(Locality_N,post*is_t,ref="Boundary")| id + id_m , data=dat, cluster="id")
# est_did_loc_y <- felm(ihs(ntl) ~ factor(id_m):post:factor(Locality_N) | id + id_m | 0 | id, data=dat)
# est_did_loc <- felm(ihs(ntl) ~ is_t:post:factor(Locality_N) | id + id_m | 0 | id, data=dat)

etable(est_did_loc,tex=T)
# dat$jab <- ifelse(dat$Locality_N=="Jabalya Camp",1,0)
# est_did_loc2 <- felm(ihs(ntl) ~ post:is_t:jab | id + id_m | 0 | id, data=dat)
# est_did_loc3 <- felm(ihs(ntl) ~ ihs(damage)*jab | id + id_m | 0 | id, data=dat)

pal_shp <- st_read("~/academica/Gaza/gaza shapefiles/gaza_fulldept.shp")
# plot(pal_shp$geom)

# est_use_loc <- est_did_loc$coefficients[-which(is.nan(est_did_loc$coefficients))]

est_loc <- matrix(0,length(est_did_loc$coefficients),4)
colnames(est_loc) <- c("Locality_N","est","ntl","pv")
# est_loc[,2] <- est_did_loc$coefficients[-which(is.nan(est_did_loc$coefficients[,1]))]
est_loc[,2] <- est_did_loc$coefficients
# est_loc[,3] <-  exp(est_did_loc$coefficients[-which(is.nan(est_did_loc$coefficients[,1]))] ) -1
est_loc[,3] <-  exp(est_did_loc$coefficients) -1
# est_loc[,4] <- est_did_loc$pval[-which(is.nan(est_did_loc$coefficients[,1]))]
est_loc[,4] <- est_did_loc$coeftable[,4]

# name_loc <- function(pos){
# name <- suppressWarnings(substr(tidy(est_did_loc)[pos,1],29,nchar(tidy(est_did_loc)[pos,1])))
# return(name)
# }
name_loc <- function(pos){
  name <- suppressWarnings(substr(tidy(est_did_loc)[pos,1],13,nchar(tidy(est_did_loc)[pos,1])-12))
  return(name)
}



locn <- sapply(c(1:30),name_loc)
est_loc[,1]   <- locn
est_loc[,2:4] <- as.numeric(est_loc[,2:4])

est_loc <- as.data.frame(est_loc)

pal_shp2 <- left_join(pal_shp,est_loc,by="Locality_N")
pal_shp2 <- pal_shp2[-which(is.na(pal_shp2$est)),]

cee         <- st_centroid(pal_shp2)
coord_cent  <- st_coordinates(cee)
coord_cent2 <- coord_cent
moved       <- 10
coord_cent2[moved,1] <- coord_cent[moved,1]*0.994-1000
coord_cent2[moved,2] <- coord_cent[moved,2]
pal_shp2$est2 <- round(as.numeric(pal_shp2$est),3)

# ggplot() + geom_sf(data=pal_shp2,aes(fill=-round(as.numeric(est),3))) + 
ggplot() + geom_sf(data=pal_shp2,aes(fill=est2)) + 
  scale_fill_gradient2(name="ATT",low="red",high="white",aesthetics = "fill",space="Lab",midpoint=-0.5,
                       breaks=c(-0.54,-1,-1.5,-2))+
  # scale_fill_gradient2(position="bottom" , low = "blue", high = "red", 
  #                      midpoint = median(abs(as.numeric(pal_shp2$est)))) +
  geom_label(aes(x=coord_cent2[,1],y=coord_cent[,2],label=100*round(as.numeric(pal_shp2$ntl),3)),size=3)+
  theme_void()


###### evac zones

d_ev <-st_read("~/academica/Gaza/CE lab data/gaza_blocks.geojson")
d_ev$name <- NA

d_ev$name[which(d_ev$description == "ע'זה אלזיתונ")] <- "Alzitun"
d_ev$name[which(d_ev$description == "ע'זה")] <- "Gaza"
d_ev$name[which(d_ev$description == "רפח")] <- "Rafah"
d_ev$name[which(d_ev$description == "אבו מדינ")] <- "Abu Madin"
d_ev$name[which(d_ev$description == "ח'אנ יונס")] <- "Khan Yunis"
d_ev$name[which(d_ev$description == "אלנציראת")] <- "Alanzirat"
d_ev$name[which(d_ev$description == "עבסאנ")] <- "Absan"
d_ev$name[which(d_ev$description == "ג'באליא")] <- "Jabalia"
d_ev$name[which(d_ev$description == "בית לאהיא")] <- "Beit Lahia"
d_ev$name[which(d_ev$description == "דיר אלבלח")] <- "Deir Albalah"
d_ev$name[which(d_ev$description == "אלנזלה")] <- "Alanzala"
d_ev$name[which(d_ev$description == "<Null>")] <- "Rural Areas"
d_ev$name[which(d_ev$description == "ע'זה אלדרג'")] <- "by Eldarj"
d_ev$name[which(d_ev$description == "ע'זה אלתרכמאנ")] <- "This is Altrachman"
d_ev$name[which(d_ev$description == "ע'זה אלתפאח")] <- "This is Altafah"
d_ev$name[which(d_ev$description == "ע'זה אלג'דידה")] <- "This is Aljadida"
d_ev$name[which(d_ev$description == "דמרה")] <- "Damara"
d_ev$name[which(d_ev$description == "בית חאנון")] <- "Beit Hanon"
d_ev$name[which(d_ev$description == "בני סהילא")] <- "Sons of Sahila"
d_ev$name[which(d_ev$description == "ח'רבה ח'זאעה(בני סהילא)")] <- "Kharba Khaza'a (Bani Sahila)"
d_ev$name[which(d_ev$description == "אלסמירי")] <- "Alsmiri"
d_ev$name[which(d_ev$description == "אלמע'אזי")] <- "Alma'azi"
d_ev$name[which(d_ev$description == "ג'באליא אלנזלה")] <- "Jabalia Albanzala"
d_ev$name[which(d_ev$description == "אלבריג'")] <- "Albridge"
d_ev$name[which(d_ev$description == "ח'רבה ח'זאעה")] <- "Kharba Khaza'a"
d_ev$name[which(d_ev$description == "בית חאנונ")] <- "Beit Hanon"


evac_group <- d_ev %>% group_by(name) %>% summarise(geometry = st_union(geometry))
plot(st_geometry(evac_group))
# st_write(evac_group,"~/academica/Gaza/CE lab data/gaza_evac_shape_aggr.shp")

# plot(st_centroid(evac_group))

names_evac <- read.csv("~/academica/Gaza/CE lab data/gaza_evac_zones_full.csv")
evac_use <- names_evac[,c("id","name")]
dat_evac <- left_join(dat,evac_use,by="id")

### fix spelling + boundary issues by hand 

dat_evac$name[which(dat_evac$Locality_N=="Rafah" & dat_evac$name=="")] <- "Rafah"
dat_evac$name[which(dat_evac$Locality_N=="Gaza" & dat_evac$name=="")] <- "Gaza"
dat_evac$name[which(dat_evac$Locality_N=="Beit Lahiya" & dat_evac$name=="")] <- "Beit Lahia"
dat_evac$name[which(dat_evac$Locality_N=="Beit Hanun" & dat_evac$name=="")] <- "Beit Hanon"
dat_evac$name[which(dat_evac$Locality_N=="Jabalya" & dat_evac$name=="")] <- "Jabalia"
dat_evac$name[which(dat_evac$Locality_N=="Al Shokat" & dat_evac$name=="")] <- "Rafah"
dat_evac$name[which(dat_evac$Locality_N=="Deir al Balah" & dat_evac$name=="")]  <- "Deir Albalah"
dat_evac$name[which(dat_evac$Locality_N=="Al Fukhkhari" & dat_evac$name=="")]  <- "Khan Yunis"
dat_evac$name[which(dat_evac$Locality_N=="'Abasan al Kabira" & dat_evac$name=="")]  <- "Sons of Sahila"
dat_evac$name[which(dat_evac$Locality_N=="Al Qarara" & dat_evac$name=="")]  <- "Alsmiri"
dat_evac$name[which(dat_evac$Locality_N=="Khuza'a" & dat_evac$name=="")]  <- "Sons of Sahila"
dat_evac$name[which(dat_evac$Locality_N=="Az Zawayda" & dat_evac$name=="")]  <- "Alanzirat"
dat_evac$name[which(dat_evac$Locality_N=="Madinat Ezahra" & dat_evac$name=="")]  <- "Abu Madin"
dat_evac$name[which(dat_evac$Locality_N=="Wadi as Salqa" & dat_evac$name=="")]  <- "Alsmiri"
dat_evac$name[which(dat_evac$Locality_N=="Al Maghazi" & dat_evac$name=="")]  <- "Alma'azi"
dat_evac$name[which(dat_evac$Locality_N=="Al Musaddar" & dat_evac$name=="")]  <- "Rural Areas"
dat_evac$name[which(dat_evac$Locality_N=="Al Bureij" & dat_evac$name=="")]  <- "by Eldarj"
dat_evac$name[which(dat_evac$Locality_N=="Juhor ad Dik" & dat_evac$name=="")]  <- "Alzitun"
dat_evac$name[which(dat_evac$Locality_N=="Um Al-Nnaser (Al Qaraya al Badawiya)" & dat_evac$name=="")]  <- "Beit Lahia"



est_did_evac_feols <- feols(ihs(ntl) ~ post:is_t:factor(name)| id + id_m , data=dat_evac, cluster="id")
est_did_evac <- felm(ihs(ntl) ~ is_t:post:factor(name) | id + id_m | 0 | id, data=dat_evac)


est_evac <- matrix(0,dim(tidy(est_did_evac)[-1,])[1],4)
colnames(est_evac) <- c("name","est","ntl","pv")
est_evac[,2] <- est_did_evac$coefficients[-1,1]
# est_evac[,3] <-  exp(est_did_evac$coefficients[-1] - tidy(est_did_evac)[-1,3]^2/2) -1
est_evac[,3] <-  exp(est_did_evac$coefficients[-1])-1
est_evac[,4] <- est_did_evac$pval[-1]

name_evac <- function(pos){
  name <- suppressWarnings(substr(tidy(est_did_evac)[pos,1],23,nchar(tidy(est_did_evac)[pos,1])))
  return(name)
}

evacn <- sapply(c(1:24),name_evac)
est_evac[,1]   <- evacn[-1]
est_evac[,2:4] <- as.numeric(est_evac[,2:4])

est_evac <- as.data.frame(est_evac)

evac_shp <- left_join(evac_group,est_evac,by="name")
evac_shp <- evac_shp[-which(is.na(evac_shp$est)),]

cent_evac         <- st_centroid(evac_shp)
coord_cent_e  <- st_coordinates(cent_evac)
coord_cent_e_2 <- coord_cent_e


#### change coords of centroid labels for better plotting (arrows by hand)
eldarj       <- which(evac_shp$name=="by Eldarj")
coord_cent_e_2[eldarj,1] <- coord_cent_e[eldarj,1]*0.999
coord_cent_e_2[eldarj,2] <- coord_cent_e[eldarj,2]*1.001

alz       <- which(evac_shp$name=="Alzitun")
coord_cent_e_2[alz,1] <- coord_cent_e[alz,1]*1.001
coord_cent_e_2[alz,2] <- coord_cent_e[alz,2]*0.999

rur       <- which(evac_shp$name=="Rural Areas")
coord_cent_e_2[rur,1] <- coord_cent_e[rur,1]*1.0015
coord_cent_e_2[rur,2] <- coord_cent_e[rur,2]*0.999

evac_shp$est2 <- round(as.numeric(evac_shp$est),3)

# ggplot() + geom_sf(data=pal_shp2,aes(fill=-round(as.numeric(est),3))) + 
ggplot() + geom_sf(data=evac_shp,aes(fill=est2)) + 
  scale_fill_gradient2(name="ATT",low="red",high="white",aesthetics = "fill",space="Lab",midpoint=-0.5,
                       breaks=c(-0.54,-1,-1.5,-2))+
  # scale_fill_gradient2(position="bottom" , low = "blue", high = "red", 
  #                      midpoint = median(abs(as.numeric(pal_shp2$est)))) +
  geom_label(aes(x=coord_cent_e_2[,1],y=coord_cent_e_2[,2],label=100*round(as.numeric(evac_shp$ntl),3)),size=3)+
  theme_void()





##### other stuff


est_did_yr <- feols(ihs(ntl) ~ i(id_m, is_t, ref="202309")  | id + id_m ,
                    data=dat,cluster="Locality_N")

# etable(est_did_yr,tex=T)

dat_5 <- dat %>% group_by(id) %>% summarise(dam_cumul_tot = last(damage_cumul))
dat   <- left_join(dat,dat_5, by="id")

est_did_yr_c = feols(ihs(ntl) ~ i(id_m, ihs(dam_cumul_tot), ref="202309") | id + id_m + Locality_N^id_m ,
                     data=dat,cluster="Locality_N")
# plot(est2)
summary(est_did_yr_c)



########## staggered DiD designs ################



dat_cs <- dat %>% group_by(id)  %>% mutate(group = t[first(which(treatment>0))]) %>% ungroup()

dat_cs2 <- dat_cs

dat_cs$Locality_N2 <- dat_cs$Locality_N
dat_cs$Locality_N2[which(dat_cs$Locality_N2 =="Boundary")] <- "Khan Younis"

dat_cs$ihs_ntl <- ihs(dat_cs$ntl)

dat_cs$group[which(is.na(dat_cs$group))]   <- 0
#### grids bombed after 8/11/13 months for the first time are very few and with very little damage: ungrouping solves warning, fixes pre-trends & yields identical results
dat_cs$group3 <- ifelse( dat_cs$group == 5 |dat_cs$group == 8 | dat_cs$group == 11 | dat_cs$group == 13,0,dat_cs$group) 


est_stagg <- att_gt(yname = "ihs_ntl",
                    tname = "t",
                    idname = "id",
                    gname = "group3",
                    data = dat_cs,
                    cluster="Locality_N")



d1 <- aggte(est_stagg, type="simple", cband=T ,na.rm=T,cluster="Locality_N")
d2 <- aggte(est_stagg, type="dynamic", cband=T , na.rm=T,cluster="Locality_N")
# ggdid(d2,title="NTL loss by months bombed")

est_cs <- tidy(d2)[,4]
upper <- tidy(d2)[,7]
lower <- tidy(d2)[,6]

##### ML stuff 

set.seed(1234)
dml_did <- function(y1, y0, D, covariates,
                    ml_g = lrn("regr.ranger"),
                    ml_m = lrn("classif.ranger"),
                    n_folds = 10, n_rep = 1, ...) {
  
  # warning if n_rep > 1 to handle mapping from psi to inf.func
  if (n_rep > 1) {
    warning("n_rep > 1 is not supported.")
  }
  # Compute difference in outcomes
  delta_y <- y1 - y0
  # Prepare data backend
  dml_data = DoubleML::double_ml_data_from_matrix(X = covariates, y = delta_y, d = D)
  # Compute the ATT
  dml_obj = DoubleML::DoubleMLIRM$new(dml_data, ml_g = ml_g, ml_m = ml_m, score = "ATTE", n_folds = n_folds)
  dml_obj$fit()
  att = dml_obj$coef[1]
  # Return results
  inf.func <- dml_obj$psi[, 1, 1]
  output <- list(ATT = att, att.inf.func = inf.func)
  return(output)
}


est_stagg_ML <- att_gt(yname = "ihs_ntl",
                       tname = "t",
                       idname = "id",
                       gname = "group3",
                       data = dat_cs,
                       xformla=~Locality_N+camp+NS,
                       cluster= "Locality_N",
                       bstrap = TRUE,
                       cband = TRUE,
                       est_method = dml_did)
d2_ml <- aggte(est_stagg_ML, type="dynamic",bstrap=T,cband=T ,na.rm=T,cluster="Locality_N") 
d1_ml <- aggte(est_stagg_ML, type="simple",bstrap=T,cband=T ,na.rm=T) 

overall <- 100*round((exp(d1_ml$overall.att - d1_ml$overall.se^2/2)-1),3)

est_cs_ml <- c(0,tidy(d2_ml)[,4])
upper_ml <- c(0,tidy(d2_ml)[,7])
lower_ml <- c(0,tidy(d2_ml)[,6])

ols_e  <- c(rep(0,2),est_did_yr$coeftable[,1][1:12],0,est_did_yr$coeftable[,1][13:25])
ols_e_u <- c(rep(0,2),est_did_yr$coeftable[,1][1:12] + 1.96*est_did_yr$coeftable[,2][1:12], 0 , est_did_yr$coeftable[,1][13:25] + 1.96*est_did_yr$coeftable[,2][13:25])
ols_e_l <- c(rep(0,2),est_did_yr$coeftable[,1][1:12] - 1.96*est_did_yr$coeftable[,2][1:12], 0 , est_did_yr$coeftable[,1][13:25] - 1.96*est_did_yr$coeftable[,2][13:25])


# dat_cs_pl <- as.data.frame(cbind(t = tidy(d2)[,3]+1,impacts,impacts_l,impacts_h,est,lower,upper,ols_e,ols_e_u,ols_e_l))
dat_cs_pl <- as.data.frame(cbind(t = tidy(d2)[,3]+1,est_cs,lower,upper,
                                 ols_e,ols_e_u,ols_e_l,est_cs_ml,upper_ml,lower_ml))
# dat_cs_pl2 <- dat_cs_pl[-c(1:3,32),] 
dat_cs_pl2 <- dat_cs_pl[-c(1:2),] 


count <- dat_cs %>% group_by(id) %>% summarise(group = unique(group) )
ncells <- length(unique(dat_cs$id))
ncells_bombed <- length(which(count$group==1))+
  length(which(count$group==2))+
  length(which(count$group==3))+
  length(which(count$group==4))+
  length(which(count$group==5))+
  length(which(count$group==8))

ceasef <- round(100*(abs(exp(est_cease$coeftable[1,1] )-1 ) -  abs(exp(est_cease$coeftable[1,1] + est_cease$coeftable[2,1] ) - 1 )),2)

ggplot(data=dat_cs_pl2  , aes(x=t, y= est_cs)) +geom_line()+
  geom_ribbon(aes(x=t,ymin=lower,ymax=upper),fill="blue",alpha=0.1)+
  geom_hline(yintercept=0, col="red",linetype="dashed",alpha=0.5)+
  geom_vline(xintercept=0, col="black",linetype='dashed',alpha=0.3)+
  xlab("Month") + ylab(expression(~ theta[t]))+ 
  geom_line(aes(x=t,y=est_cs_ml),linetype="dashed",alpha=0.5)+
  geom_ribbon(aes(x=t,ymin=lower_ml,ymax=upper_ml),fill="red",alpha=0.1)+
  geom_line(aes(x=t,y=ols_e),linetype="dotted",alpha=0.5)+
  geom_ribbon(aes(x=t,ymin=ols_e_l,ymax=ols_e_u),fill="green",alpha=0.1)+
  geom_label(aes(x=8,y=ols_e[23]-0.05),
             label="LS")+
  geom_label(aes(x=8.5,y=est_cs[20]+0.02),
             label="CS")+
  geom_label(aes(x=3,y=est_cs_ml[19]+0.07),
             label="CS-ML")+
  # geom_label(aes(x=-6,y=-0.6),
  #            label=paste("Overall: ",round(d1_ml$overall.att,3)," (",round(d1_ml$overall.se,3),")",sep=""))+
  geom_label(aes(x=-6,y=-0.88),
             label=paste("Overall average NTL change: \n",overall,"%"))+
  geom_label(aes(x=-6,y=-1.35),
             label=paste("Damaged cells: ",ncells_bombed," (",round(100*ncells_bombed/ncells,1),"%)",
                         "\nFirst time in Oct 2023: ",length(which(count$group==1)),
                         "\nNov 2023: ",length(which(count$group==2)),
                         "\nDec 2023: ",length(which(count$group==3)),
                         "\nJan 2024: ",length(which(count$group==4)),
                         "\nFeb 2024: ",length(which(count$group==5)),
                         "\nMay 2024: ",length(which(count$group==8)),sep="")
  )+
  geom_label(aes(x=-6,y=-1.8),label=paste("Impact of ceasefire (22-30 Nov '23):\n",ceasef," % NTL gain",sep=""))+
  scale_x_continuous(breaks=c(-10,-5,0,3,6,9,11,13),labels=c("-10","-5","Conflict\nstarts","Jan\n2024","Mar\n2024", "June\n2024","August\n2024","October\n2024"))+
  labs(x="")+
  theme_classic()


library(xtable)
res_tab_stagg <- cbind(
  paste( round(tidy(d2)[,4],3) , " (", round(tidy(d2)[,5],3), ")",sep="")[3:28],
  paste( round(tidy(d2_ml)[,4],3) , " (", round(tidy(d2_ml)[,5],3), ")",sep="")[2:27])
rownames(res_tab_stagg) <- c(substr(names(est_did_yr$coefficients),7,12)[1:12],"202209",substr(names(est_did_yr$coefficients),7,12)[13:25])
xtable(res_tab_stagg)
##### marginal estimates ######

dat_prebuilt <- dat[which(dat$id_m==202309),]

est_corr_surf <- lm(ihs(ntl) ~ ihs(area_m2),data=dat_prebuilt)
summary(est_corr_surf)

est_marg_d       <- feols(ihs(ntl) ~ ihs(damage):post | id + id_m + Locality_N^id_m, data=dat,cluster="Locality_N")
est_marg_d_t     <- feols(ihs(ntl) ~ ihs(damage):factor(id_m)| id + id_m + Locality_N^id_m, data=dat,cluster="Locality_N")



#### cumulative damage impacts  
est_marg_ns_c     <- feols(ihs(ntl) ~ ihs(damage_cumul):factor(id_m)| id + id_m+Locality_N^id_m   , data=dat,cluster="Locality_N")
est_marg_ns_c_det <- feols(ihs(ntl) ~ I(damage_cumul/area_m2):factor(id_m)| id + id_m  , data=dat,cluster="Locality_N")
est_marg_c        <- feols(ihs(ntl) ~ ihs(damage_cumul):post | id + id_m + Locality_N^id_m, data=dat,cluster="Locality_N")
est_marg_c_iv     <- feols(ihs(ntl) ~ 1 | id_m+id+Locality_N^id_m | ihs(damage_cumul)~ HubDist:id_m, data=dat,cluster="id")

etable(est_marg_c,est_marg_c_iv , tex=T )


dat_pl_ns <- as.data.frame(cbind(Month = rep(0:12),
                                 est_d_n = c(0,est_marg_d_t$coeftable[1:12,1]),
                                 # est_d_s = c(0,est_marg_south$coeftable[12:22,1]),
                                 sd_d_n = c(0,est_marg_d_t$coeftable[1:12,2]),
                                 # sd_d_s = c(0,est_marg_south$coeftable[12:22,2]),
                                 est_d_n_c = c(0,est_marg_ns_c$coeftable[1:12,1]),
                                 # est_d_s_c = c(0,est_marg_ns_c$coeftable[11:20,1]),
                                 sd_d_n_c = c(0,est_marg_ns_c$coeftable[1:12,2])
                                 # sd_d_s_c = c(0,est_marg_ns_c$coeftable[11:20,2])
))

#### multiple bombing rounds

## average months bomved
mean(dat$n_bombed)
## average weeks bombed
dat_w_n <- dat_w %>% group_by(id) %>% summarise(n_bombed = length(which(damage > 0 )))
dat_w <- left_join(dat_w, dat_w_n)
dat_w <- left_join(dat_w, dat_dam2)

mean(dat_w_n$n_bombed)
max(dat_w_n$n_bombed)

### number and location of cells bombed the most 
length(unique(dat$id[which(dat$n_bombed==12)]))
length(unique(dat_w$id[which(dat_w$n_bombed==max(dat_w_n$n_bombed))]))
dat_w$Locality_N[which(dat_w$n_bombed==max(dat_w_n$n_bombed))]

unique(dat$Locality_N[which(dat$n_bombed==12)])

dat$n_bombed_g <- 0

dat$n_bombed_g <- ifelse(dat$n_bombed <= 13 & dat$n_bombed >= 10, 40, dat$n_bombed_g)
dat$n_bombed_g <- ifelse(dat$n_bombed <= 9 & dat$n_bombed >= 7, 30, dat$n_bombed_g)
dat$n_bombed_g <- ifelse(dat$n_bombed <= 6 & dat$n_bombed >= 4, 20, dat$n_bombed_g)
dat$n_bombed_g <- ifelse(dat$n_bombed <= 3 & dat$n_bombed >= 1, 10, dat$n_bombed_g)


### estimations
est_marg_nb <- feols(ihs(ntl) ~ treatment:factor(n_bombed_g)| id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
etable(est_marg_nb,tex=T)
# 
# est_marg_nb = feols(ihs(ntl) ~ treatment:factor(Locality_N)| id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
# summary(est_marg_nb)

est_marg_nb_treat <- feols(ihs(ntl) ~ i(n_bombed,post,ref=0) | id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
etable(est_marg_nb_treat,tex=T)
round(exp(est_marg_nb_treat$coeftable[,1] ) - 1,2) ### for the NTL impacts put in the table

est_marg_nb_d <- feols(ihs(ntl) ~ ihs(damage):factor(n_bombed) | id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
# 
est_marg_nb_d_att <- feols(ihs(ntl) ~ is_t:post:factor(n_bombed) | id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
etable(est_marg_nb_d_att, tex=T)

per <- function(vec) {
  perc <- round(mean(dat$damage_cumul[which(dat$n_bombed==vec & dat$id_m==202409)])/10000,1)
  return(perc)
}
nbs <- sort(unique(dat$n_bombed))
percs <- sapply( nbs,per)

# est_marg_nb_d = feols(ihs(ntl) ~ factor(id)+factor(id_m) + factor(Locality_N):factor(id_m) | ihs(damage):factor(n_bombed) ~ HubDist:factor(id_m) , data=dat,cluster="Locality_N")
dat_pl_nb <- data.frame(cbind(est = tidy(est_marg_nb_d)[,2],sd = tidy(est_marg_nb_d)[,3],nb = seq(1,12), pv = tidy(est_marg_nb_d)[,5]), percs = percs[2:13])


est_dam = feols(ihs(ntl) ~ ihs(damage):factor(id_m)| id + id_m , data=dat, cluster="Locality_N")
summary(est_dam)


est_did_camp = feols(ihs(ntl) ~ ihs(damage):factor(id_m):factor(camp) | id + id_m , data=dat, cluster="Locality_N")
# plot(est2)
summary(est_did_camp)

est_did_camp_2 = feols(ihs(ntl) ~ ihs(damage_cumul):factor(id_m):factor(camp) | id + id_m , data=dat, cluster="Locality_N")
# plot(est2)
summary(est_did_camp_2)

dat_pl_camp <- as.data.frame(cbind(Month = rep(0:11),
                                   # est_d = c(0,est_did_camp$coeftable[1:11,1]),
                                   est_d = c(0,est_dam$coeftable[1:11,1]),
                                   est_d_c = c(0,est_did_camp$coeftable[12:22,1]),
                                   # sd_d = c(0,est_did_camp$coeftable[1:11,2]),
                                   sd_d = c(0,est_dam$coeftable[1:11,2]),
                                   sd_d_c = c(0,est_did_camp$coeftable[12:22,2])
))


ggplot(data=dat_pl_camp , aes(x=Month, y= est_d)) +geom_line() +
  geom_ribbon(aes(x=Month,ymin=est_d-1.96*sd_d,ymax=est_d+1.96*sd_d),fill="blue",alpha=0.1)+
  geom_hline(yintercept=0, col="red",linetype="dashed",alpha=0.5)+
  geom_vline(xintercept=0, col="black",linetype='dashed')+
  geom_line(aes(x=Month,y=est_d_c),linetype="dashed",alpha=0.5)+
  geom_ribbon(aes(x=Month,ymin=est_d_c -1.96*sd_d_c,ymax=est_d_c +1.96*sd_d_c),fill="red",alpha=0.1)+
  # geom_line(aes(x=t,y=est_cs_ml),alpha=0.5)+
  # geom_ribbon(aes(x=t,ymin=lower_ml,ymax=upper_ml),fill="blue",alpha=0.2)+
  geom_label(aes(x=11.7,y=last(est_d)),
             label="Overall")+
  geom_label(aes(x=11.7,y=last(est_d_c)),
             label="Camp")+
  scale_x_continuous(breaks=c(0,4,8,11),labels=c("Bombing\nstarts","Jan 2024","May 2024","September\n2024"))+
  labs( x = "Month", y = "% NTL loss for 1% increase in damaged grid area")+
  theme_classic()



####### pscore / MTE stuff ######



summary(feols(is_t ~ I(HubDist/1000)  , data=dat))


dat_pre  <- dat %>% group_by(id) %>% filter(t < 1) %>% summarise(bombed = max(is_t) , distance = unique(HubDist)/1000, 
                                                                 camp = unique(camp),n_bombed=unique(n_bombed),loc=unique(Locality_N),
                                                                 ntl_pre = mean(ntl))
dat_post <- dat %>%  group_by(id) %>% filter(t > 0) %>% summarise(bombed = max(is_t) , distance = unique(HubDist)/1000, 
                                                                  camp = max(camp),
                                                                  int_b = sum(damage),ntl = mean(ntl),damage=mean(damage),
                                                                  damage_cumul = last(damage_cumul),
                                                                  n_bombed = unique(n_bombed),
                                                                  loc=unique(Locality_N))

dat_post <- left_join(dat_post,dat_pre[,c("id","ntl_pre")], by ="id")
# dat_post$ntl_ch <- (dat_post$ntl - dat_post$ntl_pre)/dat_post$ntl_pre
dat_post$ntl_ch <- (dat_post$ntl - dat_post$ntl_pre)

library(randomForest)

ps <- glm(bombed~distance, data=dat_pre, family = "binomial")

dat_pre$psc <- predict(ps, type="response")

plot(dat_pre$distance, dat_pre$psc,ylab="Predicted probability of being damaged", xlab="Distance in km")

dat_pre$psc_weights <- dat_pre$bombed/dat_pre$psc + (1- dat_pre$bombed)/(1-dat_pre$psc)
dat_psc             <- left_join(dat, dat_pre[,c("id","psc_weights")])
dat_post            <- left_join(dat_post,dat_pre[,c("id","psc")])

est_did_psc = feols(ihs(ntl) ~ post:is_t | id + id_m + Locality_N , 
                    data=dat_psc, cluster="Locality_N", weights= dat_psc$psc_weights)
# plot(est2)
summary(est_did_psc)


est_did_psc2 = feols(ihs(ntl) ~ ihs(damage_cumul):factor(id_m) | id + id_m , data=dat_psc,
                     cluster="Locality_N", weights= dat_psc$psc_weights)
# plot(est2)
summary(est_did_psc2)



D1 <- ifelse(dat_pre$bombed == 1, 1, NA)
D0 <- ifelse(dat_pre$bombed == 0, 1, NA)
pscore_1 <- na.omit(dat_pre$psc*D1)
pscore_0 <- na.omit(dat_pre$psc*D0)
# summary(pscore_1)
# summary(pscore_0)
# hist(pscore_1)
# hist(pscore_0)
# Range of common support

CS_min <- max(min(pscore_1),min(pscore_0))
CS_max <- min(max(pscore_1),max(pscore_0))
CS <- cbind(CS_min,CS_max)
CS_dummy_min <- ifelse(CS_min <= dat_pre$psc,1,NA)
CS_dummy_max <- ifelse(dat_pre$psc  <= CS_max,1,NA)
CS_dummy <- CS_dummy_min*CS_dummy_max
summary(CS_dummy)
CS

# Construct histograms of propensity score
# for treated and untreated individuals
pscore_1_2 <- pscore_1[-which(pscore_1>0.99)]
pscore.hist_1 <- hist(pscore_1_2, br=50)
pscore.hist_0 <- hist(pscore_0, br=50)
plot(pscore.hist_1,main="P(bombed=1) histogram for bombed (blue) and unbombed (red) grids",xlab="P(bombed)", col=adjustcolor("blue", 0.3))
lines(pscore.hist_0, lty=2, col=adjustcolor("red", 0.3))


library(np)



bw0   <- npregbw(I(ntl_ch/mean(dat_post$ntl_pre)) ~ psc , data=dat_post, regtype="ll", bwmethod="cv.aic")
Y.np0 <- npreg(bws = bw0, gradient=TRUE)

plot(Y.np0, plot.errors.method="bootstrap", gradient=TRUE,
     ylab="MTE", xlab="Predicted probability of being bombed")
# The MTE is valid over the region of common support
mte <- Y.np0$grad*CS_dummy

dat_mte <- as.data.frame(cbind(psc = dat_post$psc, mte_p = mte, err_p= Y.np0$gerr))

mean(mte,na.rm=T )
min(mte,na.rm=T )

# ### Li and Racine bw + bootstrapped se 
# # 
# bw1 <- npregbw(formula = I(ntl_ch/mean(dat_post$ntl_pre)) ~ psc,data=dat_post,method="ll")
# Y.np1 <- npreg(bws = bw1, gradient=TRUE)
#  summary(Y.np1)
# plot(Y.np1, plot.errors.method="bootstrap")
# plot(Y.np1, plot.errors.method="asymptotic", gradient=TRUE,
#      ylab="MTE", xlab="Predicted probability of being bombed")
# mte <- Y.np1$grad*CS_dummy



####### nonlinear stuff ######
# 
# nl <- gam( ihs(ntl) ~ s(ihs(damage)*is_t,k=4) + factor(id) + factor(id_m), data=dat)
# 
# nl_p      <- getViz(nl)
# nl_pl     <- plot( sm(nl_p,1),allTerms = FALSE)
# nl_pl + l_fitLine(colour = "blue") +l_ciLine(mul = 1.96, colour = "black", linetype = 2) +
#   l_ciLine(mul = 1.96, colour = "black", linetype = 3) +
#   l_points(shape = 19, size = 1, alpha = 0.1) + ylim(-0.7,0.1)+
#   labs(x="Log Damage (m2)", y="NTL (%)",title= "Damage")  +
#   theme_classic()


# 
# dat[which(dat$damage_norm==max(dat$damage_norm,na.rm=T)),]
# dat[which(dat$damage_norm>0.80),]
dat$damage_norm2 <- ifelse(is.na(dat$damage/dat$area_m2),0,dat$damage/dat$area_m2)
est_marg_norm <- feols(ihs(ntl) ~ damage_norm2:factor(post)| id + id_m + Locality_N^id_m , data=dat,cluster="Locality_N")
# 
# nl2 <- gam( ihs(ntl) ~ s(damage_norm2,k=4) + factor(id) + factor(id_m), data=dat)
# nl2_p      <- getViz(nl2)
# nl2_pl     <- plot( sm(nl2_p,1),allTerms = FALSE)
# nl2_pl + l_fitLine(colour = "blue") +l_ciLine(mul = 1.96, colour = "black", linetype = 2) +
#   # l_ciLine(mul = 1.96, colour = "black", linetype = 3) +
#   l_points(shape = 19, size = 1, alpha = 0.1) + ylim(-0.9,0.1)+
#   labs(x=" Damage (m2) / Total detectable area", y="NTL (%)")  +
#   geom_label(aes(x=0.5,y=-0.78),label=paste("Average impact:\n",round(tidy(est_marg_norm)[,2],2)," (",round(tidy(est_marg_norm)[,3],2),")\n",
#                                             "Avg: ",round(100*mean(dat$damage_norm[which(dat$damage > 0 )],na.rm=T),2),"% of grid cell size\nMax: 100% (Rafah, Dec 2023) ",sep=""))+
#   theme_classic()


dat$damage_norm_c <- ifelse(is.na(dat$damage_cumul/dat$area_m2),0,dat$damage_cumul/dat$area_m2)
dat$damage_norm_c <- ifelse(is.infinite(dat$damage_norm_c),0,dat$damage_norm_c)
# dat$damage_norm_c <- ifelse(dat$damage_norm_c>1,1,dat$damage_norm_c)

dat_fin <- dat[which(dat$id_m == max(dat$id_m)),]

# nl_c <- gam(ihs(ntl) ~  s(I(damage_cumul/1000000),k=4)+ factor(id) + factor(id_m) , data=dat)
nl_c <- gam(ihs(ntl) ~  s(damage_norm_c,k=4)+ factor(id) + factor(id_m) , data=dat)

nl_c_p      <- getViz(nl_c)
nl_c_pl     <- plot( sm(nl_c_p,1),allTerms = FALSE)
nl_c_pl + l_fitLine(colour = "blue") +l_ciLine(mul = 1.96, colour = "black", linetype = 2) +
  l_ciLine(mul = 1.96, colour = "black", linetype = 3) +
  l_points(shape = 19, size = 1, alpha = 0.1) + ylim(-2,0.8)+xlim(0,1)+
  labs(x="Cumulative damage (m2) / total detectable area", y="Change in NTL (%)")  +
  geom_label(aes(x=0.3,y=-1.2),label=paste("Avg. cumulative damage / total detectable area:\n",
             round(100*mean(dat_fin$damage_norm_c),2),"% (as of October 2024)",sep=""))+
  geom_label(aes(x=0.3,y=-1.68),label=paste("Average impact,\n monthly damage (m2) / total detectable area:\n",round(tidy(est_marg_norm)[,2],2)," (",round(tidy(est_marg_norm)[,3],2),")\n",
                                            "Avg: ",round(100*mean(dat$damage_norm[which(dat$damage > 0 )],na.rm=T),2),"% of grid cell size\nMax: 100% (Rafah, Dec 2023) ",sep=""))+
  theme_classic()

####### figures for marginal / NL / mte #########


### cumulative damage figure
ggplot(data=dat_pl_ns, aes(x=Month, y= est_d_n_c)) +geom_line(linetype="dashed") +
  geom_ribbon(aes(x=Month,ymin=est_d_n_c -1.96*sd_d_n_c ,ymax=est_d_n_c +1.96*sd_d_n_c),fill="green",alpha=0.1)+
  geom_hline(yintercept=0, col="red",linetype="dashed",alpha=0.5)+
  # geom_label(aes(x=8,y=est_d_s[11]-0.001),
  #            label="South")+
  # geom_label(aes(x=11,y=est_d_n_c[11]-0.02),
  #            label="Cumulative")+
  # geom_label(aes(x=9,y=-0),label=paste("Avg. cumulative damage / total detectable area:\n",
  # round(100*mean(dat_fin$damage_norm_c),2),"% (as of early November 2024)",sep=""))+
  geom_label(aes(x=8.5,y=-0.030),label=paste("Overall NTL loss: ",-round(mean(mte,na.rm=T ),2),"*pre-war average NTL",sep=""))+
  geom_label(aes(x=8.5,y=-0.017),
             label=paste(
               # "Avg. incremental impact: ",round(est_marg_d$coeftable[1],2)," (",round(est_marg_d$coeftable[2],3),")\n",
               "Avg. cumulative impact: ",round(est_marg_c$coeftable[1],3)," (",round(est_marg_c$coeftable[2],3),")\n",
               "Avg. monthly impact: ",100*round(est_marg_c$coeftable[1],2),"%",sep=""))+
  geom_label(aes(x=8.3,y=-0.048),label=paste("Avg. impact monthly damage (m2) / total built area:\n",round(tidy(est_marg_norm)[,2],2)," (",round(tidy(est_marg_norm)[,3],2),")\n",
                                             "Avg monthly : ",round(100*mean(dat$damage_norm[which(dat$damage > 0 )],na.rm=T),2),"% of grid cell size",sep=""))+
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12),
                     # labels=c("Bombing\nstarts","Jan 2023","Feb 2024","Apr 2024","June 2024","August 2024") )+
                     labels=c("Conflict\nstarts","Nov\n2023","Jan\n2024","Mar\n2024","Jun\n2024","Aug\n2024","Oct\n2024") )+
  labs( x = "Month", y = "% NTL loss for 1% increase in damaged grid area")+
  theme_classic()

§#### effect of multiple damage rounds

ggplot(data=dat_pl_nb, aes(x=estimate,y=nb)) + geom_point()+
  geom_errorbarh(aes(xmin=estimate - 1.96*std.error, xmax=estimate + 1.96*std.error),height=0.75,
                 linetype=2,alpha=09,colour="black",size=0.3)+
  geom_vline(xintercept=0,linetype="dashed",col="red")+
  geom_label(aes(label=paste(percs, " %", sep=""),colour=p.value))+
  scale_color_gradient(name="p.val",low="darkblue",high="red",space="Lab")+
  scale_y_continuous(breaks=seq(1:12))+
  labs( x = "Average monthly % NTL per 1 % surface damaged ", y = "Number of months in which a cell is damaged")+
  theme_classic()



# GAM / nonlinear effects of normalized detectable damage
nl_c_p      <- getViz(nl_c)
nl_c_pl     <- plot( sm(nl_c_p,1),allTerms = FALSE)
nl_c_pl + l_fitLine(colour = "blue") +l_ciLine(mul = 1.96, colour = "black", linetype = 2) +
  l_ciLine(mul = 1.96, colour = "black", linetype = 3) +
  l_points(shape = 19, size = 1, alpha = 0.1) + ylim(-2,0.8)+xlim(0,1)+
  labs(x="Cumulative damage (m2) / total detectable area", y="Change in NTL (%)")  +
  geom_label(aes(x=0.3,y=-1.2),label=paste("Avg. cumulative damage / total detectable area:\n",
                                           round(100*mean(dat_fin$damage_norm_c),2),"% (as of October 2024)",sep=""))+
  geom_label(aes(x=0.3,y=-1.68),label=paste("Average impact,\n monthly damage (m2) / total detectable area:\n",round(tidy(est_marg_norm)[,2],2)," (",round(tidy(est_marg_norm)[,3],2),")\n",
                                            "Avg: ",round(100*mean(dat$damage_norm[which(dat$damage > 0 )],na.rm=T),2),"% of grid cell size\nMax: 100% (Rafah, Dec 2023) ",sep=""))+
  theme_classic()

# MTE curves
ggplot(data=dat_mte,aes(x=psc, y = mte)) + geom_line() +
  geom_line(aes(x=psc,y=V2+1.96*V3), linetype="dashed")+
  geom_line(aes(x=psc,y=V2-1.96*V3), linetype="dashed")+
  ylab("NTL loss as proportion of pre-conflict average")+ xlab("Predicted probability of being damaged")+
  geom_label(aes(x=0.5,y=-2.5),label=paste("Overall impact:\n",round(mean(mte,na.rm=T ),2)," times the pre-Oct 7th\n average NTL",sep=""))+
  theme_classic()

plot(Y.np0, plot.errors.method="asymptotic", gradient=TRUE,
     ylab="MTE", xlab="Predicted probability of being bombed")

# other fig for SM


d_pl_n <- read.csv("~/academica/Gaza/CE lab data/dpm_ic_masked_cumulative_dates_gridded_refactor-total_percentage.csv")

d_plot_x <-  as.data.frame(cbind(as.numeric(d_pl_n[1,2:60]),as.numeric(d_pl_n[2,2:60]),gsub("\\.", "/",  mdy(substr(colnames(d_pl_n)[2:60],2,11))) ))
colnames(d_plot_x) <- c("perc","cumul","date")
d_plot_x <- rbind(c(0,0,"2023-10-07"),d_plot_x)

ggplot(d_plot_x) + 
  geom_line(aes(as.Date(date),as.numeric(cumul)),color="blue") +
  geom_line(aes(as.Date(date),as.numeric(perc)/1000000)) + 
  theme_classic()+
  scale_x_date(breaks = "weeks")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_label(aes(x=as.Date(date)[54],y=58),label="Normalised,\ntotal builtup area")+
  geom_label(aes(x=as.Date(date)[52],y=76),label="Proportion of total grid cells area")+
  xlab("Weeks")+ylab("% total damaged area, normalised")


########### GDP-NTL EXCHANGE RATE ########### 


## USING THE CLIPPED RASTERS
# wd <- "/Users/dr463/academica/Gaza/GDP-ntl/NTL monthly/NTL/NTL_clipped"
# setwd(wd)
# files <- list.files(pattern = "\\.tif$")
# 
# ntl_m <- rep(0,length(files))
# 
# for (i in 1:length(files)){ 
# dat <- raster(files[i])
# ntl_m[i] <- mean(values(dat),na.rm=T)
# }

## USING THE RASTER-TO-POINT DATASETS (easier to remove anomalies)
# setwd("/Users/dr463/academica/Gaza/GDP-ntl/NTL monthly/NTL/NTL_clipped/points_NTL_clipped")
files <- list.files(pattern = "point_cropNTL-")
ntl_m <- rep(0,length(files))

for (i in 1:length(files)){ 
  dat <- read.csv(files[i])
  dat$VALUE[which(dat$VALUE==65535)] <- NA ### in the NASA data 65535 = NA. Remove or skews the averages!
  ntl_m[i] <- mean(dat$VALUE,na.rm=T)
}
# 
library(openxlsx) 

# gdp = read.xlsx("gaza_gdp.xlsx") # for the West Bank
gdp = read.xlsx("Gaza quarterly GDP.xlsx") # for Gaza, per capita
cons <- read.xlsx("consumption_gaza.xlsx")

ntl_m2 <- ntl_m[seq(1,141,by=3)] # get the quarterly values of NTL
gdp2 <- gdp$Gaza.Strip[49:95] # avoid the last quarter as GDP obviously craters
# 

dates <- seq(as.Date("2012-01-01"),as.Date("2023-10-01"),by="months")
dates2 <- dates[seq(1,141,by=3)]

ntl_ts <- ts(ihs(ntl_m2))
gdp_ts <- ts(ihs(gdp2))
cons_ts <- ts(ihs(cons[,2]))
qt      <-  rep(c(1,2,3,4),17)[1:47]
year    <-   year(dates2)

dat_ts <- data.frame(ntl_ts, gdp_ts, cons_ts, cons, dates2,qt,year)



# ts.plot(15+ntl_ts,gdp_ts)
library(lfe)
library(plm)

### RE
ntlgdp  <- plm(gdp_ts  ~ -1+ntl_ts, data=dat_ts, index=c("year"), model="random")
ntlcons <- plm(cons_ts ~ -1+ntl_ts, data=dat_ts, index=c("year"), model="random")


### FE
summary(felm(ntl_ts ~ gdp_ts |year | 0 |year, data=dat_ts))
# summary(felm(gdp_ts ~ ntl_ts | year | 0 |0, data=dat_ts))
summary(felm(ntl_ts~cons_ts | qt | 0 |0,data=dat_ts))


ntl_nofe <- felm(ntl_ts~ 1 | year+qt | 0 |year, data=dat_ts)$resid
gdp_nofe <- felm(gdp_ts~ 1 | year+qt | 0 |year, data=dat_ts)$resid
cons_nofe <- felm(cons_ts~ 1 | year+qt | 0 |year, data=dat_ts)$resid
df_pl <- data.frame(dates2,ts(ntl_nofe),ts(gdp_nofe))

# ts.plot(df_pl[,2:3],lty=c(1:2),col=c(1:2), main = "log GDP and log NTL",xlab = "Quarter")

ggplot() + geom_line(data=df_pl,aes(x=dates2,y=ntl_nofe),color="black",linetype="dashed") +
  geom_line(data=df_pl,aes(x=dates2,y=gdp_nofe),color="red") + xlab("Year") +ylab("")+
  ggtitle("Gazan quarterly GDP per capita and night-time luminosity \n(detrended, deseasonalized)")+
  scale_x_date(date_breaks="12 month", date_labels="20%y")  +
  theme_classic()


ggplot() + geom_line(data=df_pl,aes(x=dates2,y=gdp_nofe),color="black",linetype="dashed") +
  geom_line(data=df_pl,aes(x=dates2,y=cons_nofe),color="red") + xlab("Year") +ylab("")+
  ggtitle("Gazan quarterly consumption per capita and night-time luminosity \n(detrended, deseasonalized)")+
  scale_x_date(date_breaks="12 month", date_labels="20%y")  +
  theme_classic()


ggsave("gdp-ntl.pdf")





