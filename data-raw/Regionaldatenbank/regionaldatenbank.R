library(tidyverse)


# load population data ----------------------------------------------------
df <- as_tibble(read.csv("./data-raw/Regionaldatenbank/Regionaldatenbank.csv",
                         sep = ";",header = T,
                         skip = 7))
dim(df)

df<-df[-c(1),]

head(df)
df <- df %>% rename(
  "id"="X",
  "name"="X.1",
  "age"="X.2",
  "total"="Insgesamt")

df$name <- as.factor(gsub(" ","",as.character(df$name)))

df

df <- df %>% filter(!age%in%c("Insgesamt",""))

df <- df %>% droplevels()
df$total <- as.numeric(as.character(df$total))



# reformat age ------------------------------------------------------------
df$age <- readr::parse_number(as.character(df$age))

pop<-df
pop
pop[1,"age"]<-0
use_data(pop, overwrite = TRUE)


df <- df %>% mutate(
  age = factor(
    ifelse(age<=4,"A00-A04",
           ifelse(age<=14,"A05-A14",
                  ifelse(age<=34,"A15-A34",
                         ifelse(age<=59,"A35-A59",
                                ifelse(age<=79,"A60-A79","A80+"))))))
)


# reformat Berlin and Hamburg to one id/name ------------------------------
df$id <- as.character(df$id)

### Format Berlin in covid data
# Berlin has only population data for whole Bundesland
#df %>% filter(grepl("Berlin",name)) %>% View
#Covid::rki_stand%>% filter(grepl("Berlin",Landkreis)) %>% View

#drop zeros
df <- df %>% filter(total!=0)
df$id[df$name=="Berlin"]<-"11"



### Format Hamburg in regional data
# Hamburg has only covid data for whole Bundesland
#df %>% filter(grepl("Hamburg",name)) %>% View
#Covid::rki_stand%>% filter(grepl("Hamburg",Landkreis)) %>% View
df$id[df$name=="Hamburg"]<-"2"

df$id <- as.character(df$id)



# assign admin level ------------------------------------------------------

# find BUndesland
df$adm.level <- ifelse(as.numeric(df$id)<=16,2,3)
#df %>% filter(is.na(adm.level)) %>% View()

# add nationwide, which was a character "DG" and is now NA
df$adm.level[is.na(df$adm.level)] <- 1

# sum up population within age --------------------------------------------
df <- df %>% group_by(id,name,age,adm.level) %>%
  summarise_at(vars(total),sum, na.rm=TRUE) %>% ungroup()


# add area data ----------------------------------------------------

area <- as_tibble(read.csv("./data-raw/Regionaldatenbank/Flaeche.csv",
                           sep = ";",header = T,
                           skip = 2))
area <- area %>% rename(
  "id"="X.1",
  "name"="X.2",
  "area1"="Insgesamt",
  "area2"="Insgesamt2")

area$name <- as.factor(gsub(" ","",as.character(area$name)))

area <- area %>% droplevels()
area$area2 <- as.numeric(as.character(area$area2))

area <- area %>% filter(!is.na(area2))

area$id[grepl("ttingen",area$name)]<-3152

df <- df %>% left_join(area%>%select(id,area2))

# show missing
df %>% filter(is.na(area2)) %>% select(name,id) %>% unique()


# compute density ---------------------------------------------------------

df <- df %>% group_by(name) %>%
  mutate(
    density = sum(total)/area2
  ) %>% ungroup()

# safe regionaldatenbank df in package ------------------------------------
regionaldatenbank <- df
use_data(regionaldatenbank,overwrite = TRUE)

# + test merge Kreis --------------------------------------------------------------

data <- Covid::rki_stand %>%
  filter(Datenstand == max(Datenstand),
         Refdatum > as.Date("2020-02-15")) %>%
  group_by(Refdatum,age,IdLandkreis)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Refdatum
  ) %>% ungroup()

data <- data %>%
  left_join(
    Covid::rki_stand %>% select(Landkreis,IdLandkreis) %>% group_by(IdLandkreis) %>% filter(row_number()==1),
    by = c("IdLandkreis"))%>%
  rename(
    name=Landkreis,
    id=IdLandkreis
  )

data$name <- as.factor(data$name)


data <- left_join(
  data%>%ungroup(),
  regionaldatenbank%>%filter(adm.level==3) %>%ungroup(),
  by=c("age","id"))

data
mean(is.na(data$total))


# check remaining ---------------------------------------------------------
data%>%filter(is.na(total))%>%select(id,name.x)%>%unique()

check <- "ttingen"
check <- "Rostock"
check <- "Greifswald"
check <- "lust"
check <- "archim"

data %>% filter(grepl(check,name.x)) %>% select(id,name.x) %>% unique()
df %>% filter(grepl(check,name)) %>% select(id,name) %>% unique()


