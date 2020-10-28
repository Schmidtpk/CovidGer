library(dplyr)
library(ggplot2)

#source("./data-raw/Regionaldatenbank/regionaldatenbank.R")

# combine Datenstand ------------------------------------------------------------

Date.formats <-c("%Y-%m-%d",
                 "%m/%d/%Y",
                 "%Y/%m/%d",
                 "%d.%m.%Y")
a <- data.frame()
dates <- seq.Date(as.Date("2020-03-27"),Sys.Date(),by = "days")
for(i in 150:length(dates)){
  date.cur <- dates[i]
  path.cur <- paste0("./data-raw/RKI/git_copy/RKI_COVID19_",date.cur,".csv")

  # print non existing Datenstand dates
  if(!file.exists(path.cur))
    message(date.cur)
  else{
    # read data
    a.cur <- read.csv(path.cur)

    ### rename if non-standard variable names
    if("NeuerTodesfall" %in% ls(a.cur)){
      a.cur<- a.cur %>% rename(
        Neuer.Todesfall = NeuerTodesfall,
        Neuer.Fall = NeuerFall)
    }

    if("ï..IdBundesland" %in% ls(a.cur)){
      a.cur<- a.cur %>% rename(
        IdBundesland = 'ï..IdBundesland'
      )
    }

    if("Landkreis.ID" %in% ls(a.cur)){
      a.cur<- a.cur %>% rename(
        IdLandkreis = Landkreis.ID
      )
    }

    if("Referenzdatum" %in% ls(a.cur)){
      a.cur<- a.cur %>% rename(
        Refdatum = Referenzdatum
      )
    }

    ### format dates
    a.cur$Datenstand <- as.Date(a.cur$Datenstand,
                                tryFormats = Date.formats)
    if(any(a.cur$Datenstand!=date.cur))
      warning("Different date")

    ### if refdatum exists
    if("Refdatum" %in% ls(a.cur)){

      if(is.numeric(a.cur$Refdatum))
        a.cur$Refdatum <- lubridate::as_date(a.cur$Refdatum/(24*60*60*1000))
      else
        a.cur$Refdatum <- as.Date(a.cur$Refdatum,
                                  tryFormats = Date.formats)

      a.cur$Refdatum_exists <- TRUE

      ### checks
      if(mean(a.cur$Refdatum==a.cur$Meldedatum |
              is.na(a.cur$Refdatum))>.5)
        browser()
      dfs <- a.cur %>% filter(Neuer.Fall==1)
      if(mean(dfs$Refdatum==dfs$Meldedatum |
              is.na(dfs$Refdatum))>.5)
        browser()
      if(any(is.na(a.cur$Refdatum)))
        browser()

    } else {
      ### if Refdatum does not exists, create NA-column
      a.cur$Refdatum <- as.Date(NA)
      a.cur$Refdatum_exists <- FALSE
    }

    if(is.numeric(a.cur$Meldedatum))
      a.cur$Meldedatum <- lubridate::as_date(a.cur$Meldedatum/(24*60*60*1000))
    else
      a.cur$Meldedatum <- as.Date(a.cur$Meldedatum,
                                  tryFormats = Date.formats)

    if(any(is.na(a.cur$Meldedatum)))
      browser()

    if("IstErkrankungsbeginn" %in% ls(a.cur))
      a.cur$IstErkrankungsbeginn_exists <- TRUE
    else
      a.cur$IstErkrankungsbeginn_exists <- FALSE

    ### combine results
    a <- plyr::rbind.fill(a,a.cur)
  }
}

### drop unnecessary columns
a <- a %>% select(-c(Altersgruppe2,
                     AnzahlGenesen,
                     Anzahl.Genesen,
                     Neu.Genesen,
                     NeuGenesen,
                     FID,
                     "ï..FID",
                     "ObjectId"
                     ))

safe <- a
a<-safe


# format ids --------------------------------------------------------------

###Berlin
# join all berlin observations in rki data to 11000 code
# as no population data for subregions in regionaldatenbank
a$Landkreis <- as.character(a$Landkreis)
a$IdLandkreis[grepl("Berlin",a$Landkreis)]<-11000
a$Landkreis[grepl("Berlin",a$Landkreis)]<-"Berlin"


a$IdLandkreis <- as.factor(a$IdLandkreis)

# format Refdatum and IstErkrankungsbeginn --------------------------------

a$IstErkrankungsbeginn <- a$IstErkrankungsbeginn==1

# assign Refdatum NA if not timing of symptom onset
a<- a%>%group_by(Datenstand) %>%
  mutate(
    Refdatum_exists = any(!is.na(Refdatum)),
    IstErkrankungsbeginn_exists = any(!is.na(IstErkrankungsbeginn)),
    Refdatum = if_else(IstErkrankungsbeginn_exists,
                      if_else(IstErkrankungsbeginn,Refdatum,as.Date(NA)),
                      if_else(Refdatum!=Meldedatum,Refdatum,as.Date(NA)))
  ) %>% ungroup()



# drop too early dates ----------------------------------------------------
table(a$Refdatum)
a <- a %>% filter(Refdatum>"2020-01-01"|is.na(Refdatum))

# Age formating -----------------------------------------------------------

a <- a %>% rename(
  age = Altersgruppe,
  gender = Geschlecht
)


# drop unkown age and gender ----------------------------------------------

### age
message(paste("Drop unkown age group. Number of observations:",sum(a$age=="unbekannt")))
a <- a %>% filter(age!="unbekannt")
a$age <- droplevels(a$age)

# levels(a$age)[levels(a$age)%in%c("A00-A04","A05-A14")]<-"A0-14"
# levels(a$age)[levels(a$age)%in%c("A15-A34","A35-A59")]<-"A15-59"
#levels(a$age)[!levels(a$age)%in%c("A0-14","A15-59")]<-"A60+"

### gender
message(paste("Drop unkown gender. Number of observations:",sum(a$gender=="unbekannt")))
a <- a %>% filter(gender!="unbekannt")
a$gender <- droplevels(a$gender)


# Safe rki_stand ----------------------------------------------------------
rki_stand <- a
usethis::use_data(rki_stand,overwrite = TRUE)

# safe rki_new
rki_new <- a %>% filter(Datenstand == max(Datenstand))
usethis::use_data(rki_new,overwrite = TRUE)


# loop over landkreis -----------------------------------------------------
start.date <- "2020-03-01"
end.date <- "2020-07-01"
# + for all reports -------------------------------------------------------

dfs <- rki_stand %>%
  filter(Datenstand==max(Datenstand),
         Meldedatum > as.Date(start.date),
         Meldedatum < as.Date(end.date)) %>%
  group_by(Landkreis,Meldedatum)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)]),
    pos.sym = sum(AnzahlFall[Neuer.Fall%in%c(0,1) & !is.na(Refdatum)]),
    pos.no.sym = sum(AnzahlFall[Neuer.Fall%in%c(0,1) & is.na(Refdatum)]),
    srate = pos.sym/pos.new)

vec.kreis <- unique(dfs$Landkreis)
#vec.kreis <- sample(unique(dfs$Landkreis),6)
# vec.kreis <- unique(dfs$Landkreis)[
#   grepl("nchen",unique(dfs$Landkreis))|
#   grepl("Heinsberg",unique(dfs$Landkreis))|
#   grepl("Tirschen",unique(dfs$Landkreis))|
#   grepl("Jena",unique(dfs$Landkreis))
#     ]

### individual regressions
coef<-NULL
for(lk.cur in vec.kreis)
{
  df.cur <- dfs %>% filter(Landkreis == lk.cur)

  df.cur$t <- as.numeric(df.cur$Meldedatum-min(df.cur$Meldedatum))

  coef.cur <- as.data.frame(summary(lm(srate~I(t-mean(t)),data = df.cur))$coefficients)
  coef.cur$Landkreis <- lk.cur
  coef.cur <- coef.cur[,-c(3)]
  coef.cur$coef <- rownames(coef.cur)
  rownames(coef.cur)<-NULL
  coef.cur$pos <- sum(df.cur$pos.new,na.rm = TRUE)


  coef <- rbind(coef.cur,coef)
}

# format results
coef <- coef %>%
  tidyr::pivot_wider(id_cols = -c(1,2,3),
              names_from = coef,
              values_from = Estimate) %>%
  rename(
    Intercept ='(Intercept)',
    beta_time = 'I(t - mean(t))',
  )

### Define troublesome
coef$troublesome <- NA;coef$dist <-NA
for(lk.cur in vec.kreis)
{
  coef.cur <- coef %>% filter(Landkreis==lk.cur)

  diff1 <- coef.cur$Intercept-mean(coef$Intercept)
  diff2 <- (coef.cur$beta_time-mean(coef$beta_time))
  diff <- cbind(diff1,diff2)
  cov <- cov(coef%>%select(Intercept,beta_time))
  dist.cur <- diff%*%solve(cov)%*%t(diff)

  ### Define troublesome reporting in Landkreis
  if(
    # coef.cur$Estimate[1]<.05| # rate of pre/asymptomatic too low
    # coef.cur$Estimate[1]>.5| # rate of asymptomatic too high
    # coef.cur$Estimate[2]< (-.005)| # changes over time contrary to common pattern
    # coef.cur$Estimate[2]> .01
    dist.cur>1.5
  ){
    troublesome.cur <- TRUE
  } else {
    troublesome.cur <- FALSE
  }

  coef$troublesome[coef$Landkreis==lk.cur] <- troublesome.cur
  coef$dist[coef$Landkreis==lk.cur] <- dist.cur
}
lk_summary <- coef
usethis::use_data(lk_summary,overwrite = TRUE)

# + for dead only -------------------------------------------------------

dfs <- rki_stand %>%
  filter(Datenstand==max(Datenstand),
         Meldedatum < as.Date(end.date),
         Neuer.Todesfall %in% c(0,1))
vec.kreis <- unique(dfs$Landkreis)
#vec.kreis <- sample(unique(dfs$Landkreis),6)
# vec.kreis <- unique(dfs$Landkreis)[
#   grepl("nchen",unique(dfs$Landkreis))|
#   grepl("Heinsberg",unique(dfs$Landkreis))|
#   grepl("Tirschen",unique(dfs$Landkreis))|
#   grepl("Jena",unique(dfs$Landkreis))
#     ]

### individual regressions
coef<-NULL
for(lk.cur in vec.kreis)
{
  df.cur <- dfs %>% filter(Landkreis == lk.cur)

  df.cur$t <- as.numeric(df.cur$Meldedatum-min(df.cur$Meldedatum))

  coef.cur <- as.data.frame(summary(lm(is.na(Refdatum)~I(t-mean(t)),data = df.cur))$coefficients)
  coef.cur$Landkreis <- lk.cur
  coef.cur <- coef.cur[,-c(3)]
  coef.cur$coef <- rownames(coef.cur)
  rownames(coef.cur)<-NULL
  coef.cur$pos <- sum(df.cur$AnzahlFall[df.cur$Neuer.Fall%in%c(0,1)],na.rm=TRUE)


  coef <- rbind(coef.cur,coef)
}

# format results
coef <- coef %>%
  tidyr::pivot_wider(id_cols = -c(1,2,3),
              names_from = coef,
              values_from = Estimate) %>%
  rename(
    Intercept ='(Intercept)',
    beta_time = 'I(t - mean(t))',
  )

### Define troublesome
coef$troublesome <- NA;coef$dist <-NA
for(lk.cur in coef %>% filter(!is.na(Intercept),!is.na(beta_time)) %>% pull(Landkreis)%>%unique())
{
  coef.cur <- coef %>% filter(Landkreis==lk.cur)

  diff1 <- coef.cur$Intercept-mean(coef$Intercept,na.rm = TRUE)
  diff2 <- (coef.cur$beta_time-mean(coef$beta_time,na.rm = TRUE))
  diff <- cbind(diff1,diff2)
  cov <- cov(coef%>%select(Intercept,beta_time)%>%filter(complete.cases(Intercept,beta_time)))
  dist.cur <- diff%*%solve(cov)%*%t(diff)

  ### Define troublesome reporting in Landkreis
  if(
    # coef.cur$Estimate[1]<.05| # rate of pre/asymptomatic too low
    # coef.cur$Estimate[1]>.5| # rate of asymptomatic too high
    # coef.cur$Estimate[2]< (-.005)| # changes over time contrary to common pattern
    # coef.cur$Estimate[2]> .01
    dist.cur>1.5
  ){
    troublesome.cur <- TRUE
  } else {
    troublesome.cur <- FALSE
  }

  coef$troublesome[coef$Landkreis==lk.cur] <- troublesome.cur
  coef$dist[coef$Landkreis==lk.cur] <- dist.cur
}
lk_dsummary <- coef
usethis::use_data(lk_dsummary,overwrite = TRUE)


### combine results from all reports and dead only
lk_summary_combined <- left_join(lk_summary,lk_dsummary,
                                 by = c("Landkreis"),
                                 suffix = c(".p",".d"))






# traced ratio ------------------------------------------------------------
# compute ratio of traced symptom onsets within last 7 days
dfs <- rki_new %>% mutate(name = Landkreis)

df.reported <- data.frame(unique(dfs %>% rename(date=Refdatum)%>%select(date,name)))
df.reported <- df.reported%>%
  tidyr::expand(name,date =
                  seq.Date(min(df.reported$date,na.rm = TRUE),
                           max(df.reported$date,na.rm = TRUE),
                           by="days"))

df.reported <- df.reported%>%filter(!is.na(date),!is.na(name))
df.reported$ratio <-NA
for(i in 1:nrow(df.reported)){

  date.cur <- df.reported$date[i]
  name.cur <- df.reported$name[i]

  dfss <- dfs %>% filter(Landkreis == name.cur,
                         Refdatum > date.cur - as.difftime(7,unit="days"),
                         Refdatum <= date.cur)

  symptoms <- dfss %>%
    summarise(
      sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))

  reported <- dfss %>% filter(Meldedatum <= date.cur) %>%
    summarise(
      sum(AnzahlFall[Neuer.Fall%in%c(0,1)]))

  if(symptoms>0)
    df.reported$ratio[i] <- as.numeric(reported/symptoms)
}

traced <- df.reported %>% rename(traced = ratio)
use_data(traced, overwrite = TRUE)
