#install.packages("rdwd")
#updateRdwd(pack = "rdwd", user = "brry", vignette = TRUE, quiet = FALSE)

# Load the package into library (needed in every R session):
library(rdwd)
library(dplyr)


# Landkreis ---------------------------------------------------------------

df <- Covid::pop.ger %>% select(latitude,longitude,admin3.code,asciiname) %>% unique() %>%
  mutate(
    lat = as.numeric(latitude),
    lon = as.numeric(longitude))

pb = txtProgressBar(min = 0, max = nrow(df), initial = 1)
cur.radius <-50
dfall <- data.frame()
for(k in 1:nrow(df)) {
  setTxtProgressBar(pb,k)
  # find nearby stations
  sta <-nearbyStations(df$lat[k],df$lon[k],
                       radius = cur.radius,
                       res="daily",
                       var="kl",
                       per="recent",quiet = TRUE)

  sta <- sta[sta$url!="",]

  #max(sta$bis_datum,na.rm=T)
  #unique(sta$Stations_id)

  # todo: drop high stations
  #max(sta$Stationshoehe)

  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(sta$url, read=FALSE,quiet = TRUE,force = TRUE,overwrite = TRUE)
  # Read the file from the zip folder:
  try <-try(readDWD(file, varnames=TRUE,quiet = TRUE,fread = FALSE),silent = TRUE)
  if(class(try)=="try-error"){
    file <- dataDWD(sta$url, read=FALSE,force = TRUE,quiet = TRUE,overwrite = TRUE,fread = FALSE)
    clim <- readDWD(file, varnames=TRUE,quiet = TRUE)
  } else {
    clim <- try
  }

  df_files <- data.frame()
  for(i in 1:length(clim))
  {
    df_files <-
      plyr::rbind.fill(
        clim[[i]],
        df_files
        )
  }
  df_files <- as_tibble(df_files) %>% select(-c(QN_3,
                                            QN_4,
                                            SHK_TAG.Schneehoehe,
                                            RSKF.Niederschlagsform,
                                            eor))


  df_files %>%  summarise(max(MESS_DATUM))

  dfsummary <- df_files %>% group_by(MESS_DATUM) %>%
    summarise_all(
      list(~ mean(., na.rm = TRUE))
    ) %>% rename(
      date = MESS_DATUM
    ) %>% mutate(
      latitude = df$latitude[k],
      longitude = df$longitude[k],
      admin3.code = df$admin3.code[k],
      name = df$asciiname[k]
    )

  dfall <- plyr::rbind.fill(dfall,dfsummary)
}
ls(dfall)
weather_dwd <- dfall %>% select(-c(STATIONS_ID))
usethis::use_data(weather_dwd,overwrite = TRUE)


# check for missing
weather_dwd[!complete.cases(weather_dwd),]%>%
  select(date,name)%>%unique()%>%View

# pdat <- weather_dwd %>% filter(name %in% sample(weather_dwd$name,1)) %>%
#                                                 tidyr::pivot_longer(
#                                                   -c(date,latitude,longitude,name,admin3.code),
#                                                   names_to = "var",
#                                                   names_repair = "minimal")
# ggplot(pdat,
#        aes(x=date,y=value))+geom_line()+facet_wrap(vars(var),scales = "free")


# Bundesland --------------------------------------------------------------


df <- Covid::popall %>% filter(adm_level==2,country.code=="DE") %>%
  select(latitude,longitude,region.code,asciiname) %>% unique() %>%
  mutate(
    lat = as.numeric(latitude),
    lon = as.numeric(longitude))

pb = txtProgressBar(min = 0, max = nrow(df), initial = 1)
cur.radius <-100
dfall <- data.frame()
for(k in 1:nrow(df)) {
  setTxtProgressBar(pb,k)
  # find nearby stations
  sta <-nearbyStations(df$lat[k],df$lon[k],
                       radius = cur.radius,
                       res="daily",
                       var="kl",
                       per="recent",quiet = TRUE)

  sta <- sta[sta$url!="",]

  #max(sta$bis_datum,na.rm=T)
  #unique(sta$Stations_id)

  # todo: drop high stations
  #max(sta$Stationshoehe)

  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(sta$url, read=FALSE,quiet = TRUE,force = TRUE,overwrite = TRUE)
  # Read the file from the zip folder:
  try <-try(readDWD(file, varnames=TRUE,quiet = TRUE,fread = FALSE),silent = TRUE)
  if(class(try)=="try-error"){
    file <- dataDWD(sta$url, read=FALSE,force = TRUE,quiet = TRUE,overwrite = TRUE,fread = FALSE)
    clim <- readDWD(file, varnames=TRUE,quiet = TRUE)
  } else {
    clim <- try
  }

  df_files <- data.frame()
  for(i in 1:length(clim))
  {
    df_files <-
      plyr::rbind.fill(
        clim[[i]],
        df_files
      )
  }
  df_files <- as_tibble(df_files) %>% select(-c(QN_3,
                                                QN_4,
                                                SHK_TAG.Schneehoehe,
                                                RSKF.Niederschlagsform,
                                                eor))


  #df_files %>%  summarise(max(MESS_DATUM))

  dfsummary <- df_files %>% group_by(MESS_DATUM) %>%
    summarise_all(
      list(~ mean(., na.rm = TRUE))
    ) %>% rename(
      date = MESS_DATUM
    ) %>% mutate(
      latitude = df$latitude[k],
      longitude = df$longitude[k],
      admin.code = df$region.code[k],
      name = df$asciiname[k]
    )

  dfall <- plyr::rbind.fill(dfall,dfsummary)
}
ls(dfall)
weather_dwd_Bundesland <- dfall %>% select(-c(STATIONS_ID))
usethis::use_data(weather_dwd_Bundesland,overwrite = TRUE)

weather_dwd_Bundesland %>% group_by(admin.code) %>% summarise(max(date))

# check for missing
weather_dwd_Bundesland[!complete.cases(weather_dwd_Bundesland),]%>%
  select(date,name)%>%unique()

# pdat <- weather_dwd_Bundesland %>%
#   filter(name %in% sample(weather_dwd_Bundesland$name,2)) %>%
#   tidyr::pivot_longer(
#     -c(date,latitude,longitude,name,admin.code),
#     names_to = "var")
#
# ggplot(pdat %>% filter(date>"2020-03-01"),
#        aes(x=date,y=value))+geom_line()+
#   facet_wrap(name~var,scales = "free",nrow=2)

