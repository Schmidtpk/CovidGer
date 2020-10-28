library(tidyverse)

# read in intervention data -----------------------------------------------

googledrive::drive_download("https://docs.google.com/spreadsheets/d/1cmGBMUhBt5y6jwiqaF7lh7VQNMN6D5FCoIltlOzhMfI/edit#gid=0",
                            overwrite = TRUE,
                            path = "./data-raw/measures/interventions.csv")

df <- read_csv("./data-raw/measures/interventions.csv")

# drop NAs
df <- df%>%filter(check %in% c("CORRECT","NEW"),
                  !is.na(intervention)) %>% select(-c(id,check))


# define regional unit
df$unit <- ifelse(df$adm.level==2, df$Bundesland,
                  ifelse(df$adm.level==3,df$Landkreis,NA))

# generate data with date,name,adm.level ----------------------------------

data <- data.frame()
data <- plyr::rbind.fill(data,
                   data.frame(date=seq.Date(as.Date("2020-02-01"),Sys.Date(),by = "days")))

data <- plyr::rbind.fill(data,
                   data.frame(unit = unique(df$unit)))

data <- data %>%
  complete(date,unit) %>%
  filter(!is.na(unit) & !is.na(date)) %>%
  left_join(df%>%select(unit,Bundesland)%>%unique(),
            by=c("unit"))


# categorize interventions --------------------------------------
unique(df$comment)
#valid_comments <- c("limited","3","10","hh+1","2hh", "1000", "100","50","150","5","6","outside")
valid_comments <- c("Merkel","limited","outside")

df$comment[(!is.na(df$comment)) & (!df$comment %in% valid_comments)] <- "limited"

df$intervention <- paste0(df$intervention,ifelse(is.na(df$comment),"",df$comment))

intervention.list <- df
use_data(intervention.list,overwrite = TRUE)

# checks ------------------------------------------------------------------

if(any(is.na(df$start) & is.na(df$end)))
  stop("Intervention without start and end date")


# loop over implications -------------------------------------

unique(df$intervention[grep("events",df$intervention)])
unique(df$intervention[grep("gatherings",df$intervention)])
unique(df$intervention[grep("sport",df$intervention)])
unique(df$intervention[grep("shops",df$intervention)])
unique(df$intervention[grep("schools",df$intervention)])


implications <- list(
  list("events open",
       "events openlimited100",
       "events openlimited",
       "events",
       "events10","events100","events1000","eventslimited", "events recommended", "events recommendedlimited"),
  list("curfew","curfewlimited"),
  list("gatherings","gatheringshh+1","gatherings2hh","gatherings3","gatherings10","gatheringslimited"),
  list("gatherings open","gatherings openlimited"),
  list("sports open", "sports openlimited", "sports", "sportslimited"),
  list("shops open","shops openlimited", "shops","shopslimited"),
  list("schools open","schools openlimited","schools","schoolslimited"),
  list("kitas full","kitas open", "kitas openlimited","kitas","kitaslimited"),
  list("restaurants open", "restaurants openlimited", "restaurants openoutside","restaurants","restaurantslimited"),
  list("masks","maskslimited","masks recommended")
)




#generate copy
df2 <- df

for(i in 1:nrow(df)){
  for(i.list.cur in implications){
    for(i.cur.strong in i.list.cur[-length(i.list.cur)]){

      weaker.interventions <- i.list.cur[-(1:which(i.list.cur==i.cur.strong))]


        ### assign end to strong intervention from first weaker intervention (if end not given)
        if(df$intervention[i]==i.cur.strong & is.na(df$end[i])){
          dfs <- df %>% filter(
            unit == df$unit[i],
            intervention %in% weaker.interventions,
            start > df$start[i])

          if(nrow(dfs)>0)
            df2$end[i] <- dfs %>% pull(start) %>% min()
        }

        ### move start to first strong intervention that started before
        # if(df$intervention[i]==i.cur.weak){
        #   dfs <- df %>% filter(
        #     unit == df$unit[i],
        #     intervention == i.cur.strong,
        #     start < df$start[i])
        #   if(nrow(dfs)>0)
        #     df2$start[i] <- dfs %>% pull(start) %>% min()
        # }
    }
  }
}
df <- df2



rm("df_add_all")
for(i in 1:nrow(df)){
  for(i.list.cur in implications){
    for(i.cur.strong in i.list.cur[-length(i.list.cur)]){
      for(i.cur.weak in i.list.cur[-(1:which(i.list.cur==i.cur.strong))]){

        ### add weaker intervention if it exists in data
        if(df$intervention[i]==i.cur.strong &
           i.cur.weak %in% unique(df$intervention)){
          df_add <- df[i,]
          df_add$intervention <- i.cur.weak
          if(exists("df_add_all"))
            df_add_all <- rbind(df_add_all,df_add)
          else
            df_add_all <- df_add
        }
      }
    }
  }
}


df <- rbind(df,df_add_all)


# loop over interventions and fill in -------------------------------------

# generate rows
for(int in unique(df$intervention)){
  data[,int]<-NA
}

df <- df %>% arrange((adm.level),start) %>% ungroup()

for(i in 1:nrow(df)){
  int <- df$intervention[i]

  if(is.na(df$end[i]))
    date.choice <- data$date>=df$start[i]
  else
    date.choice <- data$date>=df$start[i] & data$date<df$end[i]

  if(df$adm.level[i]==1)
    name.choice <- TRUE
  else if(df$adm.level[i]==2)
    name.choice <- data$Bundesland==df$unit[i]
  else if(df$adm.level[i]==3)
    name.choice <- data$unit == df$unit[i]

  if( colSums(data[date.choice & name.choice ,int],na.rm = TRUE)>0)
    message(paste("already defined values for intervention number ",i,
                  "with invervention ",df$intervention[i],
                  "in", df$unit[i]))

  data[date.choice & name.choice ,int]<-TRUE
}


# set NA to FALSE ---------------------------------------------------------

data[unique(df$intervention)][is.na(data[unique(df$intervention)])]<- FALSE


# show results ------------------------------------------------------------

# safe bundesland (region) where interventions are coded
BL.interventions <- intervention.list %>% filter(adm.level==2) %>%
  group_by(unit) %>% summarise(number = n()) %>%
  filter(number>15)%>%pull(unit)

# safe dummy for those regions
data$all_interventions_exist <- data$Bundesland %in% BL.interventions

ggplot(data%>%
         filter(all_interventions_exist,
                date>as.Date("2020-02-25"),
                date<as.Date("2020-06-01"))%>%
         pivot_longer(cols = -c(date,unit,Bundesland,all_interventions_exist)),
       aes(x=date,y=name,col=value))+
  geom_point()+facet_wrap(vars(unit))
#ggsave('./data-raw/measures/byunit.pdf',width=10,height = 20)

ggplot(data%>%
         filter(all_interventions_exist,
                date>as.Date("2020-02-25"),
                date<as.Date("2020-06-01"))%>%
         pivot_longer(cols = -c(date,unit,Bundesland,all_interventions_exist)),
       aes(x=date,y=unit,col=value))+
  geom_point()+facet_wrap(vars(name),nrow=3)+
  geom_vline(xintercept = as.Date("2020-05-15"))
#ggsave('./data-raw/measures/byintervention.pdf',width=10,height = 20)

interventions <- data
usethis::use_data(interventions,overwrite = TRUE)
