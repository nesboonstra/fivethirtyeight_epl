rm(list=ls())

library(tidyverse)
library(lubridate)


df <- read_csv("spi_matches.csv",
               col_names=c("season","date","league_id","league",
                           "club_home","club_away","spi_home","spi_away",
                           "winprob_home","winprob_away","winprob_draw",
                           "projscore_home","projscore_away",
                           "importance_home","importance_away",
                           "score_home","score_away","xg_home","xg_away",
                           "nsxg_home","nsxg_away","adjg_home","adjg_away"),
               col_types="icicccdddddddddiidddddd",
               skip=1)



df_epl <- df %>% 
  mutate(date = as_date(date)) %>% 
  filter(league_id == 2411) %>% 
  select(!c(league,league_id)) %>% 
  mutate(tblpts_home = case_when(
    score_home > score_away ~ 3,
    score_home == score_away ~ 1,
    score_home < score_away ~ 0
  )) %>% 
  mutate(tblpts_away = case_when(
    score_home < score_away ~ 3,
    score_home == score_away ~ 1,
    score_home > score_away ~ 0
  )) %>%
  mutate(matchid = row_number())

df_epl <- na.omit(df_epl)



# I want to pivot longer so that an observation is one team's performance.
# There can be columns for opponents, but one obs is one team's match.
# I would love to know how to parse out home and away.

df_epl_home <- df_epl %>% 
  select(!contains(c("away","draw"))) %>% 
  mutate(home=1) %>% 
  rename_all(
    funs(
      str_replace_all(.,"_home","")
    )
  )


df_epl_away <- df_epl %>% 
  select(!contains(c("home","draw"))) %>% 
  mutate(home=0) %>% 
  rename_all(
    funs(
      str_replace_all(.,"_away","")
    )
  )

epl_full <- bind_rows(df_epl_home,df_epl_away)

epl_full <- epl_full %>% 
  mutate(win=case_when(
    tblpts == 3 ~ 1,
    TRUE ~ 0
  ))


fit_proj <- lm(score ~ projscore, data = epl_full)
summary(fit_proj)

fit_xg <- lm(score ~ xg + nsxg, data = epl_full)
summary(fit_xg)

xg_proj <- lm(xg ~ projscore, data = epl_full)
summary(xg_proj)

fit_win <- lm(win ~ winprob, data = epl_full)
summary(fit_win)

fit_score <- lm(winprob ~ projscore, data = epl_full)
summary(fit_score)


tables <- epl_full %>% 
  group_by(club,season) %>% 
  summarise(projscore=sum(projscore),gf=sum(score),xgf=sum(xg),nsxgf=sum(nsxg),
            adjgf=sum(adjg),tblpts=sum(tblpts),wins=sum(win),avgwin=mean(winprob)) %>% 
  arrange(season,desc(tblpts)) %>% 
  mutate(winpercent=(wins/38))

tables








