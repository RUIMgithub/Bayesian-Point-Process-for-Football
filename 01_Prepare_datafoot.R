library(readxl)
library(dplyr)
library(stringr)
library(purrr)


# -------------------------------------------------
# 1. Read Excel file

matches_raw <- read_excel("League_1_pt.xlsx")

# -------------------------------------------------
# 2. Create team indices

teams <- sort(unique(c(matches_raw$Home, matches_raw$Away)))
team_index <- setNames(seq_along(teams), teams)

matches_raw$index_home <- team_index[matches_raw$Home]
matches_raw$index_away <- team_index[matches_raw$Away]

# -------------------------------------------------
# 3. Parse INC column

parse_inc <- function(x){
  x <- gsub("\\[|\\]", "", x)
  x <- gsub('"', "", x)
  strsplit(x, ",\\s*")[[1]]
}

events <- lapply(matches_raw$INC, parse_inc)

# -------------------------------------------------
# 4. Convert time strings to minutes

convert_time <- function(t){

  if(is.na(t)) return(NA)

  if(str_detect(t,"\\+")){
    p <- str_split(t,"\\+")[[1]]
    as.numeric(p[1]) + as.numeric(p[2])
  } else {
    as.numeric(t)
  }

}

# -------------------------------------------------
# 5. Extract goal times (including own goals)

get_goal_times <- function(events, side){

  events <- events[!is.na(events)]
  goal_events <- events[grepl("Goal_", events) | grepl("Own_", events)]
  if(length(goal_events) == 0) return(numeric(0))
  times <- c()
  for(e in goal_events){
    minute <- stringr::str_extract(e, "^[0-9]+(\\+[0-9]+)?")
    if(is.na(minute)) next
    minute <- convert_time(minute)
    if(side == "Home"){
      if(grepl("Goal_Home", e) | grepl("Own_Away", e))
        times <- c(times, minute)
    }
    if(side == "Away"){
      if(grepl("Goal_Away", e) | grepl("Own_Home", e))
        times <- c(times, minute)
    }
  }
  times
}

# -------------------------------------------------
# 6. Extract red card intervals

get_red_intervals <- function(events, side, l=15){

  events <- events[!is.na(events)]

  red <- events[str_detect(events, paste0("Red_Card_", side))]

  vec <- rep(0, l)

  if(length(red) == 0) return(vec)

  times <- str_extract(red, "^[0-9]+(\\+[0-9]+)?")
  times <- sapply(times, convert_time)

  # remove problematic times
  times <- times[!is.na(times)]

  if(length(times) == 0) return(vec)

  # collapse stoppage time
  times[times > 90] <- 90

  tau <- seq(0, 90, length.out = l + 1)

  idx <- findInterval(times, tau)

  idx <- idx[idx >= 1 & idx <= l]

  if(length(idx) > 0){
    vec[min(idx):l] <- 1
  }

  vec
}

# -------------------------------------------------
# 7. Build red card matrices

df_home <- t(sapply(events, get_red_intervals, side="Home"))
df_away <- t(sapply(events, get_red_intervals, side="Away"))

df_home <- as.matrix(df_home)
df_away <- as.matrix(df_away)


# -------------------------------------------------
# 8. Build data_foot dataframe

data_foot <- data.frame(
  index_home = matches_raw$index_home,
  index_away = matches_raw$index_away,
  ind_home = 1,
  ind_away = 1,
  Date = matches_raw$Date
)

# goal times
data_foot$time_home <- map(events, get_goal_times, side="Home")
data_foot$time_away <- map(events, get_goal_times, side="Away")

# replace NULL
data_foot$time_home <- lapply(data_foot$time_home,
                              function(x) if(is.null(x)) numeric(0) else x)

data_foot$time_away <- lapply(data_foot$time_away,
                              function(x) if(is.null(x)) numeric(0) else x)

# goals
data_foot$res_home <- sapply(data_foot$time_home, length)
data_foot$res_away <- sapply(data_foot$time_away, length)

data_foot$res <- paste0(data_foot$res_home,"-",data_foot$res_away)

# team names
data_foot$team_home <- names(team_index)[data_foot$index_home]
data_foot$team_away <- names(team_index)[data_foot$index_away]

# red cards
data_foot$red_home <- split(df_home, row(df_home))
data_foot$red_away <- split(df_away, row(df_away))


# -------------------------------------------------
# 9. Save dataset

saveRDS(data_foot, "data_foot.rds")



