library(XML)
library(RCurl)

# Extracts the sub-table of a player table, corresponding to the give team number.
get_player_sub_table <- function(table, team_no) {
  # Find the row at the start of the second team.
  row_num <- -1
  for (i in seq(1, nrow(table))) {
    val <- table[i, 1]
    if (toupper(val) == val) {  # The row is the first one that contains all caps.
      row_num <- i
      break
    }
  }
  
  # Extract a sub-table finishing or starting with the found row.
  if (team_no == 0) {
    sub_table <- table[1:(row_num-1), ]
  }
  else {
    sub_table <- table[(row_num+1):nrow(table), ]
  }
  return(sub_table)
}

# Gets the team data for either the whole match, 1st half, or 2nd half.
get_player_data <- function(match_tables, period, team_no) {
  
  # Table 10: Stand out players.
  # 11: Summary total. Both teams.
  # 12: Points total. Both teams.
  # 13: Runs total. Both teams.
  # 14: Tackles total. Both teams.
  # 15: Kicks total. Both teams.
  # 16: Summary 1H
  # 17: Points 1H.
  # 18: Runs 1H.
  # 19: Tackles 1h.
  # 20: Kicks 1H.
  # 21: Summary 2H
  # 22: Points 2H.
  # 23: Runs 2H.
  # 24: Tackles 2h.
  # 25: Kicks 2H.
  
  # 26: Team possession percent.
  # 27, 28: Legend for the match flow. Not useful.
  
  if (period == 0) {
    # Whole match.
    summary_table_num <- 11
    points_table_num <- 12
    runs_table_num <- 13
    tackles_table_num <- 14
    kicks_table_num <- 15
    period <- "match"
  }
  else if (period == 1) {
    # 1st half.
    summary_table_num <- 16
    points_table_num <- 17
    runs_table_num <- 18
    tackles_table_num <- 19
    kicks_table_num <- 20
    period <- "1st_half"
  }
  else if (period == 2) {
    # 2nd half.
    summary_table_num <- 21
    points_table_num <- 22
    runs_table_num <- 23
    tackles_table_num <- 24
    kicks_table_num <- 25
    period <- "2nd_half"
  }
  else {
    return()
  }
  
  if (team_no == 1) {
    team_col_no = 1
  }
  else if (team_no == 2) {
    team_col_no = 3
  }
  
  # Get the summary table just so we can get the number of players in the team.
  summary_table <- get_player_sub_table(match_tables[[summary_table_num]], team_no)
  
  n_rows <- nrow(summary_table)
  player_data <- data.frame(
    date=character(n_rows),
    time=character(n_rows),
    round=character(n_rows),
    team=character(n_rows),
    period=character(n_rows),
    name=character(n_rows),
    number=character(n_rows),
    position=character(n_rows),
    mins=numeric(n_rows),
    tries=numeric(n_rows),
    try_assists=numeric(n_rows),
    conversions=character(n_rows), # Numeric
    conversion_attempts=character(n_rows), # Numeric
    penalty_goals=numeric(n_rows),
    field_goals=numeric(n_rows),
    total_points=numeric(n_rows),
    receives=numeric(n_rows),
    total_runs=character(n_rows), # Change to numeric
    total_runs_metres=character(n_rows), # Change to numeric
    hitups=character(n_rows), # Change to numeric
    hitups_metres=character(n_rows), # Change to numeric
    runs=character(n_rows), # Change to numeric
    runs_metres=character(n_rows), # Change to numeric
    dummy_half_runs=character(n_rows), # Change to numeric
    dummy_half_runs_metres=character(n_rows), # Change to numeric
    first_receiver_runs=character(n_rows), # Change to numeric
    first_receiver_runs_metres=character(n_rows), # Change to numeric
    ruck_runs=character(n_rows), # Change to numeric
    ruck_runs_metres=character(n_rows), # Change to numeric
    kick_return_runs=character(n_rows), # Change to numeric
    kick_return_runs_metres=character(n_rows), # Change to numeric
    tackle_breaks=numeric(n_rows),
    tackles=numeric(n_rows),
    one_on_one_tackles=character(n_rows), # Change to numeric
    ineffective_tackles=character(n_rows), # Change to numeric
    missed_tackles=numeric(n_rows),
    offloads=numeric(n_rows),
    line_breaks=numeric(n_rows),
    line_break_assists=numeric(n_rows),
    errors=numeric(n_rows),
    penalties_conceded=numeric(n_rows),
    kicks=character(n_rows), # Make numeric
    kicks_metres=character(n_rows),
    forty_twenty_kicks=character(n_rows),
    stringsAsFactors=FALSE
  )
  
  summary_table <- get_player_sub_table(match_tables[[summary_table_num]], team_no)
  points_table <- get_player_sub_table(match_tables[[points_table_num]], team_no)
  runs_table <- get_player_sub_table(match_tables[[runs_table_num]], team_no)
  tackles_table <- get_player_sub_table(match_tables[[tackles_table_num]], team_no)
  kicks_table <- get_player_sub_table(match_tables[[kicks_table_num]], team_no)
  for (i in seq(1:nrow(summary_table))) {
    player_data$name[i] <- summary_table[i, 1]
    player_data$number[i] <- summary_table[i, 1]
    player_data$position[i] <- summary_table[i, 2]
    player_data$mins[i] <- summary_table[i, 3]
    player_data$tries[i] <- points_table[i, 4]
    player_data$try_assists[i] <- summary_table[i, 6]
    player_data$conversions[i] <- points_table[i, 5]
    player_data$conversion_attempts[i] <- points_table[i, 5]
    player_data$penalty_goals[i] <- points_table[i, 6]
    player_data$field_goals[i] <- points_table[i, 7]
    player_data$total_points[i] <- points_table[i, 8]
    player_data$receives[i] <- summary_table[i, 4]
    player_data$total_runs[i] <- runs_table[i, 4]
    player_data$total_runs_metres[i] <- runs_table[i, 4]
    player_data$hitups[i] <- runs_table[i, 5]
    player_data$hitups_metres[i] <- runs_table[i, 5]
    player_data$runs[i] <- runs_table[i, 6]
    player_data$runs_metres[i] <- runs_table[i, 6]
    player_data$dummy_half_runs[i] <- runs_table[i, 7]
    player_data$dummy_half_runs_metres[i] <- runs_table[i, 7]
    player_data$first_receiver_runs[i] <- runs_table[i, 8]
    player_data$first_receiver_runs_metres[i] <- runs_table[i, 8]
    player_data$ruck_runs[i] <- runs_table[i, 9]
    player_data$ruck_runs_metres[i] <- runs_table[i, 9]
    player_data$kick_return_runs[i] <- runs_table[i, 10]
    player_data$kick_return_runs_metres[i] <- runs_table[i, 10]
    player_data$tackle_breaks[i] <- summary_table[i, 7]
    player_data$tackles[i] <- tackles_table[i, 4]
    player_data$one_on_one_tackles[i] <- tackles_table[i, 5]
    player_data$ineffective_tackles[i] <- tackles_table[i, 6]
    player_data$missed_tackles[i] <- tackles_table[i, 7]
    player_data$offloads[i] <- summary_table[i, 9]
    player_data$line_breaks[i] <- summary_table[i, 10]
    player_data$line_break_assists[i] <- summary_table[i, 11]
    player_data$errors[i] <- summary_table[i, 12]
    player_data$penalties_conceded[i] <- summary_table[i, 13]
    player_data$kicks[i] <- kicks_table[i, 4]
    player_data$kicks_metres[i] <- kicks_table[i, 5]
    player_data$forty_twenty_kicks[i] <- kicks_table[i, 6]
  }
  
  return(player_data)
}

# Gets the team data for either the whole match, 1st half, or 2nd half.
get_team_data <- function(match_tables, period, team_no) {
  
  if (period == 0) {
    # Whole match.
    summary_table_num <- 4
    team_stats_table_num <- 7
    period <- "match"
  }
  else if (period == 1) {
    # 1st half.
    summary_table_num <- 5
    team_stats_table_num <- 8
    period <- "1st_half"
  }
  else if (period == 2) {
    # 2nd half.
    summary_table_num <- 6
    team_stats_table_num <- 9
    period <- "2nd_half"
  }
  else {
    return()
  }
  
  if (team_no == 1) {
    team_col_no = 1
  }
  else if (team_no == 2) {
    team_col_no = 3
  }
  n_rows <- 1
  team_data <- data.frame(
    date=character(n_rows),
    time=character(n_rows),
    round=character(n_rows),
    team=character(n_rows),
    period=character(n_rows),
    score=numeric(n_rows),
    tries=character(n_rows), # Change to numeric
    try_assists=numeric(n_rows),
    conversions=character(n_rows), # Change to numeric
    penalty_goals=character(n_rows), # Change to numeric
    field_goals=character(n_rows), # Change to numeric
    sin_bins=character(n_rows), # Change to numeric
    send_offs=character(n_rows), # Change to numeric
    penalties=character(n_rows), # Change to numeric
    scrums=character(n_rows), # Change to numeric
    sets=character(n_rows), # Change to numeric
    completions=character(n_rows), # Change to numeric
    total_runs=character(n_rows), # Change to numeric
    total_runs_metres=character(n_rows), # Change to numeric
    hitups=character(n_rows), # Change to numeric
    hitups_metres=character(n_rows), # Change to numeric
    runs=character(n_rows), # Change to numeric
    runs_metres=character(n_rows), # Change to numeric
    dummy_half_runs=character(n_rows), # Change to numeric
    dummy_half_runs_metres=character(n_rows), # Change to numeric
    first_receiver_runs=character(n_rows), # Change to numeric
    first_receiver_runs_metres=character(n_rows), # Change to numeric
    ruck_runs=character(n_rows), # Change to numeric
    ruck_runs_metres=character(n_rows), # Change to numeric
    kick_return_runs=character(n_rows), # Change to numeric
    kick_return_runs_metres=character(n_rows), # Change to numeric
    total_runs_in_general_play=character(n_rows), # Change to numeric
    total_runs_in_general_play_metres=character(n_rows), # Change to numeric
    line_breaks=numeric(n_rows),
    line_break_assists=character(n_rows), # Change to numeric
    tackle_breaks=numeric(n_rows),
    receives=numeric(n_rows),
    tackles=numeric(n_rows),
    one_on_one_tackles=character(n_rows), # Change to numeric
    ineffective_tackles=character(n_rows), # Change to numeric
    missed_tackles=numeric(n_rows),
    offloads=numeric(n_rows),
    errors=numeric(n_rows),
    kicks=character(n_rows), # Make numeric
    kicks_metres=character(n_rows),
    forty_twenty_kicks=character(n_rows),
    line_dropouts=character(n_rows),
    play_the_balls_total=character(n_rows),
    play_the_balls_slow=character(n_rows),
    play_the_balls_neutral=character(n_rows),
    play_the_balls_fast=character(n_rows),
    possession_percent=character(n_rows),
    used_plays=character(n_rows),
    unused_plays=character(n_rows),
    possession_efficiency_percent=character(n_rows),
    stringsAsFactors=FALSE
  )
  
  # This data is in table 3, but seems to be missing from the table. Its probably considered heading.
  #team_data$date[n_rows] <- colcolnames(names(match_tables[[2]])[team_col_no]
  #team_data$time[n_rows] <- colnames(match_tables[[2]])[team_col_no]
  #team_data$round[n_rows] <- colnames(match_tables[[2]])[team_col_no]
  team_data$period[n_rows] <- period
  team_data$team[n_rows] <- colnames(match_tables[[2]])[team_col_no]
  
  # Overall team stats.
  table_num <- summary_table_num
  team_data$score[n_rows] <- match_tables[[table_num]][1, team_col_no]
  team_data$tries[n_rows] <- match_tables[[table_num]][2, team_col_no]
  team_data$conversions[n_rows] <- match_tables[[table_num]][3, team_col_no]
  team_data$penalty_goals[n_rows] <- match_tables[[table_num]][4, team_col_no]
  team_data$field_goals[n_rows] <- match_tables[[table_num]][5, team_col_no]
  team_data$sin_bins[n_rows] <- match_tables[[table_num]][6, team_col_no]
  team_data$send_offs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$penalties[n_rows] <- match_tables[[table_num]][8, team_col_no]
  team_data$scrums[n_rows] <- match_tables[[table_num]][9, team_col_no]
  team_data$sets[n_rows] <- match_tables[[table_num]][10, team_col_no]
  team_data$completions[n_rows] <- match_tables[[table_num]][10, team_col_no] # Extract from sets
  team_data$total_runs[n_rows] <- match_tables[[table_num]][11, team_col_no]
  team_data$total_runs_metres[n_rows] <- match_tables[[table_num]][11, team_col_no] # Extract from runs
  team_data$line_breaks[n_rows] <- match_tables[[table_num]][12, team_col_no]
  team_data$tackles[n_rows] <- match_tables[[table_num]][13, team_col_no]
  team_data$missed_tackles[n_rows] <- match_tables[[table_num]][14, team_col_no]
  team_data$offloads[n_rows] <- match_tables[[table_num]][15, team_col_no]
  team_data$errors[n_rows] <- match_tables[[table_num]][16, team_col_no]
  table_num <- team_stats_table_num
  team_data$try_assists[n_rows] <- match_tables[[table_num]][1, team_col_no]
  team_data$line_break_assists[n_rows] <- match_tables[[table_num]][3, team_col_no] # Extract LBA
  team_data$receives[n_rows] <- match_tables[[table_num]][4, team_col_no]
  team_data$one_on_one_tackles[n_rows] <- match_tables[[table_num]][5, team_col_no] # Extract
  team_data$ineffective_tackles[n_rows] <- match_tables[[table_num]][5, team_col_no] # Extract
  team_data$tackle_breaks[n_rows] <- match_tables[[table_num]][6, team_col_no]
  team_data$hitups[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$hitups_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$runs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$runs_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$dummy_half_runs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$dummy_half_runs_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$first_receiver_runs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$first_receiver_runs_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$ruck_runs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$ruck_runs_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$kick_return_runs[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$kick_return_runs_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$total_runs_in_general_play[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$total_runs_in_general_play_metres[n_rows] <- match_tables[[table_num]][7, team_col_no]
  team_data$kicks[n_rows] <- match_tables[[table_num]][8, team_col_no]
  team_data$kicks_metres[n_rows] <- match_tables[[table_num]][8, team_col_no]
  team_data$forty_twenty_kicks[n_rows] <- match_tables[[table_num]][8, team_col_no]
  team_data$line_dropouts[n_rows] <- match_tables[[table_num]][8, team_col_no]
  team_data$play_the_balls_total[n_rows] <- match_tables[[table_num]][9, team_col_no]
  team_data$play_the_balls_slow[n_rows] <- match_tables[[table_num]][9, team_col_no]
  team_data$play_the_balls_neutral[n_rows] <- match_tables[[table_num]][9, team_col_no]
  team_data$play_the_balls_fast[n_rows] <- match_tables[[table_num]][9, team_col_no]
  team_data$possession_percent[n_rows] <- match_tables[[table_num]][10, team_col_no]
  team_data$used_plays[n_rows] <- match_tables[[table_num]][10, team_col_no]
  team_data$unused_plays[n_rows] <- match_tables[[table_num]][10, team_col_no]
  team_data$possession_efficiency_percent[n_rows] <- match_tables[[table_num]][10, team_col_no]
  
  return(team_data)
}

get_match_data <- function(match_tables) {
  n_rows <- 1
  match_data <- data.frame(
    date=character(n_rows),
    time=character(n_rows),
    round=character(n_rows),
    team_1=character(n_rows),
    team_2=character(n_rows),
    team_score_1=numeric(n_rows),
    team_score_2=numeric(n_rows),
    venue=character(n_rows),
    weather=character(n_rows),
    referees=character(n_rows),
    crowd=character(n_rows), # Change to numeric
    surface=character(n_rows),
    stringsAsFactors=FALSE
    )
  
  # This data is in table 3, but seems to be missing from the table. Its probably considered heading.
  #match_data$date[n_rows] <- colnames(names(match_tables[[2]])[1]
  #match_data$time[n_rows] <- colnames(match_tables[[2]])[1]
  #match_data$round[n_rows] <- colnames(match_tables[[2]])[1]
  match_data$team_1[n_rows] <- colnames(match_tables[[2]])[1]
  match_data$team_2[n_rows] <- colnames(match_tables[[2]])[3]
  match_data$team_score_1[n_rows] <- match_tables[[4]][1, 1]
  match_data$team_score_2[n_rows] <- match_tables[[4]][1, 3]
  match_data$venue[n_rows] <- match_tables[[3]][1, 1]
  match_data$weather[n_rows] <- match_tables[[3]][1, 2]
  match_data$referees[n_rows] <- match_tables[[3]][2, 1]
  match_data$crowd[n_rows] <- match_tables[[3]][6, 1]
  match_data$surface[n_rows] <- match_tables[[3]][1, 3]
  
  return(match_data)
}

# Process the page of an individual match.
process_match_page <- function(match_url) {
  print(match_url)
  
  # Extract the tables on the match page.
  #match_html <- htmlParse(match_url)
  #match_html<-gsub("<br>", " | ", match_html)
  
  match_html <- getForm(match_url, ajax="false")
  # Replace the <br> tags with something that is retained and we can split on.
  match_html <- gsub("<br>", " | ", match_html)
  match_html <- gsub("<br />", " | ", match_html)
  write(match_html, file="match.html")
  #saveXML(match_html, file="match.html")
  match_tables <- readHTMLTable(match_html, stringsAsFactors = FALSE, asText=TRUE)
  
  # Loop through the match tables.
  # The first table is bogus.
  for (i in seq(2,length(match_tables))) {
    match_table <- match_tables[[i]]
    # Write the table out to a CSV file.
    write.csv(match_table, paste("match-table_", sprintf("%02d",i), ".csv", sep=""))
  }
  
  # Get the overall match data.
  match_data <- get_match_data(match_tables)
  
  # Get the team data for each period.
  team_data_1_match <- get_team_data(match_tables, 0, 1)
  team_data_1_1h <- get_team_data(match_tables, 1, 1)
  team_data_1_2h <- get_team_data(match_tables, 2, 1)
  team_data_2_match <- get_team_data(match_tables, 0, 2)
  team_data_2_1h <- get_team_data(match_tables, 1, 2)
  team_data_2_2h <- get_team_data(match_tables, 2, 2)
  # Merge the team data for the different periods.
  team_data_1 <- team_data_1_match
  team_data_1 <- rbind(team_data_1, team_data_1_1h)
  team_data_1 <- rbind(team_data_1, team_data_1_2h)
  team_data_2 <- team_data_2_match
  team_data_2 <- rbind(team_data_2, team_data_2_1h)
  team_data_2 <- rbind(team_data_2, team_data_2_2h)
  
  # Get the player data for each period.
  player_data_1_match <- get_player_data(match_tables, 0, 0)
  player_data_1_1h <- get_player_data(match_tables, 1, 1)
  player_data_1_2h <- get_player_data(match_tables, 2, 1)
  player_data_2_match <- get_player_data(match_tables, 0, 2)
  player_data_2_1h <- get_player_data(match_tables, 1, 2)
  player_data_2_2h <- get_player_data(match_tables, 2, 2)
  # Merge the player data for the different periods.
  player_data_1 <- rbind(player_data_1_match, player_data_1_1h)
  player_data_1 <- rbind(player_data_1, player_data_1_2h)
  player_data_2 <- rbind(player_data_2_match, player_data_2_1h)
  player_data_2 <- rbind(player_data_2, player_data_2_2h)
  player_data <- rbind(player_data_1, player_data_2)
  write.csv(player_data, paste("player-data-table.csv", sep=""))
  #match_team_data <- get_match_team_data(match_tables)
  return()
  
  for (i in seq(1,length(match_team_data))) {
    match_team_data_table <- match_team_data[[i]]
    # Write the table out to a CSV file.
    write.csv(match_team_data_table, paste("match-team-data-table_", sprintf("%02d",i), ".csv", sep=""))
  }
  
  return()
}

# Process one of the tables on the main season page that contains the matches in
# an individual round.
process_round_table <- function(round_df, round_links_df, base_url) {
  for (row_num in seq(1, nrow(round_df))) {
    # For each row (match), build a data frame that contains all the information
    # we want to pass on to the next function.
    
    
    match_url <- round_links_df[row_num, "Match"]
    match_url <- paste(base_url, match_url, sep="")
    process_match_page(match_url)
    
    return()
  }

}

hrefFun <- function(x){
  xpathSApply(x,'./a',xmlAttrs)  
}

base_url <- "http://live.nrlstats.com"
season_url <- paste(base_url, "/nrl/season2015.html", sep="")
print(season_url)
season_html <- htmlParse(season_url)

# Get the round tables and the hyperlinks.
round_tables <- readHTMLTable(season_html)
round_tables_links <- readHTMLTable(season_html, elFun = hrefFun, stringsAsFactors = FALSE)
# Remove the first table, which is bogus.
round_tables[[1]] <- NULL
round_tables_links[[1]] <- NULL

# Loop through the round tables.
for (i in seq(1,length(round_tables))) {
  round_table <- round_tables[[i]]
  round_table_links <- round_tables_links[[i]]
  # Write the table out to a CSV file.
  write.csv(round_table, paste("round-table_", sprintf("%02d",i), ".csv", sep=""))
  # Process the round table.
  process_round_table(round_table, round_table_links, base_url)
  
  return()
}