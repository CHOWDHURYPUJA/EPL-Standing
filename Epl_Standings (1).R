library(dplyr)
library(lubridate)

################################################################################

Date_Formating_for_argument = "%m/%d/%Y"
Argument_Date_formating = function(givenDate) as.Date(givenDate, format=Date_Formating_for_argument)

Format_data_Date = "%d/%m/%Y"
formatDataDate = function(givenDate) {
  formattedDate = as.Date(givenDate, format = Format_data_Date)
  formattedDate = FourDigitYear(formattedDate)
  formattedDate
}

sub_string_Right = function(x, n) substr(x, nchar(x)-n+1, nchar(x))
last_two_season = function (x) sub_string_Right(x,2)
joing_variables = function (x,y) paste(x,y,sep="")

# computes the string needed to create the URL to load data
given_season_string = function(season) {
  seasons = unlist(strsplit(season,"/"))
  season_url = Reduce(joing_variables, Map(last_two_season, seasons))
}

epl_data_for_season <- function (season){
  data_file_url = paste("http://www.football-data.co.uk/mmz4281/",given_season_string(season),"/E0.csv", sep="")
  read.csv(data_file_url)
}

# converts the year to a 4 digit year threshold is 1990 since the oldest data available is from 1993/94
FourDigitYear <- function(given_date, threshold=1990){
  years_after_cent <- year(given_date) %% 100
  year(given_date) <- ifelse(years_after_cent > (threshold %% 100), 1900+years_after_cent, 2000+years_after_cent)
  given_date
}

# keeps only relevant columns from the raw data
required_columns_rows =
  function(epl_game_data, given_date) subset(epl_game_data, Date <= given_date, select = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR"))

#takes game data, and summary(matches played, goals scored, goals allowed) and compiles records and points
record_compile_method <- function(played_gs_ga, games) {
  game_wins = games %>% filter(Result=="W") %>% group_by(TeamName) %>% summarise(game_wins = n())
  game_losses = games %>% filter(Result=="L") %>% group_by(TeamName) %>% summarise(game_losses = n())
  game_ties = games %>% filter(Result=="T") %>% group_by(TeamName) %>% summarise(game_ties = n())
  game_playing_records = full_join(game_wins, game_losses, by="TeamName") %>%
    full_join(game_ties, by="TeamName") %>%
    full_join(played_gs_ga, by="TeamName")
  game_playing_records[is.na(game_playing_records)] = 0
  game_playing_records$points = game_playing_records$game_wins*3+game_playing_records$game_ties
  game_playing_records$record = paste(game_playing_records$game_wins, game_playing_records$game_losses, game_playing_records$game_ties, sep = "-")
  game_playing_records
}


# Takes the record string (ex. WWWL) as input and returns streak (W3)
streak = function(record){
  chars <- unlist(strsplit(record, ""))
  streak_length = 1
  record_without_first_character <- chars[-1]
  first_character <- chars[1]
  for (c in record_without_first_character){
    if(c==first_character)
      streak_length = streak_length+1
    else
      break
  }
  Streak = paste(chars[1], streak_length, sep = "", collapse = "")
  Streak
}

#loads epl data for and season, checks that the given date is after the season start date and returns
# data from the season only upto the given date
get_relevant_data <- function(given_date, season) {
  given_date = Argument_Date_formating(given_date)
  epl_game_data = epl_data_for_season(season)
  epl_game_data$Date = formatDataDate(epl_game_data$Date)
  if(given_date < min(epl_game_data$Date))
    stop("Please enter a date later than the season start date")
  epl_game_data = required_columns_rows(epl_game_data, given_date)
  epl_game_data
}

playing_game_data <- function(data, columnName, game_type) {
  games = data.frame(data)
  games = games %>%
    rename(TeamName = columnName) %>%
    select(Date, TeamName, FTHG, FTAG, FTR)
  if(dim(games)[1]>0){
    games = games %>%
      mutate(Result = ifelse(FTR=="D", "T",ifelse (FTR==game_type, "W" , "L" )))
  }
  games
}

#creates a copy of the game_data and computes wins-loss-tie Result for Away Team
away_game_data = function(data) playing_game_data(data,"AwayTeam",'A')

#creates a copy of the game_data and computes wins-loss-tie Result for Home Team
home_game_data = function(data) playing_game_data(data,"HomeTeam",'H')

clean_up <- function(all_games_played_records) {
  all_games_played_records[is.na(all_games_played_records)] = 0
  all_games_played_records[all_games_played_records$record.home == 0, "record.home"]= "0-0-0"
  all_games_played_records[all_games_played_records$record.away == 0, "record.away"]= "0-0-0"
  all_games_played_records
}





################################################################################

#return standings for the specified season at the conclusion of all matches played until the given date
EPL_Standings = function(standings_as_on, season){
	epl_game_data = get_relevant_data(standings_as_on, season)

	games_played_at_home = home_game_data(epl_game_data)
	games_played_away = away_game_data(epl_game_data)

	games_played_home_records = games_played_at_home %>%
		group_by(TeamName) %>%
		summarise(matches = n(), GS = sum(FTHG), GA = sum(FTAG)) %>%
		record_compile_method(games_played_at_home)

	games_played_away_records = games_played_away %>%
		group_by(TeamName) %>%
		summarise(matches = n(), GS = sum(FTAG), GA = sum(FTHG)) %>%
		record_compile_method(games_played_away)

	all_games_played_records = full_join(games_played_home_records, games_played_away_records, by = "TeamName", suffix=c(".home",".away")) %>% clean_up

	played_all_games_records <- rbind(games_played_at_home, games_played_away) %>%
		group_by(TeamName)

	#Compute win/loss/ties Streak
	played_games_win_loss_ties_streak = played_all_games_records	%>%
		arrange(desc(Date), TeamName) %>%
		summarise(record = paste(Result, sep="", collapse = "")) %>%
		mutate(Streak = sapply(record, streak)) %>%
		select(TeamName, Streak)

	#Compute the record for last 10 gamges
	played_last_ten_games_record = played_all_games_records %>%
		top_n(10, Date) %>%
		#arrange(TeamName, Date) %>%
		group_by(TeamName) %>%
		summarise(game_wins = sum(Result=="W") , game_losses = sum(Result=="L"), game_ties = sum(Result=="T")) %>%
		mutate(Last10 = paste(game_wins,game_losses,game_ties, sep="-")) %>%
		select(TeamName, Last10)

	#Compute the standings as of the given date
	team_standings = all_games_played_records %>%
		mutate(MatchesPlayed = (matches.home + matches.away),
			   Points = points.home + points.away,
			   PPM = Points/MatchesPlayed,
			   PtPct = Points/(3*MatchesPlayed),
			   game_wins = game_wins.home + game_wins.away,
			   game_losses = game_losses.home + game_losses.away,
			   game_ties = game_ties.home + game_ties.away,
			   HomeRec = record.home,
			   AwayRec = record.away,
			   Record = paste(game_wins, game_losses, game_ties, sep="-"),
			   GS = GS.home + GS.away,
			   GA = GA.home + GA.away,
			   GSM = GS/MatchesPlayed,
			   GAM = GA/MatchesPlayed
		) %>%
		arrange(desc(PPM), desc(game_wins), desc(GS) , GAM) %>%
		select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM) %>%
		full_join(played_last_ten_games_record, by = "TeamName") %>%
		full_join(played_games_win_loss_ties_streak, by = "TeamName")


	as.data.frame(team_standings)
}
#Run the code by commending EPL_Standings("10/30/2019","2018/19")

