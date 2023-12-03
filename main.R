library(worldfootballR)
library(DescTools)
library(tibble)
library(caret)
library(randomForest)
library(kernlab)
library(rpart)
library(neuralnet)

lin_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/Linear_Reg.rds")))
tree_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/Reg_Tree.rds")))
rf_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/Rand_For.rds")))
ann_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/ANN.rds")))
svm_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/SVM.rds")))
knn_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/KNN.rds")))
stacked_m <- readRDS(gzcon(url("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/Stacked.rds")))

vals <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/players.csv")

# mapped_players <- player_dictionary_mapping()

# bio <- tm_player_bio(player_url = "https://www.transfermarkt.com/ollie-watkins/profil/spieler/324358")

fun1 <- function(fbref) {
standard <- fb_player_season_stats(fbref, stat_type = 'standard', time_pause = 0)
shooting <- fb_player_season_stats(fbref, stat_type = 'shooting', time_pause = 0)
gca <- fb_player_season_stats(fbref, stat_type = 'gca', time_pause = 0)
passing <- fb_player_season_stats(fbref, stat_type = 'passing', time_pause = 0)
possession <- fb_player_season_stats(fbref, stat_type = 'possession', time_pause = 0)

# Fill na as 0
standard[is.na(standard)] <- 0
shooting[is.na(shooting)] <- 0
gca[is.na(gca)] <- 0
passing[is.na(passing)] <- 0
possession[is.na(possession)] <- 0

total_goals <- sum(standard[which(standard$Season == "2022-2023"), "Gls"])
total_assists <- sum(standard[which(standard$Season == "2022-2023"), "Ast"])
total_goals_pens <- sum(standard[which(standard$Season == "2022-2023"), "PK"])
total_xg <- sum(standard[which(standard$Season == "2022-2023"), "xG_Expected"])
total_xa <- sum(standard[which(standard$Season == "2022-2023"), "xAG_Expected"])
total_npxg <- sum(standard[which(standard$Season == "2022-2023"), "npxG_Expected"])
total_mins_per_90 <- sum(standard[which(standard$Season == "2022-2023"), "Mins_Per_90_Time"])

total_sca <- sum(gca[which(gca$Season == "2022-2023"), "SCA_SCA"])
total_gca <- sum(gca[which(gca$Season == "2022-2023"), "GCA_GCA"])

total_shots <- sum(shooting[which(shooting$Season == "2022-2023"), "Sh_Standard"])
total_shots_target <- sum(shooting[which(shooting$Season == "2022-2023"), "SoT_Standard"])

passes_completed <- sum(passing[which(passing$Season == "2022-2023"), "Cmp_Total"])
passes_attempted <- sum(passing[which(passing$Season == "2022-2023"), "Att_Total"])

test_data <- data.frame(goals_per90 = total_goals/total_mins_per_90,
                        assists_per90 = total_assists/total_mins_per_90,
                        goals_pens_per90 = total_goals_pens/total_mins_per_90,
                        xg_per90 = total_xg/total_mins_per_90,
                        xa_per90 = total_xa/total_mins_per_90,
                        sca_per90 = total_sca/total_mins_per_90,
                        npxg_per90 = total_npxg/total_mins_per_90,
                        shots_total_per90 = total_shots/total_mins_per_90,
                        shots_on_target_per90 = total_shots_target/total_mins_per_90,
                        gca_per90 = total_gca/total_mins_per_90,
                        # age = as.numeric(bio$age[1]),
                        age = standard[which(standard$Season == "2022-2023"), "Age"][1],
                        # foot = factor(bio$foot, levels = c("", "both", "left", "right")),
                        foot = factor(vals[which(vals$name == standard$player_name[1]), "foot"], levels = c("", "both", "left", "right")),
                        # height = bio$height * 100,
                        height = as.numeric(vals[which(vals$name == standard$player_name[1]), "height_in_cm"]),
                        minutes = sum(standard[which(standard$Season == "2022-2023"), "Min_Time"]),
                        games = sum(standard[which(standard$Season == "2022-2023"), "MP"]),
                        games_starts = sum(standard[which(standard$Season == "2022-2023"), "Starts_Time"]),
                        passes_pct = (passes_completed/passes_attempted) * 100,
                        touches_att_3rd = sum(possession[which(possession$Season == "2022-2023"), "Att 3rd_Touches"]),
                        value = as.numeric(vals[which(vals$name == standard$player_name[1]), "market_value_in_eur"])
                        )

if(any(is.na(test_data))) {stop("Missing predictors detected. Please choose another player.")}

table_out1 <<- data.frame(Name = standard$player_name[1],
                         Age = test_data$age[1],
                         Height_in_cm = test_data$height[1],
                         Foot = test_data$foot[1],
                         Goals_Per_90 = test_data$goals_per90[1],
                         Assists_Per_90 = test_data$assists_per90[1],
                         Penalty_Goals_Per_90 = test_data$goals_pens_per90[1],
                         Expected_Goals_Per_90 = test_data$xg_per90[1],
                         Expected_Assists_Per_90 = test_data$xa_per90[1],
                         Shot_Creating_Action_Per_90 = test_data$sca_per90[1]
)

table_out2 <<- data.frame(Non_Penalty_Expected_Goals_Per_90 = test_data$npxg_per90[1],
                         Shots_Per_90 = test_data$shots_total_per90[1],
                         Shots_On_Target_Per_90 = test_data$shots_on_target_per90[1],
                         Goal_Creating_Action_Per_90 = test_data$gca_per90[1],
                         Minutes_Played = test_data$minutes[1],
                         Games_Played = test_data$games[1],
                         Games_Starts = test_data$games_starts[1],
                         Pass_Completion_Percentage = test_data$passes_pct[1],
                         Touches_Attacking_3rd = test_data$touches_att_3rd[1]
)

testmm <- test_data
testmm <- add_column(testmm, foot4 = 0, .after = "foot")
testmm <- add_column(testmm, foot3 = 0, .after = "foot")
testmm <- add_column(testmm, foot2 = 0, .after = "foot")

if(testmm[1, "foot"] == "both") {
  test_data$foot <- factor(2, levels = c("2", "3", "4"))
  testmm$foot2 <- 1
} else if(testmm[1, "foot"] == "left") {
  test_data$foot <- factor(3, levels = c("2", "3", "4"))
  testmm$foot3 <- 1
} else {
  test_data$foot <- factor(4, levels = c("2", "3", "4"))
  testmm$foot4 <- 1
}

testmm$foot <- NULL

preProc <- readRDS("PreProc.rds")

test_norm <- predict(preProc, testmm)

lin_p <- predict(lin_m, test_data)
tree_p <- predict(tree_m, test_data)
rf_p <- predict(rf_m, test_data)

ann_p <- predict(ann_m, test_norm)
svm_p <- predict(svm_m, test_norm)
knn_p <- predict(knn_m, test_norm)

stacked <- data.frame(lin = lin_p, tree = tree_p, rf = rf_p, ann = ann_p, svm = svm_p, knn = knn_p, value = test_data$value)

stacked_p <- predict(stacked_m, stacked)

options(scipen = 100, digits = 4)

pred <<- as.numeric(stacked_p)
given <<- stacked$value
}
