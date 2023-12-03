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

big5_player_standard <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/standard.csv")
big5_player_shooting <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/shooting.csv")
big5_player_gca <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/gca.csv")
big5_player_passing <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/passing.csv")
big5_player_possession <- read.csv("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/possession.csv")

# mapped_players <- player_dictionary_mapping()

# bio <- tm_player_bio(player_url = "https://www.transfermarkt.com/ollie-watkins/profil/spieler/324358")

fun1 <- function(name_input) {
# standard <- fb_player_season_stats(fbref, stat_type = 'standard', time_pause = runif(1, 0, 1.5))
# shooting <- fb_player_season_stats(fbref, stat_type = 'shooting', time_pause = runif(1, 0, 1.5))
# gca <- fb_player_season_stats(fbref, stat_type = 'gca', time_pause = runif(1, 0, 1.5))
# passing <- fb_player_season_stats(fbref, stat_type = 'passing', time_pause = runif(1, 0, 1.5))
# possession <- fb_player_season_stats(fbref, stat_type = 'possession', time_pause = runif(1, 0, 1.5))

standard <- big5_player_standard[which(big5_player_standard$Player == name_input), ]
shooting <- big5_player_shooting[which(big5_player_shooting$Player == name_input), ]
gca <- big5_player_gca[which(big5_player_gca$Player == name_input), ]
passing <- big5_player_passing[which(big5_player_passing$Player == name_input), ]
possession <- big5_player_possession[which(big5_player_possession$Player == name_input), ]

if (dim(standard)[1] == 0) {return("Please write your name correctly or pick a person from the Big 5 league during 22/23 season")}

# Fill na as 0
standard[is.na(standard)] <- 0
shooting[is.na(shooting)] <- 0
gca[is.na(gca)] <- 0
passing[is.na(passing)] <- 0
possession[is.na(possession)] <- 0

total_goals <- as.numeric(standard[, "Gls"])
total_assists <- as.numeric(standard[, "Ast"])
total_goals_pens <- as.numeric(standard[, "PK"])
total_xg <- as.numeric(standard[, "xG_Expected"])
total_xa <- as.numeric(standard[, "xAG_Expected"])
total_npxg <- as.numeric(standard[, "npxG_Expected"])
total_mins_per_90 <- as.numeric(standard[, "Mins_Per_90_Playing"])

total_sca <- as.numeric(gca[, "SCA_SCA"])
total_gca <- as.numeric(gca[, "GCA_GCA"])

total_shots <- as.numeric(shooting[, "Sh_Standard"])
total_shots_target <- as.numeric(shooting[, "SoT_Standard"])

passes_completed <- as.numeric(passing[, "Cmp_Total"])
passes_attempted <- as.numeric(passing[, "Att_Total"])

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
                        age = as.numeric(standard[, "Age"]),
                        # foot = factor(bio$foot, levels = c("", "both", "left", "right")),
                        foot = factor(vals[which(vals$name == name_input), "foot"], levels = c("", "both", "left", "right")),
                        # height = bio$height * 100,
                        height = as.numeric(vals[which(vals$name == name_input), "height_in_cm"]),
                        minutes = as.numeric(standard[, "Min_Playing"]),
                        games = as.numeric(standard[, "MP_Playing"]),
                        games_starts = as.numeric(standard[, "Starts_Playing"]),
                        passes_pct = (passes_completed/passes_attempted) * 100,
                        touches_att_3rd = as.numeric(possession[, "Att.3rd_Touches"]),
                        value = as.numeric(vals[which(vals$name == name_input), "market_value_in_eur"])
                        )

if(any(is.na(test_data))) {return("Missing predictors detected. Please choose another player.")}

table_out1 <<- data.frame(Name = name_input,
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

return("Success!")
}
