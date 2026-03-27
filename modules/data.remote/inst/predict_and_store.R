##loops through all the parcel ids to make predictions for each from year 2024 - 2040 or something
##store the predictions in a new file, then graph a sample. 

setwd("/projectnb/dietzelab/ananyak")
library(ggplot2)
library(data.table)

seq_long = fread('seq_long.csv')
season_idx = 1
end_year = 2030

##read tmat file but make it numeric again and fix formatting
tmat_df = fread('full_transition_matrix.csv')
states = colnames(tmat_df)[-1]
tmat_final = as.matrix(tmat_df[, -1, with = FALSE])
rownames(tmat_final) = tmat_df$V1
colnames(tmat_final) = states
storage.mode(tmat_final) = "double"

#check
print(head(rownames(tmat_final)))
print(head(colnames(tmat_final)))

setDT(seq_long)

tmat_year = tmat_final 

start_info = seq_long[
  season == season_idx,
  .SD[which.max(year)],
  by = parcel_id
]

#clean classes and set in same order as transition matrix 
start_info[, CLASS := trimws(as.character(CLASS))]
states = rownames(tmat_final)

##all predictions 
all_preds = start_info[, {
  
  p = setNames(rep(0, length(states)), states)
  
  idx0 = match(CLASS, states)
  if (is.na(idx0)) return(NULL)
  
  p[idx0] = 1
  
  years = seq(year + 1, end_year)   # FIXED
  
  preds = character(length(years))
  probs = numeric(length(years))
  
  for (i in seq_along(years)) {
    p = as.numeric(p %*% tmat_year)
    
    idx = sample(seq_along(p), 1, prob = p)
    
    preds[i] = states[idx]
    probs[i] = p[idx]
  }
  
  .(
    season = season_idx,
    year = years,
    pred_class = preds,
    pred_prob = probs
  )
  
}, by = parcel_id]

#store actual classes from 2018-2023 
actual_hist = seq_long[
  season == season_idx,
  .(parcel_id, year, pred_class = NA, actual_class = CLASS)
]

#2024 to end year (right now is 2030)
preds_future = all_preds[, .(
  parcel_id,
  year,
  pred_class,
  actual_class = NA
)]

#comboine to plot both for comparisons 
plot_data = rbind(actual_hist, preds_future, fill = TRUE)

sample_pids = sample(unique(plot_data$parcel_id), 1)

plot_subset = plot_data[parcel_id %in% sample_pids]

plot_subset[, pred_class := factor(pred_class, levels = states)]
plot_subset[, actual_class := factor(actual_class, levels = states)]

ggplot(plot_subset, aes(x = year)) +
  
  geom_point(
    data = plot_subset[!is.na(pred_class)],
    aes(y = pred_class, color = pred_class),
    size = 3
  ) +
  
  geom_point(
    data = plot_subset[!is.na(actual_class)],
    aes(y = actual_class),
    shape = 1,
    size = 3,
    color = "black"
  ) +
  
  facet_wrap(~parcel_id, ncol = 2) +
  
  scale_x_continuous(
    limits = c(2018, end_year),
    breaks = seq(2018, end_year, by = 1)
  ) +
  
  ggtitle(sprintf("Predicted and Actual crop classes for parcel %s", sample_pids)) +
  theme_minimal()

