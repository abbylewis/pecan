ry(data.table)
library(stringr)

file_2018 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2018.rds")
file_2019 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2019.rds")
file_2020 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2020.rds")
file_2021 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2021.rds")
file_2022 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2022.rds")
file_2023 = readRDS("/projectnb/dietzelab/ananyak/annual_landiq_PFT_2023.rds")

crops_full = rbind(file_2018, file_2019, file_2020, file_2021, file_2022, file_2023)
setDT(crops_full)

setorder(crops_full, parcel_id, year, season)

crop_sequences = crops_full[, .(
  crop_sequence = paste(CLASS, collapse = "-")
), by = .(parcel_id, year)]


#merging rules 
fix_seq = function(seq) {
  parts = strsplit(seq, "-", fixed = TRUE)[[1]]
  n = length(parts)
  
  if (n > 1) for (i in 2:n) if (parts[i] == "X") parts[i] = parts[i - 1]
  if (n > 1) for (i in 2:n) if (parts[i - 1] == "YP" && parts[i] == "**") parts[i] = "P"
  if (n > 1) for (i in 1:(n - 1)) if (parts[i] %in% c("**", "X") && parts[i + 1] == "P") parts[i] = "YP"
  
  vals = parts[parts != "**"]
  u = unique(vals)
  if (length(u) == 1 && all(parts %in% c(u, "**"))) parts = rep(u, n)
  
  paste(parts, collapse = "-")
}

drop_sequences = c("**-**-**-**", "U-U-U-U", "UL-UL-UL-UL")
crop_sequences = crop_sequences[!crop_sequence %chin% drop_sequences]
crop_sequences[, crop_sequence := vapply(crop_sequence, fix_seq, character(1))]

##transition format df for matrix 
#this unfortunately takes a while, this was the only way I could think of writing this
#saved seq_long (and final transition matrix) as a csv at bottom so this only has to be run once
#the prediction file (predict_and_stroe) reloads the saved files 

seq_long = crop_sequences[, {
  parts = strsplit(crop_sequence, "-", fixed = TRUE)[[1]]
  data.table(season = seq_along(parts), CLASS = parts)
}, by = .(parcel_id, year)]

setorder(seq_long, parcel_id, year, season)

seq_long[, `:=`(
  from = CLASS,
  to = shift(CLASS, type = "lead"),
  next_year = shift(year, type = "lead")
), by = parcel_id]

transitions_full = seq_long[
  !is.na(to) & next_year == year + 1 & season == season_idx,
  .(N = .N),
  by = .(from, to)]

#build matrix 
states_all = c("**","V","P","X","G","YP","U","D","C","I","T","F","R","UL")

tmat_counts = dcast(transitions_full, from ~ to, value.var = "N", fill = 0)

# add missing columns
missing_cols = setdiff(states_all, colnames(tmat_counts))
for (mc in missing_cols) tmat_counts[[mc]] = 0

# add missing rows
missing_rows = setdiff(states_all, tmat_counts$from)
if (length(missing_rows) > 0) {
  zero_rows = data.table(from = missing_rows)
  for (s in states_all) zero_rows[[s]] = 0
  tmat_counts = rbind(tmat_counts, zero_rows, fill = TRUE)
}

# order matrix
tmat_counts[, ord := match(from, states_all)]
setorder(tmat_counts, ord)
tmat_counts[, ord := NULL]
tmat_counts = tmat_counts[, c("from", states_all), with = FALSE]


#normalize 
rn = tmat_counts$from
prob_mat = as.matrix(tmat_counts[, ..states_all])
storage.mode(prob_mat) = "double"

#smoothing
prob_mat = prob_mat + 1e-3

#normalize again 
tmat_final = prob_mat / rowSums(prob_mat)

rownames(tmat_final) = rn
colnames(tmat_final) = states_all

#checks
stopifnot(all(rownames(tmat_final) == states_all))
stopifnot(all(colnames(tmat_final) == states_all))

##save final transition matrix and huge transition sequence file (seq_long)
write.csv(seq_long, 'seq_long.csv')
write.csv(tmat_final, 'full_transition_matrix.csv')


