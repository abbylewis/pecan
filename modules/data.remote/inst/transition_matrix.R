library(data.table)
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

#make per-(parcel,year) 4-season sequence 
crop_sequences = crops_full[, .(
  crop_sequence = paste(CLASS, collapse = "-")
), by = .(parcel_id, year)]

#drop fully NA sequences or urban plots with no changes
drop_sequences = c("**-**-**-**", "U-U-U-U", "UL-UL-UL-UL")
crop_sequences = crop_sequences[!crop_sequence %chin% drop_sequences]

##merging rules 
fix_seq = function(seq) {
  parts = strsplit(seq, "-", fixed = TRUE)[[1]]
  n = length(parts)
  
  # Rule 1: X forward-fill
  if (n > 1) for (i in 2:n) if (parts[i] == "X") parts[i] = parts[i - 1]
  
  # Rule 2: YP followed by ** -> make that ** into P
  if (n > 1) for (i in 2:n) if (parts[i - 1] == "YP" && parts[i] == "**") parts[i] = "P"
  
  # Rule 2a: ** or X immediately before P -> change prior slot to YP
  if (n > 1) for (i in 1:(n - 1)) if (parts[i] %in% c("**", "X") && parts[i + 1] == "P") parts[i] = "YP"
  
  # Rule 3: only one crop class (plus **) -> fill all seasons with that class
  vals = parts[parts != "**"]
  u = unique(vals)
  if (length(u) == 1 && all(parts %in% c(u, "**"))) parts = rep(u, n)
  
  paste(parts, collapse = "-")
}

crop_sequences[, crop_sequence := vapply(crop_sequence, fix_seq, character(1))]

# expand to 'long' format to include cross-year transitions
seq_long = crop_sequences[, {
  parts = strsplit(crop_sequence, "-", fixed = TRUE)[[1]]
  data.table(season = seq_along(parts), CLASS = parts)
}, by = .(parcel_id, year)]

setorder(seq_long, parcel_id, year, season)

#transitions across seasons + years
seq_long[, from := CLASS]
seq_long[, to   := shift(CLASS, type = "lead"), by = parcel_id]

transitions_full = seq_long[!is.na(to), .N, by = .(from, to)]

#build tmat 
states_all = c("**","V","P","X","G","YP","U","D","C","I","T","F","R","UL")
#should match unique(crops_full$CLASS)

tmat_counts = dcast(transitions_full, from ~ to, value.var = "N", fill = 0)

#add missing columns and rows to ensure square matrix 
missing_cols = setdiff(states_all, colnames(tmat_counts))
for (mc in missing_cols) tmat_counts[[mc]] = 0

#missing rows
missing_rows = setdiff(states_all, tmat_counts$from)
if (length(missing_rows) > 0) {
  zero_rows = data.table(from = missing_rows)
  for (s in states_all) zero_rows[[s]] = 0
  tmat_counts = rbind(tmat_counts, zero_rows, fill = TRUE)
}

#order and normalize
tmat_counts[, ord := match(from, states_all)]
setorder(tmat_counts, ord)
tmat_counts[, ord := NULL]
tmat_counts = tmat_counts[, c("from", states_all), with = FALSE]

rn = tmat_counts$from
prob_mat = as.matrix(tmat_counts[, ..states_all])
storage.mode(prob_mat) = "double"

rs = rowSums(prob_mat)
rs[rs == 0] = 1

tmat_final = prob_mat / rs
rownames(tmat_final) = rn
colnames(tmat_final) = states_all

stopifnot(all(rownames(tmat_final) == states_all))
stopifnot(all(colnames(tmat_final) == states_all))