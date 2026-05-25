##creates the generalized transition matrix for all parcels by creating crop class sequences

setwd("/projectnb/dietzelab/ananyak")
library(data.table)
library(arrow)
library(dplyr)

##-----setup------
path_management = "/projectnb/dietzelab/ccmmf/management"
path_landiq_v4  = "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1"

lookup = fread(file.path(path_management, "LandIQ_cropCode_lookup_table.csv"))
ag_classes = unique(lookup[is_agricultural == TRUE, as.character(CLASS)])

year_min = 2018L
year_max = 2023L

crops_full <- as.data.table(
  arrow::open_dataset(file.path(path_landiq_v4, "crops_all_years.parq")) |>
    filter(year >= year_min, year <= year_max, CLASS %in% ag_classes) |>
    select(parcel_id, year, season, CLASS, SUBCLASS, centx, centy) |>
    collect()
)

crops_full[, `:=`(
  parcel_id = as.character(parcel_id),
  year = as.integer(year),
  season = as.integer(season),
  CLASS = as.character(CLASS),
  SUBCLASS = as.character(SUBCLASS)
)]

##-------order for sequence building - add season if needed-------
setorder(crops_full, parcel_id, year, season)
write.csv(crops_full, 'crops_full.csv')

##-------run some cleaning rules to lower the amount of messy/unrealistic sequences (for 'X' cases)------
fix_seq = function(seq) {
  parts = strsplit(seq, "-", fixed = TRUE)[[1]]
  n = length(parts)
  
  if (n == 0) return(seq)
  
  
  #Rule 1: if X between identical classes, replace with that class
  if (n >= 3) {
    for (i in 2:(n - 1)) {
      if (parts[i] == "X" && parts[i - 1] == parts[i + 1]) {
        parts[i] = parts[i - 1]
      }
    }
  }
  
  #Rule 2: similar to rule 2 but with a 'short run' of X's
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      while (i <= n && parts[i] == "X") i = i + 1
      end = i - 1
      run_len = end - start + 1
      
      if (
        run_len <= 2 &&
        start > 1 &&
        end < n &&
        parts[start - 1] == parts[end + 1]
      ) {
        parts[start:end] = parts[start - 1]
      }
    } else {
      i = i + 1
    }
  }
  
  #Rule 3: 'edge X's' where its all one class + one X -- replace with the majority 
  if (n >= 2) {
    if (parts[1] == "X") {
      parts[1] = parts[2]
    }
    if (parts[n] == "X") {
      parts[n] = parts[n - 1]
    }
  }
  
  #Rule 4: remaining short X run with one valid neighbor side --> fill from that side
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      while (i <= n && parts[i] == "X") i = i + 1
      end = i - 1
      run_len = end - start + 1
      
      left_val  = if (start > 1) parts[start - 1] else NA_character_
      right_val = if (end < n) parts[end + 1] else NA_character_
      
      if (run_len <= 2) {
        if (!is.na(left_val) && left_val != "X" && (is.na(right_val) || right_val == "X")) {
          parts[start:end] = left_val
        } else if (!is.na(right_val) && right_val != "X" && (is.na(left_val) || left_val == "X")) {
          parts[start:end] = right_val
        }
      }
    } else {
      i = i + 1
    }
  }
  paste(parts, collapse = "-")
}

##-----sequence formatting by parcel-----

#for every parcel: season 1 class - season 2 class - ... - season 4 class each year 2018-2023
#**sequences are way smaller because most data has been removed 

crop_sequences = crops_full[
  ,
  .(
    crop_sequence = paste(CLASS, collapse = "-"),
    season_sequence = paste(season, collapse = "-")
  ),
  by = .(parcel_id, year)
]

##-------apply sequence-fixing rules-------
crop_sequences[, crop_sequence := vapply(crop_sequence, fix_seq, character(1))]
unique(crop_sequences$crop_sequence)

##------dominant crop and probabilities-------
unique(crop_sequences[nchar(crop_sequence) == 1, season_sequence])

##add a dominant crop column to crop_sequences (most frequent class) and a probability column 
#for two character long sequences that are different, make the dominant crop the class in season 2 
#*season 2 = default dominant crop season  
seq_lookup = unique(crop_sequences[, .(crop_sequence, season_sequence)])

seq_lookup[, c("dominant_crop", "non_dom_prob") := {
  crop_split   = strsplit(crop_sequence, "-", fixed = TRUE)
  season_split = strsplit(season_sequence, "-", fixed = TRUE)
  
  dom  = character(length(crop_split))
  prob = numeric(length(crop_split))
  
  for (i in seq_along(crop_split)) {
    x = crop_split[[i]]
    s = as.integer(season_split[[i]])
    
    # special rule:
    # for length 2 or 3 sequences where all crop classes are different,
    # use the crop corresponding to season 2
    if (length(x) %in% c(2, 3) &&
        length(unique(x)) == length(x) &&
        2 %in% s) {
      
      dom[i] = x[which(s == 2)[1]]
      prob[i] = 1 - 1 / length(x)
      
    } else {
      tab = table(x)
      j = which.max(tab)
      dom_n = unname(tab[j])
      
      dom[i] = names(tab)[j]
      prob[i] = 1 - dom_n / length(x)
    }
  }
  
  .(dom, prob)
}]

crop_sequences = seq_lookup[crop_sequences, on = c("crop_sequence", "season_sequence")]

##store dominant crops&probabilites as a separate dataset
file = crop_sequences[, c("crop_sequence", "season_sequence", 
                          'dominant_crop', 'non_dom_prob')]
write.csv(file, 'dominant_crop_classes.csv')

##--------transition matrix items---------
#transitions between each year using each parcels dominant crop 
#currently incorporates non dominant probabilities into the transition matrix as weights
   #transitions with a non dominant prob != 0 gets weighed less in the tmat 

#new df with just dominant crop 
year_states = copy(crop_sequences)[
  ,
  .(parcel_id, year, dominant_crop, non_dom_prob)
]

year_states[, dominant_crop := trimws(as.character(dominant_crop))]

setorder(year_states, parcel_id, year)

##year to year transitions for each parcel 
year_states[, `:=`(
  from = dominant_crop,
  to = shift(dominant_crop, type = "lead"),
  next_year = shift(year, type = "lead"),
  from_non_dom = non_dom_prob,
  to_non_dom = shift(non_dom_prob, type = "lead")
), by = parcel_id]

transitions_full = year_states[
  !is.na(to) & next_year == year + 1
]

##weight column 
transitions_full[, weight := pmax(0.05, (1 - from_non_dom) * (1 - to_non_dom))]

## aggregate weighted counts
transitions_weighted = transitions_full[
  ,
  .(N = sum(weight, na.rm = TRUE)),
  by = .(from, to)
]

states_all = c("YP","D","X","T","G","F","P","C","I","V","R")

## cast to count matrix
tmat_counts = dcast(transitions_weighted, from ~ to, value.var = "N", fill = 0)

## add missing columns
missing_cols = setdiff(states_all, colnames(tmat_counts))
for (mc in missing_cols) tmat_counts[[mc]] = 0

## add missing rows
missing_rows = setdiff(states_all, tmat_counts$from)
if (length(missing_rows) > 0) {
  zero_rows = data.table(from = missing_rows)
  for (s in states_all) zero_rows[[s]] = 0
  tmat_counts = rbind(tmat_counts, zero_rows, fill = TRUE)
}

## order rows/cols
tmat_counts[, ord := match(from, states_all)]
setorder(tmat_counts, ord)
tmat_counts[, ord := NULL]
tmat_counts = tmat_counts[, c("from", states_all), with = FALSE]

## convert to matrix
rn = tmat_counts$from
prob_mat = as.matrix(tmat_counts[, ..states_all])
storage.mode(prob_mat) = "double"

## normalize rows safely
row_totals = rowSums(prob_mat)
tmat_final = prob_mat
tmat_final[row_totals > 0, ] = prob_mat[row_totals > 0, ] / row_totals[row_totals > 0]
tmat_final[row_totals == 0, ] = 0

rownames(tmat_final) = rn
colnames(tmat_final) = states_all

## checks
stopifnot(all(rownames(tmat_final) == states_all))
stopifnot(all(colnames(tmat_final) == states_all))
stopifnot(all(abs(rowSums(tmat_final)[row_totals > 0] - 1) < 1e-10))

##save outputs##
write.csv(year_states, "year_states.csv", row.names = FALSE)
write.csv(transitions_full, "year_to_year_transitions.csv", row.names = FALSE)
write.csv(tmat_final, "transition_matrix.csv")