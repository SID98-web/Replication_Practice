# 01_build_regulation.R — construct regulation panel from soc_state_data.csv

log_step(1, "Building regulation data from soc_state_data.csv")

raw_reg <- fread(file.path(data_dir, "soc_state_data.csv"))

raw_reg[, reg_restrict := probability * restrictions]
raw_reg[, reg_words := probability * words]
raw_reg[, year := as.integer(substr(as.character(date_collected), 1, 4))]

setnames(raw_reg, "jurisdiction", "state_name")
setnames(raw_reg, "soc_code", "soc2")

# Collapse to state x soc2 x year
soc_state <- raw_reg[!is.na(state_name),
                      .(reg_restrict = sum(reg_restrict, na.rm = TRUE),
                        reg_words    = sum(reg_words, na.rm = TRUE)),
                      by = .(state_name, soc2, year)]

# Capitalize first letter (raw data is lowercase)
soc_state[, state_name := paste0(toupper(substr(state_name, 1, 1)),
                                  substr(state_name, 2, nchar(state_name)))]

# State crosswalk
state_xwalk <- data.table(
  state_name = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                  "Connecticut","Delaware","Dc","Florida","Georgia","Hawaii","Idaho",
                  "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
                  "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                  "Missouri","Montana","Nebraska","Nevada","New hampshire","New jersey",
                  "New mexico","New york","North carolina","North dakota","Ohio",
                  "Oklahoma","Oregon","Pennsylvania","Rhode island","South carolina",
                  "South dakota","Tennessee","Texas","Utah","Vermont","Virginia",
                  "Washington","West virginia","Wisconsin","Wyoming"),
  state_abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
                  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
                  "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
                  "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  st = c(1L,2L,4L,5L,6L,8L,9L,10L,11L,12L,13L,15L,16L,17L,18L,19L,20L,21L,22L,
         23L,24L,25L,26L,27L,28L,29L,30L,31L,32L,33L,34L,35L,36L,37L,38L,39L,
         40L,41L,42L,44L,45L,46L,47L,48L,49L,50L,51L,53L,54L,55L,56L)
)

soc_state <- merge(soc_state, state_xwalk, by = "state_name", all.x = TRUE)
soc_state <- soc_state[!is.na(st)]
soc_state[, lreg_restrict := log(reg_restrict)]
soc_state[, lreg_words := log(reg_words)]

setorder(soc_state, st, soc2, year)
soc_state[, lag1_lreg_restrict := shift(lreg_restrict, 1), by = .(st, soc2)]

# State-level totals
state_reg <- soc_state[, .(regst_restrict = sum(reg_restrict, na.rm = TRUE),
                            regst_words    = sum(reg_words, na.rm = TRUE)),
                        by = .(st, year)]

cat(sprintf("  %d obs, %d states, %d occupations\n",
            nrow(soc_state), uniqueN(soc_state$st), uniqueN(soc_state$soc2)))

rm(raw_reg)
