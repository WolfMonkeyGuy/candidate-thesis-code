library(readxl)
library(dplyr)
library(stringr)


NEW_FILE = ""
CLEAN_FILE = ""
OUT_FILE = ""


swedish_months = c(
  "jan" = 1L, "januari" = 1L,
  "feb" = 2L, "febr" = 2L, "februari" = 2L,
  "mar" = 3L, "mars" = 3L, "marts" = 3L,
  "apr" = 4L, "apri" = 4L, "april" = 4L,
  "maj" = 5L,
  "jun" = 6L, "juni" = 6L,
  "jul" = 7L, "juli" = 7L,
  "aug" = 8L, "augusti" = 8L,
  "sep" = 9L, "sept" = 9L, "spet" = 9L, "september" = 9L,
  "okt" = 10L, "ookt" = 10L, "oktober" = 10L,
  "nov" = 11L, "november" = 11L,
  "dec" = 12L, "december" = 12L
)

#Firm name normalization

firm_lookup = c(
  "Skop" = "SKOP",
  "Sentio Research" = "Sentio",
  "Sifo (extra)" = "Sifo",
  "Novus (1)" = "Novus",
  "Novus (2)" = "Novus",
  "ZAPERA" = "Zapera",
  "Temo" = "TEMO",
  "Synovate Temo" = "Synovate",
  "Ipsos (Synovate)" = "Ipsos",
  "SVT VALU" = "SVT Valu",
  "Zapera exit" = "Zapera",
  "Demoskop valdag" = "Demoskop",
  "YouGov Valdag 1" = "YouGov",
  "YouGov Valdag 2" = "YouGov"
)

#publication date
parse_publicerad = function(pub, yr, mon_word) {
  if (is.na(yr)) return(as.Date(NA))
  yr = as.integer(yr)
  
  if (!is.na(pub)) {
    if (inherits(pub, c("Date", "POSIXct", "POSIXt"))) return(as.Date(pub))
    if (is.numeric(pub)) return(as.Date(pub, origin = "1899-12-30"))
    pub_str = as.character(pub)
    if (grepl("^\\d{5}(\\.\\d+)?$", pub_str)) {
      return(as.Date(as.numeric(pub_str), origin = "1899-12-30"))
    }
    m = str_match(tolower(pub_str), "(\\d{1,2})\\s*([a-zåäö]+)")
    if (!is.na(m[1, 1])) {
      day = as.integer(m[1, 2])
      mon = unname(swedish_months[m[1, 3]])
      if (!is.na(mon)) {
        return(suppressWarnings(as.Date(sprintf("%d-%02d-%02d", yr, mon, day))))
      }
    }
  }
  
  # Fallback 15th 
  if (!is.na(mon_word)) {
    mon = unname(swedish_months[tolower(trimws(mon_word))])
    if (!is.na(mon)) return(as.Date(sprintf("%d-%02d-15", yr, mon)))
  }
  as.Date(NA)
}

#Parse Intervjuperiod
parse_intervjperiod = function(s, yr) {
  na_pair = as.Date(c(NA, NA))
  if (is.na(s) || is.na(yr)) return(na_pair)
  yr = as.integer(yr)
  s  = str_replace_all(str_to_lower(str_trim(as.character(s))), "--", "-")
  
  # Form A: "d1 mon1 - d2 mon2"
  m = str_match(s, "^(\\d{1,2})\\s+([a-zåäö]+)\\s*-\\s*(\\d{1,2})\\s+([a-zåäö]+)")
  if (!is.na(m[1, 1])) {
    d1 = as.integer(m[1, 2]); mo1 = unname(swedish_months[m[1, 3]])
    d2 = as.integer(m[1, 4]); mo2 = unname(swedish_months[m[1, 5]])
    if (!is.na(mo1) && !is.na(mo2)) {
      sy = if (mo1 > mo2) yr - 1L else yr
      return(suppressWarnings(as.Date(c(
        sprintf("%d-%02d-%02d", sy, mo1, d1),
        sprintf("%d-%02d-%02d", yr, mo2, d2)
      ))))
    }
  }
  # Form B: "d1 - d2 mon"
  m = str_match(s, "^(\\d{1,2})\\s*-\\s*(\\d{1,2})\\s+([a-zåäö]+)")
  if (!is.na(m[1, 1])) {
    d1 = as.integer(m[1, 2]); d2 = as.integer(m[1, 3])
    mo = unname(swedish_months[m[1, 4]])
    if (!is.na(mo)) {
      return(suppressWarnings(as.Date(c(
        sprintf("%d-%02d-%02d", yr, mo, d1),
        sprintf("%d-%02d-%02d", yr, mo, d2)
      ))))
    }
  }
  m = str_match(s, "^(\\d{1,2})\\s+([a-zåäö]+)(?:\\s*\\(.*\\))?\\s*$")
  if (!is.na(m[1, 1])) {
    d1 = as.integer(m[1, 2]); mo = unname(swedish_months[m[1, 3]])
    if (!is.na(mo)) {
      d = suppressWarnings(as.Date(sprintf("%d-%02d-%02d", yr, mo, d1)))
      return(c(d, d))
    }
  }
  na_pair
}

# read
new_raw = read_excel(NEW_FILE, sheet = "Alla mätningar")
clean   = read.csv(CLEAN_FILE, stringsAsFactors = FALSE)
clean$Date      = as.Date(clean$Date)
clean$date_from = as.Date(clean$date_from)

#Clean up firm names in clean_dataset_v2 before the merge
clean = clean %>%
  mutate(firm = ifelse(firm == "Indikator Opinion", "Indikator", firm)) %>%
  filter(firm != "Demoskop/Inizio")

cat("clean_dataset_v2 rows (post-cleanup):", nrow(clean), "\n")
cat("valjarbarometer2000 raw rows:        ", nrow(new_raw), "\n")

#Process new polls
new_polls = new_raw %>%
  filter(!if_all(everything(), is.na)) %>%
  filter(!is.na(Företag)) %>%
  rowwise() %>%
  mutate(
    pub_date = parse_publicerad(Publicerad, År, Månad),
    intervj = list(parse_intervjperiod(Intervjperiod, År)),
    intervj_start = intervj[[1]][1],
    intervj_end = intervj[[1]][2]
  ) %>%
  ungroup() %>%
  filter(!is.na(pub_date)) %>%
  mutate(
    # Date = end of fieldwork , fall back to publication date
    Date      = if_else(is.na(intervj_end), pub_date, intervj_end),
    date_from = intervj_start,
    firm = ifelse(Företag %in% names(firm_lookup),
                  unname(firm_lookup[Företag]),
                  Företag),
    parties.S = S,
    parties.M = M,
    parties.C = C,
    parties.V = V,
    parties.L = `L (FP)`,
    parties.MP = MP,
    parties.KD = KD,
    parties.SD = SD,
    parties.Fi = FI,
    parties.NYD = NA_real_,
    sample_size = suppressWarnings(as.numeric(`Intervjuer (totalt)`))
  ) %>%
  select(Date, parties.S, parties.M, parties.C, parties.V, parties.L,
         parties.MP, parties.KD, parties.NYD, parties.SD, parties.Fi,
         firm, date_from, sample_size)

cat("new polls after parsing:             ", nrow(new_polls), "\n")
cat("  with date_from parsed:             ", sum(!is.na(new_polls$date_from)), "\n")

#Pad new_polls
missing_cols = setdiff(names(clean), names(new_polls))
for (col in missing_cols) new_polls[[col]] = NA
new_polls = new_polls[, names(clean)]

#ombine + dedupe, two passes, originals win on conflict
clean$.priority     = 0L
new_polls$.priority = 1L
combined = bind_rows(clean, new_polls)
n_before = nrow(combined)


combined = combined %>%
  mutate(.row_id = row_number(),
         .key1 = ifelse(is.na(date_from),
                          paste0("NA_", .row_id),
                          as.character(date_from))) %>%
  arrange(.priority) %>%
  distinct(firm, .key1, .keep_all = TRUE) %>%
  select(-.key1, -.row_id)

combined = combined %>%
  arrange(.priority) %>%
  distinct(firm, Date, .keep_all = TRUE) %>%
  select(-.priority)

cat("after dedup:                         ", nrow(combined),
    "(dropped", n_before - nrow(combined), "duplicates)\n")


write.csv(combined, OUT_FILE, row.names = FALSE)
cat("wrote: ", OUT_FILE, "\n", sep = "")


cat("\nYear distribution (post-merge):\n")
print(table(format(combined$Date, "%Y")))

cat("\nFirms in combined set:\n")
print(sort(table(combined$firm), decreasing = TRUE))