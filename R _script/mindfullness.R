getwd()
#setwd("~/Desktop/R Mindfullness")

##SPSS Daten einlesen
# install.packages("foreign")
library(foreign)
library(data.table)

mindfulness <-read.spss("resource_TSST_GLM.sav",to.data.frame = TRUE) 
setDT(mindfulness)

# raw_cort
# "cort_bl2" omitted
raw_cort <- c(
        "cort_bl",
        "cort_m22",
        "cort_m05",
        "cort_10",
        "cort_20",
        "cort_30",
        "cort_40",
        "cort_55",
        "cort_70"
)

mindfulness_1 <- data.table::melt(
        data = mindfulness,
        measure.vars = raw_cort,
        variable.name = "raw_cort_variables",
        value.name = "raw_cort_values"
)

# create lookup
lookup <- data.frame(
  key = c("bl",
          "m22",
          "m05",
          "t10",
          "t20",
          "t30",
          "t40",
          "t55",
          "t70"),
  var_cort_wins = c(
    "cort_bl_ln.w",
    NA,
    NA,
    NA,
    "cort_20_ln.w",
    "cort_30_ln.w",
    NA,
    "cort_55_ln.w",
    NA
  ),
  var_raw_cort = unique(mindfulness_1$raw_cort_variables),
  var_zcort_ln = c(
    "Zcort_bl_ln",
    NA,
    NA,
    NA,
    "Zcort_20_ln",
    "Zcort_30_ln",
    "Zcort_40_ln",
    "Zcort_55_ln",
    NA
  ),
  var_aa = c("aa_bl",
             "aa_m22",
             "aa_m05", 
             "aa_10",  
             "aa_20",
             "aa_30",
             "aa_40",
             "aa_55",
             "aa_70"),
  var_aa_ln = c(
    "aa_bl_ln",
    "aa_m22_ln",
    "aa_m05_ln",
    "aa_10_ln",
    "aa_20_ln",
    "aa_30_ln",
    "aa_40_ln",
    "aa_55_ln",
    "aa_70_ln"
  ),
  var_aa_ln_w = c("aa_bl_ln.w",
                  NA,
                  NA,
                  "aa_10_ln.w",
                  "aa_20_ln.w",
                  "aa_30_ln.w",
                  "aa_40_ln.w",
                  NA,
                  NA),
  var_zaa = c("Zaa_bl_ln",
              NA,
              NA,
              "Zaa_10_ln",
              "Zaa_20_ln",
              "Zaa_30_ln",
              "Zaa_40_ln",
              NA,
              NA)
)

# merge key column
mindfulness_1 <- merge(mindfulness_1,
      lookup[, c("key", "var_raw_cort")],
      by.x = "raw_cort_variables",
      by.y = "var_raw_cort")

# Regex-Suche: suche nach Namen oder Zeichen
# in den Spaltennamen von "mindfulness"
# value = TRUE gibt Spaltennamen
# value = FALSE gibt Spaltenindices
raw_cort_ln <- grep("(?=^cort.*ln$)(?!.*inc)", names(mindfulness), value = TRUE,
                    perl = TRUE)

mindfulness_2 <- data.table::melt(
        data = mindfulness[, c("id", raw_cort_ln), with = FALSE],
        measure.vars = raw_cort_ln,
        variable.name = "raw_cort_ln_variables",
        value.name = "raw_cort_ln_values"
)

# bind raw_cort & raw_cort_ln
# mindfulness_1 and _2 have the same order
# all(mindfulness_1$id == mindfulness_2$id)
mindfulness_3 <- cbind(mindfulness_1, mindfulness_2[, .(raw_cort_ln_variables,
                                                        raw_cort_ln_values)])

# raw cort ln.w
raw_cort_ln_w <- c("cort_bl_ln.w",
                   "cort_20_ln.w",
                   "cort_30_ln.w",
                   "cort_55_ln.w")

mindfulness_wins <- data.table::melt(
        data = mindfulness,
        measure.vars = raw_cort_ln_w,
        variable.name = "raw_cort_ln_w_variables",
        value.name = "raw_cort_ln_w_values"
)

# merge via key column
mindfulness_wins <- merge(mindfulness_wins,
                          lookup[, c("key", "var_cort_wins")],
                          by.x = "raw_cort_ln_w_variables",
                          by.y = "var_cort_wins")

mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_wins[, .(id,
                                            key,
                                            raw_cort_ln_w_variables,
                                            raw_cort_ln_w_values)],
                       by = c("id", "key"),
                       all = TRUE)

# check
# View(mindfulness_3[, .(id, key, raw_cort_variables, raw_cort_values,
#                        raw_cort_ln_variables, raw_cort_ln_values,
#                        raw_cort_ln_w_variables,
#                        raw_cort_ln_w_values)])


# Zcort
# names(mindfulness_3)
zcort_ln <- c("Zcort_bl_ln",
              "Zcort_20_ln",
              "Zcort_30_ln",
              "Zcort_40_ln",
              "Zcort_55_ln")

mindfulness_zcort_ln <- data.table::melt(
        data = mindfulness[, c("id", zcort_ln), with = FALSE],
        measure.vars = zcort_ln,
        variable.name = "zcort_ln_variables",
        value.name = "zcort_ln_values"
)
mindfulness_zcort_ln <- merge(mindfulness_zcort_ln,
      lookup[, c("key", "var_zcort_ln")],
      by.x = "zcort_ln_variables",
      by.y = "var_zcort_ln")

mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_zcort_ln,
                       by = c("id", "key"), 
                       all = TRUE)

# check
# View(mindfulness_3[, .(id, raw_cort_variables, raw_cort_values,
#                   zcort_ln_variables, zcort_ln_values, key)])

###########ab hier neu erstellt Datensätze#################

#### Stai-Spalte 
# names(mindfulness)
stai_tl<- c("STAIs_tl1","STAIs_tl2","STAIs_tl3", "STAIs_tl4", "STAIs_tl5","STAIs_tl6")


mindfulness_stai <- data.table::melt(
        data = mindfulness[, c("id", stai_tl), with = FALSE],
        measure.vars = stai_tl,
        variable.name = "stai_time",
        value.name = "stai_tl_values"
)

## Stai_w-Spalten
stai_tl_w<- c("STAIs_tl1.w","STAIs_tl3.w", "STAIs_tl4.w", "STAIs_tl5.w")

mindfulness_stai_tl_w <- data.table::melt(
        data = mindfulness[, c("id", stai_tl_w), with = FALSE],
        measure.vars = stai_tl_w,
        variable.name = "stai_tl_w_variables",
        value.name = "stai_tl_w_values"
)

# create time column as key
mindfulness_stai_tl_w[, stai_time := (sub("\\..*", "", stai_tl_w_variables))]

## Zstai
zstai_tl <- c("ZSTAIs_tl1","ZSTAIs_tl2","ZSTAIs_tl3","ZSTAIs_tl4","ZSTAIs_tl5" )

mindfulness_zstai <- data.table::melt(
        data = mindfulness[, c("id", zstai_tl), with = FALSE],
        measure.vars = zstai_tl,
        variable.name = "zstai_tl_variables",
        value.name = "zstai_tl_values"
)

# create time col as key
mindfulness_zstai[, stai_time := sub("Z", "", zstai_tl_variables)]

#eine neue Spalte mit 9 Messzeitpunkten erzeugen
mindfulness_3$stai_time <-rep(
        c(
                "STAIs_tl1",
                "STAIs_tl2",
                "STAIs_tl3",
                "STAIs_tl4",
                "STAIs_tl5",
                "STAIs_tl6",
                "STAIs_tl7",
                "STAIs_tl8",
                "STAIs_tl9"
        ),times= 332
)
# Überprüfung
# mindfulness_3$stai_time

# zusammenführen
mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_stai,
                       by = c("id", "stai_time"),
                       all = TRUE)

mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_stai_tl_w,
                       by = c("id", "stai_time"),
                       all = TRUE)

mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_zstai,
                       by = c("id", "stai_time"),
                       all = TRUE)
# check
# View(mindfulness_3[, .(
#   id,
#   stai_time,
#   stai_tl_values,
#   stai_tl_w_variables,
#   stai_tl_w_values,
#   zstai_tl_variables,
#   zstai_tl_values
# )])

names (mindfulness)

# alpha amylase
aa_cols <- c("aa_bl",
             "aa_m22",
             "aa_m05",
             "aa_10",
             "aa_20",
             "aa_30",
             "aa_40",
             "aa_55",
             "aa_70")

mindfulness_aa <- data.table::melt(
  data = mindfulness[, c("id", aa_cols), with = FALSE],
  measure.vars = aa_cols,
  variable.name = "aa_variables",
  value.name = "aa_values")

mindfulness_aa <- merge(mindfulness_aa,
                        lookup[, c("key", "var_aa")],
                        by.x = "aa_variables",
                        by.y = "var_aa")
# 
aa_ln_cols <- c(
  "aa_bl_ln",
  "aa_m22_ln",
  "aa_m05_ln",
  "aa_10_ln",
  "aa_20_ln",
  "aa_30_ln",
  "aa_40_ln",
  "aa_55_ln",
  "aa_70_ln"
)

mindfulness_aa_ln <- data.table::melt(
  data = mindfulness[, c("id", aa_ln_cols), with = FALSE],
  measure.vars = aa_ln_cols,
  variable.name = "aa_ln_variables",
  value.name = "aa_ln_values")

mindfulness_aa_ln <- merge(mindfulness_aa_ln,
                        lookup[, c("key", "var_aa_ln")],
                        by.x = "aa_ln_variables",
                        by.y = "var_aa_ln")

aa_ln_w_cols <- c("aa_bl_ln.w",
                  "aa_10_ln.w",
                  "aa_20_ln.w",
                  "aa_30_ln.w",
                  "aa_40_ln.w")

mindfulness_aa_ln_w <- data.table::melt(
  data = mindfulness[, c("id", aa_ln_w_cols), with = FALSE],
  measure.vars = aa_ln_w_cols,
  variable.name = "aa_ln_w_variables",
  value.name = "aa_ln_w_values"
)

mindfulness_aa_ln_w <- merge(mindfulness_aa_ln_w,
                           lookup[, c("key", "var_aa_ln_w")],
                           by.x = "aa_ln_w_variables",
                           by.y = "var_aa_ln_w")

zaa_cols <- grep("Zaa", names(mindfulness), value = TRUE)

mindfulness_zaa <- data.table::melt(
  data = mindfulness[, c("id", zaa_cols), with = FALSE],
  measure.vars = zaa_cols,
  variable.name = "zaa_variables",
  value.name = "zaa_values"
)

mindfulness_zaa <- merge(x = mindfulness_zaa,
                         y = lookup[, c("key", "var_zaa")],
                         by.x = "zaa_variables",
                         by.y = "var_zaa")

# merge back to mindfulness
mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_aa,
                       by = c("id", "key"))
mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_aa_ln,
                       by = c("id", "key"),
                       all = TRUE)
mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_aa_ln_w,
                       by = c("id", "key"), 
                       all = TRUE)
mindfulness_3 <- merge(mindfulness_3,
                       mindfulness_zaa,
                       by = c("id", "key"), 
                       all = TRUE)
# check
# View(mindfulness_3[, .(id, raw_cort_variables, raw_cort_values,
#                        aa_values, aa_variables,
#                        aa_ln_variables, aa_ln_values,
#                        aa_ln_w_variables,
#                        aa_ln_w_values)])

#doppelte Variablen löschen
mindfulness_3[, c(aa_cols,
                  aa_ln_cols,
                  aa_ln_w_cols,
                  raw_cort_ln,
                  raw_cort_ln_w,
                  stai_tl,
                  stai_tl_w,
                  zcort_ln,
                  zstai_tl,
                  zaa_cols) := NULL]

# change col order
setcolorder(mindfulness_3, 
            c("id",
              "count",
              "missings",                
              "sex1",
              "population",
              "group",
              "module",            
              "timepoint",
              "city",
              "age",
              "sex",               
              "hormones",
              "bmi",
              "TSST_INFO",
              "StartTime",
              "key",
              "raw_cort_variables",
              "raw_cort_values",
              "raw_cort_ln_variables",
              "raw_cort_ln_values",
              "raw_cort_ln_w_variables",
              "raw_cort_ln_w_values",
              "zcort_ln_variables",
              "zcort_ln_values",
              "aa_variables",
              "aa_values",
              "aa_ln_variables",
              "aa_ln_values",
              "aa_ln_w_variables",
              "aa_ln_w_values",
              "zaa_variables",
              "zaa_values",
              "stai_time",
              "stai_tl_values",
              "stai_tl_w_variables",
              "stai_tl_w_values",
              "zstai_tl_variables",
              "zstai_tl_values"))
# check
View(mindfulness_3)

# replace missing values in raw_cort_ln_w
mindfulness_3[, raw_cort_ln_w_values := ifelse(is.na(raw_cort_ln_w_values),
                                               raw_cort_ln_values,
                                               raw_cort_ln_w_values)]

#replace missing values in stai_tl_w_values
mindfulness_3[,stai_tl_w_values:= ifelse(is.na(stai_tl_w_values),
                                          stai_tl_values,
                                          stai_tl_w_values)]

#sort out unnecessary colums
mindfulness_3[, c("sex1",
                  "missings",
                  "TSST_Info",
                  "aa_variables",
                  "aa_values",
                  "aa_ln_variables",
                  "aa_ln_values",
                  "aa_ln_w_variables",
                  "aa_ln_w_values",
                  "aa_inc_ln",
                  "aa_inc_ln.w",
                  "zaa_variables",
                  "zaa_values",
                  "PSYCH",
                  "id2b",
                  "CHR_STR",
                  "id3",
                  "PHYSIO"):= NULL]

#save as csv.
write.csv(x = mindfulness_3,
          file = "~/Desktop/R Mindfullness/mindfulness_final.csv",
          row.names = FALSE)
