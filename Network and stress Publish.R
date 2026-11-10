#clean data & environment
rm(list = ls(all = TRUE))
suppressWarnings(
  if (!is.null(names(sessionInfo()$otherPkgs))) {
    invisible(lapply(
      paste0('package:', names(sessionInfo()$otherPkgs)),
      detach,
      character.only = TRUE,
      unload = TRUE
    ))}
)
graphics.off()

#set the working directory
setwd("D:/Dropbox/####000UF Research/MEGA/Analysis_2026")

#library (might be used later)
library(readxl)
library(tidyverse)
library(ggplot2)
library(igraph)
library(dplyr)
library(car)
library(broom)


#MAKE EGO DATA READY---------------------------------------
#read ego data 
#it has multiple sheets
file_ego <- "RAW/ego-level-data-02-20-2022.xlsx"
sheets_ego <- excel_sheets(file_ego)
#combine sheets into one table, name it as egoraw
egoraw <- bind_rows(
  lapply(sheets_ego, function(s) {
    read_excel(file_ego, sheet = s, col_types = "text")
  }),
  .id = "sheet"
)
#Original Ego ID is a combination of survey info + egoID
egoraw <- egoraw %>%
  rename(egoID = battery)#lower case is the only egoID
egoraw <- egoraw %>%
  relocate(egoID, .before = 1)#make it first column




####++++++++++social stress ==============
egodata <- egoraw %>%
  mutate(across(ends_with("_occurred"), ~ as.numeric(.))) %>%
  mutate(across(ends_with("_stressful"), ~ as.numeric(.)))

egodata <- egodata %>%
  mutate(
    #SOCIAL STRESS
    #around people who speak S/E 
    s_ss1  = ifelse(`4.10.1.1_MASI1_occurred` == 1, `4.10.2.1_MASI1_stressful`,  0),
    e_ss1 = ifelse(`4.10.1.2_MASI1A_occurred` == 1, `4.10.2.2_MASI1A_stressful`, 0),
    #treat unfairly
    s_ss2  = ifelse(`4.10.1.7_MASI4_occurred`== 1, `4.10.2.7_MASI4_stressful`,  0),
    e_ss2 = ifelse(`4.10.1.8_MASI4A_occurred` == 1, `4.10.2.8_MASI4A_stressful`, 0),
    #assume speak
    s_ss3  = ifelse(`4.10.1.9_MASI5_occurred`== 1, `4.10.2.9_MASI5_stressful`,  0),
    e_ss3 = ifelse(`4.10.1.10_MASI5A_occurred` == 1, `4.10.2.10_MASI5A_stressful`, 0),
    #discrimination
    s_ss4  = ifelse(`4.10.1.11_MASI6_occurred`== 1, `4.10.2.11_MASI6_stressful`,  0),
    e_ss4 = ifelse(`4.10.1.12_MASI6A_occurred` == 1, `4.10.2.12_MASI6A_stressful`, 0),
    #pressure to learn
    s_ss5  = ifelse(`4.10.1.3_MASI2_occurred`== 1, `4.10.2.3_MASI2_stressful`,  0),
    e_ss5 = ifelse(`4.10.1.4_MASI2A_occurred` == 1, `4.10.2.4_MASI2A_stressful`, 0),
    #understand 
    s_ss6  = ifelse(`4.10.1.5_MASI3_occurred`== 1, `4.10.2.5_MASI3_stressful`,  0),
    e_ss6 = ifelse(`4.10.1.6_MASI3A_occurred` == 1, `4.10.2.6_MASI3A_stressful`, 0)
  )

egodata <- egodata %>%
  mutate(
    # SUM scores
    s_stress_sum = rowSums(across(s_ss1:s_ss6)),
    e_stress_sum = rowSums(across(e_ss1:e_ss6)),
    total_stress_sum = s_stress_sum + e_stress_sum,
    # MEAN scores
    s_stress_mean = rowMeans(across(s_ss1:s_ss6)),
    e_stress_mean = rowMeans(across(e_ss1:e_ss6)),
    total_stress_mean = (s_stress_mean + e_stress_mean) / 2
  )


egodata$agey<-as.numeric(egodata$`1.1_age_ego`)
egodata$age<-2022-egodata$agey
egodata <- egodata %>%
  mutate(
    gender = factor(
      `1.2_gender`,
      levels = c(1, 2, 3),
      labels = c("male", "female", "nonbinary")
    )
  )



#MAKE ALTER ATTRI READY------------------------------------------
#alter attributes table(seems a combination of ego and alter attri)
file_alter <- "RAW/ego-alter-data-02-20-2022.xlsx"
sheets_alter <- excel_sheets(file_alter)

alterraw <- bind_rows(
  lapply(sheets_alter, function(s) {
    read_excel(file_alter, sheet = s, col_types = "text")
  }),
  .id = "sheet"
)
#lower case egoID is the real ID
alterraw$egoID <- egoraw$egoID[match(alterraw$EgoID, egoraw$EgoID)]
#make it first column
alterraw <- alterraw %>%
  relocate(egoID, .before = 1)
#AID is alter ID
#alter ID is a comb of egoID with alter number
alterraw$AID <- paste(alterraw$egoID, alterraw$`Alter Number`, sep = "_")
alterraw <- alterraw %>%
  relocate(AID, .after = 1)

nl_table <- alterraw %>%
  dplyr::filter(`2.1_native_languages` == 1) %>%
  dplyr::select(
    egoID,
    `Alter Name`,
    `2.1_native_languages`,
    `2.2_native_years`
  )
length(unique(nl_table$egoID))
# Number of unique participants reporting English
nl_table %>%
  filter(tolower(`Alter Name`) == "english") %>%
  summarise(n = n_distinct(egoID))

nl_table %>%
  filter(tolower(`Alter Name`) == "spanish") %>%
  summarise(n = n_distinct(egoID))

# articipants reporting BOTH English and Spanish
bothl <- intersect(
  nl_table %>%
    filter(tolower(`Alter Name`) == "english") %>%
    pull(egoID) %>%
    unique(),
  
  nl_table %>%
    filter(tolower(`Alter Name`) == "spanish") %>%
    pull(egoID) %>%
    unique()
)

length(bothl)

34/51


native_years <- nl_table %>%
  filter(`2.1_native_languages` == 1) %>%
  filter(tolower(`Alter Name`) %in% c("english", "spanish")) %>%
  select(egoID, `Alter Name`, `2.2_native_years`) %>%
  mutate(
    language = tolower(`Alter Name`)
  ) %>%
  select(-`Alter Name`) %>%
  pivot_wider(
    names_from = language,
    values_from = `2.2_native_years`,
    names_glue = "{language}_native_years"
  )
native_years <- native_years %>%
  mutate(
    english_native_years = as.numeric(english_native_years),
    spanish_native_years = as.numeric(spanish_native_years)
  )
native_years %>%
  summarise(
    mean_english = mean(english_native_years, na.rm = TRUE),
    sd_english = sd(english_native_years, na.rm = TRUE),
    mean_spanish = mean(spanish_native_years, na.rm = TRUE),
    sd_spanish = sd(spanish_native_years, na.rm = TRUE)
  )

#MAKE ALTER-ALTER TIES READY---------------------------------
#AA table 
file_aa <- "RAW/alter-pair-data-02-20-2022.xlsx"
sheets_aa <- excel_sheets(file_aa)
aaraw <- bind_rows(
  lapply(sheets_aa, function(s) {
    read_excel(file_aa, sheet = s, col_types = "text")
  }),
  .id = "sheet"
)
#lower case egoID is the real ID
aaraw$egoID <- egoraw$egoID[match(aaraw$EgoID, egoraw$EgoID)]
#make it first column
aaraw <- aaraw %>%
  relocate(egoID, .before = 1)
#create from and to variables
aaraw$from <- paste(aaraw$egoID, aaraw$`Alter 1 Number`, sep = "_")
aaraw$to <- paste(aaraw$egoID, aaraw$`Alter 2 Number`, sep = "_")
#make these into first several columns-easy to read
aaraw <- aaraw %>%
  relocate(from, .after = 1)
aaraw <- aaraw %>%
  relocate(to, .after = 2)
#early AA ties 
aae<-aaraw %>%
  filter(`7.1_pair_early` > 0) 
aae <- aae %>%
  relocate(`7.1_pair_early`, .after = 3)
#late AA ties 
aal<-aaraw %>%
  filter(`10.1_pair_late` > 0) 
aal <- aal %>%
  relocate(`10.1_pair_late`, .after = 3)


#Analysis----------------------------------------
#make sure egoraw has same ID as alterraw
setequal(unique(egoraw$egoID), unique(alterraw$egoID))
setdiff(unique(egoraw$egoID), unique(alterraw$egoID))
#that's why i=1 does not work, bc this ID does not have alterraw part


#graph
ego_ids <- unique(egoraw$egoID)
i <- 16
ego_i <- ego_ids[i]

#nodes early
nodes_early <- alterraw %>%
  filter(
    egoID == ego_i,
    generator_early == 1
  ) %>%
  distinct(AID, .keep_all = TRUE)
#check if the amount is right, should be 15
table(nodes_early$generator_early)
nrow(nodes_early)
#edges early
edges_early <- aae %>%
  filter(egoID == ego_i) %>%
  filter(
    from %in% nodes_early$AID &
      to %in% nodes_early$AID
  ) %>%
  select(from, to)

g_early <- graph_from_edgelist(
  as.matrix(edges_early[, c("from", "to")]),
  directed = FALSE
)

missing_enodes <- setdiff(nodes_early$AID, V(g_early)$name)
g_early <- add_vertices(g_early, nv = length(missing_enodes), name = missing_enodes)
par(mar = c(1, 1, 1, 1))
plot(g_early, vertex.label = NA)



#nodes late
nodes_late <- alterraw %>%
  filter(
    egoID == ego_i,
    generator_late == 1
  ) %>%
  distinct(AID, .keep_all = TRUE)
#check if the amount is right, should be 15
table(nodes_late$generator_late)
nrow(nodes_late)
#edges late
edges_late <- aal %>%
  filter(egoID == ego_i) %>%
  filter(
    from %in% nodes_late$AID &
      to %in% nodes_late$AID
  ) %>%
  select(from, to)

g_late <- graph_from_edgelist(
  as.matrix(edges_late[, c("from", "to")]),
  directed = FALSE
)

missing_lnodes <- setdiff(nodes_late$AID, V(g_late)$name)
g_late <- add_vertices(g_late, nv = length(missing_lnodes), name = missing_lnodes)
par(mar = c(1, 1, 1, 1))
plot(g_late, vertex.label = NA)

# language speak 
parse_lang <- function(x) {
  if (is.na(x)) return(NA_character_)
  
  x <- tolower(x)
  x <- gsub("\\s+", "", x)  # remove spaces
  
  has_eng <- grepl("english", x)
  has_spa <- grepl("spanish", x)
  
  if (has_eng & has_spa) return("Both")
  if (has_spa) return("Spanish")
  if (has_eng) return("English")
  
  return("Other")
}
nodes_early$lang_cat_early <- sapply(nodes_early$`6.1_language_early`, parse_lang)
nodes_late$lang_cat_late   <- sapply(nodes_late$`9.1_language_late`,  parse_lang)


# EARLY attributes 
idx_e <- match(V(g_early)$name, nodes_early$AID)
V(g_early)$race <- nodes_early$`5.4_hispanic_general_early`[idx_e]
V(g_early)$lang <- nodes_early$lang_cat_early[idx_e]

# LATE attributes 
idx_l <- match(V(g_late)$name, nodes_late$AID)
V(g_late)$race <- nodes_late$`8.4_hispanic_general_late`[idx_l]
V(g_late)$lang <- nodes_late$lang_cat_late[idx_l]

# language proportion
p_spanish_any_early <- mean(V(g_early)$lang %in% c("Spanish","Both"), na.rm = TRUE)
p_english_any_early <- mean(V(g_early)$lang %in% c("English","Both"), na.rm = TRUE)

p_spanish_any_late <- mean(V(g_late)$lang %in% c("Spanish","Both"), na.rm = TRUE)
p_english_any_late <- mean(V(g_late)$lang %in% c("English","Both"), na.rm = TRUE)


# latino orgin? Calculate the proportion
p_latino_early <- mean(V(g_early)$race == 1, na.rm = TRUE) 
p_latino_late <- mean(V(g_late)$race == 1, na.rm = TRUE)

#winthin density
within_latino_density <- function(g) {
  v_lat <- V(g)$name[V(g)$race == 1]
  if (length(v_lat) < 2) return(NA_real_)
  edge_density(induced_subgraph(g, v_lat), loops = FALSE)
}
latino_den_early <- within_latino_density(g_early)
latino_den_late  <- within_latino_density(g_late)

latino_den_early
latino_den_late

n_latino_early <- sum(V(g_early)$race == 1, na.rm = TRUE)
n_latino_late  <- sum(V(g_late)$race == 1,  na.rm = TRUE)

n_latino_early
n_latino_late








#write into a function to all-----------------------------
# library(dplyr)
# library(igraph)

run_one_ego_prop_density <- function(ego_i, alterraw, aae, aal,
                                     plot_graph = FALSE) {
  
  # ---------------- EARLY ----------------
  nodes_early <- alterraw %>%
    filter(
      egoID == ego_i,
      generator_early == 1
    ) %>%
    distinct(AID, .keep_all = TRUE)
  
  edges_early <- aae %>%
    filter(egoID == ego_i) %>%
    filter(
      from %in% nodes_early$AID &
        to %in% nodes_early$AID
    ) %>%
    select(from, to)
  
  # same as your code: build from edgelist
  # (safety: if 0 edges, graph_from_edgelist would error)
  if (nrow(edges_early) == 0) {
    g_early <- make_empty_graph(directed = FALSE)
  } else {
    g_early <- graph_from_edgelist(
      as.matrix(edges_early[, c("from", "to")]),
      directed = FALSE
    )
  }
  
  missing_enodes <- setdiff(nodes_early$AID, V(g_early)$name)
  g_early <- add_vertices(g_early, nv = length(missing_enodes), name = missing_enodes)
  
  if (plot_graph) plot(g_early, vertex.label = NA)
  
  # EARLY: assign race exactly as before (needed for p_latino + density + count)
  idx_e <- match(V(g_early)$name, nodes_early$AID)
  V(g_early)$race <- nodes_early$`5.4_hispanic_general_early`[idx_e]
  parse_lang <- function(x) {
    if (is.na(x)) return(NA_character_)
    
    x <- tolower(x)
    x <- gsub("\\s+", "", x)  # remove spaces
    
    has_eng <- grepl("english", x)
    has_spa <- grepl("spanish", x)
    
    if (has_eng & has_spa) return("Both")
    if (has_spa) return("Spanish")
    if (has_eng) return("English")
    
    return("Other")
  }
  nodes_early$lang_cat_early <- sapply(nodes_early$`6.1_language_early`, parse_lang)
  V(g_early)$lang <- nodes_early$lang_cat_early[idx_e]
  
  # make language category into 2 levels (early)
  # lang1 = Spanish involvement (1 = spanish or both; 0 = english)
  V(g_early)$lang1 <- NA_integer_
  V(g_early)$lang1[tolower(V(g_early)$lang) %in% c("spanish", "both")] <- 1
  V(g_early)$lang1[tolower(V(g_early)$lang) == "english"]              <- 0
  # lang2 = English involvement (1 = english or both; 0 = spanish)
  V(g_early)$lang2 <- NA_integer_
  V(g_early)$lang2[tolower(V(g_early)$lang) %in% c("english", "both")] <- 1
  V(g_early)$lang2[tolower(V(g_early)$lang) == "spanish"]              <- 0
  
  
  # ---------------- LATE ----------------
  nodes_late <- alterraw %>%
    filter(
      egoID == ego_i,
      generator_late == 1
    ) %>%
    distinct(AID, .keep_all = TRUE)
  
  edges_late <- aal %>%
    filter(egoID == ego_i) %>%
    filter(
      from %in% nodes_late$AID &
        to %in% nodes_late$AID
    ) %>%
    select(from, to)
  
  if (nrow(edges_late) == 0) {
    g_late <- make_empty_graph(directed = FALSE)
  } else {
    g_late <- graph_from_edgelist(
      as.matrix(edges_late[, c("from", "to")]),
      directed = FALSE
    )
  }
  
  missing_lnodes <- setdiff(nodes_late$AID, V(g_late)$name)
  g_late <- add_vertices(g_late, nv = length(missing_lnodes), name = missing_lnodes)
  
  if (plot_graph) plot(g_late, vertex.label = NA)
  
  # LATE: assign race exactly as before
  idx_l <- match(V(g_late)$name, nodes_late$AID)
  V(g_late)$race <- nodes_late$`8.4_hispanic_general_late`[idx_l]
  nodes_late$lang_cat_late <- sapply(nodes_late$`9.1_language_late`, parse_lang)
  V(g_late)$lang <- nodes_late$lang_cat_late[idx_l]

  # lang1 = Spanish involvement (1 = spanish or both; 0 = english)
  V(g_late)$lang1 <- NA_integer_
  V(g_late)$lang1[tolower(V(g_late)$lang) %in% c("spanish", "both")] <- 1
  V(g_late)$lang1[tolower(V(g_late)$lang) == "english"]              <- 0
  # lang2 = English involvement (1 = english or both; 0 = spanish)
  V(g_late)$lang2 <- NA_integer_
  V(g_late)$lang2[tolower(V(g_late)$lang) %in% c("english", "both")] <- 1
  V(g_late)$lang2[tolower(V(g_late)$lang) == "spanish"]              <- 0
  
  # Language entropy
  # Measures how diverse the language categories are in one ego network
  # Normalized by the fixed number of meaningful categories: English, Spanish, Both
  
  lang_entropy <- function(lang_vec, normalized = TRUE) {
    lang_vec <- lang_vec[!is.na(lang_vec)]

    # Keep only meaningful language categories for this study
    lang_vec <- lang_vec[lang_vec %in% c("English", "Spanish", "Both")]

    if (length(lang_vec) == 0) return(NA_real_)

    p <- prop.table(table(lang_vec))
    h <- -sum(p * log(p))

    if (normalized) {
      # fixed denominator for comparability across ego networks
      h <- h / log(3)
    }

    return(as.numeric(h))
  }
  # 
  # 
  # ---------------- METRICS ----------------
  lang_entropy_early <- lang_entropy(V(g_early)$lang, normalized = TRUE)
  lang_entropy_late  <- lang_entropy(V(g_late)$lang,  normalized = TRUE)

  
 
  p_latino_early <- mean(V(g_early)$race == 1, na.rm = TRUE)
  p_latino_late  <- mean(V(g_late)$race == 1,  na.rm = TRUE)
  
  within_latino_density <- function(g) {
    v_lat <- V(g)$name[V(g)$race == 1]
    if (length(v_lat) < 2) return(NA_real_)
    edge_density(induced_subgraph(g, v_lat), loops = FALSE)
  }
  
  latino_den_early <- within_latino_density(g_early)
  latino_den_late  <- within_latino_density(g_late)
  
  n_latino_early <- sum(V(g_early)$race == 1, na.rm = TRUE)
  n_latino_late  <- sum(V(g_late)$race == 1,  na.rm = TRUE)
  
  ps_early <- mean(V(g_early)$lang %in% c("Spanish"), na.rm = TRUE)
  pe_early <- mean(V(g_early)$lang %in% c("English"), na.rm = TRUE)
  pb_early <- mean(V(g_early)$lang %in% c("Both"), na.rm = TRUE)
  
  p_spanish_any_early <- mean(V(g_early)$lang %in% c("Spanish","Both"), na.rm = TRUE)
  p_english_any_early <- mean(V(g_early)$lang %in% c("English","Both"), na.rm = TRUE)
  
  ps_late <- mean(V(g_late)$lang %in% c("Spanish"), na.rm = TRUE)
  pe_late <- mean(V(g_late)$lang %in% c("English"), na.rm = TRUE)
  pb_late <- mean(V(g_late)$lang %in% c("Both"), na.rm = TRUE)
  
  
  
  p_spanish_any_late <- mean(V(g_late)$lang %in% c("Spanish","Both"), na.rm = TRUE)
  p_english_any_late <- mean(V(g_late)$lang %in% c("English","Both"), na.rm = TRUE)
  
  den_all_early <- edge_density(g_early, loops = FALSE)
  den_all_late  <- edge_density(g_late,  loops = FALSE)
    
  lang_all_same_early <- {
    lv <- V(g_early)$lang
    lv <- lv[!is.na(lv)]
    if (length(lv) == 0) NA_integer_ else as.integer(length(unique(lv)) == 1)
            }
  lang_all_same_late <- {
    lv <- V(g_late)$lang
    lv <- lv[!is.na(lv)]
    if (length(lv) == 0) NA_integer_ else as.integer(length(unique(lv)) == 1)
            }
    
  lang_assort1 <- function(g, attr) {
    if (ecount(g) == 0) return(NA_real_)
    x <- vertex_attr(g, attr)          # e.g., lang1 or lang2
    keep <- !is.na(x)
    if (sum(keep) < 2) return(NA_real_)
    g2 <- induced_subgraph(g, vids = V(g)[keep])
    x2 <- vertex_attr(g2, attr)
    if (length(unique(x2)) < 2) return(NA_real_)
    if (ecount(g2) == 0) return(NA_real_)
    assortativity_nominal(g2, as.factor(x2), directed = FALSE)
  }
  
  # early: Spanish-involved assort and English-involved assort
  assort_s_early <- lang_assort1(g_early, "lang1")
  assort_e_early <- lang_assort1(g_early, "lang2")
  
  # late: Spanish-involved assort and English-involved assort
  assort_s_late  <- lang_assort1(g_late,  "lang1")
  assort_e_late  <- lang_assort1(g_late,  "lang2")
  
  #language assort by three levels, s, e, bi
  lang_assort <- function(g) {
    # no ties -> no structure
    if (ecount(g) == 0) return(0)
    lv <- V(g)$lang
    keep <- !is.na(lv)
    if (sum(keep) < 2) return(0)
    g2 <- induced_subgraph(g, vids = V(g)[keep])
    lv2 <- V(g2)$lang
    if (ecount(g2) == 0) return(0)
    # only one niche -> maximal alignment
    if (length(unique(lv2)) < 2) return(1)
    assortativity_nominal(g2, as.factor(lv2), directed = FALSE)
  }
  
  lang_assort_early <- lang_assort(g_early)
  lang_assort_late  <- lang_assort(g_late)
  
  
  
  lang_cross_edge_rate <- function(g) {
    if (ecount(g) == 0) return(NA_real_)
    el <- as.data.frame(get.edgelist(g), stringsAsFactors = FALSE)
    a <- el[,1]; b <- el[,2]
    la <- V(g)$lang[match(a, V(g)$name)]
    lb <- V(g)$lang[match(b, V(g)$name)]
    # keep edges where both endpoints have non-missing language
    ok <- !is.na(la) & !is.na(lb)
    if (!any(ok)) return(NA_real_)
    la <- tolower(la[ok])
    lb <- tolower(lb[ok])
    # cross ONLY if one is english and the other is spanish
    cross <- (la == "english" & lb == "spanish") |
      (la == "spanish" & lb == "english")
    mean(cross)
  }
  
  lang_cross_early <- lang_cross_edge_rate(g_early)
  lang_cross_late  <- lang_cross_edge_rate(g_late)
  

 # return: graphs + check stats
  list(
    egoID = ego_i,
    
    # optional objects for debugging/plotting
    g_early = g_early,
    g_late  = g_late,
    nodes_early = nodes_early,
    nodes_late  = nodes_late,
    
    # quick checks
    n_nodes_early = vcount(g_early),
    n_nodes_late  = vcount(g_late),
    n_edges_early = ecount(g_early),
    n_edges_late  = ecount(g_late),
    
    # outcomes
    p_latino_early = p_latino_early,
    p_latino_late  = p_latino_late,
    latino_den_early = latino_den_early,
    latino_den_late  = latino_den_late,
    n_latino_early = n_latino_early,
    n_latino_late  = n_latino_late,
    ps_early = ps_early,
    pe_early = pe_early,
    pb_early= pb_early,
    ps_late = ps_late,
    pe_late = pe_late,
    pb_late= pb_late,
    p_spanish_any_early = p_spanish_any_early,
    p_english_any_early = p_english_any_early,
    p_spanish_any_late  = p_spanish_any_late,
    p_english_any_late  = p_english_any_late,
    den_all_early = den_all_early,
    den_all_late  = den_all_late,
    lang_all_same_early = lang_all_same_early,
    lang_all_same_late  = lang_all_same_late,
    lang_assort_early = lang_assort_early,
    lang_assort_late = lang_assort_late,
    assort_s_early = assort_s_early,
    assort_e_early = assort_e_early,
    assort_s_late  = assort_s_late,
    assort_e_late  = assort_e_late,
    lang_cross_early = lang_cross_early,
    lang_cross_late  = lang_cross_late,
    lang_entropy_early = lang_entropy_early,
    lang_entropy_late  = lang_entropy_late
  )
}


library(purrr)
library(tibble)

ego_ids <- unique(egoraw$egoID)

results_list <- map(ego_ids, ~ run_one_ego_prop_density(.x, alterraw, aae, aal, plot_graph = FALSE))

results_df <- map_dfr(results_list, \(x) {
  tibble(
    egoID = x$egoID,
    n_nodes_early = x$n_nodes_early,
    n_edges_early = x$n_edges_early,
    p_latino_early = x$p_latino_early,
    latino_den_early = x$latino_den_early,
    n_latino_early = x$n_latino_early,
    n_nodes_late = x$n_nodes_late,
    n_edges_late = x$n_edges_late,
    p_latino_late = x$p_latino_late,
    latino_den_late = x$latino_den_late,
    n_latino_late = x$n_latino_late,
    p_spanish_any_early = x$p_spanish_any_early,
    p_english_any_early = x$p_english_any_early,
    p_spanish_any_late  = x$p_spanish_any_late,
    p_english_any_late  = x$p_english_any_late,
    den_all_early = x$den_all_early,
    den_all_late  = x$den_all_late,
    lang_all_same_early = x$lang_all_same_early,
    lang_all_same_late  = x$lang_all_same_late,
    lang_assort_early = x$lang_assort_early,
    lang_assort_late = x$lang_assort_late, 
    assort_s_early = x$assort_s_early,
    assort_e_early = x$assort_e_early,
    assort_s_late = x$assort_s_late,
    assort_e_late = x$assort_e_late,
    lang_cross_early = x$lang_cross_early,
    lang_cross_late  = x$lang_cross_late,
    ps_early = x$ps_early,
    pe_early = x$pe_early,
    pb_early= x$pb_early,
    ps_late = x$ps_late,
    pe_late = x$pe_late,
    pb_late= x$pb_late,
    lang_entropy_early = x$lang_entropy_early,
    lang_entropy_late  = x$lang_entropy_late
  )
})

results_df

results_df <- results_df %>%
  mutate(
    # change in composition
    delta_p_latino = p_latino_late - p_latino_early,
    # change in within-Latino cohesion
    delta_latino_den = latino_den_late - latino_den_early,
    # change in absolute exposure
    delta_n_latino = n_latino_late - n_latino_early,
    delta_ps= ps_late-ps_early,
    delta_pe=pe_late-pe_early,
    delta_pb=pb_late-pb_early, 
    delta_p_spanish=p_spanish_any_late - p_spanish_any_early,
    delta_p_english=p_english_any_late - p_english_any_early,
    delta_s_assort=assort_s_late - assort_s_early,
    delta_e_assort=assort_e_late - assort_e_early,
    delta_lang_assort=lang_assort_late - lang_assort_early,
    delta_all_den= den_all_late - den_all_early,
    delta_lang_entropy = lang_entropy_late - lang_entropy_early
    # delta_lang_entropy_total = lang_entropy_total_late - lang_entropy_total_early
  )

options(scipen = 999)

results_df <- results_df %>%
  left_join(
    egodata %>%
      select(
        egoID,
        s_stress_sum,
        s_stress_mean,
        e_stress_sum,
        e_stress_mean,
        total_stress_sum,
        total_stress_mean,
        age,
        gender
      ),
    by = "egoID"
  )


library(sandwich)
library(lmtest)
results_df$s_social_any <- as.integer(results_df$s_stress_sum > 0)
results_df$e_social_any <- as.integer(results_df$e_stress_sum > 0)
results_df <- results_df[-1, ]
cor(
  results_df[, c("p_latino_early",
                 "p_spanish_any_early",
                 "p_english_any_early",
                 "lang_assort_early",
                 "lang_cross_early", 
                 "assort_s_early",
                 "assort_e_early",
                 "den_all_early",
                 "ps_early",
                 "pe_early",
                 "pb_early",
                 "lang_entropy_early"
                 )],
  use = "complete.obs"
)

hist(results_df$s_stress_mean)
summary(results_df$total_stress_mean)

####Descriptive Stats
table(results_df$gender)
summary(results_df$age)
38/51
2/51
summary(results_df$p_latino_early)
summary(results_df$p_latino_late)
summary(results_df$n_edges_early)
summary(results_df$n_edges_late)
summary(results_df$ps_early)
summary(results_df$ps_late)
summary(results_df$pe_early)
summary(results_df$pe_late)
summary(results_df$pb_early)
summary(results_df$pb_late)
summary(results_df$den_all_early)
summary(results_df$den_all_late)
summary(results_df$lang_assort_early)
summary(results_df$lang_assort_late)
summary(results_df$lang_cross_early)
summary(results_df$lang_cross_late)
summary(results_df$s_stress_mean)
summary(results_df$e_stress_mean)
summary(results_df$s_stress_sum)
summary(results_df$e_stress_sum)
summary(results_df$lang_entropy_early)
summary(results_df$lang_entropy_late)
table(results_df$s_social_any)
table(results_df$e_social_any)
26/51

sd_results <- results_df %>%
  summarise(
    sd_lang_assort_early  = sd(lang_assort_early, na.rm = TRUE),
    sd_lang_assort_late   = sd(lang_assort_late, na.rm = TRUE),
    sd_lang_cross_early   = sd(lang_cross_early, na.rm = TRUE),
    sd_lang_cross_late    = sd(lang_cross_late, na.rm = TRUE),
    sd_s_stress_mean      = sd(s_stress_mean, na.rm = TRUE),
    sd_e_stress_mean      = sd(e_stress_mean, na.rm = TRUE),
    sd_s_stress_sum       = sd(s_stress_sum, na.rm = TRUE),
    sd_e_stress_sum       = sd(e_stress_sum, na.rm = TRUE),
    sd_lang_entropy_early = sd(lang_entropy_early, na.rm = TRUE),
    sd_lang_entropy_late  = sd(lang_entropy_late, na.rm = TRUE)
  )

print(sd_results, width = Inf)

sd(results_df$n_edges_early, na.rm = TRUE)
sd(results_df$n_edges_late, na.rm = TRUE)



#Bayesian model---------------------------------------------
library(brms)
#Spanish stress in early period 
bm1 <- brm(
  s_social_any ~
    lang_entropy_early+
    lang_assort_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)


summary(bm1)
posterior_summary(bm1, probs = c(0.025, 0.975))

plot(bm1)

#Spanish stress in late period 
bm2 <- brm(
  s_social_any ~
    lang_entropy_late+
    lang_assort_late +
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)


summary(bm2)
posterior_summary(bm2, probs = c(0.025, 0.975))

#Spanish early vs late period stats
posterior_early <- posterior_summary(bm1, probs = c(0.025, 0.975)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  filter(stringr::str_detect(term, "^b_")) %>%
  mutate(
    model = "Early network",
    predictor = dplyr::recode(
      term,
      # "b_ps_early" = "Prop Spanish interactions",
      # "b_pb_early" = "Prop bilingual interactions",
      # "b_den_all_early" = "Network density",  
      "b_lang_entropy_early" = "Language entropy",
      "b_lang_assort_early" = "Language assortativity",
   
      .default = NA_character_
    )
  ) %>%
  filter(!is.na(predictor))

posterior_late <- posterior_summary(bm2, probs = c(0.025, 0.975)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  filter(stringr::str_detect(term, "^b_")) %>%
  mutate(
    model = "Late network",
    predictor = dplyr::recode(
      term,
      "b_lang_entropy_late" = "Language entropy",
      "b_lang_assort_late" = "Language assortativity",
      
      .default = NA_character_
    )
  ) %>%
  filter(!is.na(predictor))

compare_df <- bind_rows(posterior_early, posterior_late)

ggplot(compare_df, aes(x = Estimate, y = predictor)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(xmin = Q2.5, xmax = Q97.5),
    position = position_dodge(width = 0.6),
    linewidth = 0.8
  ) +
  facet_wrap(~ model) +
  labs(
    x = "Posterior estimate 95% CI",
    y = NULL,
    title = "Spanish language stress",
    subtitle = "Early vs Late"
  ) +
  theme_classic(base_size = 15)

##====================half-eye plot==========
library(tidybayes)
library(ggdist)
library(patchwork)

# Extract posterior draws from early model
draws_bm1 <- bm1 %>%
  gather_draws(
    b_lang_entropy_early,
    b_lang_assort_early
  ) %>%
  mutate(
    period = "Early network",
    predictor = case_when(
      .variable == "b_lang_entropy_early" ~ "Entropy",
      .variable == "b_lang_assort_early" ~ "Assortativity"
    )
  )

# Extract posterior draws from late model
draws_bm2 <- bm2 %>%
  gather_draws(
    b_lang_entropy_late,
    b_lang_assort_late
  ) %>%
  mutate(
    period = "Late network",
    predictor = case_when(
      .variable == "b_lang_entropy_late" ~ "Entropy",
      .variable == "b_lang_assort_late" ~ "Assortativity"
    )
  )

# Combine posterior draws
draws_spanish <- bind_rows(draws_bm1, draws_bm2) %>%
  mutate(
    period = factor(period, levels = c("Early network", "Late network")),
    predictor = factor(
      predictor,
      levels = c( "Assortativity", "Entropy")
    )
  )

# Half-eye posterior plot
p_spanish <- ggplot(
  draws_spanish,
  aes(x = .value, y = predictor, fill = predictor)
) +
  stat_halfeye(
    .width = 0.95,
    point_interval = mean_qi,
    slab_alpha = 0.75,
    normalize = "panels",
    color = "gray25"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    color = "gray35"
  ) +
  facet_wrap(~ period, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Entropy" = "#4C78A8",        # muted blue
      "Assortativity" = "#B279A2"   # muted purple
    )
  ) +
  labs(
    # title = "Spanish stress",
    # subtitle = "Posterior distributions of Bayesian logistic regression estimates",
    x = "Posterior estimate on log-odds scale",
    y = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    legend.position = "none",
    panel.spacing = unit(1.2, "lines")
  )

p_spanish



#English stress in early period 
bm3 <- brm(
  e_social_any ~
    # pe_early+
    # pb_early+
    # den_all_early +
    lang_assort_early+
    lang_entropy_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm3)

#English stress in late period 
bm4 <- brm(
  e_social_any ~
    lang_assort_late +
    lang_entropy_late+
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm4)

# posterior_eearly <- posterior_summary(bm3, probs = c(0.025, 0.975)) %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "term") %>%
#   filter(stringr::str_detect(term, "^b_")) %>%
#   mutate(
#     model = "Early network",
#     predictor = dplyr::recode(
#       term,
#       "b_pe_early" = "Prop English interactions",
#       "b_pb_early" = "Prop bilingual interactions",
#       "b_den_all_early" = "Network density",
#       "b_lang_assort_early" = "Language assortativity",
#       "b_lang_cross_early" = "Cross-language rate",
#       .default = NA_character_
#     )
#   ) %>%
#   filter(!is.na(predictor))
# 
# posterior_elate <- posterior_summary(bm4, probs = c(0.025, 0.975)) %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "term") %>%
#   filter(stringr::str_detect(term, "^b_")) %>%
#   mutate(
#     model = "Late network",
#     predictor = dplyr::recode(
#       term,
#       "b_pe_late" = "Prop English interactions",
#       "b_pb_late" = "Prop bilingual interactions",
#       "b_den_all_late" = "Network density",
#       "b_lang_assort_late" = "Language assortativity",
#       "b_lang_cross_late" = "Cross-language rate",
#       .default = NA_character_
#     )
#   ) %>%
#   filter(!is.na(predictor))
# 
# compare_edf <- bind_rows(posterior_eearly, posterior_elate)
# 
# ggplot(compare_edf, aes(x = Estimate, y = predictor)) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   geom_pointrange(
#     aes(xmin = Q2.5, xmax = Q97.5),
#     position = position_dodge(width = 0.6),
#     linewidth = 0.8
#   ) +
#   facet_wrap(~ model) +
#   labs(
#     x = "Posterior estimate 95% CI",
#     y = NULL,
#     title = "English language stress",
#     subtitle = "Early vs Late"
#   ) +
#   theme_classic(base_size = 15)
# 
# 


##====================half-eye plot==========
# Extract posterior draws from early model
draws_bm3 <- bm3 %>%
  gather_draws(
    b_lang_entropy_early,
    b_lang_assort_early
  ) %>%
  mutate(
    period = "Early network",
    predictor = case_when(
      .variable == "b_lang_entropy_early" ~ "Entropy",
      .variable == "b_lang_assort_early" ~ "Assortativity"
    )
  )

# Extract posterior draws from late model
draws_bm4 <- bm4 %>%
  gather_draws(
    b_lang_entropy_late,
    b_lang_assort_late
  ) %>%
  mutate(
    period = "Late network",
    predictor = case_when(
      .variable == "b_lang_entropy_late" ~ "Entropy",
      .variable == "b_lang_assort_late" ~ "Assortativity"
    )
  )

# Combine posterior draws
draws_english <- bind_rows(draws_bm3, draws_bm4) %>%
  mutate(
    period = factor(period, levels = c("Early network", "Late network")),
    predictor = factor(
      predictor,
      levels = c( "Assortativity", "Entropy")
    )
  )

# Half-eye posterior plot
p_english <- ggplot(
  draws_english,
  aes(x = .value, y = predictor, fill = predictor)
) +
  stat_halfeye(
    .width = 0.95,
    point_interval = mean_qi,
    slab_alpha = 0.75,
    normalize = "panels",
    color = "gray25"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    color = "gray35"
  ) +
  facet_wrap(~ period, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Entropy" = "#4C78A8",        # muted blue
      "Assortativity" = "#B279A2"   # muted purple
    )
  ) +
  labs(
    x = "Posterior estimate on log-odds scale",
    y = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    legend.position = "none",
    panel.spacing = unit(1.2, "lines")
  )

p_english


####sensitivity check
#Spanish stress in early period 
#wider prior 
bm1s1 <- brm(
  s_social_any ~
    lang_entropy_early+
    lang_assort_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 1), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm1s1)
posterior_summary(bm1s1, probs = c(0.025, 0.975))

#narrow prior
bm1s2 <- brm(
  s_social_any ~
    lang_entropy_early+
    lang_assort_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 0.5), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm1s2)
posterior_summary(bm1s2, probs = c(0.025, 0.975))

#Spanish stress in late period 
#wider prior
bm2s1 <- brm(
  s_social_any ~
    lang_entropy_late+
    lang_assort_late +
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 1), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm2s1)
#narrow prior 
bm2s2 <- brm(
  s_social_any ~
    lang_entropy_late+
    lang_assort_late +
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 0.5), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm2s2)

#English stress in early period 
#wider prior
bm3s1 <- brm(
  e_social_any ~
    lang_assort_early+
    lang_entropy_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 1), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm3s1)
#narrow prior 
bm3s2 <- brm(
  e_social_any ~
    lang_assort_early+
    lang_entropy_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 0.5), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm3s2)

#English stress in late period 
#wider prior
bm4s1 <- brm(
  e_social_any ~
    lang_assort_late +
    lang_entropy_late+
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 1), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm4s1)

#narrow prior
bm4s2 <- brm(
  e_social_any ~
    lang_assort_late +
    lang_entropy_late+
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 0.5), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm4s2)

#####additional variables----------------------------------
#Spanish early 
bm5 <- brm(
  s_social_any ~
    lang_entropy_early+
    lang_assort_early+
    ps_early+
    pe_early+
    pb_early+
    den_all_early+
    lang_cross_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm5)

#Spanish late 
bm6 <- brm(
  s_social_any ~
    lang_entropy_late+
    lang_assort_late +
    ps_late+
    pe_late+
    pb_late+
    den_all_late+
    lang_cross_late+
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm6)

#English early 
bm7 <- brm(
  e_social_any ~
    lang_assort_early+
    lang_entropy_early+
    ps_early+
    pe_early+
    pb_early+
    den_all_early+
    lang_cross_early+
    age +
    gender,
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm7)

#English late 
bm8 <- brm(
  e_social_any ~
    lang_assort_late +
    lang_entropy_late+
    ps_late+
    pe_late+
    pb_late+
    den_all_late+
    lang_cross_late+
    age +
    gender, 
  data = results_df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = "b"),        # coefficients
    prior(normal(0, 2), class = "Intercept")  # intercept
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)
summary(bm8)

