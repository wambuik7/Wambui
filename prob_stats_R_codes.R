# ============================================================
# PROBABILITY AND STATISTICS I – COMPLETE R CODE REFERENCE
# Dr. Margaret Kinyua | Karatina University
# ============================================================
# Copy each chunk into your .Rmd between ```{r chunk-name} and ```
# ============================================================


# ── SETUP CHUNKS (place at top of .Rmd) ─────────────────────

# Chunk: setup
knitr::opts_chunk$set(
  echo    = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width  = 6,
  fig.height = 5,
  fig.align  = "center"
)

# Chunk: libraries
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DiagrammeR)
library(e1071)
library(lattice)
library(mvtnorm)
library(tinytex)


# ============================================================
# TOPIC 1 – NATURE AND PRESENTATION OF STATISTICAL DATA
# ============================================================

# ── SECTION 1: DATA COLLECTION DIAGRAM ──────────────────────

# Chunk: data-collection-diagram
library(DiagrammeR)

grViz("
digraph data_collection {
  graph [layout = dot, rankdir = TB, fontname = 'Arial']
  node  [shape = box, style = 'rounded,filled', fontname = 'Arial',
         fontsize = 11, margin = '0.2,0.1', fillcolor = '#eaf4fb']
  edge  [arrowhead = normal, arrowsize = 0.7]

  A [label = 'Data Collection Methods', fillcolor = '#1f618d',
     fontcolor = 'white', style = 'rounded,filled,bold']

  B [label = 'Experimental',   fillcolor = '#2e86c1', fontcolor = 'white']
  C [label = 'Non-Experimental', fillcolor = '#2e86c1', fontcolor = 'white']

  D [label = 'Lab Experiment']
  E [label = 'Field Experiment']
  F [label = 'Simulation']

  G [label = 'Field Study']
  H [label = 'Library Research']
  I [label = 'Census']
  J [label = 'Sample Survey']
  K [label = 'Case Study']

  A -> B
  A -> C
  B -> D
  B -> E
  B -> F
  C -> G
  C -> H
  C -> I
  C -> J
  C -> K
}
")


# ── SECTION 2: UNGROUPED FREQUENCY DISTRIBUTION ─────────────

# Chunk: ungrouped-freq
scores <- c(3, 3, 6, 4, 5, 4, 10, 5, 29, 3,
            5, 6, 10, 31, 4, 10, 3, 29, 5, 31,
            29, 11, 31, 6, 10)

freq_table <- table(scores)
freq_df    <- as.data.frame(freq_table)
colnames(freq_df) <- c("Scores (x)", "Frequency (f)")

# Add a Tallies column (visual approximation)
tally_marks <- function(n) {
  groups   <- n %/% 5
  leftover <- n %%  5
  paste0(strrep("////  ", groups), strrep("/", leftover))
}
freq_df$Tallies <- sapply(freq_df$`Frequency (f)`, tally_marks)
freq_df <- freq_df[, c("Scores (x)", "Tallies", "Frequency (f)")]

# Add total row
total_row <- data.frame(`Scores (x)` = "Total",
                         Tallies      = "",
                         `Frequency (f)` = sum(freq_df$`Frequency (f)`),
                         check.names  = FALSE)
names(total_row) <- names(freq_df)
freq_df <- rbind(freq_df, total_row)

kable(freq_df,
      caption = "Ungrouped Frequency Distribution of Exam Scores (n = 25)",
      align   = c("c", "l", "c")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"),
                full_width = FALSE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white") |>
  row_spec(nrow(freq_df), bold = TRUE, background = "#d6eaf8")


# ── SECTION 3: CATEGORICAL FREQUENCY DISTRIBUTION ───────────

# Chunk: categorical-freq
fields <- c("Statistics", "Mathematics", "Statistics", "Computer Science",
            "Actuarial Science", "Statistics", "Mathematics", "Mathematics",
            "Computer Science", "Statistics", "Actuarial Science", "Statistics",
            "Mathematics", "Computer Science", "Statistics", "Mathematics",
            "Actuarial Science", "Computer Science", "Statistics", "Mathematics",
            "Statistics", "Actuarial Science", "Computer Science", "Mathematics",
            "Statistics", "Mathematics", "Computer Science", "Statistics",
            "Actuarial Science", "Statistics")

cat_freq <- as.data.frame(table(fields))
colnames(cat_freq) <- c("Field of Study", "Frequency")
cat_freq <- cat_freq[order(cat_freq$`Field of Study`), ]

# Add total row
cat_freq <- rbind(cat_freq,
                  data.frame(`Field of Study` = "Total",
                             Frequency        = sum(cat_freq$Frequency),
                             check.names      = FALSE))

kable(cat_freq,
      caption = "Categorical Frequency Distribution – Major Field of Study (n = 30)",
      align   = c("l", "c")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"),
                full_width = FALSE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white") |>
  row_spec(nrow(cat_freq), bold = TRUE, background = "#d6eaf8")


# Chunk: categorical-bar  (bar chart for the field-of-study data)
cat_plot <- as.data.frame(table(fields))
colnames(cat_plot) <- c("Field", "Frequency")

ggplot(cat_plot, aes(x = reorder(Field, Frequency), y = Frequency, fill = Field)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Frequency), hjust = -0.3, size = 4) +
  scale_fill_manual(values = c("#2e86c1", "#117a65", "#e67e22", "#8e44ad")) +
  coord_flip() +
  labs(title = "Distribution of Major Field of Study (n = 30)",
       x = "Field of Study", y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# ── SECTION 4: GROUPED FREQUENCY DISTRIBUTION (Example 1.3) ─

# Chunk: grouped-freq-exclusive
time_data <- c(20, 25, 24, 33, 13, 16, 21, 17, 11, 34,
               26,  8, 19, 31, 11, 14, 15, 21, 18, 17)

# Summary statistics
cat("n =", length(time_data), "\n")
cat("Min =", min(time_data), " Max =", max(time_data), "\n")
cat("Range =", diff(range(time_data)), "\n")

k <- ceiling(1 + 3.322 * log10(length(time_data)))
cat("k (Sturges) =", k, "\n")

CW <- ceiling(diff(range(time_data)) / k)
cat("Class width =", CW, "\n")

# Build exclusive-method table manually
exclusive_df <- data.frame(
  `Time taken (seconds)` = c("5–10", "10–15", "15–20", "20–25", "25–30", "30–35"),
  `Interval notation`    = c("5 ≤ t < 10", "10 ≤ t < 15", "15 ≤ t < 20",
                              "20 ≤ t < 25", "25 ≤ t < 30", "30 ≤ t < 35"),
  Tallies   = c("/", "////", "/////", "////", "//", "///"),
  Frequency = c(1, 4, 6, 4, 2, 3),
  `Cumulative Freq` = c(1, 5, 11, 15, 17, 20),
  `Mid-point` = c(7.5, 12.5, 17.5, 22.5, 27.5, 32.5),
  check.names = FALSE
)

kable(exclusive_df,
      caption = "Exclusive Method – Time Taken (seconds) by Students") |>
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"),
                full_width = TRUE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white")


# Chunk: grouped-freq-inclusive
inclusive_df <- data.frame(
  `Time taken (seconds)` = c("5–9", "10–14", "15–19", "20–24", "25–29", "30–34"),
  Tallies   = c("/", "////", "/////", "////", "//", "///"),
  Frequency = c(1, 4, 6, 4, 2, 3),
  `Cumulative Freq`  = c(1, 5, 11, 15, 17, 20),
  `Mid-point`        = c(7.5, 12.5, 17.5, 22.5, 27.5, 32.5),
  `Class boundaries` = c("4.5–9.5", "9.5–14.5", "14.5–19.5",
                          "19.5–24.5", "24.5–29.5", "29.5–34.5"),
  check.names = FALSE
)

kable(inclusive_df,
      caption = "Inclusive Method – Time Taken (seconds) by Students") |>
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"),
                full_width = TRUE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white")


# Chunk: sturges-demo  (interactive demo of Sturges' rule)
sturges_demo <- function(n) {
  k   <- ceiling(1 + 3.322 * log10(n))
  cat(sprintf("n = %d  →  k = %d classes\n", n, k))
}
sturges_demo(20)
sturges_demo(50)
sturges_demo(80)
sturges_demo(100)


# ── SECTION 5: HISTOGRAM ─────────────────────────────────────

# Chunk: hist-equal  (Example 1.4 i – equal class widths)
marks_freq <- c(5, 11, 19, 21, 16, 10, 8, 6, 3, 1)
breaks     <- seq(0, 100, by = 10)

hist_df <- data.frame(
  lower = breaks[-length(breaks)],
  upper = breaks[-1],
  freq  = marks_freq
)

ggplot(hist_df, aes(xmin = lower, xmax = upper, ymin = 0, ymax = freq)) +
  geom_rect(fill = "#2e86c1", color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = breaks) +
  labs(title = "Histogram of Student Marks (Equal Class Widths)",
       x = "Marks", y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# Chunk: hist-unequal  (Example 1.4 ii – unequal class widths / frequency density)
unequal_df <- data.frame(
  lower = c(14.5, 19.5, 29.5, 34.5, 39.5, 54.5),
  upper = c(19.5, 29.5, 34.5, 39.5, 54.5, 59.5),
  freq  = c(5, 8, 22, 35, 20, 10),
  width = c(5, 10,  5,  5, 15,  5)
)
unequal_df$fd <- unequal_df$freq / unequal_df$width

ggplot(unequal_df, aes(xmin = lower, xmax = upper, ymin = 0, ymax = fd)) +
  geom_rect(fill = "#117a65", color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = c(14.5, 19.5, 29.5, 34.5, 39.5, 54.5, 59.5)) +
  labs(title = "Histogram Using Frequency Density (Unequal Class Widths)",
       x = "Class Boundaries", y = "Frequency Density (f / i)") +
  theme_classic(base_size = 13) +
  theme(plot.title  = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


# ── SECTION 6: FREQUENCY POLYGON ────────────────────────────

# Chunk: freq-polygon
fp_data <- data.frame(
  midpoint  = c(  0,  5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
  frequency = c(  0,  5, 11, 19, 21, 16, 10,  8,  6,  3,  1,   0)
)
hist_bars <- data.frame(
  lower = seq(0, 90, by = 10),
  upper = seq(10, 100, by = 10),
  freq  = marks_freq
)

ggplot() +
  geom_rect(data = hist_bars,
            aes(xmin = lower, xmax = upper, ymin = 0, ymax = freq),
            fill = "#aed6f1", color = "white", linewidth = 0.4, alpha = 0.6) +
  geom_line(data  = fp_data, aes(x = midpoint, y = frequency),
            color = "#1a5276", linewidth = 1.2) +
  geom_point(data = fp_data, aes(x = midpoint, y = frequency),
             color = "#e74c3c", size = 2.5) +
  scale_x_continuous(breaks = seq(0, 105, by = 10)) +
  labs(title = "Frequency Polygon of Student Marks",
       x = "Marks (Class Midpoints)", y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# ── SECTION 7: BAR CHARTS ────────────────────────────────────

# Chunk: simple-bar  (birth rates)
birth_df <- data.frame(
  Country    = c("Kenya", "India", "China", "Uganda", "U.K.", "Sweden"),
  Birth_Rate = c(30, 33, 40, 29, 20, 15)
)
birth_df$Country <- factor(birth_df$Country,
                            levels = birth_df$Country[order(birth_df$Birth_Rate)])

ggplot(birth_df, aes(x = Country, y = Birth_Rate)) +
  geom_bar(stat = "identity", fill = "#2e86c1", width = 0.6) +
  geom_text(aes(label = Birth_Rate), vjust = -0.4, size = 4, color = "#1a5276") +
  labs(title = "Birth Rate per Thousand by Country",
       x = "Country", y = "Birth Rate (per thousand)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# Chunk: simple-bar-meningitis
mening_df <- data.frame(
  Year     = factor(2001:2005),
  Patients = c(141, 225, 205, 108, 192)
)

ggplot(mening_df, aes(x = Year, y = Patients)) +
  geom_bar(stat = "identity", fill = "#117a65", width = 0.6) +
  geom_text(aes(label = Patients), vjust = -0.4, size = 4, color = "#117a65") +
  labs(title = "Bacterial Meningitis Patients (2001–2005)",
       x = "Year", y = "Number of Patients") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# Chunk: multiple-bar  (meningitis vs malaria)
multi_long <- data.frame(
  Year    = rep(factor(2001:2005), 2),
  Disease = c(rep("Meningitis", 5), rep("Malaria", 5)),
  Patients = c(141, 225, 205, 108, 192,
               321, 251, 123, 547, 148)
)

ggplot(multi_long, aes(x = Year, y = Patients, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Patients),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Meningitis" = "#2e86c1", "Malaria" = "#e67e22")) +
  labs(title = "Meningitis vs Malaria Cases (2001–2005)",
       x = "Year", y = "Number of Patients", fill = "Disease") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        legend.position = "top")


# Chunk: component-bar  (meningitis by sex)
comp_long <- data.frame(
  Year     = rep(factor(2001:2005), 2),
  Sex      = c(rep("Male", 5), rep("Female", 5)),
  Patients = c(100, 125, 90, 20, 102,
                41, 100, 115, 88, 90)
)

ggplot(comp_long, aes(x = Year, y = Patients, fill = Sex)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = Patients),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Male" = "#2e86c1", "Female" = "#e74c3c")) +
  labs(title = "Meningitis Patients by Sex (2001–2005)",
       x = "Year", y = "Number of Patients", fill = "Sex") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        legend.position = "top")


# ── SECTION 8: PIE CHART ─────────────────────────────────────

# Chunk: pie-chart  (ABC Ltd manufacturing costs)
pie_df <- data.frame(
  Component  = c("Labour", "Raw Materials", "Maintenance Costs", "Debt Servicing"),
  Percentage = c(20, 40, 25, 15)
)

ggplot(pie_df, aes(x = "", y = Percentage, fill = Component)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.7) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Labour"            = "#2e86c1",
                               "Raw Materials"      = "#e67e22",
                               "Maintenance Costs"  = "#117a65",
                               "Debt Servicing"     = "#8e44ad")) +
  labs(title = "ABC Ltd – Per Unit Manufacturing Cost Distribution", fill = "Component") +
  theme_void(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276",
                                       face = "bold", size = 13),
        legend.position = "right")


# Chunk: pie-blood-types  (Exercise 8 – Kenya blood types)
blood_df <- data.frame(
  Type       = c("Type O", "Type A", "Type B", "Type AB"),
  Percentage = c(45, 40, 11, 4)
)

ggplot(blood_df, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.7) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Type O"  = "#2e86c1", "Type A"  = "#e67e22",
                               "Type B"  = "#117a65", "Type AB" = "#8e44ad")) +
  labs(title = "Distribution of Blood Types in Kenya", fill = "Blood Type") +
  theme_void(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276",
                                       face = "bold", size = 13),
        legend.position = "right")


# ── SECTION 9: OGIVE CURVES ──────────────────────────────────

# Chunk: less-than-ogive
lt_data <- data.frame(
  upper_boundary = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  cum_freq       = c(0,  5, 16, 35, 56, 72, 82, 90, 96, 99, 100)
)

ggplot(lt_data, aes(x = upper_boundary, y = cum_freq)) +
  geom_line(color = "#1f618d", linewidth = 1.2) +
  geom_point(color = "#e74c3c", size = 3) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = '"Less Than" Ogive Curve of Student Marks',
       x = "Upper Class Boundary (Marks)", y = "Cumulative Frequency") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# Chunk: more-than-ogive
mt_data <- data.frame(
  lower_boundary = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  cum_freq       = c(100, 95, 84, 65, 44, 28, 18, 10,  4,  1,   0)
)

ggplot(mt_data, aes(x = lower_boundary, y = cum_freq)) +
  geom_line(color = "#117a65", linewidth = 1.2) +
  geom_point(color = "#e74c3c", size = 3) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = '"More Than" Ogive Curve of Student Marks',
       x = "Lower Class Boundary (Marks)", y = "More Than Cumulative Frequency") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, color = "#1a5276", face = "bold"))


# Chunk: both-ogives  (shows intersection = median)
lt_plot <- lt_data
lt_plot$type <- "Less than"
names(lt_plot)[1] <- "boundary"

mt_plot <- mt_data
mt_plot$type <- "More than"
names(mt_plot)[1] <- "boundary"

both_ogives <- rbind(lt_plot, mt_plot)

ggplot(both_ogives, aes(x = boundary, y = cum_freq, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 34.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 37, y = 52, label = "Median ≈ 34.5",
           color = "gray30", size = 4, hjust = 0) +
  scale_color_manual(values = c("Less than" = "#1f618d", "More than" = "#117a65")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = '"Less Than" and "More Than" Ogive Curves',
       x = "Class Boundary (Marks)", y = "Cumulative Frequency",
       color = "Ogive Type") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        legend.position = "top")


# ── EXERCISE CHARTS ───────────────────────────────────────────

# Chunk: ex9-component-bar  (College ABC students by faculty)
ex9_long <- data.frame(
  Year    = rep(c("1982–83", "1983–84", "1984–85"), 3),
  Faculty = c(rep("Science", 3), rep("Arts", 3), rep("Law", 3)),
  Count   = c(1000, 1600, 2100, 1500, 2000, 4000, 200, 350, 420)
)
ex9_long$Year    <- factor(ex9_long$Year, levels = c("1982–83","1983–84","1984–85"))
ex9_long$Faculty <- factor(ex9_long$Faculty, levels = c("Law","Science","Arts"))

ggplot(ex9_long, aes(x = Year, y = Count, fill = Faculty)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = Count),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Science" = "#2e86c1",
                               "Arts"    = "#e67e22",
                               "Law"     = "#117a65")) +
  labs(title = "Students in College ABC by Faculty (1982–1985)",
       x = "Academic Year", y = "Number of Students", fill = "Faculty") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        legend.position = "top")


# Chunk: ex10-multiple-bar  (Kenyan exports and imports)
ex10_long <- data.frame(
  Year  = rep(c("1999–2000","2000–2001","2001–2002","2002–2003","2003–2004"), 2),
  Type  = c(rep("Export", 5), rep("Import", 5)),
  Value = c(160000, 170000, 180000, 200000, 200000,
            200000, 300000, 350000, 300000, 380000)
)
ex10_long$Year <- factor(ex10_long$Year,
                         levels = c("1999–2000","2000–2001","2001–2002",
                                    "2002–2003","2003–2004"))

ggplot(ex10_long, aes(x = Year, y = Value / 1000, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Value / 1000, "K")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Export" = "#2e86c1", "Import" = "#e67e22")) +
  labs(title   = "Kenyan Exports and Imports (1999–2004)",
       x       = "Year",
       y       = "Value (thousands of millions Ksh)",
       fill    = "Trade Type",
       caption = "Source: KNBS") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        axis.text.x     = element_text(angle = 30, hjust = 1),
        legend.position = "top")


# Chunk: ex11-population-bar  (Kenya 2009 census)
ex11_long <- data.frame(
  Age = rep(c("0–14","15–24","25–54","55–64","65+"), 2),
  Sex = c(rep("Male", 5), rep("Female", 5)),
  Population = c(9557274, 4552448, 8170264, 856092, 614751,
                 9497870, 4567894, 7976751, 1009075, 813320)
)
ex11_long$Age <- factor(ex11_long$Age,
                        levels = c("0–14","15–24","25–54","55–64","65+"))

ggplot(ex11_long, aes(x = Age, y = Population / 1e6, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Male" = "#2e86c1", "Female" = "#e74c3c")) +
  labs(title   = "Kenyan Population Age Structure by Sex (2009 Census)",
       x = "Age Group", y = "Population (millions)", fill = "Sex",
       caption = "Source: CIA World Factbook 2017") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        legend.position = "top")


# ============================================================
# TOPIC 2 – MEASURES OF CENTRAL TENDENCY
# ============================================================

# ── ARITHMETIC MEAN ──────────────────────────────────────────

# Chunk: mean-direct  (Example 2.1 – golf scores)
golf_scores <- c(284, 280, 277, 282, 279, 285, 281, 283, 278, 277)
n <- length(golf_scores)

mean_direct <- sum(golf_scores) / n
cat("Direct method:        x̄ =", mean_direct, "\n")

# Assumed mean method
a <- 280
d <- golf_scores - a
mean_assumed <- a + sum(d) / n
cat("Assumed mean method:  x̄ =", mean_assumed, "\n")

# Coding method
c_val <- 5
u <- d / c_val
mean_coded <- a + c_val * sum(u) / n
cat("Coding method:        x̄ =", mean_coded, "\n")


# Chunk: mean-freq  (Example 2.2 – ages of club members)
ages      <- c(15, 16, 17, 18, 19, 20)
freq_ages <- c(2,  5, 11,  9, 14, 13)

mean_age <- sum(ages * freq_ages) / sum(freq_ages)
cat("Mean age =", round(mean_age, 2), "\n")

# Summary table
mean_table <- data.frame(
  Age        = ages,
  Frequency  = freq_ages,
  `f × x`   = ages * freq_ages,
  check.names = FALSE
)
total_row <- data.frame(Age = NA, Frequency = sum(freq_ages),
                         `f × x` = sum(ages * freq_ages),
                         check.names = FALSE)
names(total_row) <- names(mean_table)
mean_table <- rbind(mean_table, total_row)
mean_table[nrow(mean_table), "Age"] <- NA

kable(mean_table,
      caption = "Arithmetic Mean of Ages – Cultural Club Members",
      align = c("c","c","c"),
      na.strings = "") |>
  kable_styling(bootstrap_options = c("striped","hover","bordered","condensed"),
                full_width = FALSE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white") |>
  row_spec(nrow(mean_table), bold = TRUE, background = "#d6eaf8")


# Chunk: mean-grouped  (Example 2.3 – three methods for grouped data)
midpoints <- c(55, 60, 65, 70, 75, 80, 85, 90)
freq_g    <- c(2, 12, 12, 25, 27, 10,  9,  3)
a_g <- 75
c_g <-  5

d_g <- midpoints - a_g
u_g <- d_g / c_g

ex23_df <- data.frame(
  Class   = c("53–57","58–62","63–67","68–72","73–77","78–82","83–87","88–92"),
  x       = midpoints,
  f       = freq_g,
  `f×x`   = freq_g * midpoints,
  `d = x−a` = d_g,
  `f×d`   = freq_g * d_g,
  `u = d/c` = u_g,
  `f×u`   = freq_g * u_g,
  check.names = FALSE
)

# Total row
totals <- c("Total", "", sum(freq_g), sum(freq_g * midpoints),
            "", sum(freq_g * d_g), "", sum(freq_g * u_g))
ex23_df[nrow(ex23_df) + 1, ] <- totals

kable(ex23_df,
      caption = "Arithmetic Mean Using Three Methods (a = 75, c = 5)") |>
  kable_styling(bootstrap_options = c("striped","hover","bordered","condensed"),
                full_width = TRUE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white") |>
  row_spec(nrow(ex23_df), bold = TRUE, background = "#d6eaf8")

# Results
cat("Direct method:       x̄ =", sum(freq_g * midpoints) / sum(freq_g), "\n")
cat("Assumed mean method: x̄ =", a_g + sum(freq_g * d_g) / sum(freq_g), "\n")
cat("Coding method:       x̄ =", a_g + c_g * sum(freq_g * u_g) / sum(freq_g), "\n")


# Chunk: mean-correction  (Example 2.4 – correcting wrong values)

# Part i
incorrect_sum_i <- 378.40 * 25
correct_sum_i   <- incorrect_sum_i - 160 + 200
correct_mean_i  <- correct_sum_i / 25
cat("Correct mean (Part i): $", round(correct_mean_i, 2), "\n")

# Part ii
incorrect_sum_ii <- 50 * 200
correct_sum_ii   <- incorrect_sum_ii - 92 - 8 + 192 + 88
correct_mean_ii  <- correct_sum_ii / 200
cat("Correct mean (Part ii):", round(correct_mean_ii, 2), "\n")


# ── COMBINED MEAN ─────────────────────────────────────────────

# Chunk: combined-mean  (Example 2.5)

# Part i – two branches
N1 <- 100; X1 <- 4570
N2 <-  80; X2 <- 6750
combined_mean_i <- (N1 * X1 + N2 * X2) / (N1 + N2)
cat("Combined mean (Part i): $", round(combined_mean_i, 2), "\n")

# Part ii – mean marks of girls
N_total <- 100; X_combined <- 72
N_boys <- 70;   X_boys <- 75
N_girls <- N_total - N_boys
X_girls <- (X_combined * N_total - N_boys * X_boys) / N_girls
cat("Mean marks of girls (Part ii):", X_girls, "\n")

# Part iii – percentage of men and women
# 30 = (32*N1 + 25*N2)/(N1+N2) with N1+N2=100
N1_pct <- (30 * 100 - 25 * 100) / (32 - 25)
N2_pct <- 100 - N1_pct
cat(sprintf("Men: %.2f%%   Women: %.2f%%\n", N1_pct, N2_pct))

# Part iv – cold drink bottles
# 1200 = (1000*N1 + 2000*N2)/50, N1+N2=50
N2_bottles <- (1200 * 50 - 1000 * 50) / (2000 - 1000)
N1_bottles <- 50 - N2_bottles
cat("2-litre bottles:", N2_bottles,
    "  1-litre bottles:", N1_bottles, "\n")


# ── WEIGHTED MEAN ─────────────────────────────────────────────

# Chunk: weighted-mean  (Example 2.6)
subjects <- c("Mathematics", "Physics", "English", "Accounting")
marks    <- c(82, 86, 90, 70)
weights  <- c(3, 5, 3, 1)

wt_df <- data.frame(Subject = subjects, `Marks (X)` = marks,
                     `Weight (W)` = weights, `W×X` = weights * marks,
                     check.names = FALSE)
total_row_wt <- data.frame(Subject = "Total", `Marks (X)` = NA,
                             `Weight (W)` = sum(weights),
                             `W×X` = sum(weights * marks),
                             check.names = FALSE)
names(total_row_wt) <- names(wt_df)
wt_df <- rbind(wt_df, total_row_wt)

kable(wt_df, caption = "Weighted Arithmetic Mean of Student Marks",
      align = c("l","c","c","c"), na.strings = "") |>
  kable_styling(bootstrap_options = c("striped","hover","bordered","condensed"),
                full_width = FALSE) |>
  row_spec(0, bold = TRUE, background = "#1f618d", color = "white") |>
  row_spec(nrow(wt_df), bold = TRUE, background = "#d6eaf8")

weighted_mean <- sum(weights * marks) / sum(weights)
cat("Weighted arithmetic mean:", round(weighted_mean, 2), "\n")


# ── MEDIAN ────────────────────────────────────────────────────

# Chunk: median-ungrouped  (Example 2.7)
x1 <- c(1, 10, 7, 20, 5)
cat("Sorted:", sort(x1), "\n")
cat("Median:", median(x1), "\n")

x2 <- c(21, 3, 7, 17, 19, 31, 46, 20, 43)
cat("\nSorted:", sort(x2), "\n")
cat("Median:", median(x2), "\n")


# Chunk: median-grouped  (Example 2.9)
classes_med <- c("53–57","58–62","63–67","68–72","73–77","78–82","83–87","88–92")
freq_med    <- c(2, 12, 12, 25, 27, 10, 9, 3)
cf_med      <- cumsum(freq_med)
N_med       <- sum(freq_med)

# Locate median class  (N/2 = 50)
med_class_idx <- which(cf_med >= N_med / 2)[1]
L_med   <- 67.5          # lower boundary of median class 68–72
cf_prev <- cf_med[med_class_idx - 1]
f_med   <- freq_med[med_class_idx]
c_med   <- 5

Median <- L_med + ((N_med / 2 - cf_prev) / f_med) * c_med
cat("Median =", round(Median, 2), "\n")


# ── QUARTILES ─────────────────────────────────────────────────

# Chunk: quartiles  (Example 2.8 – company profits)
profit_classes <- c("20–30","30–40","40–50","50–60",
                    "60–70","70–80","80–90","90–100")
freq_profit    <- c(4, 8, 18, 30, 15, 10, 8, 7)
cf_profit      <- cumsum(freq_profit)
N_profit       <- sum(freq_profit)

# Q1 (N/4 = 25th observation) → class 40–50
Q1 <- 40 + ((N_profit / 4 - cf_profit[2]) / freq_profit[3]) * 10
cat("Q1 = $", round(Q1, 2), "\n")

# Q3 (3N/4 = 75th observation) → class 60–70
Q3 <- 60 + ((3 * N_profit / 4 - cf_profit[4]) / freq_profit[5]) * 10
cat("Q3 = $", round(Q3, 2), "\n")

cat("IQR = Q3 – Q1 = $", round(Q3 - Q1, 2), "\n")


# ── MODE ─────────────────────────────────────────────────────

# Chunk: mode-ungrouped  (Example 2.9)
values_mode <- c(13, 18, 13, 14, 13, 16, 14, 21, 13)
sorted_vals <- sort(values_mode)
cat("Sorted:", sorted_vals, "\n")
cat("Mean:  ", mean(values_mode), "\n")
cat("Median:", median(values_mode), "\n")
cat("Mode:  ", as.numeric(names(which.max(table(values_mode)))), "\n")
cat("Range: ", diff(range(values_mode)), "\n")


# Chunk: mode-grouped  (modal class 73–77)
freq_mode <- c(12, 12, 25, 27, 10, 9, 3)   # from 58–62 onwards
modal_idx <- which.max(freq_mode)           # index 4 → class 73–77

L_mode  <- 72.5
delta1  <- freq_mode[modal_idx] - freq_mode[modal_idx - 1]
delta2  <- freq_mode[modal_idx] - freq_mode[modal_idx + 1]
c_mode  <- 5

Mode_val <- L_mode + (delta1 / (delta1 + delta2)) * c_mode
cat("Mode =", round(Mode_val, 4), "units\n")


# ── GEOMETRIC MEAN ────────────────────────────────────────────

# Chunk: geom-mean  (Example 2.10 – overhead expenses)
X_overhead <- c(132, 140, 150)
G_overhead <- exp(mean(log(X_overhead)))
cat("Geometric mean:", round(G_overhead, 1), "\n")
cat("Average rate of increase:", round(G_overhead - 100, 1), "%\n")


# Chunk: geom-mean-growth  (Example 2.11 – factory output)
X_growth <- c(105.0, 107.5, 102.5, 105.0, 110.0)
G_growth <- exp(mean(log(X_growth)))
cat("Geometric mean:", round(G_growth, 1), "\n")
cat("Compound rate of growth:", round(G_growth - 100, 1), "%\n")


# ── HARMONIC MEAN ─────────────────────────────────────────────

# Chunk: harmonic-mean  (Example 2.12)

# Part a – ungrouped
x_harm <- c(10, 20, 25, 40, 50)
H_a    <- length(x_harm) / sum(1 / x_harm)
cat("Harmonic mean (Part a):", round(H_a, 2), "\n")

# Part b – grouped
marks_harm  <- c(5, 15, 25, 35, 45)   # midpoints
freq_harm   <- c(8, 15, 20, 4, 3)
H_b         <- sum(freq_harm) / sum(freq_harm / marks_harm)
cat("Harmonic mean (Part b):", round(H_b, 2), "\n")


# ── SUMMARY: CENTRAL TENDENCY COMPARISON ─────────────────────

# Chunk: central-tendency-summary
# Visualise mean, median, mode on a single distribution
set.seed(42)
sample_data <- c(rep(13, 4), 14, 14, 16, 18, 21)

ggplot(data.frame(x = sample_data), aes(x = x)) +
  geom_dotplot(binwidth = 0.8, fill = "#aed6f1", color = "#1f618d") +
  geom_vline(xintercept = mean(sample_data),   color = "#e74c3c",
             linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = median(sample_data), color = "#117a65",
             linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 13,                  color = "#8e44ad",
             linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = mean(sample_data)   + 0.3, y = 0.9,
           label = paste("Mean =",   round(mean(sample_data), 1)),
           color = "#e74c3c", hjust = 0, size = 4) +
  annotate("text", x = median(sample_data) + 0.3, y = 0.75,
           label = paste("Median =", median(sample_data)),
           color = "#117a65", hjust = 0, size = 4) +
  annotate("text", x = 13 - 0.3, y = 0.6,
           label = "Mode = 13", color = "#8e44ad", hjust = 1, size = 4) +
  labs(title = "Comparing Mean, Median and Mode",
       x = "Value", y = "") +
  theme_classic(base_size = 13) +
  theme(plot.title   = element_text(hjust = 0.5, color = "#1a5276", face = "bold"),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())
