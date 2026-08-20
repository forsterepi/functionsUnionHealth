# Create initial dataset -------------------------------------------------

## Define country-level data ----
def_countries <- simstudy::defData(
  varname = "v2x_libdem",
  dist = "uniform",
  formula = "0;0.9",
  id = "idc"
) |>
  simstudy::defData(
    varname = "tud",
    dist = "beta",
    formula = 0.37,
    variance = 6.34,
    link = "identity"
  ) |>
  simstudy::defData(
    varname = "cbc",
    dist = "uniform",
    formula = "0;1"
  ) |>
  simstudy::defData(
    varname = "gini",
    dist = "uniform",
    formula = "23;65"
  ) |>
  simstudy::defData(
    varname = "clustSize",
    formula = 1000,
    dist = "clusterSize"
  )

## Create country and year ----
c_year <- data.frame(
  c = c("CAN", "CAN", "COL", "COL", "EGY", "EGY", "SVN", "SVN", "URY", "URY"),
  country = c(
    "Canada",
    "Canada",
    "Colombia",
    "Colombia",
    "Egypt",
    "Egypt",
    "Slovenia",
    "Slovenia",
    "Uruguay",
    "Uruguay"
  ),
  year = c(2000, 2006, 1998, 2018, 2008, 2018, 1995, 2005, 1996, 2006),
  class = c(
    "high",
    "high",
    "low",
    "low",
    "low",
    "lower-middle",
    "upper-middle",
    "high",
    "lower-middle",
    "upper-middle"
  ),
  wave = c(4, 5, 3, 7, 5, 7, 3, 5, 3, 5)
)

## Generate country-level data ----
d_countries <- simstudy::genData(10, def_countries) |>
  vctrs::vec_cbind(c_year)
rm(def_countries, c_year)

## Define individual-level data ----
def_indiv <- simstudy::defDataAdd(
  varname = "srh",
  dist = "categorical",
  formula = simstudy::genCatFormula(n = 4),
  link = "identity"
) |>
  simstudy::defDataAdd(
    varname = "swl",
    dist = "categorical",
    formula = simstudy::genCatFormula(n = 5),
    link = "identity"
  ) |>
  simstudy::defDataAdd(
    varname = "gender",
    dist = "categorical",
    formula = simstudy::genCatFormula(n = 2),
    link = "identity"
  ) |>
  simstudy::defDataAdd(
    varname = "age_group",
    dist = "categorical",
    formula = simstudy::genCatFormula(n = 6),
    link = "identity"
  ) |>
  simstudy::defDataAdd(
    varname = "job",
    dist = "categorical",
    formula = simstudy::genCatFormula(n = 3),
    link = "identity"
  ) |>
  simstudy::defDataAdd(
    varname = "tu_member",
    dist = "categorical",
    formula = simstudy::genCatFormula(n = 2),
    link = "identity"
  )

## Generate individual-level data ----
test_data <- simstudy::genCluster(
  d_countries,
  "idc",
  numIndsVar = "clustSize",
  level1ID = "idperson"
)
test_data <- simstudy::addColumns(def_indiv, test_data)
rm(d_countries, def_indiv)

# Process initial dataset ------------------------------------------------

test_data <- test_data |>
  dplyr::mutate(
    srh = factor(
      srh,
      levels = 1:4,
      labels = c("Very good", "Good", "Fair", "Poor or very poor"),
      ordered = TRUE
    ),
    swl = factor(
      swl,
      levels = 1:5,
      labels = c("very high", "high", "medium", "low", "very low"),
      ordered = TRUE
    ),
    job = factor(
      job,
      levels = 1:3,
      labels = c(
        "Primarily non-manual",
        "Primarily manual",
        "Agricultural"
      )
    ),
    tu_member = factor(
      tu_member,
      levels = 1:2,
      labels = c("nonmember", "member")
    ),
    age_group = factor(
      age_group,
      levels = 1:6,
      labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60+")
    ),
    gender = factor(
      gender,
      levels = 1:2,
      labels = c("male", "female")
    ),
    class = as.factor(class),
    wave = as.factor(wave),
    country = as.factor(country),
    decades_since_start = (year - 1981) / 10,
    tud_original = tud,
    cbc_original = cbc,
    v2x_libdem_original = v2x_libdem,
    gini_original = gini,
    tud = (tud - mean(tud)) / sd(tud),
    cbc = (cbc - mean(cbc)) / sd(cbc),
    v2x_libdem = (v2x_libdem - mean(v2x_libdem)) / sd(v2x_libdem),
    gini = (gini - mean(gini)) / sd(gini)
  ) |>
  dplyr::select(-idc, -clustSize, -idperson)

# Save dataset in package ------------------------------------------------

usethis::use_data(test_data, overwrite = TRUE)
