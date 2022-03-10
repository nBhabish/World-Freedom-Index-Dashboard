# Loading Dependencies ----------------------------------------------------

library(tidytuesdayR)
library(sf) # for reading in shapefiles
library(leaflet)
library(tidyverse)

library(countrycode)

library(reactable)
library(reactablefmtr)


# TidyTuesday Data --------------------------------------------------------

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>% 
  janitor::clean_names()

# Importing World Shapefile -----------------------------------------------

world_shp <- read_sf("world shapefile/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# Reactable Table Work ----------------------------------------------------

freedom %>% 
  distinct(country) %>% 
  pull() %>% 
  datapasta::vector_paste_vertical()  

# flag icons embedded in the data -----------------------------------------

freedom_with_flags <- freedom %>%
  # Adding Flags
  mutate(
    flag_img = case_when(
      country == "Afghanistan" ~ "https://cdn-icons-png.flaticon.com/512/197/197515.png",
      country == "Albania" ~ "https://cdn-icons-png.flaticon.com/512/197/197520.png",
      country == "Algeria" ~ "https://cdn-icons-png.flaticon.com/512/197/197511.png",
      country == "Andorra" ~ "https://cdn-icons-png.flaticon.com/512/197/197535.png",
      country == "Angola" ~ "https://cdn-icons-png.flaticon.com/512/197/197513.png",
      country == "Antigua and Barbuda" ~ "https://cdn-icons-png.flaticon.com/512/197/197533.png",
      country == "Argentina" ~ "https://cdn-icons-png.flaticon.com/512/197/197573.png",
      country == "Armenia" ~ "https://cdn-icons-png.flaticon.com/512/197/197516.png",
      country == "Australia" ~ "https://cdn-icons-png.flaticon.com/512/197/197507.png",
      country == "Austria" ~ "https://cdn-icons-png.flaticon.com/512/197/197447.png",
      country == "Azerbaijan" ~ "https://cdn-icons-png.flaticon.com/512/197/197512.png",
      country == "Bahamas" ~ "https://cdn-icons-png.flaticon.com/512/197/197442.png",
      country == "Bahrain" ~ "https://cdn-icons-png.flaticon.com/512/197/197521.png",
      country == "Bangladesh" ~ "https://cdn-icons-png.flaticon.com/512/197/197509.png",
      country == "Barbados" ~ "https://cdn-icons-png.flaticon.com/512/197/197526.png",
      country == "Belarus" ~ "https://cdn-icons-png.flaticon.com/512/197/197527.png",
      country == "Belgium" ~ "https://cdn-icons-png.flaticon.com/512/197/197583.png",
      country == "Belize" ~ "https://cdn-icons-png.flaticon.com/512/197/197522.png",
      country == "Benin" ~ "https://cdn-icons-png.flaticon.com/512/197/197539.png",
      country == "Bhutan" ~ "https://cdn-icons-png.flaticon.com/512/197/197437.png",
      country == "Bolivia (Plurinational State of)" ~ "https://cdn-icons-png.flaticon.com/512/197/197504.png",
      country == "Bosnia and Herzegovina" ~ "https://cdn-icons-png.flaticon.com/512/197/197524.png",
      country == "Botswana" ~ "https://cdn-icons-png.flaticon.com/512/197/197510.png",
      country == "Brazil" ~ "https://cdn-icons-png.flaticon.com/512/197/197386.png",
      country == "Brunei Darussalam" ~ "https://cdn-icons-png.flaticon.com/512/197/197530.png",
      country == "Bulgaria" ~ "https://cdn-icons-png.flaticon.com/512/197/197502.png",
      country == "Burkina Faso" ~ "https://cdn-icons-png.flaticon.com/512/197/197519.png",
      country == "Burundi" ~ "https://cdn-icons-png.flaticon.com/512/197/197534.png",
      #country == "Cabo Verde" ~ "",
      country == "Cambodia" ~ "https://cdn-icons-png.flaticon.com/512/197/197505.png",
      country == "Cameroon" ~ "https://cdn-icons-png.flaticon.com/512/197/197531.png",
      country == "Canada" ~ "https://cdn-icons-png.flaticon.com/512/197/197430.png",
      country == "Central African Republic" ~ "https://cdn-icons-png.flaticon.com/512/197/197546.png",
      country == "Chad" ~ "https://cdn-icons-png.flaticon.com/512/197/197542.png",
      country == "Chile" ~ "https://cdn-icons-png.flaticon.com/512/197/197586.png",
      country == "China" ~ "https://cdn-icons-png.flaticon.com/512/197/197375.png",
      country == "Colombia" ~ "https://cdn-icons-png.flaticon.com/512/197/197575.png",
      country == "Comoros" ~ "https://cdn-icons-png.flaticon.com/512/197/197547.png",
      country == "Congo" ~ "https://cdn-icons-png.flaticon.com/512/197/197378.png",
      country == "Democratic Republic of the Congo" ~ "https://cdn-icons-png.flaticon.com/512/197/197396.png",
      country == "Costa Rica" ~ "https://cdn-icons-png.flaticon.com/512/197/197506.png",
      country == "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" ~ "https://cdn-icons-png.flaticon.com/512/197/197391.png",
      country == "Croatia" ~ "https://cdn-icons-png.flaticon.com/512/197/197503.png",
      country == "Cuba" ~ "https://cdn-icons-png.flaticon.com/512/197/197508.png",
      country == "Cyprus" ~ "https://cdn-icons-png.flaticon.com/512/197/197421.png",
      country == "Czechia" ~ "https://cdn-icons-png.flaticon.com/512/197/197576.png",
      country == "Denmark" ~ "https://cdn-icons-png.flaticon.com/512/197/197565.png",
      country == "Djibouti" ~ "https://cdn-icons-png.flaticon.com/512/197/197425.png",
      country == "Dominica" ~ "https://cdn-icons-png.flaticon.com/512/197/197490.png",
      country == "Dominican Republic" ~ "https://cdn-icons-png.flaticon.com/512/197/197619.png",
      country == "Ecuador" ~ "https://cdn-icons-png.flaticon.com/512/197/197588.png",
      country == "Egypt" ~ "https://cdn-icons-png.flaticon.com/512/197/197558.png",
      country == "El Salvador" ~ "https://cdn-icons-png.flaticon.com/512/197/197622.png",
      country == "Equatorial Guinea" ~ "https://cdn-icons-png.flaticon.com/512/197/197395.png",
      country == "Eritrea" ~ "https://cdn-icons-png.flaticon.com/512/197/197426.png",
      country == "Estonia" ~ "https://cdn-icons-png.flaticon.com/512/197/197379.png",
      #country == "Eswatini" ~ "",
      country == "Ethiopia" ~ "https://cdn-icons-png.flaticon.com/512/197/197636.png",
      country == "Fiji" ~ "https://cdn-icons-png.flaticon.com/512/197/197492.png",
      country == "Finland" ~ "https://cdn-icons-png.flaticon.com/512/197/197585.png",
      country == "France" ~ "https://cdn-icons-png.flaticon.com/512/197/197560.png",
      country == "Gabon" ~ "https://cdn-icons-png.flaticon.com/512/197/197450.png",
      country == "Georgia" ~ "https://cdn-icons-png.flaticon.com/512/197/197380.png",
      country == "Germany" ~ "https://cdn-icons-png.flaticon.com/512/197/197571.png",
      country == "Ghana" ~ "https://cdn-icons-png.flaticon.com/512/197/197381.png",
      country == "Greece" ~ "https://cdn-icons-png.flaticon.com/512/197/197566.png",
      country == "Grenada" ~ "https://cdn-icons-png.flaticon.com/512/197/197454.png",
      country == "Guatemala" ~ "https://cdn-icons-png.flaticon.com/512/197/197597.png",
      country == "Guinea" ~ "https://cdn-icons-png.flaticon.com/512/197/197483.png",
      country == "Guinea-Bissau" ~ "https://cdn-icons-png.flaticon.com/512/197/197427.png",
      country == "Guyana" ~ "https://cdn-icons-png.flaticon.com/512/197/197457.png",
      country == "Haiti" ~ "https://cdn-icons-png.flaticon.com/512/197/197389.png",
      country == "Honduras" ~ "https://cdn-icons-png.flaticon.com/512/197/197610.png",
      country == "Hungary" ~ "https://cdn-icons-png.flaticon.com/512/197/197584.png",
      country == "Iceland" ~ "https://cdn-icons-png.flaticon.com/512/197/197596.png",
      country == "India" ~ "https://cdn-icons-png.flaticon.com/512/197/197419.png",
      country == "Indonesia" ~ "https://cdn-icons-png.flaticon.com/512/197/197559.png",
      country == "Iran (Islamic Republic of)" ~ "https://cdn-icons-png.flaticon.com/512/197/197574.png",
      country == "Iraq" ~ "https://cdn-icons-png.flaticon.com/512/197/197630.png",
      country == "Ireland" ~ "https://cdn-icons-png.flaticon.com/512/197/197567.png",
      country == "Israel" ~ "https://cdn-icons-png.flaticon.com/512/197/197577.png",
      country == "Italy" ~ "https://cdn-icons-png.flaticon.com/512/197/197626.png",
      country == "Jamaica" ~ "https://cdn-icons-png.flaticon.com/512/323/323331.png",
      country == "Japan" ~ "https://cdn-icons-png.flaticon.com/512/197/197604.png",
      country == "Jordan" ~ "https://cdn-icons-png.flaticon.com/512/197/197595.png",
      country == "Kazakhstan" ~ "https://cdn-icons-png.flaticon.com/512/197/197603.png",
      country == "Kenya" ~ "https://cdn-icons-png.flaticon.com/512/197/197608.png",
      country == "Kiribati" ~ "https://cdn-icons-png.flaticon.com/512/197/197458.png",
      country == "Kuwait" ~ "https://cdn-icons-png.flaticon.com/512/197/197459.png",
      country == "Kyrgyzstan" ~ "https://cdn-icons-png.flaticon.com/512/197/197428.png",
      country == "Lao People's Democratic Republic" ~ "https://cdn-icons-png.flaticon.com/512/197/197568.png",
      country == "Latvia" ~ "https://cdn-icons-png.flaticon.com/512/197/197605.png",
      country == "Lebanon" ~ "https://cdn-icons-png.flaticon.com/512/197/197629.png",
      country == "Lesotho" ~ "https://cdn-icons-png.flaticon.com/512/197/197392.png",
      country == "Liberia" ~ "https://cdn-icons-png.flaticon.com/512/197/197418.png",
      country == "Libya" ~ "https://cdn-icons-png.flaticon.com/512/197/197411.png",
      country == "Liechtenstein" ~ "https://cdn-icons-png.flaticon.com/512/197/197420.png",
      country == "Lithuania" ~ "https://cdn-icons-png.flaticon.com/512/197/197612.png",
      country == "Luxembourg" ~ "https://cdn-icons-png.flaticon.com/512/197/197614.png",
      country == "Madagascar" ~ "https://cdn-icons-png.flaticon.com/512/197/197383.png",
      country == "Malawi" ~ "https://cdn-icons-png.flaticon.com/512/197/197384.png",
      country == "Malaysia" ~ "https://cdn-icons-png.flaticon.com/512/197/197581.png",
      country == "Maldives" ~ "https://cdn-icons-png.flaticon.com/512/197/197404.png",
      country == "Mali" ~ "https://cdn-icons-png.flaticon.com/512/197/197429.png",
      country == "Malta" ~ "https://cdn-icons-png.flaticon.com/512/197/197625.png",
      country == "Marshall Islands" ~ "https://cdn-icons-png.flaticon.com/512/197/197431.png",
      country == "Mauritania" ~ "https://cdn-icons-png.flaticon.com/512/197/197460.png",
      country == "Mauritius" ~ "https://cdn-icons-png.flaticon.com/512/197/197616.png",
      country == "Mexico" ~ "https://cdn-icons-png.flaticon.com/512/197/197397.png",
      country == "Micronesia (Federated States of)" ~ "https://cdn-icons-png.flaticon.com/512/197/197432.png",
      country == "Republic of Moldova" ~ "https://cdn-icons-png.flaticon.com/512/197/197405.png",
      country == "Monaco" ~ "https://cdn-icons-png.flaticon.com/512/197/197594.png",
      country == "Mongolia" ~ "https://cdn-icons-png.flaticon.com/512/197/197385.png",
      country == "Montenegro" ~ "https://cdn-icons-png.flaticon.com/512/197/197406.png",
      country == "Morocco" ~ "https://cdn-icons-png.flaticon.com/512/197/197551.png",
      country == "Mozambique" ~ "https://cdn-icons-png.flaticon.com/512/197/197631.png",
      country == "Myanmar" ~ "https://cdn-icons-png.flaticon.com/512/197/197609.png",
      country == "Namibia" ~ "https://cdn-icons-png.flaticon.com/512/197/197617.png",
      country == "Nauru" ~ "https://cdn-icons-png.flaticon.com/512/197/197464.png",
      country == "Nepal" ~ "https://cdn-icons-png.flaticon.com/512/197/197387.png",
      country == "Netherlands" ~ "https://cdn-icons-png.flaticon.com/512/197/197441.png",
      country == "New Zealand" ~ "https://cdn-icons-png.flaticon.com/512/197/197589.png",
      country == "Nicaragua" ~ "https://cdn-icons-png.flaticon.com/512/197/197623.png",
      country == "Niger" ~ "https://cdn-icons-png.flaticon.com/512/197/197433.png",
      country == "Nigeria" ~ "https://cdn-icons-png.flaticon.com/512/197/197627.png",
      country == "Democratic People's Republic of Korea" ~ "https://cdn-icons-png.flaticon.com/512/197/197600.png",
      country == "North Macedonia" ~ "https://cdn-icons-png.flaticon.com/512/6184/6184771.png",
      country == "Norway" ~ "https://cdn-icons-png.flaticon.com/512/197/197579.png",
      country == "Oman" ~ "https://cdn-icons-png.flaticon.com/512/197/197635.png",
      country == "Pakistan" ~ "https://cdn-icons-png.flaticon.com/512/197/197606.png",
      country == "Palau" ~ "https://cdn-icons-png.flaticon.com/512/197/197495.png",
      country == "Panama" ~ "https://cdn-icons-png.flaticon.com/512/197/197590.png",
      country == "Papua New Guinea" ~ "https://cdn-icons-png.flaticon.com/512/197/197407.png",
      country == "Paraguay" ~ "https://cdn-icons-png.flaticon.com/512/197/197376.png",
      country == "Peru" ~ "https://cdn-icons-png.flaticon.com/512/197/197563.png",
      country == "Philippines" ~ "https://cdn-icons-png.flaticon.com/512/197/197561.png",
      country == "Poland" ~ "https://cdn-icons-png.flaticon.com/512/197/197529.png",
      country == "Portugal" ~ "https://cdn-icons-png.flaticon.com/512/197/197463.png",
      country == "Qatar" ~ "https://cdn-icons-png.flaticon.com/512/197/197618.png",
      country == "Romania" ~ "https://cdn-icons-png.flaticon.com/512/197/197587.png",
      country == "Russian Federation" ~ "https://cdn-icons-png.flaticon.com/512/197/197408.png",
      country == "Rwanda" ~ "https://cdn-icons-png.flaticon.com/512/197/197393.png",
      country == "Samoa" ~ "https://cdn-icons-png.flaticon.com/512/197/197388.png",
      country == "San Marino" ~ "https://cdn-icons-png.flaticon.com/512/197/197422.png",
      country == "Sao Tome and Principe" ~ "https://cdn-icons-png.flaticon.com/512/197/197435.png",
      country == "Saudi Arabia" ~ "https://cdn-icons-png.flaticon.com/512/330/330552.png",
      country == "Senegal" ~ "https://cdn-icons-png.flaticon.com/512/197/197377.png",
      country == "Serbia" ~ "https://cdn-icons-png.flaticon.com/512/197/197602.png",
      country == "Seychelles" ~ "https://cdn-icons-png.flaticon.com/512/197/197487.png",
      country == "Sierra Leone" ~ "https://cdn-icons-png.flaticon.com/512/197/197488.png",
      country == "Singapore" ~ "https://cdn-icons-png.flaticon.com/512/197/197496.png",
      country == "Slovakia" ~ "https://cdn-icons-png.flaticon.com/512/197/197592.png",
      country == "Slovenia" ~ "https://cdn-icons-png.flaticon.com/512/197/197633.png",
      country == "Solomon Islands" ~ "https://cdn-icons-png.flaticon.com/512/197/197474.png",
      country == "Somalia" ~ "https://cdn-icons-png.flaticon.com/512/197/197438.png",
      country == "South Africa" ~ "https://cdn-icons-png.flaticon.com/512/197/197562.png",
      country == "Republic of Korea" ~ "https://cdn-icons-png.flaticon.com/512/197/197582.png",
      country == "South Sudan" ~ "https://cdn-icons-png.flaticon.com/512/197/197476.png",
      country == "Spain" ~ "https://cdn-icons-png.flaticon.com/512/197/197593.png",
      country == "Sri Lanka" ~ "https://cdn-icons-png.flaticon.com/512/197/197398.png",
      country == "Saint Kitts and Nevis" ~ "https://cdn-icons-png.flaticon.com/512/197/197470.png",
      country == "Saint Lucia" ~ "https://cdn-icons-png.flaticon.com/512/323/323348.png",
      country == "Saint Vincent and the Grenadines" ~ "https://cdn-icons-png.flaticon.com/512/323/323374.png",
      country == "Sudan" ~ "https://cdn-icons-png.flaticon.com/512/197/197415.png",
      country == "Suriname" ~ "https://cdn-icons-png.flaticon.com/512/197/197439.png",
      country == "Sweden" ~ "https://cdn-icons-png.flaticon.com/512/197/197564.png",
      country == "Switzerland" ~ "https://cdn-icons-png.flaticon.com/512/197/197540.png",
      country == "Syrian Arab Republic" ~ "https://cdn-icons-png.flaticon.com/512/197/197598.png",
      country == "Tajikistan" ~ "https://cdn-icons-png.flaticon.com/512/197/197400.png",
      country == "United Republic of Tanzania" ~ "https://cdn-icons-png.flaticon.com/512/197/197634.png",
      country == "Thailand" ~ "https://cdn-icons-png.flaticon.com/512/323/323281.png",
      country == "Gambia" ~ "https://cdn-icons-png.flaticon.com/512/197/197478.png",
      country == "Timor-Leste" ~ "https://cdn-icons-png.flaticon.com/512/197/197410.png",
      country == "Togo" ~ "https://cdn-icons-png.flaticon.com/512/197/197443.png",
      country == "Tonga" ~ "https://cdn-icons-png.flaticon.com/512/197/197499.png",
      country == "Trinidad and Tobago" ~ "https://cdn-icons-png.flaticon.com/512/197/197401.png",
      country == "Tunisia" ~ "https://cdn-icons-png.flaticon.com/512/197/197624.png",
      country == "Turkey" ~ "https://cdn-icons-png.flaticon.com/512/197/197518.png",
      country == "Turkmenistan" ~ "https://cdn-icons-png.flaticon.com/512/197/197444.png",
      country == "Tuvalu" ~ "https://cdn-icons-png.flaticon.com/512/197/197445.png",
      country == "Uganda" ~ "https://cdn-icons-png.flaticon.com/512/197/197628.png",
      country == "Ukraine" ~ "https://cdn-icons-png.flaticon.com/512/197/197572.png",
      country == "United Arab Emirates" ~ "https://cdn-icons-png.flaticon.com/512/323/323301.png",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "https://cdn-icons-png.flaticon.com/512/197/197374.png",
      country == "United States of America" ~ "https://cdn-icons-png.flaticon.com/512/197/197484.png",
      country == "Uruguay" ~ "https://cdn-icons-png.flaticon.com/512/197/197599.png",
      country == "Uzbekistan" ~ "https://cdn-icons-png.flaticon.com/512/197/197416.png",
      country == "Vanuatu" ~ "https://cdn-icons-png.flaticon.com/512/197/197489.png",
      country == "Venezuela (Bolivarian Republic of)" ~ "https://cdn-icons-png.flaticon.com/512/197/197580.png",
      country == "Viet Nam" ~ "https://cdn-icons-png.flaticon.com/512/323/323319.png",
      country == "Yemen" ~ "https://cdn-icons-png.flaticon.com/512/197/197417.png",
      country == "Zambia" ~ "https://cdn-icons-png.flaticon.com/512/197/197621.png",
      country == "Zimbabwe" ~ "https://cdn-icons-png.flaticon.com/512/197/197394.png"
    )
  ) %>%
  
  select(region_name, flag_img, country, everything()) %>%
  
  mutate(
    status = case_when(
      status == "NF" ~ "Not Free",
      status == "PF" ~ "Partially Free",
      TRUE ~ "Free"
    ),
    is_ldc = case_when(is_ldc == 1 ~ "Least Developed",
                       is_ldc == 0 ~ "Developed")
  ) %>%
  
  mutate(is_ldc_colors = case_when(is_ldc == "Least Developed" ~ "#F6955E",
                                   TRUE ~ "#A8CDEC")) %>%
  
  select(-region_code) 


# Creating Filtering Table ------------------------------------------------

library(crosstalk)
library(htmltools)

crosstalk_data <- SharedData$new(freedom_with_flags)

country_filter <- filter_select(
  id = "country", 
  label = "Country", 
  group = ~ country, 
  sharedData = crosstalk_data
)

year_filter <- filter_slider(
  id = "year", 
  label = "Year", 
  column = ~ year, 
  sharedData = crosstalk_data
)

cl_filter <- filter_slider(
  id = "cl", 
  label = "Civil Liberty", 
  column = ~ cl,
  sharedData = crosstalk_data
)
  

# Reactable begins here ---------------------------------------------------

freedom_with_flags %>% 
  reactable(
    # Add theme
    theme = fivethirtyeight(
      font_size = 12,
      header_font_size = 12,
      centered = TRUE
    ),
    
    compact = TRUE,
    defaultSorted = "region_name",
    
    # Column Grouped
    
    columnGroups = list(
      colGroup(name = "Freedom Scores", 
               columns = c("cl", "pr")), 
      
      colGroup(name = "Status", 
               columns = c("status", "is_ldc"))
    ),
    
    # Separate groups by this column
    rowStyle = group_border_sort("region_name"),
    
    # Modify column names
    columns = list(
      
      country = colDef(
        name = "country",
        style = list(borderRight = "1px solid #777")
      ),
      flag_img = colDef(
        name = "",
        style = background_img(height = "45%", width = "100%"),
        maxWidth = 20,
        sortable = FALSE,
        align = "right"
      ),
      cl = colDef(name = "Civil Liberties", align = "center"),
      pr = colDef(
        name = "Political Rights",
        align = "center",
        style = list(borderRight = "1px solid #777")
      ),
      status = colDef(name = "Free?", align = "center"),
      region_name = colDef(
        name = "Region",
        style = group_merge_sort("Region_Name"),
        align = "center"
      ),
      is_ldc = colDef(
        name = "Dev. Status",
        align = "center",
        cell = pill_buttons(., color_ref = "is_ldc_colors", opacity = 0.8)
      ),
      year   = colDef(
        name = "Year",
        maxWidth = 60,
        style = list(borderRight = "1px solid #777"),
        align = "center"
      ),
      is_ldc_colors = colDef(show = FALSE)
    ),
    # controls the width of the whole table
    fullWidth = FALSE
  ) %>%
  add_title("World Freedom Index: Development Status", margin = margin(0, 0, 10, 0))



# filtering reactable for shiny w/ crosstalk ------------------------------

tbl <- htmltools::browsable(
  tagList(
    tags$br(),
    br(),
    reactable(crosstalk_data, 
              compact = TRUE,
              theme = fivethirtyeight(
                font_size = 12,
                header_font_size = 12,
                centered = TRUE
              ),
              columns = list(
                country = colDef(
                  name = "country",
                  style = list(borderRight = "1px solid #777")
                ),
                flag_img = colDef(
                  name = "",
                  style = background_img(height = "45%", width = "100%"),
                  maxWidth = 20,
                  sortable = FALSE,
                  align = "right"
                ),
                cl = colDef(name = "Civil Liberties", align = "center"),
                pr = colDef(
                  name = "Political Rights",
                  align = "center",
                  style = list(borderRight = "1px solid #777")
                ),
                status = colDef(name = "Free?", align = "center"),
                region_name = colDef(
                  name = "Region",
                  style = group_merge_sort("Region_Name"),
                  align = "center"
                ),
                is_ldc = colDef(
                  name = "Dev. Status",
                  align = "center",
                  cell = pill_buttons(freedom_with_flags, color_ref = "is_ldc_colors", opacity = 0.8)
                ),
                year   = colDef(
                  name = "Year",
                  maxWidth = 60,
                  style = list(borderRight = "1px solid #777"),
                  align = "center"
                ),
                is_ldc_colors = colDef(show = FALSE)
              ), 
              fullWidth = FALSE)))


# Adding iso2c country code -----------------------------------------------

test_data <- freedom %>%
  mutate(country_code = countrycode(country, "country.name", "iso2c")) %>% 
  filter(year == 2020)


# Left joining test_data to world shapefile  ------------------------------

# This is also to make sure that the countries are rightly represented

world_shp_1 <- world_shp %>%
  left_join(test_data,
            by = c("WB_A2" = "country_code"),
            suffix = c("", "_wfi")) %>%
  mutate(cl_label = str_glue("Civil Liberty Score: {cl} \n\n Country: {ADMIN}"),
         is_ldc = as_factor(is_ldc),
         fill_color_val = case_when(is_ldc == 0 ~ "#A8CDEC",
                                    TRUE ~ "#F6955E"),
         cl = as_factor(cl)) 


# Leaflet map check -------------------------------------------------------

leaflet(
  options=list(
    center = c(0, 0),
    zoom = 0.5,
    worldCopyJump = FALSE,
    maxBounds = list(
      list(-90, -180), # hides Antarctica on big map
      list(90, 180)
    ))
) %>% 
  addTiles() %>% 
  addPolygons(data = world_shp, 
              weight = 1, 
              smoothFactor = 0.5, 
              color = "white",
              fillColor = ~world_shp_1$fill_color_val, 
              opacity = 1.0,
              fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ), 
              popup = ~world_shp_1$cl_label)

