reset_crs <- function(df) {
  
  df <- st_as_sf(df)
  
  df2 <- st_transform(df, st_crs(get_tracts)) %>%
    st_make_valid()
  
  return(df2)
}






load_census_data <- function(geo, snap = TRUE, medicaid = TRUE, 
                             pop = TRUE)
{
  
  temp <- get_acs(
    
  )
  
  
}



load_packages <- function()
  
{
  # load_packages.R
  required_packages <- c("readxl", "data.table", "DT", 
                         "sf", "mapview", "dplyr", "scales",
                         "tidycensus", "tidyverse", "nngeo", 
                         "leaflet", "janitor", "kableExtra", 
                         "ggmap", "tidyr")
  
  lapply(required_packages, library, character.only = TRUE)
  
  
}









load_chas <- function()
{
  
  
  chas_df <- read.csv("/Users/jacobford/Library/CloudStorage/GoogleDrive-jake@solstice.us/Shared drives/Product | Jake/Geocoding/Engie:Microsoft Geodata/CHAS/Table11.csv")
  
  # return(chas_df)
  chas_df <- chas_df %>%
    filter(st %in% c(17,35,25,34,36,27, 6, 10)) %>%

    mutate(geoid = gsub("14000US", "", geoid)) %>%
    
    
    mutate(
      
      AMI_30 = T11_est47 +T11_est48+T11_est61+T11_est62+T11_est75+T11_est76,
      AMI_50 = T11_est47 +T11_est48+T11_est49+T11_est50+T11_est61+T11_est62+T11_est63+T11_est64+T11_est75+T11_est76+T11_est77+T11_est78,
      
      
      AMI_80 = T11_est4+T11_est5+T11_est6+T11_est7+T11_est9+T11_est10+T11_est18+T11_est19+T11_est20+T11_est21+T11_est23+T11_est24+T11_est32+T11_est33+T11_est34+T11_est35+T11_est37+T11_est38+T11_est47+T11_est48+T11_est49+
        T11_est50+T11_est52+T11_est53+T11_est61+T11_est62+T11_est63+T11_est64+T11_est66+T11_est67+T11_est75+T11_est76+T11_est77+T11_est78+T11_est80+T11_est81,
      
      AMI_Over_80 = 
        T11_est11+
        T11_est12+
        T11_est13+
        T11_est14+
        T11_est15+
        T11_est16+
        T11_est25+
        T11_est26+
        T11_est27+
        T11_est28+
        T11_est29+
        T11_est30+
        T11_est39+
        T11_est40+
        T11_est41+
        T11_est42+
        T11_est43+
        T11_est44+
        T11_est54+
        T11_est55+
        T11_est56+
        T11_est57+
        T11_est58+
        T11_est59+
        T11_est68+
        T11_est69+
        T11_est70+
        T11_est71+
        T11_est72+
        T11_est73+
        T11_est82+
        T11_est83+
        T11_est84+
        T11_est85+
        T11_est86+
        T11_est87
      
    ) %>%
    mutate(
      AMI_100 = T11_est4+T11_est5+T11_est6+T11_est7+T11_est8+T11_est9+T11_est10
      +T11_est11+T11_est12+T11_est18+T11_est19+T11_est20
      +T11_est21+T11_est22+T11_est23+T11_est24+T11_est25+T11_est26
      +T11_est32+T11_est33+T11_est34+T11_est35+T11_est36+T11_est37+T11_est38
      +T11_est39+T11_est40+T11_est47+T11_est48+T11_est49
      +T11_est50+T11_est51+T11_est52+T11_est53+T11_est54+T11_est55+
        +T11_est61+T11_est62+T11_est63+T11_est64+T11_est65+T11_est66+T11_est67
      +T11_est68+T11_est69+T11_est75+T11_est76+T11_est77
      +T11_est78+T11_est79+T11_est80+T11_est81+T11_est82+T11_est83,
      AMI_Over_100 =T11_est16+T11_est30+T11_est44+T11_est59+T11_est73+T11_est87 +T11_est29+T11_est43+T11_est58+T11_est72+T11_est86+T11_est42+T11_est57+T11_est71+T11_est85 + T11_est14 +T11_est28+
        T11_est13+T11_est27+T11_est41+T11_est56+T11_est70+T11_est84
      
      
    ) %>%
    mutate(
      AMI_115 = T11_est4+T11_est5+T11_est6+T11_est7+T11_est8+T11_est9+T11_est10
      +T11_est11+T11_est12+T11_est13+T11_est18+T11_est19+T11_est20
      +T11_est21+T11_est22+T11_est23+T11_est24+T11_est25+T11_est26+T11_est27
      +T11_est32+T11_est33+T11_est34+T11_est35+T11_est36+T11_est37+T11_est38
      +T11_est39+T11_est40+T11_est41+T11_est47+T11_est48+T11_est49
      +T11_est50+T11_est51+T11_est52+T11_est53+T11_est54+T11_est55+T11_est56
      +T11_est61+T11_est62+T11_est63+T11_est64+T11_est65+T11_est66+T11_est67
      +T11_est68+T11_est69+T11_est70+T11_est75+T11_est76+T11_est77
      +T11_est78+T11_est79+T11_est80+T11_est81+T11_est82+T11_est83+T11_est84,
      AMI_Over_115 =T11_est16+T11_est30+T11_est44+T11_est59+T11_est73+T11_est87 +T11_est29+T11_est43+T11_est58+T11_est72+T11_est86+T11_est42+T11_est57+T11_est71+T11_est85 + T11_est14 +T11_est28
      
      
    ) %>%
    mutate(
      AMI_120 = T11_est4+T11_est5+T11_est6+T11_est7+T11_est8+T11_est9+T11_est10+T11_est11+T11_est12+T11_est13+T11_est14+T11_est18+T11_est19+T11_est20
      +T11_est21+T11_est22+T11_est23+T11_est24+T11_est25+T11_est26+T11_est27+T11_est28+T11_est32+T11_est33+T11_est34+T11_est35+T11_est36+T11_est37+T11_est38
      +T11_est39+T11_est40+T11_est41+T11_est42+T11_est47+T11_est48+T11_est49+T11_est50+T11_est51+T11_est52+T11_est53+T11_est54+T11_est55+T11_est56+T11_est57
      +T11_est61+T11_est62+T11_est63+T11_est64+T11_est65+T11_est66+T11_est67+T11_est68+T11_est69+T11_est70+T11_est71+T11_est75+T11_est76+T11_est77
      +T11_est78+T11_est79+T11_est80+T11_est81+T11_est82+T11_est83+T11_est84+T11_est85,
      AMI_Over_120 =T11_est16+T11_est30+T11_est44+T11_est59+T11_est73+T11_est87 +T11_est29+T11_est43+T11_est58+T11_est72+T11_est86 +T11_est15
      
    ) %>%
    mutate(
      AMI_140 = T11_est4+T11_est5+T11_est6+T11_est7+T11_est8+T11_est9+T11_est10
      +T11_est11+T11_est12+T11_est13+T11_est14+T11_est15+T11_est18+T11_est19+T11_est20
      +T11_est21+T11_est22+T11_est23+T11_est24+T11_est25+T11_est26+T11_est27+T11_est28
      +T11_est29+T11_est32+T11_est33+T11_est34+T11_est35+T11_est36+T11_est37+T11_est38
      +T11_est39+T11_est40+T11_est41+T11_est42+T11_est43+T11_est47+T11_est48+T11_est49
      +T11_est50+T11_est51+T11_est52+T11_est53+T11_est54+T11_est55+T11_est56+T11_est57
      +T11_est58+T11_est61+T11_est62+T11_est63+T11_est64+T11_est65+T11_est66+T11_est67
      +T11_est68+T11_est69+T11_est70+T11_est71+T11_est72+T11_est75+T11_est76+T11_est77
      +T11_est78+T11_est79+T11_est80+T11_est81+T11_est82+T11_est83+T11_est84+T11_est85
      +T11_est86,
      AMI_Over_140 =T11_est16+T11_est30+T11_est44+T11_est59+T11_est73+T11_est87 ) %>%
    mutate(
      AMI_140_Pct = AMI_140/(AMI_140+AMI_Over_140),
      AMI_120_Pct = AMI_120/(AMI_120 + AMI_Over_120),
      AMI_115_Pct = AMI_115/(AMI_115 + AMI_Over_115),
      AMI_100_Pct = AMI_100/(AMI_100 + AMI_Over_100),
      AMI_80_Pct = AMI_80/(AMI_80+AMI_Over_80),
    #  Total_Pop = AMI_120 + AMI_Over_120
    ) %>%
    select(geoid, name, state=st, AMI_140_Pct, AMI_120_Pct, AMI_115_Pct,AMI_100_Pct,AMI_80_Pct,
           AMI_140, AMI_120, AMI_115,AMI_100,AMI_80, AMI_50, AMI_30) %>%
    mutate(
      n_140 = case_when(
        AMI_140_Pct >= 0.5 ~ 1,
        TRUE ~ 0),
      n_120 = case_when(
        AMI_120_Pct >= 0.5 ~ 1,
        TRUE ~ 0),
      n_115 = case_when(
        AMI_115_Pct >= 0.5 ~ 1,
        TRUE ~ 0),
      n_100 = case_when(
        AMI_100_Pct >= 0.5 ~ 1,
        TRUE ~ 0)
    )
  
  chas_df
  
  
}


throw_some_geo_on_that <- function(state_shape, census_lines)
  # takes the state utility zone layer as a SF, overlays the 
  # census tracts and counts the number of HHs <= 100% AMI 

{

  
  
}
































