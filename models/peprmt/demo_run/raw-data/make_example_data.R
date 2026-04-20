
# Manually enter site info
US_EDN = list(id = "US_EDN",
              name = "Eden Landing",
              lat = 37.615,
              lon = -122.114,
              site.pft = "default",
              met.start = "2018-04-03",
              met.end = "2024-11-19")

US_SRR = list(id = "US_SRR",
              name = "Rush Ranch",
              lat = 38.200,
              lon = -122.026,
              site.pft = "default",
              met.start = "2014-03-12",
              met.end = "2018-09-20")

US_DMG = list(id = "US_DMG",
              name = "Dutch Slough",
              lat = 38.0015,
              lon = -121.6691,
              site.pft = "default",
              met.start = "2021-09-22",
              met.end = "2024-12-31") 
# NOTE on US_DMG: Hypothetically we have data through "2025-10-07", but I don't 
# have an ERA5 met file for 2025 yet

site_info <- list(US_EDN,
                  US_SRR,
                  US_DMG) |>
  dplyr::bind_rows()

write.csv(site_info, 
          here::here("models", "peprmt", "demo_run", "data",
                     "site_info.csv"),
          row.names = F)
