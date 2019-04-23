### I like having a clean location for listing counties
mn_county2 <- subset(counties, region == "minnesota" & 
c(subregion == "cottonwood" |
subregion == "lac qui parle"|
subregion == "yellow medicine"|
subregion == "redwood"|
subregion == "brown"|
subregion == "blue earth"|
subregion == "faribault"|
subregion == "martin"|
subregion == "watonwan"|
subregion == "jackson"|
subregion == "nobles"|
subregion == "rock"|
subregion == "pipestone"|
subregion == "lincoln"|
subregion == "lyon"|
subregion == "murray"))