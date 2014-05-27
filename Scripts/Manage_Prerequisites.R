source("Scripts/Check_for_and_download_HMD.R")

# For reproducibility, the old data should be used. However, this code will also allow
# the user to produce the same graphics using the newer data


# Make derived data

# Country codes : older
dir.create("Data/Derived/", showWarnings=F, recursive=T)
if (OlderData){
    Country_Codes_Older <- read.xlsx(
        file="Data/CountryCodes.xlsx",
        sheetName="Country_Codes"    
    )
    # Derived Data : Older
    
    if(file.exists("Data/Derived/Derived_Data_Older.RData")){
        print("Older Derived Data already found")
        load("Data/Derived/Derived_Data_Older.RData")
    } else {
        print("Older Derived Data not found. Making from Scratch")
        source("Scripts/Make_Derived_Data_Older.R")
    }
    
    
} else {
    # Country codes: newer
    Country_Codes_Newer <- Make_Country_DF(
        directory="Data/HMD/Newer/population/Population/"    
    )
    # Derived Data : Newer
    if(file.exists("Data/Derived/Derived_Data_Newer.RData")){
        print("Newer Derived Data already found")
        load("Data/Derived/Derived_Data_Newer.RData")
    } else {
        print("Newer Derived Data not found. Making from Scratch")
        source("Scripts/Make_Derived_Data_Newer.R")
    }
    
    
}





