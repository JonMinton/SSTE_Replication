# Make derive

New_Country_Codes <- Make_Country_DF(
    directory="Data/HMD/population/Population/"    
)

Make_Derived_Data(
    HMD_Location="Data/HMD",
    Country.Codes=New_Country_Codes,
    Outfile_Location="Data/Derived",
    Outfile_Name="Derived_Data.RData",
    old_HMD=FALSE
)
