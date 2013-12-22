#test_ModisLoader.R
library(testthat)
library(blissR)


#files[1] <- "/mnt/lun0/csv4scidb/bk/MOD09Q1A2013001h11v100052013017012246bandMOD_Grid_250m_Surface_Reflectancesur_refl_b01.txt"
#files[2] <- "/mnt/lun0/csv4scidb/bk/MOD09Q1A2013001h11v100052013017012246bandMOD_Grid_250m_Surface_Reflectancesur_refl_b02.txt"
#files[3] <- "/mnt/lun0/csv4scidb/bk/MOD09Q1A2013001h11v100052013017012246bandMOD_Grid_250m_Surface_Reflectancesur_refl_qc_250m.txt"
#files[1] <- "/mnt/lun0/csv4scidb/f1.txt"
#files[2] <- "/mnt/lun0/csv4scidb/f1.txt"

files <- list.files("/mnt/lun0/csv4scidb", full.names = TRUE, recursive = FALSE, include.dirs = FALSE, pattern = "MOD")