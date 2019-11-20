pacman::p_load(readr,sparklyr,dplyr)
spark_installed_versions()
sparkversion <- spark_installed_versions()$spark
sc <- spark_connect(master = "local", version = sparkversion)
lending <- spark_read_csv(sc,"lendingclub_12-15.csv")
lending %>% sample_n(5)
