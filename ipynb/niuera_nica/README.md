##Explanation of Data Sources:

###Hourly Data
For this data, there is a folder called niuera_nica_data that contains all of the relevant hourly data. When loading the class, you will give it a data directory that points to this folder.

All hourly data has been scraped and put into monthly .csv files. These files are all in named folders such as NatGenDespacho and NatHydroHeight. These files are necessary for this to work. To add new data to these files, there is a scraping notebook that downloads a raw csv. Typically this csv must be slightly adjusted when a new plant comes online (columns must be shifted over), but once this is complete the csv file can be stored in the proper folder. The prediction data has been combined and put into a single .csv file called predicted_hourly.csv

###15-Second Data
For this data, you need to go through the process of creating a new postgres database called cndc_15_sec_db
with user cndc_15_sec and password cndc_15_sec so that you can load the dump into postgres
I can't find the documentation for how you get into the psql admin mode on a mac but it was something along the lines of:
    
    sudo -u Diego psql postgres
    
Once in postgres in the terminal, run the following to create a new database (use password 'cndc_15_sec'):
    
    create database cndc_15_sec_db;
    create user cndc_15_sec;
    \password cndc_15_sec;
    grant all privileges on database cndc_15_sec_db to cndc_15_sec;
    \q
    
Now navigate in the terminal to the file that is in Dropbox: Nicaragua_DR/Data_for_Diego/cndc_15_sec.5.3.2016.sql and load it into sql using the following command:
     
     psql -U cndc_15_sec cndc_15_sec_db < cndc_15_sec.5.3.2016.sql
     
The password is:
    
    cndc_15_sec

If everything goes well, the data should be loaded and this notebook should show values. There are a lot of values so it may take awhile to load into a dataframe.



##Future Development (Turning everything into sql tables)
user: niuera_analyzer
pass: analysis

db_hourly: niuera_cndc_hourly_db
db_15_sec: niuera_cndc_15_sec_db
db_other: user prices, weather



