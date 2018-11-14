-- This is sql file to create a database and some tables
-- in MySQL in order to manipulate recruit restaurant reservation data.
-- MySQL version is 8.0.12

DROP TABLE IF EXISTS air_visit;
CREATE TABLE air_visit (
 air_store_id VARCHAR(255),
 visit_date DATE,
 visitors INT DEFAULT NULL
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/air_visit_data.csv'
INTO TABLE air_visit
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS air_reserve;
CREATE TABLE air_reserve (
 air_store_id VARCHAR(255),
 visit_datatime DATETIME,
 reserve_datetime DATETIME,
 reserve_visitors INT DEFAULT NULL
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/air_reserve.csv'
INTO TABLE air_reserve
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS hpg_reserve;
CREATE TABLE hpg_reserve (
 hpg_store_id VARCHAR(255),
 visit_datatime DATETIME,
 reserve_datetime DATETIME,
 reserve_visitors INT DEFAULT NULL
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/hpg_reserve.csv'
INTO TABLE hpg_reserve
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS air_store;
CREATE TABLE air_store (
 air_store_id VARCHAR(255),
 air_genre_name VARCHAR(255),
 air_area_name VARCHAR(255),
 latitude DOUBLE,
 longitude DOUBLE
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/air_store_info.csv'
INTO TABLE air_store
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS hpg_store;
CREATE TABLE hpg_store (
 hpg_store_id VARCHAR(255),
 hpg_genre_name VARCHAR(255),
 hpg_area_name VARCHAR(255),
 latitude DOUBLE,
 longitude DOUBLE
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/hpg_store_info.csv'
INTO TABLE hpg_store
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS holiday;
CREATE TABLE holiday (
 calendar_date DATE,
 day_of_week VARCHAR(255),
 holiday_flg BOOL
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/date_info.csv'
INTO TABLE holiday
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

DROP TABLE IF EXISTS store_id;
CREATE TABLE store_id (
 air_store_id VARCHAR(255),
 hpg_store_id VARCHAR(255)
);
LOAD DATA INFILE '/Users/yuki.ieiri/Workspace/kaggle/rcr_rsr_vst/data/store_id_relation.csv'
INTO TABLE store_id
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

-- end of file
