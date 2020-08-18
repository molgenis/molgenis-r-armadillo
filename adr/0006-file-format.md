# 6. File format

Date: 2020-08-12

## Status

Accepted

## Context
There are 2 possible solutions within the table loading part of the Armadillo.
- Make use of `.RData` files to store the data frames you want to use
- Make use of `.parquet` files to store the data frame you want to use

In the Armadillo we have a general concept of buckets. These buckets represent all the data of a cohort or study specific data. e.g.
- gecko-all
- gecko-diabetes

### Managing data with `.RData` files
Within the buckets we have `.RData` files containing all the tables (R data frames) that are bound in a specific context.
- gecko-all
  - 1_0_core_1_1.RData
    - non_rep
    - yearly_rep
    - monthly_rep
    - trimester_rep
  - 1_0_outcome_1_1_non_rep.RData
- gecko-diabetes
  - 1_0_core_1_1.RData
    - non_rep
    - yearly_rep

#### Make the data available
When you login to the Armadillo you specify which `.RData` files you want to have available in your analysis environment. 

`workspace=gecko-diabetes/1_0_core_1_1&workspace=gecko-all/1_0_outcome_1_1`

Specifying these workspaces allows you to assign all the columns that are available in the data frames that are in the `.RData` files.
Technically it loads the workspaces in memory to be able to see which tables are available.

#### Assigning the data
When you want to use the data in a DataSHIELD analysis you need to assign the data. So when you login the data becomes available in a private environment in R. When you assign data it will be copied to the analysis environment in R.
You need to assign all data or parts of the data depending on the analysis you want to do. Assigning parts of the data is more efficient and analysis will become faster.

In the RData solution all the data in the private and the analysis environment is in memory. When you assign the data, it will be copied to the analysis environment. You can specify a subset of the data as well. The copy will contain only the selected variables.

#### Advantages
- You can use native `R` components to get the data available for your analysis
- You can implement resources fairly easy in `.RData` or `rds` files

#### Disadvantages
- It uses large amounts of data per user when loading large files. Which means you can not work with a large number of users at the same time on the Armadillo.
- Logging in takes a lot of time, because workspaces are `.RData` files and contain all the tables. Tables need to be loaded to be checked by the DataSHIELD package.

### Managing data in `.parquet` files
Managing data in `.parquet` files allows us to deal with larger data more efficiently. This changes the concepts a little within our file storage (minio). We already work with buckets as described above. Parquet does not allow to have more than one data.frame in a parquet file. The parquet files are more or less substitutes for RData files. The logical structure is the same, however the technical structure is somewhat different.

- gecko-all
  - 1_0_core_1_1 (minio -> folder concept)
    - non_rep.parquet
    - yearly_rep.parquet
    - monthly_rep.parquet
    - trimester_rep.parquet
  - 1_0_outcome_1_1_non_rep (minio -> folder concept)
- gecko-diabetes
  - 1_0_core_1_1 (minio -> folder concept)
    - non_rep.parquet
    - yearly_rep.parquet

#### Make the data available
When you login you only specify the tables you want to load. You need specify the fully quelified names of the tables you want to load:

`folder/workspace/table.parquet`

For example:

`gecko-all/1_0-core-1_1/non-rep.parquet`

Specifying these workspaces allows you to assign all the columns that are available in the data.frames that are in the `.parquet` files.
This is not loading the workspaces in memory, but loads them when you assign the tables. At login time you only check wether you may access the folders.

#### Assigning the data
The difference with the RData solution is that the files will not be loaded in memory initially only when the data is assigned. More specifically only the selected data will be assigned. You do not have a copy of the whole data in memory in a private environment.

#### Storing complex structures
Another format to store complex structures is `rds`. The `rds` format is capable to store 1 object in 1 file. You can directly see what is in the file. With `RData` you need to load the file when you want to know what it is containing. In `rds` you see directly what the contents is. 

#### Advantages
- You do not need to specify the workspace(s) or table(s) during login which makes it faster
- You are able to delay loading the data in the R environment, which makes it faster to login and assign data
- When you specify specific columns from the dataset it is highly efficient to load the data in the R environment
- Loading and saving parquet files is faster. So data handling actions will be more efficient.

#### Disadvantages
- An extra dependency in the package
- More risk when converting that is not R-native (think of dates, number of significant bits, etc.)

## Decision
The principle we are going implement is to store 1 object per file. Which means that for table-like object we are going to use `.parquet`and for more complex structures we are going to use `.rds` (such as *resources* used in Opal). 
We are going to use the parquet files in the Armadillo package. As data manager you do not *have* to work with the parquet format.

## Consequences
- Restriction on table names (depending on Minio file restrictions)
- `.parquet` files require a package to be added to the depenency list
- You do not need to specify workspace(s) or table(s) during login
- Assigning data will be a lot faster
- Creating datasets as a datamanager will be a lot faster
- You can update single tables with `.parquet`
- In MOLGENIS you can build a parquet exporter fairly easy, usefull when using MOLGENIS as data provider
- Users work with fully qualified table names