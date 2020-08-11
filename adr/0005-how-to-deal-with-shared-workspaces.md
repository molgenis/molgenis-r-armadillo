# 5. How to deal with shared workspaces

Date: 2020-08-11

## Status

Accepted

## Context

In LifeCycle we are using 2 kinds of collections of data files/tables.

**Method 1** - Datasets containing all the harmonised data within LifeCycle
- lc_core_gecko_1_1 - *projects in Opal, RData files in the Armadillo*
  - 1_1_core_non_rep_1_1 - *tables in Opal, R data.frame objects in Armadillo*
  - 1_1_core_yearly_rep_1_1
  - 1_1_core_monthly_rep_1_1
  - 1_1_core_trimester_rep_1_1
- lc_outcome_gecko_1_1 - *projects in Opal, RData files in the Armadillo*
  - 1_1_core_non_rep_1_1 - *tables in Opal, R data.frame objects in Armadillo*
  - 1_1_core_yearly_rep_1_1
  - 1_1_core_monthly_rep_1_1
  - 1_1_core_trimester_rep_1_1

**Method 2** - Study specific datasets which contains only the variables you want to expose
  - lc_core_gecko_1_1 - *projects in Opal, RData files in the Armadillo*
    - 1_1_core_non_rep_diabetes_1_1 - *tables in Opal, R data.frame objects in Armadillo*
  - lc_core_gecko_1_1 - *projects in Opal, RData files in the Armadillo*
    - 1_1_core_non_rep_diabetes_1_1 - *tables in Opal, R data.frame objects in Armadillo*

### Versioning
In the current version scheme of LifeCycle we distinguish 2 versions. The dictionary version and the data version. 

`x_x_#dictionary_kind#_#cohort#_x_x`.

The first `x_x` = the dictionary version (data model version)
The second `x_x` = the data version (verison

### Accessing data
Based upon the cohort guidelines on exposing data to researchers one way or the other is chosen to give researchers access to the data.

In Opal you need both datasets. One dataset containing all the harmonised data, shown in method 1.

When cohorts only want to expose the variables that are requested for a specific study, you can use views on top of these tables. The views do not contain the data, only the representation of the variables. In Opal you can manage permissions per view, which makes it possible to put all the tables and views related to 1 dictionary version in one project.

## Possible solutions in Armadillo
There are 2 possible solutions within the table loading part of the Armadillo.
- Make use of `.RData` files to store the data frames you want to use
- Make use of `.parquet` files to store the data frame you want to use

In the Armadillo we have a general concept of buckets. These buckets represent all the data of a cohort or study specific data. e.g.
- GECKO_ALL
- GECKO_DIABETES

### Managing data with `.RData` files
Within the buckets we have RData files containing all the tables (R data frames) that are bound in a specific context.
- GECKO_ALL
  - 1_0_core_1_1.RData
    - non_rep
    - yearly_rep
    - monthly_rep
    - trimester_rep
  - 1_0_outcome_1_1_non_rep.RData
- GECKO_DIABETES
  - 1_0_core_1_1.RData
    - non_rep
    - yearly_rep

#### Make the data available
When you login to the Armadillo you specify which RData files you want to have available in your analysis environment. 

`workspace=GECKO_DIABETES/1_0_core_1_1&workspace=GECKO_ALL/1_0_outcome_1_1`

Specifying these workspaces allows you to assign all the columns that are avialble in the data frames that are in the RData files.

#### Assigning the data
When you want to use the data in a DataSHIELD analysis you need to assign the data. So when you login the data becomes available in a private environment in R and when you assign data it will be copied to the analysis environment in R.
You will assign all data or parts of the data depending on the analysis  you want to do.

Assigning parts of the data is more efficient and analysis will become faster.

#### Advantages
- You can use native R components to get the data available for your analyis
- You can implement resources fairly easy in RData files

#### Disadvantages
- When you load the data it can take a lot of time to get the data into the R-environment
- Subsetting the data is not efficient in larger datasets

### Managing data in `.parquet` files
Managing data in `.parquet` files allows us to deal with larger data more efficiently. This changes the concepts a little within our file storage (minio). We already work with buckets as described above. Parquet does not allow to have more than one table 

- GECKO_ALL
  - 1_0_core_1_1 (minio -> folder concept)
    - non_rep.parquet
    - yearly_rep.parquet
    - monthly_rep.parquet
    - trimester_rep.parquet
  - 1_0_outcome_1_1_non_rep (minio -> folder concept)
- GECKO_DIABETES
  - 1_0_core_1_1 (minio -> folder concept)
    - non_rep.parquet
    - yearly_rep.parquet

## Decision


## Consequences
- The approach is based upon 
