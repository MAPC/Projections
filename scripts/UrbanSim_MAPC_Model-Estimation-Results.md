This page documents price model and choice model estimation results for the block-level MAPC UrbanSim model. The location choice models drive the placement of new and relocating agents in the simulation.The price regression models estimate the variation of buildings' values. For each submodel, a table of estimated coefficients/significances are presented. Measures of fit and other model evaluation metrics accompany the coefficient tables.

Model acronyms are defined as: \* HLCM: Household location choice model \* ELCM: Employment location choice model \* RDPLCM: Residential development project location choice model \* REPM: Residential/Employment Price model for values and price

## Household location choice model by Income, Age of Head, and Household Type

**2HLCM1 Model**

-   Agent segment: 1

-   Segment definition: (recent_mover == 1) & (household_type == 1)

-   Segment description: income: -35,000, age: -35 , persons: 1

-   Log likelihood ratio: 0.5296033046363722

-   pseudo-R²: 0.4703966953636278

Estimated coefficients for 2hlcm1:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_total_households         |    0.394425 |  0.0351686 |  11.2153 |
| st_mean_income                           |   -0.928303 |  0.0779589 | -11.9076 |
| st_pumas_density_residential_units       |    0.552475 |  0.0312159 |  17.6985 |
| st_ratio_households_to_residential_units |     2.95504 |   0.103479 |  28.5568 |
| st_res_btype_mode4                       |    0.564138 |  0.0464345 |  12.1491 |
| st_tracts_density_jobs                   |    0.257239 |  0.0211852 |  12.1424 |

**2HLCM2 Model**

-   Agent segment: 2

-   Segment definition: (recent_mover == 1) & (household_type == 2)

-   Segment description: income: -35,000, age: -35 , persons: 2+, children: 0

-   Log likelihood ratio: 0.5110973900136302

-   pseudo-R²: 0.4889026099863698

Estimated coefficients for 2hlcm2:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_total_households          |    0.433212 |  0.0357008 |  12.1345 |
| st_mean_income                            |   -0.949516 |  0.0889531 | -10.6743 |
| st_pumas_density_residential_units        |    0.627959 |    0.03859 |  16.2726 |
| st_ratio_households_to_residential_units  |     2.78843 |   0.115116 |  24.2228 |
| st_res_btype_mode4                        |    0.611893 |  0.0484628 |   12.626 |
| st_zones_total_jobs_7_dollars_travel_time |    0.328345 |  0.0752425 |  4.36383 |

**2HLCM3 Model**

-   Agent segment: 3

-   Segment definition: (recent_mover == 1) & (household_type == 3)

-   Segment description: income: -35,000, age: -35 , persons: 2+, children: 1

-   Log likelihood ratio: 0.666060993017088

-   pseudo-R²: 0.333939006982912

Estimated coefficients for 2hlcm3:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| PctSW_ISUSHW                             |    0.907998 |   0.162646 |  5.58265 |
| st_block_groups_mean_rent                |   -0.404079 |  0.0391771 | -10.3141 |
| st_block_groups_total_households         |    0.147401 |  0.0389734 |  3.78209 |
| st_nodes_high_income_hh_1500m            |   -0.566116 |  0.0786118 | -7.20141 |
| st_pumas_density_residential_units       |    0.365362 |  0.0364799 |  10.0154 |
| st_ratio_households_to_residential_units |     1.94952 |    0.12314 |  15.8317 |
| st_res_btype_mode4                       |    0.907753 |  0.0357692 |  25.3781 |

**2HLCM4 Model**

-   Agent segment: 4

-   Segment definition: (recent_mover == 1) & (household_type == 4)

-   Segment description: income: -35,000, age: 35-64 , persons: 1

-   Log likelihood ratio: 0.6925600497273285

-   pseudo-R²: 0.30743995027267146

Estimated coefficients for 2hlcm4:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| PctSW_ISUSHW                             |   -0.743439 |   0.136306 | -5.45417 |
| st_block_groups_mean_rent                |  -0.0651531 |  0.0392165 | -1.66137 |
| st_mean_income                           |    -1.42287 |  0.0822261 | -17.3044 |
| st_pumas_density_residential_units       |  -0.0643491 |  0.0349465 | -1.84136 |
| st_ratio_households_to_residential_units |     2.64913 |   0.102991 |  25.7221 |
| st_res_btype_mode4                       |    0.494751 |  0.0364684 |  13.5666 |
| st_tracts_density_jobs                   |    0.100401 |   0.035599 |  2.82033 |

**2HLCM5 Model**

-   Agent segment: 5

-   Segment definition: (recent_mover == 1) & (household_type == 5)

-   Segment description: income: -35,000, age: 35-64 , persons: 2+, children: 0

-   Log likelihood ratio: 0.6627721644178177

-   pseudo-R²: 0.3372278355821823

Estimated coefficients for 2hlcm5:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_density_residential_units             |    0.128779 |  0.0194187 |  6.63169 |
| st_mean_income                           |     -1.5111 |  0.0831237 | -18.1789 |
| st_nodes_high_income_hh_1500m            |   -0.421719 |  0.0592507 | -7.11754 |
| st_ratio_households_to_residential_units |     2.43379 |  0.0817987 |  29.7535 |
| st_res_btype_mode4                       |    0.468584 |   0.037675 |  12.4375 |
| st_tracts_density_jobs                   |  0.00674761 |  0.0405226 | 0.166515 |

**2HLCM6 Model**

-   Agent segment: 6

-   Segment definition: (recent_mover == 1) & (household_type == 6)

-   Segment description: income: -35,000, age: 35-64 , persons: 2+, children: 1

-   Log likelihood ratio: 0.664566468070141

-   pseudo-R²: 0.33543353192985903

Estimated coefficients for 2hlcm6:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| PctSW_ISUSHW                             |    0.419839 |   0.170828 |  2.45767 |
| st_block_groups_mean_rent                |   -0.109139 |   0.042566 |   -2.564 |
| st_block_groups_total_households         |   0.0480345 |  0.0382652 |  1.25531 |
| st_mean_income                           |    -1.34041 |  0.0861567 | -15.5578 |
| st_nodes_high_income_hh_1500m            |    -0.44001 |  0.0825565 |  -5.3298 |
| st_pumas_density_residential_units       |    0.143729 |  0.0387714 |   3.7071 |
| st_ratio_households_to_residential_units |     2.73788 |   0.119186 |  22.9714 |
| st_res_btype_mode4                       |    0.519322 |  0.0391495 |  13.2651 |
| st_tracts_density_jobs                   |   -0.134906 |  0.0635708 | -2.12214 |

**2HLCM7 Model**

-   Agent segment: 7

-   Segment definition: (recent_mover == 1) & (household_type == 7)

-   Segment description: income: 35,000-100,000, age: -35 , persons: 1

-   Log likelihood ratio: 0.659877547378658

-   pseudo-R²: 0.340122452621342

Estimated coefficients for 2hlcm7:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_total_households         |    0.449176 |  0.0317138 |  14.1634 |
| st_mean_income                           |   -0.297503 |  0.0731082 | -4.06935 |
| st_nodes_post2010_du_1500m               |    0.186477 |  0.0315446 |  5.91152 |
| st_pumas_density_residential_units       |    0.313466 |  0.0315039 |  9.95006 |
| st_ratio_households_to_residential_units |     2.38005 |  0.0803173 |  29.6331 |
| st_res_btype_mode4                       |    0.589132 |  0.0370323 |  15.9086 |
| st_tracts_density_jobs                   |    0.156599 |  0.0241264 |  6.49078 |

**2HLCM8 Model**

-   Agent segment: 8

-   Segment definition: (recent_mover == 1) & (household_type == 8)

-   Segment description: income: 35,000-100,000, age: -35 , persons: 2+, children: 0

-   Log likelihood ratio: 0.6661260963513765

-   pseudo-R²: 0.33387390364862346

Estimated coefficients for 2hlcm8:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_total_households         |    0.291161 |  0.0355856 |    8.182 |
| st_mean_income                           |   -0.345787 |  0.0787306 | -4.39203 |
| st_pumas_density_residential_units       |    0.432552 |  0.0282731 |   15.299 |
| st_ratio_households_to_residential_units |     2.51144 |  0.0798291 |  31.4602 |
| st_res_btype_mode4                       |    0.622897 |  0.0372802 |  16.7085 |

**2HLCM9 Model**

-   Agent segment: 9

-   Segment definition: (recent_mover == 1) & (household_type == 9)

-   Segment description: income: 35,000-100,000, age: -35 , persons: 2+, children: 1

-   Log likelihood ratio: 0.7589210164054502

-   pseudo-R²: 0.24107898359454982

Estimated coefficients for 2hlcm9:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_value               |   -0.402856 |  0.0592501 | -6.79925 |
| st_mean_income                           |    -0.58601 |  0.0923427 | -6.34603 |
| st_nodes_high_income_hh_1500m            |  -0.0821201 |  0.0642909 | -1.27732 |
| st_pumas_density_residential_units       |   -0.147455 |  0.0412228 | -3.57702 |
| st_ratio_households_to_residential_units |     2.43983 |  0.0634766 |  38.4367 |
| st_res_btype_mode4                       |    0.484914 |  0.0346701 |  13.9865 |

**2HLCM10 Model**

-   Agent segment: 10

-   Segment definition: (recent_mover == 1) & (household_type == 10)

-   Segment description: income: 35,000-100,000, age: 35-65 , persons: 1

-   Log likelihood ratio: 0.7783463682573432

-   pseudo-R²: 0.22165363174265684

Estimated coefficients for 2hlcm10:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_mean_income                           |   -0.656572 |  0.0686161 | -9.56878 |
| st_pumas_density_residential_units       |   -0.165873 |  0.0337213 | -4.91893 |
| st_ratio_households_to_residential_units |     2.45449 |  0.0595651 |  41.2069 |
| st_res_btype_mode4                       |    0.465131 |  0.0319213 |  14.5712 |
| st_tracts_density_jobs                   |   0.0600326 |  0.0446309 |  1.34509 |

**2HLCM11 Model**

-   Agent segment: 11

-   Segment definition: (recent_mover == 1) & (household_type == 11)

-   Segment description: income: 35,000-100,000, age: 35-65 , persons: 2+, children: 0

-   Log likelihood ratio: 0.772964212382061

-   pseudo-R²: 0.22703578761793897

Estimated coefficients for 2hlcm11:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_mean_income                           |   -0.762544 |  0.0749618 | -10.1724 |
| st_nodes_high_income_hh_1500m            |   -0.174838 |  0.0506817 | -3.44972 |
| st_ratio_households_to_residential_units |     2.74258 |  0.0629292 |   43.582 |
| st_res_btype_mode4                       |    0.425724 |  0.0326749 |  13.0291 |
| st_tracts_density_jobs                   |   -0.204169 |  0.0768992 | -2.65502 |

**2HLCM12 Model**

-   Agent segment: 12

-   Segment definition: (recent_mover == 1) & (household_type == 12)

-   Segment description: income: 35,000-100,000, age: 35-65 , persons: 2+, children: 1

-   Log likelihood ratio: 0.7578005956376918

-   pseudo-R²: 0.24219940436230825

Estimated coefficients for 2hlcm12:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_value               |    -0.31578 |  0.0602546 | -5.24076 |
| st_mean_income                           |   -0.503251 |  0.0881754 | -5.70738 |
| st_nodes_high_income_hh_1500m            |   -0.400757 |  0.0699619 | -5.72821 |
| st_pumas_density_residential_units       |    0.168354 |  0.0398992 |  4.21948 |
| st_ratio_households_to_residential_units |     2.58931 |  0.0623558 |  41.5248 |
| st_res_btype_mode4                       |    0.490197 |  0.0357547 |    13.71 |
| st_tracts_density_jobs                   |   -0.430687 |   0.100466 | -4.28689 |

**2HLCM13 Model**

-   Agent segment: 13

-   Segment definition: (recent_mover == 1) & (household_type == 13)

-   Segment description: income: +100,000, age: -35 , persons: 1

-   Log likelihood ratio: 0.5695042142309676

-   pseudo-R²: 0.43049578576903236

Estimated coefficients for 2hlcm13:

|                                           | Coefficient | Std. Error | T-Score |
|:------------------------------------------|------------:|-----------:|--------:|
| st_block_groups_mean_year_built           |    0.226696 |  0.0352428 | 6.43242 |
| st_mean_income                            |    0.594008 |  0.0444882 |  13.352 |
| st_pumas_density_residential_units        |     1.01501 |  0.0374469 | 27.1053 |
| st_ratio_households_to_residential_units  |     2.25908 |  0.0945502 | 23.8929 |
| st_res_btype_mode4                        |    0.503247 |  0.0341014 | 14.7574 |
| st_zones_total_jobs_7_dollars_travel_time |    0.504692 |  0.0912162 | 5.53292 |

**2HLCM14 Model**

-   Agent segment: 14

-   Segment definition: (recent_mover == 1) & (household_type == 14)

-   Segment description: income: +100,000, age: -35 , persons: 2+, children: 0

-   Log likelihood ratio: 0.6833052922530228

-   pseudo-R²: 0.31669470774697717

Estimated coefficients for 2hlcm14:

|                                          | Coefficient | Std. Error | T-Score |
|:-----------------------------------------|------------:|-----------:|--------:|
| st_block_groups_mean_year_built          |   0.0894073 |  0.0469239 | 1.90537 |
| st_block_groups_total_households         |    0.263839 |  0.0378386 | 6.97275 |
| st_mean_income                           |      0.1206 |  0.0665918 | 1.81104 |
| st_nodes_high_income_hh_1500m            |    0.779462 |  0.0750381 | 10.3876 |
| st_pumas_density_residential_units       |     0.31028 |  0.0409967 |  7.5684 |
| st_ratio_households_to_residential_units |     2.18585 |  0.0758277 | 28.8265 |
| st_res_btype_mode4                       |    0.400562 |  0.0331102 | 12.0978 |
| st_tracts_density_jobs                   |   0.0684875 |  0.0217424 | 3.14995 |

**2HLCM15 Model**

-   Agent segment: 15

-   Segment definition: (recent_mover == 1) & (household_type == 15)

-   Segment description: income: +100,000, age: -35 , persons: 2+, children: 1

-   Log likelihood ratio: 0.8330747642416653

-   pseudo-R²: 0.16692523575833473

Estimated coefficients for 2hlcm15:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_value               |    0.105028 |   0.047788 |   2.1978 |
| st_block_groups_mean_year_built          |    0.223619 |  0.0405447 |  5.51537 |
| st_density_residential_units             |    0.125274 |  0.0197432 |  6.34518 |
| st_mean_income                           |    0.200685 |  0.0650109 |  3.08695 |
| st_nodes_high_income_hh_1500m            |    0.195233 |  0.0609559 |  3.20286 |
| st_ratio_households_to_residential_units |     2.93559 |  0.0505811 |  58.0373 |
| st_res_btype_mode4                       |    0.276415 |  0.0324087 |  8.52903 |
| st_tracts_density_jobs                   |  -0.0716835 |  0.0423074 | -1.69435 |

**2HLCM16 Model**

-   Agent segment: 16

-   Segment definition: (recent_mover == 1) & (household_type == 16)

-   Segment description: income: +100,000, age: 35-65 , persons: 1

-   Log likelihood ratio: 0.7570625542775985

-   pseudo-R²: 0.2429374457224015

Estimated coefficients for 2hlcm16:

|                                          | Coefficient | Std. Error | T-Score |
|:-----------------------------------------|------------:|-----------:|--------:|
| st_block_groups_mean_year_built          |    0.407829 |   0.037949 | 10.7468 |
| st_density_residential_units             |    0.226632 |  0.0124237 |  18.242 |
| st_mean_income                           |    0.272065 |  0.0561857 | 4.84226 |
| st_nodes_high_income_hh_1500m            |    0.404206 |  0.0581444 | 6.95176 |
| st_ratio_households_to_residential_units |     1.83079 |  0.0577102 | 31.7239 |
| st_res_btype_mode4                       |    0.433776 |  0.0322373 | 13.4557 |

**2HLCM17 Model**

-   Agent segment: 17

-   Segment definition: (recent_mover == 1) & (household_type == 17)

-   Segment description: income: +100,000, age: 35-65 , persons: 2+, children: 1

-   Log likelihood ratio: 0.8381419886099023

-   pseudo-R²: 0.1618580113900977

Estimated coefficients for 2hlcm17:

|                                          | Coefficient | Std. Error | T-Score |
|:-----------------------------------------|------------:|-----------:|--------:|
| st_block_groups_mean_year_built          |    0.132653 |  0.0308844 | 4.29513 |
| st_density_residential_units             |    0.144291 |  0.0152481 | 9.46284 |
| st_mean_income                           |    0.187602 |  0.0543055 | 3.45457 |
| st_ratio_households_to_residential_units |     2.81473 |  0.0450304 | 62.5074 |
| st_res_btype_mode4                       |    0.243602 |  0.0321591 |  7.5749 |
| st_tracts_density_jobs                   |   0.0490511 |  0.0267499 | 1.83369 |

**2HLCM18 Model**

-   Agent segment: 18

-   Segment definition: (recent_mover == 1) & (household_type == 18)

-   Segment description: income: 35,000-100,000, age: +65 , persons: 1

-   Log likelihood ratio: 0.8316423797830624

-   pseudo-R²: 0.16835762021693756

Estimated coefficients for 2hlcm18:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built          |    0.198641 |  0.0354397 |  5.60506 |
| st_mean_income                           |    0.395249 |  0.0376309 |  10.5033 |
| st_ratio_households_to_residential_units |           3 |  0.0438835 |  68.3628 |
| st_res_btype_mode4                       |    0.158382 |  0.0371443 |  4.26396 |
| st_tracts_density_households             |   -0.182251 |  0.0438418 | -4.15702 |

**2HLCM19 Model**

-   Agent segment: 19

-   Segment definition: (recent_mover == 1) & (household_type == 19)

-   Segment description: income: -35,000, age: +65 , household_type: any

-   Log likelihood ratio: 0.6962972083564676

-   pseudo-R²: 0.3037027916435324

Estimated coefficients for 2hlcm19:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built          |    0.346297 |  0.0371074 |  9.33228 |
| st_density_residential_units             |    0.187002 |  0.0120028 |  15.5799 |
| st_mean_income                           |    -1.07186 |  0.0717574 | -14.9373 |
| st_nodes_high_income_hh_1500m            |   -0.365514 |  0.0607305 | -6.01862 |
| st_ratio_households_to_residential_units |      2.4801 |  0.0692532 |  35.8121 |
| st_res_btype_mode4                       |    0.424043 |  0.0343337 |  12.3506 |

**2HLCM20 Model**

-   Agent segment: 20

-   Segment definition: (recent_mover == 1) & (household_type == 20)

-   Segment description: income: 35,000-100,000, age: +65 , household_type: any

-   Log likelihood ratio: 0.7809014937529616

-   pseudo-R²: 0.21909850624703842

Estimated coefficients for 2hlcm20:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built          |    0.502066 |  0.0404341 |  12.4169 |
| st_density_residential_units             |    0.165721 |   0.027593 |   6.0059 |
| st_mean_income                           |   -0.496896 |  0.0652875 |  -7.6109 |
| st_nodes_high_income_hh_1500m            |   -0.282916 |  0.0666442 | -4.24517 |
| st_ratio_households_to_residential_units |     2.32494 |  0.0560118 |   41.508 |
| st_res_btype_mode4                       |    0.371741 |  0.0329431 |  11.2843 |
| st_tracts_density_jobs                   |   -0.652614 |   0.087574 | -7.45215 |

**2HLCM21 Model**

-   Agent segment: 21

-   Segment definition: (recent_mover == 1) & (household_type == 21)

-   Segment description: income: +100,000, age: +65 , household_type: any

-   Log likelihood ratio: 0.8090398898898495

-   pseudo-R²: 0.19096011011015046

Estimated coefficients for 2hlcm21:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built          |    0.601314 |  0.0372262 |  16.1529 |
| st_density_residential_units             |    0.205358 |  0.0161482 |  12.7171 |
| st_mean_income                           |    0.362025 |  0.0429996 |  8.41927 |
| st_nodes_high_income_hh_1500m            |   0.0304176 |  0.0593304 | 0.512681 |
| st_ratio_households_to_residential_units |     1.85415 |  0.0494927 |  37.4632 |
| st_res_btype_mode4                       |    0.338365 |  0.0328154 |  10.3112 |

## Employment Location Choice Models

**ELCM1 Model**

-   Agent segment: 1

-   Segment definition: aggr_sector_id == 1

-   Segment description: 23 Construction

-   Log likelihood ratio: 0.782660746654347

-   pseudo-R²: 0.21733925334565296

Estimated coefficients for elcm1:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_total_jobs                |    0.240298 |  0.0144409 |  16.6401 |
| st_density_residential_units              |   -0.191689 |  0.0354656 | -5.40493 |
| st_prop_aggr_sector_id_1                  |     0.66451 |  0.0133544 |  49.7597 |
| st_res_btype_mode1                        |   -0.115311 |  0.0378294 | -3.04819 |
| st_tracts_median_income                   |   -0.162607 |  0.0437957 | -3.71285 |
| st_zones_total_jobs_7_dollars_travel_time |   0.0828329 |  0.0388726 |  2.13088 |

**ELCM2 Model**

-   Agent segment: 2

-   Segment definition: aggr_sector_id == 2

-   Segment description: 61 Educational Services, 62 Health Care and Social Assistance

-   Log likelihood ratio: 0.5584531134873556

-   pseudo-R²: 0.4415468865126444

Estimated coefficients for elcm2:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_median_year_built         |    0.183975 |  0.0385223 |  4.77581 |
| st_prop_aggr_sector_id_2                  |     1.04363 |  0.0122274 |  85.3518 |
| st_pumas_total_jobs                       |    0.231568 |  0.0304485 |  7.60523 |
| st_res_btype_mode1                        |   -0.425659 |  0.0438697 |  -9.7028 |
| st_tracts_density_households              |  -0.0375313 |  0.0381489 | -0.98381 |
| st_tracts_mean_income                     |    0.121129 |  0.0378145 |  3.20325 |
| st_zones_total_jobs_7_dollars_travel_time |    0.191188 |  0.0476752 |  4.01022 |

**ELCM3 Model**

-   Agent segment: 3

-   Segment definition: aggr_sector_id == 3

-   Segment description: 52 Finance and Insurance, 53 Real Estate Rental and Leasing

-   Log likelihood ratio: 0.7243687136345293

-   pseudo-R²: 0.27563128636547074

Estimated coefficients for elcm3:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_prop_aggr_sector_id_3                  |    0.587981 |   0.011145 |  52.7575 |
| st_pumas_mean_income                      |    0.191913 |  0.0358078 |  5.35952 |
| st_res_btype_mode1                        |   -0.443127 |  0.0430423 | -10.2951 |
| st_tracts_density_households              |     0.17078 |  0.0241508 |  7.07139 |
| st_tracts_mean_year_built                 |     0.38065 |  0.0345845 |  11.0064 |
| st_zones_total_jobs_7_dollars_travel_time |    0.500872 |  0.0450461 |  11.1191 |

**ELCM4 Model**

-   Agent segment: 4

-   Segment definition: aggr_sector_id == 4

-   Segment description: 92 Public Administration

-   Log likelihood ratio: 0.27364141588526747

-   pseudo-R²: 0.7263585841147325

Estimated coefficients for elcm4:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_median_year_built         |    0.216132 |  0.0440935 |  4.90166 |
| st_ln_prop_aggr_sector_id_4               |    0.658696 | 0.00655732 |  100.452 |
| st_res_btype_mode1                        |    -0.36014 |  0.0601677 | -5.98561 |
| st_tracts_density_households              |    0.294718 |  0.0406705 |  7.24648 |
| st_tracts_mean_income                     |    0.140982 |  0.0547494 |  2.57505 |
| st_zones_total_jobs_7_dollars_travel_time |    0.262442 |  0.0580572 |  4.52041 |

**ELCM5 Model**

-   Agent segment: 5

-   Segment definition: aggr_sector_id == 5

-   Segment description: 51 Information

-   Log likelihood ratio: 0.41930426565131945

-   pseudo-R²: 0.5806957343486805

Estimated coefficients for elcm5:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_density_residential_units |   -0.165847 |  0.0388044 | -4.27393 |
| st_block_groups_mean_year_built           |    0.329283 |  0.0432749 |  7.60911 |
| st_ln_prop_aggr_sector_id_5               |    0.587052 |  0.0069929 |  83.9498 |
| st_pumas_total_jobs                       |    0.353016 |   0.028648 |  12.3225 |
| st_res_btype_mode1                        |   -0.543747 |  0.0496985 | -10.9409 |
| st_zones_total_jobs_7_dollars_travel_time |    0.569488 |  0.0515251 |  11.0526 |

**ELCM6 Model**

-   Agent segment: 6

-   Segment definition: aggr_sector_id == 6

-   Segment description: 44-45 Retail Trade, 71 Arts, Entertainment, and Recreation, 72 Accommodation and Food Services

-   Log likelihood ratio: 0.6228708001378972

-   pseudo-R²: 0.3771291998621028

Estimated coefficients for elcm6:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built           |    0.378698 |  0.0356658 |   10.618 |
| st_prop_aggr_sector_id_6                  |    0.980002 |   0.013112 |   74.741 |
| st_pumas_density_jobs                     |    0.210144 |  0.0233292 |  9.00777 |
| st_res_btype_mode1                        |   -0.350579 |  0.0409878 | -8.55324 |
| st_zones_total_jobs_7_dollars_travel_time |    0.111427 |  0.0421732 |  2.64212 |

**ELCM7 Model**

-   Agent segment: 7

-   Segment definition: aggr_sector_id == 7

-   Segment description: 31-33 Manufacturing

-   Log likelihood ratio: 0.440895131752665

-   pseudo-R²: 0.559104868247335

Estimated coefficients for elcm7:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_density_residential_units |    -1.02398 |  0.0586368 | -17.4631 |
| st_ln_block_groups_density_jobs           |    0.841386 |  0.0327469 |  25.6936 |
| st_prop_aggr_sector_id_7                  |    0.620014 |  0.0073612 |  84.2273 |

**ELCM8 Model**

-   Agent segment: 8

-   Segment definition: aggr_sector_id == 8

-   Segment description: 11 Agriculture, Forestry, Fishing and Hunting, 21 Mining, 81 Other Services (except Public Administration)

-   Log likelihood ratio: 0.7247014511970391

-   pseudo-R²: 0.2752985488029609

Estimated coefficients for elcm8:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_prop_aggr_sector_id_8                  |    0.661216 |  0.0108577 |  60.8984 |
| st_res_btype_mode1                        |   -0.128147 |  0.0376917 | -3.39987 |
| st_tracts_mean_income                     |   -0.171525 |   0.038974 |   -4.401 |
| st_zones_total_jobs_7_dollars_travel_time |    0.157239 |  0.0386901 |  4.06406 |

**ELCM9 Model**

-   Agent segment: 9

-   Segment definition: aggr_sector_id == 9

-   Segment description: 54 Professional, Scientific, and Technical Services, 55 Management of Companies and Enterprises, 56 Administrative and Support and Waste Management and Remediation Services

-   Log likelihood ratio: 0.6449703043819192

-   pseudo-R²: 0.35502969561808084

Estimated coefficients for elcm9:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built           |    0.730096 |  0.0372211 |  19.6151 |
| st_prop_aggr_sector_id_9                  |    0.886101 |  0.0151002 |  58.6813 |
| st_res_btype_mode1                        |   -0.727074 |  0.0418303 | -17.3815 |
| st_tracts_density_households              |    0.210069 |   0.030442 |  6.90061 |
| st_zones_total_jobs_7_dollars_travel_time |    0.462267 |  0.0445225 |  10.3828 |

**ELCM10 Model**

-   Agent segment: 10

-   Segment definition: aggr_sector_id == 10

-   Segment description: 22 Utilities, 42 Wholesale Trade, 48-49 Transportation and Warehousing

-   Log likelihood ratio: 0.6088929467793324

-   pseudo-R²: 0.39110705322066763

Estimated coefficients for elcm10:

|                                           | Coefficient | Std. Error |  T-Score |
|:------------------------------------------|------------:|-----------:|---------:|
| st_block_groups_mean_year_built           |     0.35702 |  0.0369937 |  9.65085 |
| st_prop_aggr_sector_id_10                 |    0.652183 |  0.0079139 |  82.4098 |
| st_res_btype_mode1                        |   -0.394292 |   0.037618 | -10.4815 |
| st_zones_total_jobs_7_dollars_travel_time |    0.207196 |  0.0404703 |   5.1197 |

## Residential Development Project Location Choice Model

**RDPLCM1 Model**

-   Agent segment: 1

-   Segment definition: (year_built \> 2000) & (building_type_id == 1)

-   Segment description: Own Single-family

-   Log likelihood ratio: 0.7873985044398879

-   pseudo-R²: 0.2126014955601121

Estimated coefficients for rdplcm1:

|                                          | Coefficient | Std. Error | T-Score |
|:-----------------------------------------|------------:|-----------:|--------:|
| st_mean_year_built                       |    0.762707 |   0.043089 | 17.7007 |
| st_ratio_households_to_residential_units |     2.99185 |  0.0596047 | 50.1949 |
| st_res_btype_mode1                       |    0.293286 |  0.0497451 | 5.89578 |
| st_tracts_mean_income                    |    0.104993 |  0.0379542 | 2.76632 |

**RDPLCM2 Model**

-   Agent segment: 2

-   Segment definition: (year_built \> 2000) & (building_type_id == 2)

-   Segment description: Rent Single-family

-   Log likelihood ratio: 0.7342200796194963

-   pseudo-R²: 0.2657799203805037

Estimated coefficients for rdplcm2:

|                                          | Coefficient | Std. Error | T-Score |
|:-----------------------------------------|------------:|-----------:|--------:|
| PctSW_ISUSHW                             |    0.446754 |    0.15839 | 2.82059 |
| st_block_groups_mean_rent                |    0.139763 |  0.0364034 | 3.83928 |
| st_block_groups_mean_year_built          |    0.832319 |   0.037363 | 22.2765 |
| st_nodes_high_income_hh_1500m            |     1.03804 |   0.060519 | 17.1522 |
| st_ratio_households_to_residential_units |     1.91154 |   0.106272 | 17.9872 |
| st_res_btype_mode2                       |     0.23068 |  0.0162985 | 14.1535 |

**RDPLCM3 Model**

-   Agent segment: 3

-   Segment definition: (year_built \> 2000) & (building_type_id == 3)

-   Segment description: Own Multifamily

-   Log likelihood ratio: 0.8150782666356472

-   pseudo-R²: 0.1849217333643528

Estimated coefficients for rdplcm3:

|                                          | Coefficient | Std. Error |  T-Score |
|:-----------------------------------------|------------:|-----------:|---------:|
| st_nodes_post2010_du_800m                |    0.248404 |   0.022933 |  10.8317 |
| st_pumas_density_households              |   0.0852414 |  0.0381966 |  2.23165 |
| st_ratio_households_to_residential_units |     2.02874 |  0.0455039 |   44.584 |
| st_res_btype_mode3                       |    0.201019 |  0.0146593 |  13.7128 |
| st_tracts_mean_income                    |   -0.533077 |  0.0441784 | -12.0665 |

**RDPLCM4 Model**

-   Agent segment: 4

-   Segment definition: (year_built \> 2000) & (building_type_id == 4)

-   Segment description: Rent Multifamily

-   Log likelihood ratio: 0.5948316369913816

-   pseudo-R²: 0.40516836300861836

Estimated coefficients for rdplcm4:

|                                           | Coefficient | Std. Error | T-Score |
|:------------------------------------------|------------:|-----------:|--------:|
| st_block_groups_mean_rent                 |    0.379454 |  0.0333571 | 11.3755 |
| st_ratio_households_to_residential_units  |       1.947 |  0.0723436 | 26.9133 |
| st_res_btype_mode4                        |      1.0819 |  0.0313199 | 34.5435 |
| st_tracts_median_year_built               |    0.968578 |  0.0352225 | 27.4988 |
| st_zones_total_jobs_7_dollars_travel_time |    0.310421 |  0.0510631 | 6.07916 |

## Residential/Employment Real Estate Price Models for values and prices

**REPM_RENT1 Model**

-   Agent segment: 1

-   Segment description: Regression for values

-   R²: 0.10569465673756018

Estimated coefficients for repm_rent1:

|   | Coefficient | Std. Error | T-Score |
|:---|---:|---:|---:|
| Intercept | -3.46945e-17 | 0.00395452 | -8.77336e-15 |
| st_counties_density_jobs | 0.0668551 | 0.00458748 | 14.5734 |
| st_pumas_ratio_households_to_residential_units | -0.00758194 | 0.00441492 | -1.71735 |
| st_res_btype_mode3 | 0.00755226 | 0.00395632 | 1.90891 |
| st_tracts_mean_income | 0.295405 | 0.00422742 | 69.8784 |
| st_zones_total_jobs_15_minutes_travel_time | 0.145496 | 0.00437894 | 33.2264 |

**REPM_VALUE1 Model**

-   Agent segment: 1

-   Segment description: Regression for values

-   R²: 0.6292386385621229

Estimated coefficients for repm_value1:

|   | Coefficient | Std. Error | T-Score |
|:---|---:|---:|---:|
| Intercept | 0 | 0.00254621 | 0 |
| st_pumas_ratio_households_to_residential_units | -0.176551 | 0.00264951 | -66.6352 |
| st_res_btype_mode1 | -0.0608487 | 0.00266199 | -22.8583 |
| st_tracts_mean_income | 0.807996 | 0.00266609 | 303.064 |
| st_zones_total_households_15_minutes_travel_time | 0.138758 | 0.00261654 | 53.0309 |
