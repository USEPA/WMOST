# Read.me

# Watershed Management Optimization Support Tool (WMOST)
This model is being distributed, maintained and actively supported by EPA.  

•	WMOST Releases [link](#wmost-releases)

•	Citation [link](#citation)

•	Description [link](#description)

•	Purpose / Objective [link](#purpose-/-objective)

•	Presentations [link](#presentations)

•	Training[link](#training)

•	Contact[link](#contact)  

## WMOST Releases  
| Version                                 | Release Date  |
|:----------------------------------------|--------------:|
|WMOST Climate Automation Programs (WCAP) | August 2024   |
|WMOSTv3.1 + HCAM                         | February 2022 |
|v3.1                                     | February 2022 |	
|Benefits Module                          | September 2020|
|v3.01                                    | June 2019     |
|v3.0                                     | February 2018 |
|v2.0                                     | March 2016    |
|v1                                       | December 2013 |

## Citation  
Detenbeck, N., M. ten Brink, A. Le, K. Munson, S. Ennett, H. Parker, I. Morin, and C. Weaver.  2024. WMOST Climate Automation Programs (WCAP) Instructions v.1: SWAT Batch Climate Runs, Hydrologic Comparison Assessment Module (HCAM-R) for SWAT, HAWQS. U.S. Environmental Protection Agency, Washington, DC,  EPA/600/R-24/215, 2024.

United States Environmental Protection Agency (U.S. EPA).  2020a.  WMOST Scenario Comparison with Benefits Module.  EPA/600/B-20/242.

United States Environmental Protection Agency (U.S. EPA).  2020b.  Watershed Management Optimization Support Tool Benefits Module: Theoretical Documentation.  EPA/600/R-20/244.

US EPA. 2020. WMOST v3 Case Study: Cabin John Creek, Maryland. EPA/600/R-19/185.

Detenbeck, N.E. and C. Weaver. ScenCompare: WMOST Climate Scenario Viewer and Comparison Post Processor. Version 1. U.S. Environmental Protection Agency, Washington, D.C. EPA/600/R-19/039, 2018.

Detenbeck, N.E. and C. Weaver. Watershed Management Optimization Support Tool. Version 3.01.  U.S. Environmental Protection Agency, Washington, D.C. EPA/600/R-19/039, 2018.

Detenbeck, N., A. Piscopo, M. Tenbrink, C. Weaver, A. Morrison, T. Stagnitta, R. Abele, J. Leclair, T. Garrigan, V. Zoltay, A. Brown, A. Le, J. Stein, AND I. Morin. Watershed Management Optimization Support Tool v3. U.S. Environmental Protection Agency, Washington, DC, EPA/600/C-18/001, 2018.

Detenbeck, N., A. Piscopo, M. Tenbrink, C. Weaver, A. Morrison, T. Stagnitta, R. Abele, J. Leclair, T. Garrigan, V. Zoltay, A. Brown, A. Le, J. Stein, AND I. Morin. Watershed Management Optimization Support Tool (WMOST) v3: User Guide. US EPA Office of Research and Development, Washington, DC, EPA/600/R-17/255, 2018.

Detenbeck, N., M. Tenbrink, A. Piscopo, A. Morrison, T. Stagnitta, R. Abele, J. Leclair, T. Garrigan, V. Zoltay, A. Brown, A. Le, J. Stein, AND I. Morin. Watershed Management Optimization Support Tool (WMOST) v3: Theoretical Documentation. U.S. Environmental Protection Agency, Washington, DC, EPA/600/R-17/220, 2018.

Detenbeck, N., A. Le, T. Stagnitta, and R. Sullivan. 2020. WMOST Hydrology and Water Quality Data Processor Instructions: Version 2. U.S. Environmental Protection Agency. EPA Report EPA/600/B-20/148.

Detenbeck, N., M. Tenbrink, R. Abele, J. Leclair, T. Garrigan, V. Zoltay, A. Morrison, A. Brown, B. Small, AND I. Morin. Watershed Management Optimization Support Tool (WMOST) v2: Theoretical Documentation. U.S. Environmental Protection Agency, Washington, DC, EPA/600/R-15/058, 2015.

Detenbeck, N., M. Tenbrink, R. Abele, J. Leclair, T. Garrigan, V. Zoltay, B. Small, A. Brown, I. Morin, AND A. Morrison. Watershed Management Optimization Support Tool (WMOST) v2: User Manual and Case Studies. US EPA Office of Research and Development, Washington, DC, EPA/600/R-15/059, 2016.

Detenbeck, N., A. Le, A. Piscopo, K. Dickman, C. Weaver, I. Morin, AND M. TenBrink. Hydrologic Comparison Assessment Tool (HCAM). U.S. Environmental Protection Agency, Washington, DC, 2021. EPA/600/B-21/282

Detenbeck, N., C. Weaver, A. Le, I. Morin, and M. ten Brink. 2021. Watershed Management Optimization Support Tool v3.1. EPA/600/B-21/282.

U.S. EPA. Watershed Management Optimization Support Tool (WMOST) v1: Theoretical Documentation. US EPA Office of Research and Development, Washington, DC, EPA/600/R-13/151, 2013.

U.S. EPA. Watershed Management Optimization Support Tool (WMOST) v1: User Manual and Case Study Examples. US EPA Office of Research and Development, Washington, DC, EPA/600/R-13/174, 2013.

## Description  
The Watershed Management Optimization Support Tool (WMOST) is a decision support tool that facilitates integrated water management at the local or small watershed scale. WMOST models the environmental effects and costs of management decisions in a watershed context that is, accounting for the direct and indirect effects of decisions. The model considers water flows and water quality. It is spatially lumped with options for a daily or monthly modeling time step. The optimization of management options is solved using nonlinear programming. WMOST is intended to be a screening tool used as part of an integrated watershed management process such as that described in EPA’s watershed planning handbook (EPA 2008). WMOST serves as a public-domain, efficient, and user-friendly tool for local water resources managers and planners to screen a wide range of potential water resources management options across their jurisdiction for cost-effectiveness and environmental and economic sustainability (Zoltay et al., 2010). Practices that can be evaluated include projects related to stormwater (including green infrastructure [GI] and combined sewer overflow (CSO) systems), stream restoration, water supply, wastewater and land resources such as low-impact development (LID) and land conservation. WMOST can aid in evaluating LID and green infrastructure as alternative or complementary management options in projects proposed for State Revolving Funds (SRF). In addition, the tool can enable assessing the trade-offs and co-benefits of various practices. In WMOST v3, the Baseline Hydrology and Loadings and Stormwater Hydrology and Loadings Modules assist users with input data acquisition and pre-processing. The Combined Sewer Overflow (CSO) Module allows for the evaluation of management options to minimize the number of CSO events. The Flood Module allows the consideration of flood damages and their reduction in assessing the cost-effectiveness of management practices. Version 3.01 of WMOST is compatible with the ScenCompare utility, which allows users to easily make comparisons across WMOST runs.  The Benefits Module was built upon the ScenCompare framework to allow calculation of benefits and co-benefits associated with green infrastructure. The target user group for WMOST consists of local water resources managers, including municipal water works superintendents and their consultants. Version 3.1 of WMOST works with the Hydrologic Comparison Assessment Module (HCAM) tool which facilitates preparation of WMOST runs for efficient comparison across multiple climate change scenarios.

The software programs and instruction manual for WMOST Climate Automation Programs (WCAP) allow users to conduct integrated assessments of the best combination of management practices in both urban and agricultural landscapes to meet water quantity and water quality goals under current and future climate scenarios. Previously users were able to conduct these assessments for agricultural landscapes alone using SWAT processing  (now automated with HCAM-R) or for urban landscapes alone using HCAM. Now automated optimizations can be conducted for combinations of urban and agricultural BMPs.

The WMOST VBA-based tools in Excel spreadsheets were originally released with password protection.  Code can be accessed with the password: W3m(ost.

## Purpose / Objective  
The Watershed Management Optimization Support Tool (WMOST) is a decision support tool that facilitates integrated water management at the local or small watershed scale. Integrated water management involves coordination across multiple programs for land, water, and related resources (stormwater, wastewater, drinking water, land conservation) to find sustainable and cost-effective solutions. WMOSTv3 has an associated theoretical documentation report as well as a detailed user guide. While version 2 only included management options related to water quantity, version 3 of WMOST also includes water quality management. Users can identify least-cost solutions to meet water quality criteria for lakes or streams/rivers, pollutant loading targets, and/or minimization of combined sewer overflows. Version 3 also includes riparian buffer zone restoration or conservation, a few agricultural BMPs, and additional stormwater best management practices, both structural, such as rain gardens and nonstructural, such as street sweeping. The Hydroprocessor, a program which formats output from watershed models such as SWAT and HSPF for input to WMOST, is also included.  ScenCompare allows comparisons across different WMOST runs set up under different climate scenarios.

## Presentations  
Managing Watersheds with WMOST

## Training  
The following links exit the site

NOTE: If you need captions, please click the CC button on the player to turn them on.

•	WMOST Training [1: WMOST Overview](https://www.youtube.com/watch?v=JFDD8dHbKYU)

•	WMOST Training [2: Case Study Overview](https://www.youtube.com/watch?v=-H6A2ypSmvc) 

•	WMOST Training [3: Hydrology Module](https://www.youtube.com/watch?v=aQzrvhEsEPA)

•	WMOST Training [4: Stormwater Module](https://www.youtube.com/watch?v=anxPCmFMNMg)

•	WMOST Training [5: Water Demand and Infrastructure](https://www.youtube.com/watch?v=xLDa8cI3v_E) 

•	WMOST Training [6: Optimize](https://www.youtube.com/watch?v=L9dYPp-GIyM)

•	WMOST Training [7: Flood](https://www.youtube.com/watch?v=dnTj4psX8d4)

•	WMOST Training [8: Flooding Module Data](https://www.youtube.com/watch?v=q1E7VSrWb7Q)

•	WMOST Training [9: Scenarios](https://www.youtube.com/watch?v=1MzRUcWYSFg)

•	WMOST Training [10: Model Setup](https://www.youtube.com/watch?v=o2KMp_tBwqU)

•	WMOST Training [11: Calibration](https://www.youtube.com/watch?v=ACvhiGU5H5A)

•	WMOST Training [12: Future Directions](https://www.youtube.com/watch?v=yNjtAL5JDbY)

## Contact  

Contact Naomi Detenbeck  (detenbeck.naomi@epa.gov) for information about WMOST.

### Disclaimer  

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
