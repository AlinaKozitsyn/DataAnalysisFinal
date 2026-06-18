# Data placement

The raw survey file is **not committed** to the repository. To replicate the analysis:

1. Download the dataset from Mendeley Data (V2, CC BY 4.0):
   **https://doi.org/10.17632/yzcwfyrxy2.2**
   > Mahmud, A., Siddik, M. A. B., Arefin, M. M., & Rahman, M. M. (2024). *A data set exploring
   > the link and associated factors of internet addiction, loneliness, and depression among
   > adolescents in Bangladesh.* Mendeley Data, V2.

2. Place the Excel file here, with this exact name:

   ```
   data/New Microsoft Excel Worksheet.xlsx
   ```

3. From the project root, run `Rscript cluster_analysis.R` then `Rscript build_report.R`.

## What's in it

819 Bangladeshi adolescents (ages 13–19), one row per student, **no missing values**. Each
student answered ~36 demographic/behavioural questions plus three validated instruments:
the Internet Addiction Test (IAT → `TotalIA`), PHQ-9 depression (`totalphq`), and the UCLA
Loneliness Scale (`lonelinesstotal`).

`enriched_data.csv` (the original 91 columns + engineered features such as the `group4` cluster
label and the binary `addicted` outcome) is **produced by the pipeline** — you do not need to
create it by hand.
