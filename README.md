# NAPLAN_explorer
Visualisations to explore the limits of using NAPLAN score at the single student level.

The possible location of the adaptive tests: [Staging_BK].[dbo].[NAPLAN_Online_Set_working]

The definite location of the colours for the SEA:

```{SQL}
SELECT  DISTINCT
[AAL_BG_Colour]
FROM [BI].[rel].[Assessment_Achievement_Level]
WHERE [AAL_BG_Colour] NOT IN ('#FFFF00', 'NULL')
```
