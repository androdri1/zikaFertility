# Code and dataset for replicability of EHB: The fertility-inhibiting effect of mosquitoes: socio-economic differences in response to the Zika crisis in Colombia
 
April 30, 2019 
 
This repository includes the files required to replicate the main findings of this article and its supplementary material. If you need something more detailed on the construction of the data, please contact us.

First, you have to uncompress the file "data.7z" in a folder with the same name, and then start by running "ZIKA_03_Descriptive" (adjust the global $mainFolder your directory in each do-file adequately). All these routines where designed for Stata 14, and some of them require user packages such as "texdoc", "sjlatex", "synth" and "synth_runner"

Do-files "ZIKA_04_DiDMatch_GroupsFULLInt.do" and "ZIKA_05_SyntheticControl_Tables.do" have the data required to produce the tables (matched datasets). You can also produce these datasets by running the first part of ""ZIKA_04_DiDMatch_GroupsFULLInt"" and "ZIKA_05_SyntheticControl.do".
