# Challenge 1
Many of the datasets in the IRW have response time. This is not something we've made use of. Dive into this data to see what it might tell you about the measurement process. Some questions you could potentially ask:

1. How much within-person variation is there for a respondent within a test? To the extent that this varies, how does it vary as a function of respondent theta?

2. Can you include it as a predictor of responses in a model of accuracy (similar to what we did here in m3: https://github.com/ben-domingue/252L/blob/master/cH/mysrc.R)?


IRW data with response time (as of version 4.0):
```
artistic_preferences
brain_hemisphere
broadband_inventories
chess_lnirt
credentialform_lnirt
dd_rotation
depression_anxiety_stress
face_memory_test
famous_melodies
ffm_AGR
ffm_CSN
ffm_EST
ffm_EXT
ffm_OPN
fisher_temperment
geography
graphmapping_study1
introversion_extroversion
motion
nature_relatedness
non_parametric_mixture_modeling_exp1_Cleaned
nonverbal_immediacy
pisa2015_math
pisa2015_read
pisa2015_science
protestant_workethic
roar_lexical
rr98_accuracy
spanishmegastudy
vocab_assessment_3_to_8_year_old_children
```
Some of these have additional features that you could also explore or that could make analyses challenging (for example, you'd want to probably cut the PISA data down).
