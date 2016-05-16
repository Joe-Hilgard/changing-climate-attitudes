# Changing Climate Attitudes

Publicly available data from Dav Clark's PhD dissertation research. Please note
the usage restrictions in the LICENSE file. Moreover, if you use this data,
please cite the appropriate reference. If in doubt, use
[this](http://onlinelibrary.wiley.com/doi/10.1111/tops.12187/abstract):

> Ranney, M. A. and Clark, D. (2016), Climate Change Conceptual Change:
Scientific Information Can Transform Attitudes. Topics in Cognitive Science, 8:
49–75. doi: 10.1111/tops.12187

All CSV files are provided in UTF-8 unless otherwise specified. Currently, the
most definitive description of all experimental procedures is available in Dav
Clark's dissertation, available
[here](https://github.com/davclark/UCB_thesis/releases).

College Student Data was collected via hand-entered paper forms, or Qualtrics.
All Mechanical Turk data was collected via Qualtrics.

In brief, the following terms are used to describe each dataset:

- Where collected:
    - **cco** (Climate Change Online): Data collected via Amazon Mechanical
      Turk
    - **UCp** Collected at University of California, Berkeley *on paper*
    - **UCo** Collected at University of California, Berkeley using an *online
      survey* (but still in a testing room)
    - **UT** Collected at University of Texas, Brownsville (on paper)
- Intervention:
    - **mech**: Data were collected before and after a presentation of the
      written description of the climate change mechanism.
    - **mech+stats**: Data were collected before and after an extended
      curriculum including surprising numerical statistics and the description
      of the mechanism.
    - **core_intervention**: Whatever was given to *all* participants (or
      alternatively, all columns represented with some NAs, e.g., for
      no-pretest).
    - **delayed_test**: delayed post-test data.
    - **full_intervention**: Sometimes the core was redundantly included along
      with delayed_test data.
- Desiderata:
    - **notext**: Columns containing free text responses are not included. Ask
      Dav for this info if needed.

The following are organized based on the Ranney & Clark (2016) TopiCS paper.
Note that in some cases, exported data have been reverse coded. This is noted
explicitly. If real knowledge scores have not been computed, python code is
provided in `codes.py` that can perform this computation.

- Experiment 2 - initial experiments at UC Berkeley and UT Brownsville:
    - UCp_mech_core_intervention_notext.csv (85 students)
    - UT_mech_core_intervention_notext.csv (41 students)
- Experiment 3 - online replication / extension at UC Berkeley:
    - UCo_mech_core_intervention_notext.csv (79 students with valid posttest
      and followup data, `_pre` data avilable only for `knwgbl`)
    - UCo_mech_full_intervention_notext.csv (38 students, all `_pre` data
      available from pre-screening survey - if unmarked, data is "pre")
- Experiment 4 - online replication / extension on MTurk:
    - *`gw2_1`, `evo2_5`, and `evo2_6` are reverse-coded*
    - cco_mech_core_intervention_notext.csv (38 valid participants)
    - cco_mech_delayed_test_notext.csv (28 completed)
- Experiment 5 - Expanded high school curriculum:
    - HS_mech+stats_full_intervention.csv (63 students)

If you wish to share the dataset, please use the following code (you can
copy-paste the raw HTML from the raw version of this README.md file):

<a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/">
<img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nd/4.0/88x31.png" />
</a>
<br />
<span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">
    Changing Climate Attitudes data
</span>
by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/davclark"
property="cc:attributionName" rel="cc:attributionURL">Dav Clark</a>
is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/">
    Creative Commons Attribution-NoDerivatives 4.0 International License</a>.
<br />
Based on a work at
<a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/glass-bead-labs/changing-climate-attitudes" rel="dct:source">
    https://github.com/glass-bead-labs/changing-climate-attitudes</a>.
Permissions beyond the scope of this license may be available at
<a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/glass-bead-labs/changing-climate-attitudes/blob/master/LICENSE" rel="cc:morePermissions">
    https://github.com/glass-bead-labs/changing-climate-attitudes/blob/master/LICENSE</a>.
