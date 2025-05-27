- `Title`: Analytical Data
- `Abstract`: An analytical datafile used to assess the potential computational reproducibility of research papers published in The International Journal of Geographic Information Science (IJGIS) under a recently adopted Open Data + FAIR publication policy.
- `Spatial Coverage`: Not Applicable
- `Spatial Resolution`: Not Applicable
- `Spatial Representation Type`: Not Applicable 
- `Spatial Reference System`: Not Applicable
- `Temporal Coverage`: The primary population of interest are research articles published in IJGIS between 2020 and 2024 and Volumes 34(7)-38(10). We assessed all articles in this data range. We also sample a 25 articles a set of article before from the year before FAIR policy adoption Volumes 32(8)-33(7).
- `Temporal Resolution`: Article are published in monthly issues.
- `Lineage`: For each of the articles in our analytical sample, we assessed whether the data, code, and other research artifacts identified in the data and code availability statements could be gathered using the links and source information published by the authors. We then generated the data needed to assess the potential to computationally reproduce each article using a standardized checklist that includes questions based on the principles of both the 5-star guide, AGILE Reproducible Paper Guidelines, and the broader FAIR framework. The checklist was implemented in the Qualtrics survey software and is available in the online repository. Once data were collected from all articles in the sample by independent reviewers, a random subsample of 5 percent of the sample, was collectively reviewed by the author team to ensure inter-rater consistency. To evaluate potential reproducibility levels before and after adoption of the data availability policy, we used these same procedures to assess a random sample of 25 articles published one year prior to introduction of the data availability policy (Volumes 32(8)-33(7)). A sample of 25 articles represents approximately three issues of IJGIS.
- `Distribution`: The data is available through GitHub and an associated Open Science Framework repository.
- `Constraints`: The data is available for re-use under the BSD-3-Clause license
- `Data Quality`: data quality was assessed during a re-evaluation of a random subset of articles.
- `Variables`: Variables are part of the review of a publication's code/procedure (cr) or data/metadata (dr). Within each sub-review, materials were reviewed for for finable (f), accessible (a), interoperable (i), reusable (r). 

| Label | Alias | Definition | Type | Accuracy | Domain | Missing Data Value(s) | Missing Data Frequency |
| :--: | :--: | :--: | :--: | :--: | :--: | :--: | :--: |
| progress | Progress | Indicates survey completion percentage | Numeric | NA | 0-100 | ... | NA |
| duration | Duration | Entry time | Numeric | NA | 0- | ... | NA |
| qualtrics_id | Response ID | Qualtrics ID | Text | NA | NA | ... | NA |
| id | ID | ID assigned from sampling frame | Numeric | NA | NA | ... | NA |
| doi | Digital Object Identifier | Unique DOI | Text | NA | NA | ... | NA |
| d_filter | Data Filter | Does the publication rely on one or more datasets? | Y/N | NA | NA | ... | NA |
| d_synth | Synthetic Data | Does the publication rely on synthetic or simulated data? | Y/N | NA | NA | ... | NA |
| c_filter | Code Filter | Does the publication rely on computation (e.g., code, GIS processing)? | Y/N | NA | NA | ... | NA |
| comments | Comments | Comments | text | NA | NA | ... | NA |
| cr-1 | Code availability | Code is publicly available and accessible from the publication | text | NA | NA | ... | NA |
| cr-1-1 | Code repository | What is the name of the public repository hosting the code?  | text | NA | NA | ... | NA |
| cr-1-2 | Code URL | Provide the URLs used to access the code | text | NA | :--: | :--: | :--: |
| cr-3 | Inaccessible code | For the inaccessible code, what reason was provided for not sharing the code? | text | NA | :--: | :--: | :--: |
| cr-3-text | Inaccessible code comment | comments for scr-3 other option | text | NA | :--: | :--: | :--: |
| cr-4 | Synthetic data process | procedure use to create synthetic/simulated data | Y/N | NA | :--: | :--: | :--: |
| cr-4-text | Synthetic comments | comments on scr-4 other option | text | NA | :--: | :--: | :--: |
| cr-f-1 | Code findable identifier | Code is assigned a unique and persistent identifier (e.g., DOI) | Y/N | NA | :--: | :--: | :--: |
| cr-f-2 | Code findable index | Code is registered or indexed in a searchable resource | Y/N | NA | :--: | :--: | :--: |
| cr-f-1-1 | Code identifier | What is the persistent identifier of the code? | text | NA | :--: | :--: | :--: |
| cr-a-1 | Code Accessible retrievable | Code are retrievable by their identifier using a standardized communications protocol | Y/N | NA | :--: | :--: | :--: |
| cr-a-2 | Code Accessible Commenting | Code uses a consistent and readable commenting style | Y/N | NA | :--: | :--: | :--: |
| cr-i-1 | Code Interoperable Pakages | Code identifies package dependencies | Y/N | NA | :--: | :--: | :--: |
| cr-i-2 | Code Interoperable Versions | Code identifies package versions | Y/N | NA | :--: | :--: | :--: |
| cr-i-3 | Code Interoperable Infrastructure | Code specifies computing infrastructure | Y/N | NA | :--: | :--: | :--: |
| cr-i-4| Code Interoperable Container| Code and data packaged in a container (e.g., docker) | Y/N | NA | :--: | :--: | :--: |
| cr-r-1 | Code Reusable License | Code is released with a clear and accessible license | Y/N | NA | :--: | :--: | :--: |
| cr-r-2 | Code Reusable Parameters | Code lists all parameters used in model/algorithm | Y/N | NA | :--: | :--: | :--: |
| cr-r-3 | Code Reusable Seeds | seed setting methods are described | Y/N | NA | :--: | :--: | :--: |
| cr-r-1-1 | Code License | What license is applied to the code? | text | NA | :--: | :--: | :--: |
| dr-1 | Data Available | Data used is publicly available and accessible from the publication | Y/N | NA | :--: | :--: | :--: |
| dr-1-1 | Data URL| Provide the URLs used to access the data | text | NA | :--: | :--: | :--: |
| dr-1-2 | Data Reasons | Reason was provided for not sharing the data | text | NA | :--: | :--: | :--: |
| dr-1-2-1 | Data Reasons Comment | comments for dr-1-2 other option | text | NA | :--: | :--: | :--: |
| dr-2 | Metadata Available | Metadata is provided for the data | Y/N | NA | :--: | :--: | :--: |
| dr-2-1 | Metadata URL | Provide the URLs where the metadata can be accessed | Y/N | NA | :--: | :--: | :--: |
| dr-f-1 | Data finable identifier | Data are assigned a globally unique and persistent identifier (e.g., DOI) | Y/N | NA | :--: | :--: | :--: |
| dr-f-2 | Date finable index | Data is registered or indexed in a searchable resource | Y/N | NA | :--: | :--: | :--: |
| dr-f-3 | Date finable metadata| Metadata clearly and explicitly include the identifier of the data it describes | Y/N | NA | :--: | :--: | :--: |
| dr-f-1-1 | Data identifier | What is the persistent identifier of the data? | text | NA | :--: | :--: | :--: |
| dr-a-1| Data accessible retrievable | Data are retrievable by their identifier using a standardized communications protocol | Y/N | NA | :--: | :--: | :--: |
| dr-a-2 | Data accessible metadata | Metadata are accessible even when the data are no longer available | Y/N | NA | :--: | :--: | :--: |
| dr-i-1 | Data interoperable language | Metadata use a broadly applicable language  | Y/N | NA | :--: | :--: | :--: |
| dr-i-2 | Data interoperable reference | Metadata include qualified references to other metadata | Y/N | NA | :--: | :--: | :--: |
| dr-r-1 | Data Reusable License | released with a clear and accessible data usage license | Y/N | NA | :--: | :--: | :--: |
| dr-r-2 | Data Reusable Attributes | described with accurate and relevant attributes |Y/N | NA | :--: | :--: | :--: |
| dr-r-3 | Data Reusable Provenance | Data have detailed provenance | Y/N | NA | :--: | :--: | :--: |
| dr-r-1-1 | Data License | What license is applied to the data? | text | NA | :--: | :--: | :--: |
