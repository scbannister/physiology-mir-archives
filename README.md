# physiology-mir-archives
Examples of early R and MATLAB code to perform:

1) Pre-processing, cleaning, and synthesis of physiological signals (EDA and skin temperature) with real-time self-report data

This involves three main scripts: "physiology_clean.R", "selfreport_clean.R", and "integration.R"; this final script is dependent on variable spaces generated through the other two scripts. There are also two supplementary scripts: "libraries.R" to import packages, and "read_nexus_export.R" to help import skin conductance data from the NeXuS wearable technology and BioTrace software. Scripts are related to the following publication:

•	Bannister, S. C., and Eerola, T. (2023). Vigilance and social chills with music: Evidence for two types of musical chills. Psychology of Aesthetics, Creativity, and the Arts [Advance Online Publication]. DOI: 10.1037/aca0000421

2) Music signal feature extraction within Music Information Retrieval paradigm

This involves a single "MIR.m" script as a demonstration of using the MIRToolbox in MATLAB to extract musically-relevant audio features, including RMS, event density, pulse clarity, spectral brightness, roughness, flux, pitch chromagrams, music key strength and clarity, and mode (i.e., major/minor). Script is related to the following publication:

•	Bannister, S. C., and Eerola, T. (2018). Suppressing the chills: Effects of musical manipulation on the chills response. Frontiers in Psychology, 9: 2046. DOI: 10.3389/fpsyg.2018.02046
