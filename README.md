# MergingHD
This repository contains the code used in the study presented in the paper "Merging Microarray Studies and Using Random Forest to Identify a Common Gene Expression Signature in Heart Disease" by Olga Fajarda, Sara Duarte-Pereira, Raquel M. Silva and José Luís Oliveira.

# Folder data
This folder contains the data obtained after the pre-processing:
- all_before_rem_batch: expressions obtained after the pre-processing;
- all_id: identification of the sequences through the GB accession number;
- all_platform: identification of the platform used to obtain the expressions;
- all_samples: identification of the samples;
- all_type: class of every sample (D: diseased or Z: control).

# Folder script
This folder contains the scripts used:
- batchRemoval_ComBat: to remove the batch effect;
- featureSelection_limma: feature selection using limma package;
- plot_mds_after: plot MDS using the expression after removing the batch effect;
- plot_mds_before: plot MDS using the expression before removing the batch effect;
- preprocessing: data pre-processing;
- randomForest_without: Random Forest algorithm without a sampling method;
- randomForest_withSampling: Random Forest algorithm with sampling methods.
