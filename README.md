# QualityTraits_NIRS_Calibration

The actual repo contains an R workflow that allows to pull Nirs and quality traits Data using API calls,apply preprocessing and smoothing methods to spectral data, fit models, compare and visualize models predictions and performance metrics.

The Rshiny version of the workflow initially allows data extraction based on user specifications of quality lab, crop, Country ..etc. Data quality metrics and column statistics are computed for both Quality traits data and corresponding NIRS Data.

Multiple smoothing methods are available for processing NIRS Data. Trait to be modeled is also displyed to capture spectral signatures across Traits. 

The Multivariate analysis initially allows to fit PCA to map variation of traits and spectra with calibration and test sets. Cumulative variance, distances, scores are mapped to evaluate analysis results. Additionally, custom classification parameters feature allows to create classes from numeric values of interest trait in order to run classification tasks in the next section. The density of classes and original numeric value of the interest trait are displayed side by side for user to validate the created classes.

The Modeling section allow users to fit PLS (partial least squares regression), PLS-DA (partial least squares Discriminant analysis), SIMCA (Soft Independent Modelling of Class Analogy) for classification tasks. More details are provided here : https://mdatools.com/docs/simca.html. 

The modeling section allows to visualize multiple evaluation metrics, such as, model summary, model coefficients, residuals, calibration and testing results, RMSE and selctivity ratio. More metrics will be added in future developments.

The last section, Make Predictions, allows user to specify country, crop, site and year to make predictions using the trained model. Predictions are displayed in the table for QA/QC prior to eporting them as csv, excel or pdf.

20-05-2024, Work in progress...




