\name{alzheimers}
\docType{data}
\alias{alzheimers}
\title{Alzheimer's Disease Dataset}
\format{
A data frame with 2,149 observations on the following variables:

\describe{
  \item{Age}{Age in years (60 to 90).}
  \item{Gender}{Gender (0 = Male, 1 = Female).}
  \item{Ethnicity}{Ethnicity (0 = Caucasian, 1 = African American, 2 = Asian, 3 = Other).}
  \item{EducationLevel}{Education level (0 = None, 1 = High School, 2 = Bachelor's, 3 = Higher).}
  \item{BMI}{Body Mass Index (15 to 40).}
  \item{Smoking}{Smoking status (0 = No, 1 = Yes).}
  \item{AlcoholConsumption}{Weekly alcohol consumption in units (0 to 20).}
  \item{PhysicalActivity}{Weekly physical activity in hours (0 to 10).}
  \item{DietQuality}{Diet quality score (0 to 10).}
  \item{SleepQuality}{Sleep quality score (4 to 10).}
  \item{FamilyHistoryAlzheimers}{Family history of Alzheimer's (0 = No, 1 = Yes).}
  \item{CardiovascularDisease}{Cardiovascular disease (0 = No, 1 = Yes).}
  \item{Diabetes}{Diabetes (0 = No, 1 = Yes).}
  \item{Depression}{Depression (0 = No, 1 = Yes).}
  \item{HeadInjury}{History of head injury (0 = No, 1 = Yes).}
  \item{Hypertension}{Hypertension (0 = No, 1 = Yes).}
  \item{SystolicBP}{Systolic blood pressure (90 to 180 mmHg).}
  \item{DiastolicBP}{Diastolic blood pressure (60 to 120 mmHg).}
  \item{CholesterolTotal}{Total cholesterol (150 to 300 mg/dL).}
  \item{CholesterolLDL}{LDL cholesterol (50 to 200 mg/dL).}
  \item{CholesterolHDL}{HDL cholesterol (20 to 100 mg/dL).}
  \item{CholesterolTriglycerides}{Triglycerides (50 to 400 mg/dL).}
  \item{MMSE}{Mini-Mental State Examination score (0 to 30). Lower is worse.}
  \item{FunctionalAssessment}{Functional score (0 to 10). Lower is worse.}
  \item{MemoryComplaints}{Memory complaints (0 = No, 1 = Yes).}
  \item{BehavioralProblems}{Behavioral problems (0 = No, 1 = Yes).}
  \item{ADL}{Activities of Daily Living score (0 to 10). Lower is worse.}
  \item{Confusion}{Presence of confusion (0 = No, 1 = Yes).}
  \item{Disorientation}{Presence of disorientation (0 = No, 1 = Yes).}
  \item{PersonalityChanges}{Presence of personality changes (0 = No, 1 = Yes).}
  \item{DifficultyCompletingTasks}{Difficulty completing tasks (0 = No, 1 = Yes).}
  \item{Forgetfulness}{Forgetfulness (0 = No, 1 = Yes).}
  \item{Diagnosis}{Alzheimer's diagnosis (No, Yes).}
}
}
\usage{
data(alzheimers)
}
\description{

  Health, lifestyle, and clinical data for 2,149 individuals used for
  studying Alzheimer's Disease. Variables include demographics,
  cognitive assessments, medical conditions, and symptoms.

}
\details{

  This dataset is suitable for modeling Alzheimer's risk, performing
  exploratory analysis, and evaluating statistical and machine learning
  algorithms. All individuals are uniquely identified and evaluated on a
  standardized set of clinical and behavioral measures.
  
}

\source{
Rabie El Kharoua (2024). Alzheimer's Disease Dataset. Available from
  Kaggle at \url{https://www.kaggle.com/datasets/rabieelkharoua/alzheimers-disease-dataset}
}

\examples{
\donttest{
## load the data
data(alzheimers, package = "varPro")
o <- varpro(Diagnosis~.,alzheimers)
imp <- importance(o)
print(imp)
}}

\keyword{datasets}
