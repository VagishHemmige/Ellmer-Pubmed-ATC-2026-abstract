#Abstract schema and prompt for LLMs

#Define LLM classifier
type_id_classification <- type_object(
  "
  Sequential infectious-disease classification.

  General rules:
  - is_id_related must be evaluated first.
  - When is_id_related == FALSE:
        → All pathogen subtype flags MUST be FALSE.
  - Pathogen subtype flags (virus, bacteria, fungus, parasite)
    MUST be FALSE unless that pathogen type is explicitly mentioned.
  - Do NOT infer pathogens from transplant, immunosuppression,
    rejection, sepsis, 'infection risk', biomarkers, or general infections.

  Disparities rules:
  - addresses_health_disparities must be evaluated before disparity subtype flags.
  - When addresses_health_disparities == FALSE:
        → ALL disparity subtype flags MUST be FALSE.
  - Do NOT infer disparities unless explicitly discussed.
  - Set to TRUE only if the abstract explicitly evaluates or highlights differences in access, treatment, or 
    outcomes between social groups (e.g., racial/ethnic groups, income/education levels, insurance categories, 
    neighborhoods, rural vs. urban).
    This includes:
         *Stated aims such as “to evaluate racial disparities in…”
         *Results that describe group-level differences (e.g., “Black recipients had lower access to…”).
    
  - Do NOT set to TRUE merely because race, ethnicity, sex, SES, or insurance are included as covariates 
    in a regression model.
  - Sex/gender differences should NOT be classified as gender disparities 
    unless the abstract explicitly frames them as disparities or inequities.
    

  Transplant organ rules:
  - Organ-specific transplant fields (heart, lung, kidney, liver, pancreas)
    MUST reflect only whether the abstract explicitly discusses that type of transplant.
  - These fields are independent of infectious-disease relevance.
  - A transplant-related abstract does NOT imply infection.
  - Pure transplant outcome studies should NOT be classified as infectious-disease related.
  ",
  
  # MUST BE FIRST
  is_id_related = type_boolean(
    "TRUE if infectious-disease related; FALSE otherwise."
  ),
  
  id_reason = type_string(
    "Short explanation for the infectious-disease classification, 
     referring only to explicit statements in the abstract."
  ),
  
  # DISPARITIES (evaluate early)
  addresses_health_disparities = type_boolean(
    "TRUE if abstract discusses racial, ethnic, socioeconomic, geographic, or insurance-related disparities."
  ),
  
  disparity_racial = type_boolean(
    "TRUE only if racial disparities are explicitly mentioned."
  ),
  
  disparity_ethnic = type_boolean(
    "TRUE only if ethnic disparities are explicitly mentioned."
  ),
  
  disparity_socioeconomic = type_boolean(
    "TRUE only if socioeconomic or income-related disparities are mentioned."
  ),
  
  disparity_geographic = type_boolean(
    "TRUE only if geographic disparities (distance, travel time, region, rurality) are mentioned."
  ),
  
  disparity_insurance = type_boolean(
    "TRUE only if insurance status, payer type, Medicaid, Medicare, private insurance disparities are discussed."
  ),
  
  disparity_gender = type_boolean(
    "TRUE only if sex or gender disparities are discussed."
  ),
  
  # TRANSPLANT ORGAN FIELDS
  is_heart_transplant_related = type_boolean(
    "TRUE only if the abstract explicitly discusses HEART transplantation."
  ),
  
  is_lung_transplant_related = type_boolean(
    "TRUE only if the abstract explicitly discusses LUNG transplantation."
  ),
  
  is_kidney_transplant_related = type_boolean(
    "TRUE only if the abstract explicitly discusses KIDNEY transplantation."
  ),
  
  is_liver_transplant_related = type_boolean(
    "TRUE only if the abstract explicitly discusses LIVER transplantation."
  ),
  
  is_pancreas_transplant_related = type_boolean(
    "TRUE only if the abstract explicitly discusses PANCREAS transplantation."
  ),
  
  is_multiorgan_related = type_boolean(
    "TRUE only if the abstract explicitly discusses MULTIORGAN transplantation."
  ),
  
  # OTHER TRANSPLANT DOMAINS
  discusses_immunosuppression = type_boolean(
    "TRUE if immunosuppressive medications or protocols are discussed"
  ),
  
  discusses_pediatric_population = type_boolean(
    "TRUE if the study specifically involves pediatric patients"
  ),
  
  discusses_living_donor_transplant = type_boolean(
    "TRUE if the study specifically involves living donor transplant"
  ),
  
  discusses_deceased_donor_transplant = type_boolean(
    "TRUE if the study specifically involves deceased donor transplant"
  ),
  
  discusses_donor_quality = type_boolean(
    "TRUE if the study specifically involves donor quality"
  ),
  
  discusses_policy = type_boolean(
    "TRUE if the study specifically involves transplant policy"
  ),
  
  discusses_waitlist_access = type_boolean(
    "TRUE if abstract discusses organ allocation, waitlist placement, or access to transplantation"
  ),
  
  # COMORBIDITIES
  discusses_diabetes = type_boolean(
    "TRUE if diabetes as comorbidity or outcome is discussed"
  ),
  
  discusses_cardiovascular_disease = type_boolean(
    "TRUE if cardiovascular outcomes or risk factors discussed"
  ),
  
  discusses_malignancy = type_boolean(
    "TRUE if malignancy or cancer risk discussed"
  ),
  
  discusses_obesity = type_boolean(
    "TRUE if obesity or BMI as risk factor is discussed"
  ),
  
  # PATHOGEN SUBTYPES
  is_virus_related = type_boolean(
    "TRUE only if a viral pathogen is explicitly mentioned 
     (e.g., HIV, HBV, HCV, CMV, EBV, SARS-CoV-2).
     MUST be FALSE if is_id_related is FALSE."
  ),
  
  is_bacteria_related = type_boolean(
    "TRUE only if bacterial pathogens are explicitly mentioned 
     (e.g., E. coli, S. aureus, Pseudomonas, C. difficile).
     MUST be FALSE if is_id_related is FALSE."
  ),
  
  is_fungal_related = type_boolean(
    "TRUE only if fungal pathogens are explicitly mentioned 
     (e.g., Candida, Aspergillus, Histoplasma).
     MUST be FALSE if is_id_related is FALSE."
  ),
  
  is_parasite_related = type_boolean(
    "TRUE only if parasitic pathogens are explicitly mentioned 
     (e.g., Toxoplasma, Strongyloides, malaria).
     MUST be FALSE if is_id_related is FALSE."
  )
)


id_prompt <- function(abstract) {
  glue("
You are an infectious diseases specialist and transplant researcher.
Classify the abstract using STRICT, SEQUENTIAL, and INDEPENDENT logic.

Return ONLY the schema fields. Do NOT add fields or commentary.

=========================================================
SECTION 1 — CORE LOGIC (READ CAREFULLY)
=========================================================

1. Evaluate **is_id_related FIRST**.
2. All other fields must be evaluated independently of each other.
3. Transplant fields, disparities, demographics, comorbidities, or outcomes
   MUST NOT influence infectious-disease fields.
4. Pathogen subtype fields (virus/bacteria/fungus/parasite)
   MUST reflect only pathogens explicitly named in the abstract.
5. “Infection,” “infection risk,” “postoperative infection,” sepsis, or
   “infectious complications” DO NOT count unless a pathogen type is named.

=========================================================
SECTION 2 — TRANSPLANT ORGAN FIELDS
=========================================================

Set to TRUE only if the abstract EXPLICITLY discusses:
- Heart transplantation
- Lung transplantation
- Kidney transplantation
- Liver transplantation
- Pancreas transplantation
- Multiorgan transplantation (simultaneous transplant of >1 organ)

These fields:
- Are entirely independent of infectious-disease classification.
- Must not cause is_id_related to become TRUE.

=========================================================
SECTION 3 — NON-INFECTIOUS THEMATIC FIELDS
=========================================================

Set to TRUE only if **explicitly** mentioned:
- discusses_immunosuppression
- discusses_pediatric_population
- discusses_living_donor_transplant
- discusses_deceased_donor_transplant
- discusses_donor_quality
- discusses_policy
- discusses_waitlist_access
- discusses_diabetes
- discusses_cardiovascular_disease
- discusses_malignancy
- discusses_obesity

Rules:
- These fields do NOT imply infectious disease.
- These fields must NOT influence pathogen subtype fields.

=========================================================
SECTION 4 — DISPARITIES LOGIC
=========================================================

addresses_health_disparities = TRUE only if disparities/inequities are explicitly discussed.
  - addresses_health_disparities must be evaluated before disparity subtype flags.
  - When addresses_health_disparities == FALSE:
        → ALL disparity subtype flags MUST be FALSE.
  - Do NOT infer disparities unless explicitly discussed.
  - Set to TRUE only if the abstract explicitly evaluates or highlights differences in access, treatment, or 
    outcomes between social groups (e.g., racial/ethnic groups, income/education levels, insurance categories, 
    neighborhoods, rural vs. urban).
    This includes:
         *Stated aims such as “to evaluate racial disparities in…”
         *Results that describe group-level differences (e.g., “Black recipients had lower access to…”).
    
  - Do NOT set to TRUE merely because race, ethnicity, sex, SES, or insurance are included as covariates 
    in a regression model.


Disparity subtype flags (racial, ethnic, socioeconomic, geographic, insurance, gender):
Disparity subtype flags (racial, ethnic, socioeconomic, geographic, insurance, gender):

Each subtype MUST be TRUE only when explicitly stated.  
Each subtype MUST be FALSE when 'addresses_health_disparities' == FALSE.

Below are specific examples of when to flag TRUE vs FALSE.

------------------------------------------------------------
1. RACIAL DISPARITY
------------------------------------------------------------

TRUE examples:
- “We found significant racial disparities in graft survival.”
- “Black patients experienced disproportionately higher mortality.”
- “Racial inequities persist despite adjustment.”

FALSE examples:
- “Outcomes were compared across racial groups.”
- “Black and white patients differed in baseline characteristics.”
- “Race was included as a covariate in regression models.”

**Key rule:** Differences ≠ disparities unless framed as inequity.

------------------------------------------------------------
2. ETHNIC DISPARITY
------------------------------------------------------------

TRUE examples:
- “Ethnic disparities in transplant access remain substantial.”
- “Hispanic patients face unequal access to pre-transplant evaluation.”
- “Inequities were most pronounced in Hispanic/Latino recipients.”

FALSE examples:
- “We compared outcomes among Hispanic and non-Hispanic patients.”
- “Ethnicity was recorded based on medical records.”
- “Ethnic subgroups showed different survival curves” (no inequity language).

------------------------------------------------------------
3. SOCIOECONOMIC DISPARITY
------------------------------------------------------------

TRUE examples:
- “Lower-income patients experienced inequitable access to waitlisting.”
- “Socioeconomic disadvantage was associated with worse outcomes.”
- “Neighborhood poverty drove disparities in transplant eligibility.”

FALSE examples:
- “We stratified outcomes by income quartile.”
- “Socioeconomic status was included as a covariate.”
- “Patients from lower-SES areas had lower survival” (difference stated but NOT framed as inequity).

------------------------------------------------------------
4. GEOGRAPHIC DISPARITY
------------------------------------------------------------

TRUE examples:
- “Patients in rural areas face disparities in access to transplant centers.”
- “Geographic inequity in organ availability remains a major challenge.”
- “Significant regional disparities were noted in waitlist mortality.”

FALSE examples:
- “Regional variation in practice patterns was observed.”
- “Outcomes differed across UNOS regions.”
- “Urban vs. rural differences were compared” (no mention of inequity or unequal access).

**Key rule:** Variation ≠ disparity.

------------------------------------------------------------
5. INSURANCE DISPARITY
------------------------------------------------------------

TRUE examples:
- “Medicaid patients experienced inequitable access to transplant.”
- “Insurance-based disparities persisted after adjustment.”
- “Uninsured patients faced barriers to specialty care.”

FALSE examples:
- “Outcomes differed by insurance type.”
- “Insurance status was associated with mortality” (association ≠ disparity).
- “Private insurance patients had higher transplant rates” (difference only).

------------------------------------------------------------
6. GENDER DISPARITY
(Special rule: sex/gender differences ≠ disparities)
------------------------------------------------------------

TRUE examples:
- “Gender-based disparities in access to transplant evaluation were identified.”
- “Women faced systemic barriers to referral.”
- “Inequitable outcomes were noted among female candidates.”

FALSE examples:
- “Women had lower transplant rates than men.”
- “Sex-stratified analyses showed differences in outcomes.”
- “Female recipients were younger at transplant.”

**Key rule:**  
Gender disparity requires **explicit inequity language**: “disparity,” “inequity,” “barriers,” “unequal access,” “structural disadvantage.”

------------------------------------------------------------
GLOBAL NEGATIVE RULE
------------------------------------------------------------
If an abstract NEVER uses inequity/framing language (disparity, inequity, unequal access, disproportionate burden, barrier, disadvantage),  
→ **ALL disparity flags MUST be FALSE**, even if group differences are reported.

=========================================================
SECTION 5 — INFECTIOUS-DISEASE SEQUENTIAL RULES
=========================================================

STEP 1 — Determine **is_id_related**.

is_id_related = TRUE ONLY if the abstract explicitly discusses:
- A named pathogen (virus, bacterium, fungus, parasite), OR
- Diagnosis, treatment, epidemiology, or prevention of infection, OR
- HIV (human immunodeficiency virus), HBV (hepatitis B virus), 
  hepatitis C virus, cytomegalovirus, Epstein-Barr virus, 
  BK virus, SARS-CoV-2, influenza, etc., OR
- Explicit transplant infections (e.g., CMV viremia, BK nephropathy).

is_id_related = FALSE when the abstract discusses ONLY:
- Graft outcomes, survival, rejection, allocation, organ quality,
  machine learning models, risk factors, disparities, immunosuppression,
  comorbidities, physiology, or center variation WITHOUT infections.

STEP 2 — If is_id_related == FALSE:
→ You MUST set ALL pathogen subtype fields to FALSE:
   - is_virus_related
   - is_bacteria_related
   - is_fungal_related
   - is_parasite_related

STEP 3 — If is_id_related == TRUE:
→ Set a pathogen subtype to TRUE ONLY if a pathogen of that type is explicitly named.

=========================================================
SECTION 6 — PATHOGEN SUBTYPE DEFINITIONS
=========================================================

Virus examples:
  HIV (human immunodeficiency virus)
  HBV (hepatitis v virus)
  HCV (hepatitis c virus)
  CMV (cytomegalovirus)
  EBV (Epstein-Barr virus)
  BK
  SARS-CoV-2
  influenza
  RSV (Respiratory Syncytial Virus)
  adenovirus

Bacteria examples:
  E. coli, S. aureus, Pseudomonas, Klebsiella, C. difficile.

Fungus examples:
  Candida, Aspergillus, Cryptococcus, Histoplasma, Mucor.

Parasite examples:
  Toxoplasma, Strongyloides, malaria (Plasmodium), Giardia.

NO PATHOGEN NAME → ALL pathogen subtype fields MUST be FALSE.

=========================================================
SECTION 7 — STRICT PROHIBITIONS (NO INFERENCE)
=========================================================

You MUST NOT infer infection from:
- Transplant status alone
- Immunosuppression or rejection
- “Infection risk,” “infectious complications,” or sepsis without pathogen
- Biomarkers, inflammation, cytokines, immune assays
- Prophylaxis without naming the pathogen
- General complications or postoperative morbidity
- Comorbidities (diabetes, CVD, obesity, malignancy)
- Disparities, demographics, or social determinants
- Allocation policy or waitlist changes

=========================================================
SECTION 8 — RESPONSE FORMAT (RETURN ONLY THESE FIELDS)
=========================================================

- is_heart_transplant_related
- is_lung_transplant_related
- is_kidney_transplant_related
- is_liver_transplant_related
- is_pancreas_transplant_related
- is_multiorgan_related

- discusses_immunosuppression
- discusses_pediatric_population
- discusses_living_donor_transplant
- discusses_deceased_donor_transplant
- discusses_donor_quality
- discusses_policy
- discusses_waitlist_access
- discusses_diabetes
- discusses_cardiovascular_disease
- discusses_malignancy
- discusses_obesity

- addresses_health_disparities
- disparity_racial
- disparity_ethnic
- disparity_socioeconomic
- disparity_geographic
- disparity_insurance
- disparity_gender

- is_id_related
- id_reason

- is_virus_related
- is_bacteria_related
- is_fungal_related
- is_parasite_related

=========================================================
ABSTRACT TO CLASSIFY
=========================================================
\"{abstract}\"
")
}
