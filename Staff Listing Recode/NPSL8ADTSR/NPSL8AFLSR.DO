infix using "C:\NHFS20\RECODE\EXPORT\EXPORTEDFILES\NPSL8AFLSR.dct"

label variable inv_id   "INVENTORY ID"
label variable vu11     "Provider line number"
label variable vu13     "Provider Qualification (survey specific)"
label variable vu13a    "Provider Gender"
label variable vu14     "Provides ART"
label variable vu15     "Provides HIV counseling/testing"
label variable vu16     "Provides HIV related"
label variable vu17     "Provides any malaria STI TB dx/tx"
label variable vu17a    "Provides malaria dx/tx"
label variable vu17b    "Provides TB dx/tx"
label variable vu17c    "Provides STI dx/tx"
label variable vu17d    "Provides non-communicable disease dx/tx"
label variable vu18     "Provides any ANC/FP/Delivery"
label variable vu19     "Provides other client services"
label variable vu20     "Conducts lab tests"
label variable vu21     "Individual interview attempted"
label variable vu22     "Provides ANC care"
label variable vu23     "Provides PMTCT care"
label variable vu24     "Provides Delivery care"
label variable vu25     "Provides FP care"
label variable vu26     "Provides Child health care"
label variable vu27     "Provides surgery"

#delimit ;
label define VU13    
     1 "Generalist [non-specialist] (01)"
     2 "Gynecologist / obstetrician (02)"
     3 "Anesthesiologist (03)"
     4 "Pathologist (04)"
     5 "General surgeon (05)"
     6 "Pediatrician (06)"
     7 "Medical officer (MBBS, BDS) (08)"
     8 "Other specialists medical doctors (07)"
    15 "Nurse/auxillary nurse midwife (ANM) (10,13)"
    16 "Anesthetic assistant (09)"
    25 "Lab technologist/officer/ lab tech/lab assistant (11)"
    35 "Counselor with clinical qualification (Stand-alone HTC only) (16)"
    36 "Counselor without clinical qualification (Stand-alone htc only) (17)"
    40 "Health assistant (ha) / AHW / SAHW / PHI (12)"
    42 "Other clinical staff not listed above (e.g., Dietician) (18)"
    50 "Non-clinical staff/No technical qualification (95)"
;
label define VU13A   
     1 "Male"
     2 "Female"
;
label define VU14    
     0 "No"
     1 "Yes"
;
label define VU15    
     0 "No"
     1 "Yes"
;
label define VU16    
     0 "No"
     1 "Yes"
;
label define VU17    
     0 "No"
     1 "Yes"
;
label define VU17A   
     0 "No"
     1 "Yes"
;
label define VU17B   
     0 "No"
     1 "Yes"
;
label define VU17C   
     0 "No"
     1 "Yes"
;
label define VU17D   
     0 "No"
     1 "Yes"
;
label define VU18    
     0 "No"
     1 "Yes"
;
label define VU19    
     0 "No"
     1 "Yes"
;
label define VU20    
     0 "No"
     1 "Yes"
;
label define VU21    
     0 "No"
     1 "Yes"
;
label define VU22    
     0 "No"
     1 "Yes"
;
label define VU23    
     0 "No"
     1 "Yes"
;
label define VU24    
     0 "No"
     1 "Yes"
;
label define VU25    
     0 "No"
     1 "Yes"
;
label define VU26    
     0 "No"
     1 "Yes"
;
label define VU27    
     0 "No"
     1 "Yes"
;

#delimit cr
label values vu13     VU13    
label values vu13a    VU13A   
label values vu14     VU14    
label values vu15     VU15    
label values vu16     VU16    
label values vu17     VU17    
label values vu17a    VU17A   
label values vu17b    VU17B   
label values vu17c    VU17C   
label values vu17d    VU17D   
label values vu18     VU18    
label values vu19     VU19    
label values vu20     VU20    
label values vu21     VU21    
label values vu22     VU22    
label values vu23     VU23    
label values vu24     VU24    
label values vu25     VU25    
label values vu26     VU26    
label values vu27     VU27    
