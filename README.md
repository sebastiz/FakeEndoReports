# FakeEndoReports

The idea of this project is to create synthesised Endoscopic reports, starting with gastroscopies, so that data scientists can utilise gastroenterology data to develop better visualisations of gastroenterology data

The endoscopy datasets are created from building blocks but internally checked so that the complete report makes sense. Furthermore, patients with individual conditions that, for example require ongoing surveillance, will have further endoscopies in the dataset for that condition which is truer to real life

As in real-life, some of the endoscopies will also have pathology reports where specimens are taken. The pathology reports are also synthetic and created in a format that means that the report as a whole makes sense.

The methodology for the building of these synthetic datasets will be further explained when the documentation is fully developed.

To install you will need to install the developing package. with 


**devtools::install_github("sebastiz/FakeEndoReports")**

You can then run the script FakeDataGenerator.R which is well documented and describes all the steps to create the Endoscopic and Pathological data sets




The data sets are created as follows:

**1. Creation of the endoscopy datasets.**

  + i. Create sample of Normal endoscopy report with empty reports.
  + ii. Fill empty reports with sample of pre-collated basic macroscopic findings (stricture/polyp etc).
  + iv. Add further embellishment based on the macroscopic observation.
  + iii. Proportional replacement of some of the Normal gastroscopy findings with a named disease (eg Barrett's, hiatus hernia etc.)
  + iv. Replacement of the named disease with detail about the disease and detail as needed
  + v. Report naturalisation- introduction of conjunctions and others to make the report sound less computer generated
  + vi. Extraction of which compartment has been reported on (stomach/oesophagus/ D1 etc.)- necessary to make pathology relevant to the endoscopy report
	vii. Create top and tail to the report- sedation used/indication etc.
	
**2. Extrapolation of samples taken from the endoscopy dataset to initialize a separate pathology dataset.**

  + i. Extract from the endoscopy report the labelled compartment
  + ii. Clean up the compartment to get rid of repetition etc.
  + iii. Further extraction of which compartment with randomisation of number of biopsies 
	iii.Merge with the endoscopy dataset.
	
**3. Creation of the pathology dataset.**

  + i. Create the dpathology dataset based on a) the Compartment and b) Disease specific
  + ii. The pathology report has an introductory line, an Indication (from the associated endoscopy report), A Macroscopic finding (number of biopsies and where taken from), Microscopic finding (based on the compartment and disease) and add generic list of negatives before the report conclusion. All of these come from randomly sampled pre-compiled lists. The relevant negatives are inserted as long as the terms are not already mentioned (which would result in contradiction).
  + iii. Create proportional pathology reports per disease and merge all disease (and normal) datasets for that compartment into one dataset for that compartment.
	iv. 

**4. Combine the endoscopy with the pathology dataset as long as biopsies have been taken**

**5. Tidy up loose columns**

