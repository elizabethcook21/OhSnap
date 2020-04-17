
#OhSnap! 
##A private, user-customized health record tracking web application

Authors (unordered, alphabetical): Elizabeth C. Anderson, Tyler Heaton, Ed Ringger, Erica Suh, under the direction of Dr. Samuel Payne
###Brigham Young University
April 2020

##Abstract
Although most hospitals have Electronic Health Record (EHR) systems that track symptoms and test results over time, people consistently want a more private and personal data storage option, largely due to existing shortcomings in the interoperability of data between providers, and the current lack of control users have over their own data in these systems. In addition, many people with chronic conditions visit a hospital or clinic routinely for the same tests and obtain their results as a physical sheet of paper, but wish to be able to store and track this information in a more efficient and effective way. To meet these demands, we developed OhSnap!, a web application implemented in Shiny R, which allows users to efficiently parse, process, and store data from an image of their medical test results using Tesseract Optical Character Recognition (OCR). Alongside facilitating patients in their pursuit to track and store their own data, OhSnap! allows users to visualize this data in the context of typical/expected values with interactive graphs. OhSnap! also provides basic contextual information about each test, as well as links to reputable websites for additional information, where appropriate. This version of the application focuses on reading in and storing data from Complete Blood Count and Comprehensive Metabolic Panel tests because of the potential for data from these tests to be used in predictive and preventative medicine, but future versions of the app will be capable of reading in and parsing any kind of health-related data.
