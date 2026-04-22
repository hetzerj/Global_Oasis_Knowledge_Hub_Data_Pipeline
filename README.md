# Global_Oasis_Knowledge_Hub_Data_Pipeline

This repository contains the data pipeline for the **Global Oasis Knowledge Hub**. It is used to generate, enrich, and prepare the literature data that powers the platform. The pipeline supports the regular processing of reviewed references, their matching to OpenAlex records, the construction of citation networks, and the export of platform ready output files.

## Project Structure

```text
/Global_Oasis_Knowledge_Hub_Data_Pipeline
│— run_full_update.R       # Main script for running the full pipeline locally
│— run_monthly_update.R    # Main script for updating derived outputs locally
│— R/                      # Functions used by the pipeline
│   └— enrich_reviewed_references_openalex.R   # Functions for loading reviewed references, matching them to OpenAlex, and enriching them with citation network metadata
│   └— postprocess_for_platform.R              # Functions for transforming enriched data into flat platform ready outputs
│— input/                  # Folder for manually curated input files
│— output/                 # Folder for generated pipeline outputs
│— README.md               # Project documentation
```
## How to Run 

Note: Running the pipeline requires an OpenAlex API key to be available in the environment. OpenAlex provides free API keys for registered users, and the key can be created in your account settings at `openalex.org/settings/api`. If you do not yet have an account, you can sign up at `openalex.org/signup`. :contentReference[oaicite:0]{index=0}

0. Install **R** and **RStudio** if not already installed.
1. Set the OpenAlex API Key in your local environment, for example by calling `file.edit("~/.Renviron")` and including:
```r
openalexR.apikey = your_api_key_here   
openalexR.mailto = your_email_address_here
```
2. Install the required packages, for example:  
   ```r
   install.packages(c("openalexR", "readr", "jsonlite", "arrow"))
   ```
3. Clone or download this repository.  
4. Make sure to include the reviewed reference file in the `input/` folder.  
5. Run the full pipeline locally with:  
   ```r
   source("run_full_update.R")
   ```
6. Run the monthly update locally with:  
   ```r
   source("run_monthly_update.R")
   ```

## Output

The pipeline generates processed files in the `output/` folder, including:

- matched reviewed references  
- enriched citation network data  
- node and edge tables for downstream use  
- platform ready export files  

These outputs can then be used by the **Global Oasis Knowledge Hub** platform repository.

## Future Development

This pipeline is intended to support local execution as well as automated scheduled runs via GitHub Actions. Future versions may include regular uploads of processed datasets to Zenodo for integration with the platform.

## 🌐 Live Web Interface  
The compiled web interface can be accessed here:  
🔗 [Global Oasis Knowledge Hub](https://hetzerj.shinyapps.io/Global_Oasis_Knowledge_Hub/)  

The code repository for the platform framework is:  
🔗 [Global_Oasis_Knowledge_Hub repository](https://github.com/hetzerj/Global_Oasis_Knowledge_Hub)


---

📧 *For questions or contributions, please contact the developers (Dr. Jessica Hetzer - jessica.hetzer@senckenberg.de; Dr. Rainer M. Krug - rainer.krug@senckenberg.de; Dr. Aidin Niamir - aidin.niamir@senckenberg.de)*
