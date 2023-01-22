# processALA
R package to give easy access to Atlas of Living Australia (ALA) plant occurrence data and assist the resolution of plant taxonomic or nomenclatural issues.

**processALA** is an _**R**_-package developed to make downloading and cleaning occurrence data for plant species from the Atlas of Living Australia (ALA) a simple process. By stream-lining the way you interact with ALA, the package should allow you to check taxonomic names and obtain useful occurrence data "live". That is, you can integrate the process of downloading the latest occurrence data into _R_-scripts and run them whenever you need up-to-date data. Alternatively, you can use the functions included in the package to run data download and cleaning to periodically refresh sets of local stored data files. This second approach can be the most efficient method because data from ALA changes infrequently, which is particularly true of herbarium data which is the highest value data for most use cases we encounter. In addition, live interrogation of ALA depends on a good internet connection and naturally introduces delays to running scripts.

A major re-working of **processALA** was undertaken in late November 2021 to transition from using the package **ALA4R** to the package **galah**. **ALA4R** was retired at the end of 2021 in favour of its replacement package **galah**. Development of **processALA** has been ongoing since then because the developers of **galah** frequently make significant and unannounced changes to critical data structures and functions within the package **galah**. 

To view and download the _latest_ source for **processALA** go to: https://github.com/peterbat1/processALA.

To install the package in your local _**R**_ installation, it is easiest to ensure that the following dependencies are installed in your local _**R**_ system:

* galah
* httr
* dplyr
* ozmaps (for running the worked example in this vignette)

It is most productive to install these packages using the package management features in *R-Studio* as they pull in a fair number of dependencies and it is a pain to work through that list manually before you finally install the target package.

To complete the installation of **processALA**, ensure that the package **remotes** is installed from CRAN, then enter the following command at the prompt in the _**R**_ console:

```{r eval=FALSE}
remotes::install_github("peterbat1/processALA", build_vignettes = TRUE)
```

As with any _**R**_-package installed on your system, your scripts must begin with:

```{r setup}
library(processALA)
```

## A bit about ALA and accessing data

The Atlas of Living Australia is many things:

* A gateway to a comprehensive collection of occurrence records of many types including herbarium records from Australia and New Zealand supplied by Australia's Virtual Herbarium (which is hosted by ALA)
* The source of an accepted National Species List, and repository of taxonomic and nomenclatural information which, for plants, is formed by the Australian Plant Name Index (APNI) and the Australian Plant Census (APC) hosted by ALA on behalf of the Council of Heads of Australasian Herbaria (CHAH)
* The Australian node of the Global Biodiversity Information Facility (GBIF)

ALA is an important source of occurrence data and taxonomic information which can be accessed through a web interface. However, for many researchers this can be cumbersome, particularly when many taxa must be processed. Fortunately, ALA also provides an API or Application Programming Interface which lets you perform searches and retrieve records using scripts. This makes it easier to process lists of taxa or make periodic updates of information. You can find out more about the ALA API [here](https://api.ala.org.au).

ALA also maintains an _**R**_-package, **galah**, to make it easy to use the ALA API. The functions within that package allow us to make use of the API without having to learn the ins and outs of composing query strings, and the application of web access tools such as _curl_ and _wget_. (Of course, if you're into such things there are fun times ahead if you want to work directly with curl or wget calls.)

The package **processALA** adds another level of refinement. It highly automates key steps in the retrieval and processing of ALA data by utilising a few key functions in the **galah** package and adds functions to assist in data cleaning. In addition, it adds a mechanism to access some ALA API functions not exposed in **galah**.

> **NOTE:** You must register an email address with ALA before you can use **processALA** to download occurrence data. Once you have done this, you should enter the following line in to the R console:  galah::galah_config(email = "your_email@blah.com")  obviously substituting the registered email address for "your_email@blah.com". This will ensure that, when functions in **processALA** call functions in **galah**, you will be recognised by ALA and the data you request will be supplied.
>  
> To create an account with ALA, use this link: [https://auth.ala.org.au/userdetails/registration/createAccount](https://auth.ala.org.au/userdetails/registration/createAccount)

