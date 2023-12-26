
```bash
.
├── 2023_serrapilheira_ICTP.Rproj   # RStudio project
├── README.md                       # this file
├── _site.yml                       # site config
├── 00_instructions.Rmd
├── 01_presentation.Rmd             # landing page for each class
├── 01_presentation/                # subfolder for each class + labs
│   ├── lab.Rmd                     # lab source file
│   ├── lab.html
│   ├── slides.Rmd                  # slides source file
│   ├── slides.html
│   └── xaringan-themer.css
├── index.Rmd                       # home page
├── about.Rmd                       # about us
├── docs/                           # output folder for the site 
└── theme.css
```

## To deploy this website locally

+ Clone this repository
+ Install `distill`, `xaringanExtra`: 
```
install.packages("distill")
install.packages("xaringanExtra")
```
+ Click on the Build Website button of the Build pane in RStudio or execute `rmarkdown::render_site(encoding = 'UTF-8')`. R may prompt you to install/update some extra packages.

A version of this site will be created in the `docs/` subfolder. Open **index.html** in your web browser. 

## To add slides to each class

Slides are saved in each subfolder, 01 to 09. These numbers refer to the number of topics, not classes, to allow for flexibility. E.g. all linear model classes will be in a single subfolder, independently of the number of slide decks or lab tutorials. 

Xaringan slides need xaringan: `install.packages("xaringan")`. These are `.Rmd` files with a special YAML header (you can modify the YAML based on the [sample slides](01_presentation/slides.Rmd)

You can knit the slides using the knit button, pressing `ctrl+shift+K` in RStudio, or  execute 

```
rmarkdown::render('./[0x_topic]/slides.Rmd',  encoding = 'UTF-8')`.
```

> **Note**  
> Remember to knit your slides and labs before building the site again.  
> Push the `.Rmd` and the `.html` for your slides and labs, the subfolder /libs and `xaringan-themer.css`  
> Push the contents of `/docs` as well, Netlify will build the site from there

