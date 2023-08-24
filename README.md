This repository contains an R project with the following subfolders and files around the paper titled "Anthropocene Literature":

## Corpus description

Anthrocorpus
The Anthropocene corpus, serving as the target corpus, consists of 7 post-apocalyptic novels written post-1945.

frequency list (types in list: 42983, tokens in list: 477332)

## Genre: 
Post-apocalyptic fiction

Bestsellers Corpus
Bestsellers Corpus, serving as the reference corpus, consists of 8 random best sellers.

##Genre:
Science fiction, fantasy, romance, epic fantasy, speculative fiction, thriller, contemporary romance

## Language: 
English



## Source: 
collected novels and converted to .TXT format

## Scripts
Rscript.R contains:
- exploratory-analysis: which analyses individual novels, concordances, collocations, and word frequencies
- keyword-analysis: which compares the Anthrocorpus to the Bestsellers corpus to extract Anthropocene keywords.



## Quarto
- Anthropocene-Literature.qmd: the source Quarto file of the paper.
- Anthropocene-Literature.html, Anthropocene-Literature.pdf: the output in different formats. The paper should be read in html format for better browsing/scrolling experience.

## Helpers
- bibliography.bib:, the bibtex file for literature references
- packages.bib: the bibtex file for packages
- unified-style-sheet-for-linguistics.csl: the stylesheet for references
