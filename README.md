# Analysis Tools for Case Study of Research European Collaboration from Systems Biology

This tools are used to analyse the data collected during the case study I conducted in a Research European Collaboration from Systems Biology . It contains:

- Data structures for survey, transcripts, and data-logs data.
- Visualisation functions
- Analysis functions

## Installation

Dependencies:
- R
- python
- ipython 4

Clone and use.

## Usage


```python
logsstable<- LogsTable$new()
logsstable$loadtable()
logsstable$getLogBy()
ds<-logsstable$data
```


## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D


## License

This work supports my PhD Thesis at University of Manchester.


[![DOI](https://zenodo.org/badge/3924/kjgarza/case_study_analysis.svg)](https://zenodo.org/badge/latestdoi/3924/kjgarza/case_study_analysis)


<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
