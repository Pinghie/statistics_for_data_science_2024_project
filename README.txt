The project consists of four files, replicating the experiments discussed in Breiman 2001.

**rf_errors_launcher.R** works in tandem with **rf_errors.R**. They are the implementation of Chapters 4 and 5 of the original paper. The former script selects and prepares the datasets, and calls the error functions defined in the latter, which contains exclusively the definitions of the four required functions.

**rf_empirical_plots** works in tandem with **rf_empirical_results**. They are the implementation of Chapter 6 of the original paper. The former script selects and prepares the datasets, calls the functions defined in the latter file and plots the output. The latter file contains exclusively the definitions of the five required functions.

Information about the implemented functions is extensively presented within the scripts.
