What is the effect of the `SAMPLEREPEAT` parameter set in the script `01-sensitivitiy-analysis.R`?

For `SAMPLEREPEAT = 5`, the following numbers were calculated.



As to be seen by this small experiment, increasing the `SAMPLEREPEAT` function argument only decreases the standard deviation of the results by 50%, give or take. This is to be expected following the law of large numbers. Consequentially, reducing the last bit of noise becomes increasingly computationally expensive.