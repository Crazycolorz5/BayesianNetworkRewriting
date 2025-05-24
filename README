# Abstract

It is known that Bayesian networks alone are an insufficient model for causality, as they merely describe the decomposition of a joint probability into conditional probabilities.
They fail, in most cases, to capture directionality --- a crucial aspect of causality.
Furthermore, conditional probability fails to tell us about hidden confounders.

I look at the mathematical formalizations of these two shortcomings, and review the existing corpus of work on their categorization.
I implement a tool to analyze when these shortcomings may be an issue and create ambiguities in Bayesian networks.
The tool is used to assess potential ambiguities in existing Bayesian networks, and the implications of such ambiguities on causal inference are discussed.

# Structure of this Repository

`document.tex` and `document.pdf` contain the write-up describing what the tool is implementing and relevant analyses done with it. It is recommended to take a look at them.

`main.ml` is the front-end driver of the program.
`graph.ml` is the main body of the program, and implements all necessary graph algorithms.
`parser.mly` and `lexer.mll` exist to parse graph inputs.

`liver-disorders.txt`, `smoking.txt`, and `qol.txt` are graph inputs to the tool, with `*.png` being a picture of the graph they describe (these are taken from literature).
The other `.txt` files are outputs of the tool running in various modes.
All of the subdirectories are also example outputs of the tool (from enumerating equivalencies).
