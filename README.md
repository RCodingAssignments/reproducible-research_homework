# Reproducible research: version control and R

Link for questions 1,2,3: https://github.com/RCodingAssignments/logistic_growth/tree/main 


Question 4-

While both paths are random, they each exhibit different dynamics. Both paths start at the origin but then terminate at different locations- path1 ends at (-3.1,-5.4) while path2 ends at (2.7,-2). The overall dynamics of path1 show the path becoming more negative, while path2 becomes more positive over time. I then, by chance, ran the code again and found two new random paths formed. This shows how each time you run the code, a different random path is produced however, the paths always start at the origin as the first row of the data frame specifies that the first values of x and y are both 0. 

A random seed is a number used to intitate a random number generator. This seed need not be random itself but, the subsequent sequence generated by the initial seed is pseudorandom. Notably, the sequence is repoducible such that that same seed input will produce the same random sequence. 

The reason why the brownian motion code produces a different path each time is because each time the code is executed, a different, randomly generated angle is used. To make the brownian motion simulation reproducible the angle used must remain constant. As such, I have added the set.seed() function in the code, to ensure the same seed is used each time, and that the path generated is the same. 

Question 5-

rows= 33
columns= 13

To model the data in a linear fashion, one could apply a log transformation to the equation: **$`V = \beta L^{\alpha}`$**. After logging both sides, the resulting equation is lnV=ln($\beta$) +($\alpha$)lnL. When compared to the linear line equation y=mx+c, m (the gradient) is $\alpha$ and c (the y-intercept) is ln($\beta$). The paramaters estimated by this linear model are: m=  1.5152 and c= 7.0748. This means that $\alpha$= 1.5152 and $\beta$=exp(7.0748). Both of these values are significant as the corresponding p values are <0.05. The p value of the $\alpha$ estimate is 6.44e-10 and the p value of the ln($\beta$) value, and in turn the $\beta$ estimate is 2.28e-10. These estimates are very similar to the ones found in the original paper- I estimated 1.5152 for $\alpha$ and the paper estimated 1.52, and I estimated exp(7.0748) (~1182) for $\beta$ which is what the paper estimated. 

Code for the figure is provided in the repo

Based on my estimated values, virion volume can be estimated by the equation: V=exp(7.0748)*genome length^1.5152. Therefore, when the genome length is 300 kb, the estimated virion volume is exp(7.0748)*300^(1.5152)= 6,697,006.58117nm<sup>3</sup>. 

Bonus question-

Reproducibility is when the data, analysed using the same methods, produces the same results. On the other hand, replicabiility is when the analysis is repeated using the same methods, different data, and produces the same results. In the case of the virion volume data, if someone repeated the analysis using the method and estimated the same $alpha$ and $beta$ values, this analysis is reproducible. If the same analysis was conducted on a different set of dsDNA virus data and the same $alpha$ and $beta$ values were estimated, this analysis is replicable. Github provides the ability for computing analyses to be both reproducible and replicable. Regarding the former, one could go back to a previous repository of theirs to access the code and data, and repeat the analysis they intitially done. If the same results are found, this analysis is reproducible. Public repositories enable the replicability of work to be tested- one could apply the same code (methods) to a different set of data and if the same results are obtained, the initial study is replicable. 



## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
