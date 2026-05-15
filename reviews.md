Referee expertise:

Referee #1: behavioral ecology, social insects

Referee #2: social insects, division of labor

Referee #3: social insect behaviors


Reviewers' comments:

Reviewer #1 (Remarks to the Author):

The manuscript deals with an extremely interesting topic – network properties of the social structure of bumble bee colonies with and without a queen. The authors use a novel methodology to track bumble bee behavior that allows them to collect very large amounts of data on interactions between individuals. This definitely opens a new perspective from which social organizations of insect colonies can be examined. The paper is well written and overall sound but a few details are missing that would strengthen the authors’ conclusions and clarify ambiguous points. Please see specific comments below.
1. Introduction
The introduction is well-written and provides a good background for the research question, however, it is unfortunately omitting one important piece of information. The competition point in the colony life cycle is does not just coincide with gyne production by the queen. It results from workers actively monitoring the state of the queen and the brood and starting competition when they acquire information of brood being committed to the gyne pathway (Alaux et al., 2004 and 2006). The queen apparently holds the power to inhibit gyne development and commit larvae to worker pathway (Chole et al., 2022, Franco et al., 2023) and the workers monitor her state to pick up the moment when she ceases to use this power. The main point here is that information flow from the queen to the workers, actively sought by the workers, is instrumental in the regulation of social structure in the colony in queenright conditions, and in queenless conditions information flow changes its direction because the main source of its importance, the queen who inhibits gyne-like development, is absent.
Results
2. Page 5 – top left paragraph: “This small but highly interactive population of bees contained 37 individuals across all five queenless partitions (two to twelve bees per partition). These individuals were slightly, but significantly, larger than non-outlier nestmates (Figure S13, χ2 = −2.50,p = 1.25 ∗ 10−2). “ The figure indicated here doesn’t compare the hub bees to their non-hub QL nestmates, it compares QR and QL bees. Also, it would be useful to indicate the actual mean sizes in mm.
3. Page 5 - left column: the authors claim in this part that the hub workers display queenlike behavioral phenotypes by showing differences between hub and non-hub workers. However, there is no comparison between hub workers and queens themselves. Such a comparison is essential to gauge the similarities and the differences between the roles of a queen and of hub workers in the network and to understand how queenlike these workers actually are. Also, while the authors show that the hub bees are more interactive there is no data on who initiates these interactions. For queens the authors showed the percentage of interactions initiated by the queen herself versus those initiated by workers approaching the queen. It would be useful to show this metric for hub workers too. Also, the figure illustrating behavioral differences between hub and non-hub bees would be more useful in the main body of the article rather than in supplementary materials. Another important piece of information that is missing is whether the hub bees were more likely to interact with other hub bees or with non-hub bees. In general, it would be useful to have a figure or a metric showing how interactions of each hub bee were partitioned between hub and non-hub individuals.
4. Page 5 – top right paragraph: “This coincides with a significant association between interactivity and ovary index in queenright colonies (Figure 4c, cor.test: n=100, r=0.45, p=1.81 ∗ 10−8 ) which is not seen in queenless colonies (Figure 4c, cor.test: n=114, r=0.02, p=0.81).” : probably typo, the figure and further text indicate that it is queenless, not queenright, workers that have correlation between ovaries and interactions.
5. Page 6 – top left paragraph: “Queenless networks were also more efficient (Figure 5c: χ2 = 39.6,p = 6.77 ∗ 10−271), suggesting that the larger total number of interactions in queenless colonies (Figure 2a: χ2 = 46.4,p =∼ 0) may facilitate a more rapid spread of information. Finally, queenless colonies were less disassortative than their queenright counterparts (Figure 5d: χ2 = 11.0,p = 1.22 ∗ 10−27 ), meaning the tendency of dissimilar individuals to interact was weaker among queenless nestmates, reflecting a shift toward a more neutral pattern of interactions among workers (i.e., neither an assortative nor disassortative network).” It’s not entirely clear what is meant by “efficiency” (probably the number of interactions per minute but it’s not spelled out). Also, when the authors talk about association between dissimilar individuals it’s not clear in what ways are these individuals dissimilar. That should be stated explicitly.
Discussion
6. Page 7 – top left paragraph: “For example, workers could be using standard interactions as a way to monitor their reproductive dominance status (Alaux, Jaisson, and Hefetz, 2006) or to advertise their sterility, potentially reducing within-colony conflict (Amsalem, Twele, et al., 2009).” Advertising sterility makes sense mainly to avoid aggression, so, if the workers advertise sterility to the queen, they would also advertise it to dominant workers in the QL partition. To determine if this is the case it would be useful to report how likely the dominant workers are to initiate interactions.
Methods
7. My main concern about the SLEAP methodology and the filtering of data is that it’s not clear how the model differentiated between interactions of different valence. Bumble bees display a wide range of aggressive behaviors that differ in their social significance from neutral ones such as antennation or affiliative ones such as huddling (full body contact most often performed by young bees) (Amsalem and Hefetz, 2010 and 2011). Interactions of different valence obviously carry different meaning for the network structure of the colony but may still manifest as head-to-head or head-to-body contact perceived by the SLEAP model. I wonder if there is any way to differentiate aggressive interactions from neutral or affiliative ones using this tool. Also, one of the common aggressive displays in bumble bees is darting – a rapid lunge of one bee towards another stopping short of collision. I wonder if filtering out of interactions by excessive speed might have inadvertently removed the data on darting. To clarify this matter it would be useful to report at what average speed the workers were moving and what speed limit triggered exclusion of an observation from the analysis.



Reviewer #2 (Remarks to the Author):

The authors compare the social networks of queenright and queenless bumblebee colonies using animal tracking data to infer interaction network changes after queen loss. The strength of the study is the detailed tracking of individuals which yields a large data set of individual interactions, and I believe that the quantification of these interactions is novel. The key finding of the study is that the loss of a queen alters network structure, where individuals, who enlarge their ovaries to become reproductive, take on central roles in the network, leading to an increase in behavioral variation between workers. While I can appreciate that a quantification of the network changes after queen loss is important, I also do not find these results too surprising since it is almost by necessity the case that behavioral variation between workers increases when some workers, but not others, change their reproductive physiology.

In their statistical analysis, the authors use linear mixed models to estimate means of responses given treatment and permutation tests to compare the variance between treatments. The permutation tests show that the variance differs by treatment, which leads the authors to conclude that behavioral variation increases with queen loss. However, I believe that there are some problems with these analyses. If the variance differs between treatments, then the variance should be modelled in the linear mixed models. While the estimated means from the linear mixed models will not be affected by between-treatment variation in variance, the estimates of uncertainty will be, and this can bias p-values. If the authors model the variance in the linear mixed models, it is unnecessary to do the permutation tests, but instead the estimated variance from the linear mixed models can simply be compared for queenright vs. queenless colonies. The authors use the lme4 package for their statistical analysis, which cannot model variance. I would recommend the authors to use the glmmTMB package, which can model variance by treatment (with distribution family set to gaussian, this is a lmm). Subsequently, the authors could use the emmeans package to compare the variance estimates for queenright vs. queenless colonies.

In the analyses corresponding to Fig. 4C, the authors use two correlation tests to test the association between interactivity and ovary index. The authors find that this association is significant in queenless colonies but not in queenright colonies (although the text states the reverse; it seems that “queenright” and “queenless” are confused at multiple spots in the corresponding text paragraph). However, the presence of a correlation in one case and its absence in the other does not necessarily imply that there is a significant difference in the slope of interactivity vs. ovary index in queenright vs. queenless colonies. It is therefore advisable to fit the linear mixed model with an interaction term (interactivity ~ queenless/queenright * ovary index + (1|random effects)). This comes at the additional benefit that the two correlation tests are not necessary, but just one linear mixed model.

Fig. 5a says “Queenless influencer” and “Queenless Non-Influencer Worker”. Is this supposed to mean hub bee and non-hub bee? Also see Fig. S3 and caption of Fig. S16 for additional occurrences of “Influencer”.



Reviewer #3 (Remarks to the Author):

The manuscript "Bumble bee workers adopt novel behavioral roles and reshape their social networks in the absence of a queen" demonstrates how reproductive status shapes interactions among nestmates, showing that non-reproductive workers are more likely to initiate contact with individuals of higher reproductive status (queens or workers with well-developed ovaries). This pattern leads to reproductive individuals functioning as “hub bees” with the strongest connections in the interaction network. The authors employ a split-colony design (size-matched workers, half maintained with the queen and half without) combined with automated tracking, allowing them to quantify not only spatial organization over time but also the directionality of interactions within the group. After four days in the absence of the queen, queenless workers self-organize according to reproductive status.

Overall, the introduction provides a clear rationale for the study, the experimental design is well suited to address the research aims, the results are thorough and appropriately analysed, and the conclusions are generally well supported by the data. I have only a few minor comments that the authors may wish to consider to improve clarity.

1. More information is needed in the methods section.
- In the first sentence of the Methods, it's not completely clear that the study was conducted on bees from 5 colonies that were recorded consistently over 4 days (for the 96 hours of video). These pieces of info can be deduced from the results, but it would be easy to add it here.
- Page 9: "5g of brood from the source colony." Where was the brood placed? Was spatial arrangement relative to brood included? What stage of development was the brood in? Were honey pots also added?

2. What is the biological relevance of studying spatial interactions among members of a bumble bee colony in 2-dimensional space, without access to a separate foraging arena and only a minimal amount of brood provided.
- Page 9: "Four cotton wicks soaked in nectar substitute (equal parts pure sugar water and inverted sugar water with added feeding stimulant and amino acid supplementation) were placed in one corner of the arena to allow ad libitum feeding, and 5g of ground honey bee pollen mixed with nectar substitute at a ratio of 10:1 (pollen:nectar substitute) was added for protein nutrition and a more naturalistic environment." Provisioning food directly inside the space where bumble bees are living is not really "natural" - though it is essential for them to survive.
- What was their motivation for maintaining an interaction network? Without brood to feed, temperature to maintain, or food to collect, can the results collected from this study be applicable to bees living in 3-D space? Often after the queen is gone, workers begin to do strange things in the colony. If they weren't allowed to leave the arena for 4 days, what is the certainty they were not behaving abnormally under such uniform conditions?
- Were the locations of the brood, food, or areas of defecation (presumably in the corners of the box) considered in the analyses? These factors may influence spatial arrangement.

3. Some of the conclusions that are provided are a bit vague or do not seem relevant for this study.
- Page 8: "Future work combining detailed behavioral tracking with molecular and neurobiological approaches will be crucial to uncover how these social traits develop and function, and when and how they influence both individual and collective outcomes in nature." Because this phenomenon regularly occurs at the end of the colony cycle (the queen dies and workers lay unfertilized eggs), it's not clear why this future work is necessary. Why is it crucial to uncover the mechanisms that influence social traits during the decline phase of the colony cycle?
-Page 8: "However, increased interactivity and connectivity may also carry a cost: higher interaction rates and the presence of multiple reproductive individuals could accrue higher energetic costs to the colony (Waters, 2014)." It's not completely clear what the relevance is here - how could multiple reproductive individuals accrue higher energetic costs to the colony based on what is shown in this ms about networks and interactions?
- Consider highlighting how "normal" it is for B impatiens to lose a queen and go through a queenless phase in their colony cycle in the conclusions. This study is particularly interesting because the data show that the structure of the interaction network remains strong even after the queen is gone. Instead of a colony-breakdown, queenless colonies can still maintain interaction networks.


Minor Comments:
Page 2: "we found head-to-head interactions were significantly enriched relative to head-to-body interactions..." Does this mean that there were more head-to-head interactions? The use of the term "enriched" seems to be the wrong term.

Page 2: "We detected a slight circadian effect in the frequency of head-to-head interactions" It's not clear what the circadian effect was. Fig 2 does not show a repeating pattern across the 4 days of testing, and Fig S6 & S7 separate the interactions between day and night, respectively.

Page 2: "In total, we quantified over 80 million undirected pairwise interactions across nearly 65 million frames. Of these, over 25 million were directed interactions initiated by one of the two bees (see Methods)." Of the undirected interactions, 25 million were directed? It's not clear what is being described here.

Page 2: "B. impatiens females exhibit an enrichment of head-to-head interactions in our data (Figure S6) and in previously published datasets (Wang et al., 2022; Wolf et al., 2023), have been historically used to analyze colony behavior" Check the sentence structure, so that B. impatiens females exhibited an ... or B. impatiens females that exhibit an...

Page 3: "Despite the queen interacting and moving more frequently than the workers, she explored relatively less space in the colony" This result has also been shown in Jandt & Dornhaus 2009, Animal Behaviour

Page 5: "Importantly, this lack of behavioral variation in queenright settings is not the result of a lack of physiological variation – there already variation in ovary size between workers in these queenless colonies (Figure 4c)." There seems to be a grammatical error in the sentence. It's also not clear what it being compared in the second part of the sentence. Lack of variation in queenright settings is not because they lacked physiological variation because ovary size variation was found in queenless colonies?

Page 6, Figure 4: The dashed reference lines make the small dots in Fig 4b almost impossible to see. Consider removing the reference lines and/or making the dots bigger and darker coloured. The lines in the other panels are fine, but also seem unnecessary.

Page 6: "Queenless networks were also more efficient..." Consider adding more context as to what they are more efficiently doing.

Page 6: "Queens were largely spatially constrained to the brood area..." Based on what was presented in the results, there were no analyses that included distance to the brood clump. Where does this conclusions come from?

Page 7: "A strong association between behavioral variation and reproductive physiology emerged in queenless conditions that was not observed in the presence of a queen." Technically, wouldn't the queen be a hub bee? The fact that ovarian development doesn't really start until the queen is removed suggests that reproductive physiology is a strong predictor of social organisation across contexts.

Page 7, Figure 5a: What is an "influencer" - should this say "hub bee"?

Page 7: "Queens are substantially larger and express a distinct cuticular chemistry that reflects reproductive status..." Consider adding "Bombus" before "Queens" as Apis was described in the previous sentence.


================================================================================
ROUND 2 REVIEWS
================================================================================


Reviewer #1 (Remarks to the Author):

The authors addressed my own and other reviewers' comments with great attention and in my opinion the article is ready for publication. There are still a few spots in the paper where "influencer" appears instead of "hub bee" which probably requires some minor editing but otherwise the paper is ready to go.




Reviewer #2 (Remarks to the Author):

I would like to thank the authors for revising the manuscript and taking my suggestions into account. I still have two concerns which have not been fully resolved in the previous revision.

(1) I have previously suggested to model treatment differences in variance with a linear model because the presence of treatment differences in variance can lead to inaccurate estimates of uncertainty and therefore bias p-values. The authors have now used a linear gaussian model to model treatment-specific variance, which I appreciate. However, the authors are still using the p-values from the previous linear model, which does not account for treatment-specific variance, to compare treatment means. The comparisons of treatment-specific means and variances should occur in the same model to avoid biasing p-values, even if this means that some of the previously significant effects will "disappear". That means there should be just one model that models both the means and the variances.

(2) The authors now use an interaction model for the results corresponding to Figure 5C, which again I appreciate. I am however confused that the model predictions shown in Figure 5C do not seem to have changed at all (the figures in the old and new version of the manuscript seem identical). This makes me wonder what is shown in the figure. The figure should optimally show the mean and 95% confidence interval of the mean that comes directly from the interaction model, which should also be explained in the figure caption. I would encourage the authors to show the model predictions from the interaction model (and not a separate fit from geom_smooth or someg alike), if that is not yet done, since this allows the reader to also evaluate how well the model fits the data.