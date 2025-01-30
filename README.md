# Additive Manufacturing Experimental Design and Statistics Literature Review

&nbsp;Additive manufacturing is an emerging field of engineering in which a material is applied to a 3-dimensional part in a sequential step by step and layer by layer procedure until the final part is complete. Laser-based powder bed fusion is a relatively novel form of additive manufacturing where the underlying substrate is a metallic powder to be melted into its final form. All additive manufacturing processes - and laser-based powder bed fusion in particular - are complicated and intricate, balancing dozens of environmental, operator, and machine-based factors to output a final object. Finding this balance requires careful experimental design, especially since sample sizes can be limited by the extensive monetary and temporal cost of each print. The field of statistical experimentation is rich, offering many standard practices which help avoid bias while minimizing sample size. We therefore performed a literature review to determine which of these practices are currently being used in the additive manufacturing field. 
   To perform this review, we created a list of additive manufacturing and laser-based powder bed fusion manuscripts using the SCOPUS search engine, and then randomly selected 10 papers each year from 2016 – 2024. We then evaluated each paper to determine what type of experimental designs were used, which experimental methods were used to eliminate noise and bias (such as blocking), and what kind of analysis was performed on the final dataset. We then investigate how changes in methodology progress over time and we perform statistical analyses to see which types of practices /designs/analyses were used the most often.  
   We find that while some basic designs are commonly used in additive manufacturing generally and laser-based powder bed fusion specifically, more sophisticated and efficient designs are ignored, good practices for experimental design are either not being used or are not being reported, and that the resulting data is not being analyzed well. For instance, most papers merely provided summary statistics of the results rather than performing hypothesis testing. Additionally, one of the few sophisticated classes of designs which are being used – Taguchi designs – are being used in the incorrect context. This state of affairs is also not changing with time. 
   We therefore provide a roadmap for experimentalists, showing the steps by which an experiment should be designed, implemented, and analyzed. These steps range from the pre-experimental phase, to experimental design and sampling, to statistical model fitting, selection, and adequacy checking, to data transformation, and then finally sequential experimentation. We also provide rationale for every step, providing resources that should help readers understand related topics. Finally, we discuss how the design and results should be reported so that the experiment can be reproduced by other labs and confidence in the results fairly assessed.  
   This repository contains the code and data needed to reproduce our analyses. The 
