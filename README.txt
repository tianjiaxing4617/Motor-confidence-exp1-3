General:  Run the  fitting script.R for the fitting task. Other script file is the analyze and .
           
     
Directory :

    Data: Experienment Data for fitting and Fitting result.

            processed: subject data(s10~s20)
            Cleandata_allblk: cleandata for all block(block2~4(sequence 1),5(sequence 2),6~8(sequence 3)).
            data_for_choice: cleandata of the selection information.(block2~4(sequence 1),5(sequence 2),6~8(sequence 3)).
            dataset_meta:data for meta model fitting.
            dataset_singlemotor:data for single motor memory model fitting.
            meta_model_output:result of meta model fitting.
            singlemotor_model_output:result of single motor memory model fitting.

    Model: Two stan models for fitting:

            Uncertainty meta memory model: The uncertainty model based on the tool estimate from the motor learning.
                  fitting_meta.stan: stan file.
            Single motor memory model: The single motor memory model.
                  fitting_single_motor.stan: model stan file.

    Script: Main script files:
            
            motor meta fitting.R: Fitting script for meta memory model.
            singer motor fitting.R: Fitting script for single motor memory model.
            RL_simulation.R: Fitting script for reforcement learning model.
            plot.figure1.R: Script for figure plotting(d~n in the figure1).
            Pvalue.R: Script for Pvalue analyze.
            supplyment plot: Script for supplyment figure plotting.


            subscript: 
                    
                    set_basic_params: Script for basic parameter setting.
                    Data_organize: Script for cleaning data.
                    BIC_Q_force: Script for calculate the BIC for Q learning.
                    figure1.plotdata_summary: Script for data organizing for figures.
                    plot.function: Function script of plotting.
                    Glmm summary: Function script for glmm analyze.
                    RL.function: Function script for reforcement learning model.
                   
                                        

    figures:  The results of plots.

                    figure1.d~k:
                        combined_rec_plot.R(blk5,blk6~8).
                        barplot motor before selection.R.
                        barplot Qvalue.R.
                        probability select right barplot.R.

                    figure1.i:
                        Logistic_regression_f_blk5.R.
                        Logistic_regression_Q_difference_blk5.R.
                        
                    figure1.n:
                        Logistic_regression_f_blk68.R.
                        Logistic_regression_Q_difference_blk68.R.
                        BIC_plot2.R.

                    figure1.m: BIC_plot1.R.
                    figure1.o: BIC_plot2.R.

                    figure.supplement:
                        single memory model BIC.R.
                        combined_plot(block2~8)_supplement.R.
                        Logistic_regression_confidence_difference.R.
                        Logistic_regression_motormemory.R.
                        Logistic_regression_predicted_probability.R.
                        Logistic_regression_singlemodelmemory.R.





