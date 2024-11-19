
all_subs = c(10:16,18:20)
bound_pf = c(-6.5,-5.75,-5.00,-4.25,-3.5) # Boundary for binning peak force
num_bound_pf = length(bound_pf)

bound_pvel = c(-5,-3.75,-2.5,-1.25,0) # Boundary for binning peak force
num_bound_pvel = length(bound_pvel)

bound_err = c(-10,-5,0,5,10) 
num_bound_err = length(bound_err)

bound_mem = c(0,.1,.2,.3,.4) 
num_bound_mem = length(bound_mem)

cyc_per_phase = 10 # Number of center training cycles per phase (phase changes when the center bval changes)
ct_blk = 5:8 # blocks where center training exists (i.e., not Fam, CT0, or probe)
prb_blk =  NA # c(2,4,6,8) # Probe blocks (to estimate learning and retention rate)
num_boi = length(ct_blk) # number of block of interest
ppb = 3 # phase per block
phase_tag = rep(c(1:ppb),times = num_boi, each = 1) # used for tagging data separated for fitting
blk_tag = rep(ct_blk,times = 1, each = ppb)
cf_bval = c(0,18) # center FF bvalues
num_cf_bval = length(cf_bval)
num_bvalc_ct = 4 # number of bvalues in CT blocks


