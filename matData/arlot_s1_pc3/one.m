
load Data.mat

n=numel(y);



%%% Parameters of the procedures
infos.delta=1;% (Minimal number of design points between two breakpoints)-1
V=5;% How many folds for ERMVF and LooVF ?
Dimmax=floor(0.9*(n-n/V)/(1+infos.delta));% Maximal dimension considered for all procedures (default choice: our advice for LooVF or ERMVF)
infos.threshold=floor(0.75*Dimmax);% value of the dimension threshold used in ERMBMthr and PML for the calibration of the penalty 

[D_LOOVF, mu_LOOVF, rupt_LOOVF, crit2VF_1LOO, rupt_1LOO, mu_1LOO]=proc_LOOVF(y, V, Dimmax, infos);

mu = mu_LOOVF;

save est.mat mu
