
m = 10000;
MAXJUMPS = 30;

numberjumpsV2 = zeros(1,m);
jumpsV2 = zeros(m,MAXJUMPS);
valuesV2 = zeros(m,MAXJUMPS+1);

numberjumpsV2rev = zeros(1,m);
jumpsV2rev = zeros(m,MAXJUMPS);
valuesV2rev = zeros(m,MAXJUMPS+1);

numberjumpsV5 = zeros(1,m);
jumpsV5 = zeros(m,MAXJUMPS);
valuesV5 = zeros(m,MAXJUMPS+1);

numberjumpsV5rev = zeros(1,m);
jumpsV5rev = zeros(m,MAXJUMPS);
valuesV5rev = zeros(m,MAXJUMPS+1);

Data = load(['Data.mat'], 'y').y;
DataRev = load(['DataRev.mat'], 'y').y;


for i = 1:m 
disp(i)

n=numel(Data(i,:));
infos.delta=1;% (Minimal number of design points between two breakpoints)-1
V=2;% How many folds for ERMVF and LooVF ?
Dimmax=floor(0.9*(n-n/V)/(1+infos.delta));% Maximal dimension considered for all procedures (default choice: our advice for LooVF or ERMVF)
infos.threshold=floor(0.75*Dimmax);% value of the dimension threshold used in ERMBMthr and PML for the calibration of the penalty 

[D_LOOVF, mu_LOOVF, rupt_LOOVF, crit2VF_1LOO, rupt_1LOO, mu_1LOO]=proc_LOOVF(Data(i,:), V, Dimmax, infos);

numberjumpsV2(1,i) = D_LOOVF-1;
jumpsV2(i,1:(min(length(rupt_LOOVF)-1,MAXJUMPS))) = rupt_LOOVF(1:(min(length(rupt_LOOVF)-1,MAXJUMPS)));
valuesV2(i,1:D_LOOVF) = mu_LOOVF(rupt_LOOVF);

V=5;% How many folds for ERMVF and LooVF ?
Dimmax=floor(0.9*(n-n/V)/(1+infos.delta));% Maximal dimension considered for all procedures (default choice: our advice for LooVF or ERMVF)
infos.threshold=floor(0.75*Dimmax);% value of the dimension threshold used in ERMBMthr and PML for the calibration of the penalty 

[D_LOOVF, mu_LOOVF, rupt_LOOVF, crit2VF_1LOO, rupt_1LOO, mu_1LOO]=proc_LOOVF(Data(i,:), V, Dimmax, infos);

numberjumpsV5(1,i) = D_LOOVF-1;
jumpsV5(i,1:(min(length(rupt_LOOVF)-1,MAXJUMPS))) = rupt_LOOVF(1:(min(length(rupt_LOOVF)-1,MAXJUMPS)));
valuesV5(i,1:D_LOOVF) = mu_LOOVF(rupt_LOOVF);


infos.delta=1;% (Minimal number of design points between two breakpoints)-1
V=2;% How many folds for ERMVF and LooVF ?
Dimmax=floor(0.9*(n-n/V)/(1+infos.delta));% Maximal dimension considered for all procedures (default choice: our advice for LooVF or ERMVF)
infos.threshold=floor(0.75*Dimmax);% value of the dimension threshold used in ERMBMthr and PML for the calibration of the penalty 

[D_LOOVF, mu_LOOVF, rupt_LOOVF, crit2VF_1LOO, rupt_1LOO, mu_1LOO]=proc_LOOVF(DataRev(i,:), V, Dimmax, infos);

numberjumpsV2rev(1,i) = D_LOOVF-1;
jumpsV2rev(i,1:(min(length(rupt_LOOVF)-1,MAXJUMPS))) = rupt_LOOVF(1:(min(length(rupt_LOOVF)-1,MAXJUMPS)));
valuesV2rev(i,1:D_LOOVF) = mu_LOOVF(rupt_LOOVF);

V=5;% How many folds for ERMVF and LooVF ?
Dimmax=floor(0.9*(n-n/V)/(1+infos.delta));% Maximal dimension considered for all procedures (default choice: our advice for LooVF or ERMVF)
infos.threshold=floor(0.75*Dimmax);% value of the dimension threshold used in ERMBMthr and PML for the calibration of the penalty 

[D_LOOVF, mu_LOOVF, rupt_LOOVF, crit2VF_1LOO, rupt_1LOO, mu_1LOO]=proc_LOOVF(DataRev(i,:), V, Dimmax, infos);

numberjumpsV5rev(1,i) = D_LOOVF-1;
jumpsV5rev(i,1:(min(length(rupt_LOOVF)-1,MAXJUMPS))) = rupt_LOOVF(1:(min(length(rupt_LOOVF)-1,MAXJUMPS)));
valuesV5rev(i,1:D_LOOVF) = mu_LOOVF(rupt_LOOVF);

end

save numberV2.mat numberjumpsV2
save jumpsV2.mat jumpsV2
save valuesV2.mat valuesV2

save numberV2rev.mat numberjumpsV2rev
save jumpsV2rev.mat jumpsV2rev
save valuesV2rev.mat valuesV2rev

save numberV5.mat numberjumpsV5
save jumpsV5.mat jumpsV5
save valuesV5.mat valuesV5

save numberV5rev.mat numberjumpsV5rev
save jumpsV5rev.mat jumpsV5rev
save valuesV5rev.mat valuesV5rev

