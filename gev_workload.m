% blocksize = 1000;
% nblocks = 250;
% rng default  % For reproducibility
% t = trnd(5,blocksize,nblocks);
% x = max(t); % 250 column maxima
% paramEsts = gevfit(x)
% 
% histogram(x,2:20,'FaceColor',[.8 .8 1]);
% xgrid = linspace(2,20,1000);
% line(xgrid,nblocks*...
%      gevpdf(xgrid,paramEsts(1),paramEsts(2),paramEsts(3)));
%  
 

% Urb
sigma = 0.142;
zeta = 624.640;
mu = 1804.400;

blocksize = 1000;
nblocks = 250;
rng default  % For reproducibility
t = trnd(5,blocksize,nblocks);
x = max(t); % 250 column maxima
paramEsts = gevfit(x);


xgrid = linspace(1800,1900,10);
line(xgrid,nblocks*...
     gevpdf(xgrid,zeta,sigma,mu)); % zeta sigma mu