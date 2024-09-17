% Mardia's Test for Titanic Dataset - Horvath

% Import the DatasetM file as a numeric matrix
alpha = 0.05
% Mardia's Test for MVN
[H stats] = mardiatest(DatasetM,alpha)

%     Hs: 1
%     Ps: 0
%     Ms: 7.3089e+03
%    CVs: 146.5674
%    Hsc: 1
%    Psc: 0
%    Msc: 7.3466e+03
%     Hk: 1
%     Pk: 0
%     Mk: 76.6801
%    CVk: 1.6449

% The Marida's test statistic for multivariate skewness (corrected for
% small samples) is 7.3466e+03 and the Mardia's test statistic for
% multivariate kurtosis is 76.6801. The corresponding p-values are given to
% be approcimately 0. Therefore, we reject Ho : X ~ MVN.


% Survived vs Did Not Survive
% Import X1 and X2 as numeric matrices
alpha = 0.05
B = 1000
[C, Large_crit, Perm_crit]=EqualCovtest(X1,X2,alpha,B)

% According to the textbook on page 311, Box's chi squared approximation
% works well if nl (sample size of the l-th population) exceeds 20 and if p
% and g do not exceed 5.

% Here our sample sizes are n1 = 288, n2 = 50, p = 7, and g = 2 groups total.
% So the only issue is that p is 7, not 5 or less. Still, it's pretty
% close.


%C =

%  486.9841


%Large_crit =

%   41.3371


%Perm_crit =

%  140.8244 this will change a little every time we run it

% Using Box's Test for Equality of Covariance Matrices, at significance 
% level alpha, reject Ho if C > chisquared (p(p+1)(g-1)/2  (alpha)

% C > Large_crit and perm_crit here. So reject Ho. The covariance matrix structures are
% different.
