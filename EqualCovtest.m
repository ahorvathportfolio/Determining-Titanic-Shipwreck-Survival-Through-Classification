% This file can also be found in another project of mine using Principal
% Component Analysis and was provided by my instructor for the course,
% Dr. Ellingson.

function [C, Large_crit, Perm_crit]=EqualCovtest(X1,X2,alpha,B)

[n1,p]=size(X1);
[n2,p]=size(X2);

% Calculate Test Statistic
S1=cov(X1);
S2=cov(X2);

Sp=(n1-1)/(n1+n2-2)*S1+(n2-1)/(n1+n2-2)*S2;

%Lam=(det(S1)/det(Sp))^((n1-1)/2)*(det(S2)/det(Sp))^((n2-1)/2);
%M=-2*log(Lam);

M=(n1-1)/2*(log(det(S1))-log(det(Sp)))+(n2-1)/2*(log(det(S2))-log(det(Sp)));

% Calculate Large Sample Critical Value
% g is the number of groups
% Box's Test for Equality of Covariance Matrices:
u1=1/(n1-1)+1/(n2-1)- 1/(n1-1+n2-1);
u2=(2*p^2+3*p-1)/(6*(p+1)*(2-1));
u=u1*u2;

C=(1-u)*-2*M;

nu=.5*p*(p+1)*(2-1); % degrees of freedom for chi squared

Large_crit=chi2inv(0.95,nu);

% At significance level alpha, reject Ho if C > chisquared (p(p+1)(g-1)/2  (alpha)

% Obtain Permutation Critical Value
D1=X1-mean(X1);
D2=X2-mean(X2);

D=[D1;D2];
Cp=zeros(B,1);

for ind=1:B
    permind=randperm(n1+n2);

    Dp1=D(permind(1:n1),:);
    Dp2=D(permind((n1+1):(n1+n2)),:);
    
    Sp1=cov(Dp1);
    Sp2=cov(Dp2);
    
    Spp=(n1-1)/(n1+n2-2)*Sp1+(n2-1)/(n1+n2-2)*Sp2;
    
    %Lamp=(det(Sp1)/det(Spp))^((n1-1)/2)*(det(Sp2)/det(Spp))^((n2-1)/2);
    Mp=(n1-1)/2*(log(det(Sp1))-log(det(Spp)))+(n2-1)/2*(log(det(Sp2))-log(det(Spp)));

    Cp(ind)=-2*Mp*(1-u);
end

Perm_crit=quantile(Cp,1-alpha);



