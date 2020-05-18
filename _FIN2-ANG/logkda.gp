
 logKDAvec(abund) =
 {
 local(S,J,n,m,k,k0,k1,k2,Told,Tnew,specabund,i,j,Sdiff,cnt1,cnt2);
 abund = vecsort(abund);
 S = length(abund);
 J = sum(k = 1,S,abund[k]);
 maxabund = abund[S];
 Sdiff = 1; for(i = 2,S,if(abund[i] != abund[i - 1],Sdiff++));
 specabund = matrix(2,Sdiff,i,j,0); specabund[1,1] = abund[1]; specabund[2,1] = 1;
 cnt1 = 1; cnt2 = 1;
 for(i = 2,S,
    if(abund[i] != abund[i - 1],
        cnt1++;
        cnt2 = 1;
        specabund[1,cnt1] = abund[i];
        specabund[2,cnt1] = cnt2
    ,
        cnt2++;
        specabund[2,cnt1] = cnt2
    )
 );

 polyn = vector(1,i,1);
 i = 1;
 if(specabund[1,i] == 1,i++);
 Told = vector(1,i,1);
 for(n = 2,maxabund,
    Tnew = vector(n,m,(n > m) * Told[min(n-1,m)] + Told[max(1,m - 1)] * (m - 1)/(n - 1) + 0. );
    if(n == specabund[1,i],
        for(k0 = 1,specabund[2,i],
           lenpolyn2 = length(polyn) + length(Tnew) - 1;
           polyn = vector(lenpolyn2,k1,sum(k2 = max(1,k1 + 1 - length(Tnew)),min(length(polyn),k1),polyn[k2] * Tnew[k1 + 1 - k2]));

        );
        i++;
    );
    Told = vector(n,m,Tnew[m]);
 );
 logKDA = log(polyn);

 logKDA
 }
  print(logKDAvec([ 504,431,420,365,303,291,280,276,262,254,210,180,164,162,156,152,152,141,140,137,135,132,130,124,124,120,119,111,110,106,106,106,100,100,100,96,92,91,88,86,86,82,82,82,76,74,73,72,70,69,67,66,65,63,62,60,60,58,58,55,54,53,51,51,48,48,46,46,45,42,39,38,37,37,34,33,31,28,27,26,25,25,24,23,23,22,21,19,19,19,18,17,17,17,17,17,16,16,15,14,14,14,14,13,13,13,12,11,10,9,9,9,8,8,8,7,7,7,6,6,6,5,5,5,4,4,4,4,3,3,3,3,2,2,2,2,2,2,1,1,1,1,1,1,1 ]))