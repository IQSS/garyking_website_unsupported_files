/*
**  y = sechol(A);
**  by Jeff Gill, April 2002.
**
**  This procedure almost always allows the cholesky decomposition 
**  of a matrix A; it produces:
**
**  y = chol(A+E), where E is a diagonal matrix with each element as small
**  as possible, and A and E are the same size.  E diagonal values are 
**  constrained by iteravely updated Gerschgorin bounds.  
**
**  REFERENCES:
**
**  Gill, Jeff and Gary King. ``What to do When Your Hessian is Not
**  Invertible: Alternatives to Model Respecification in Nonlinear
**  Estimation,'' Sociological Methods and Research, Vol. 32, No. 1
**  (2004): Pp. 54--87, http://gking.harvard.edu/files/abs/help-abs.shtml.
**
**  This code is also available in R format
**
*/
proc sechol(A);
   local i,j,k,m,n,gamm,tau,delta,deltaprev,sum1,sum2,eigvals,dlist,dmax,
	 P,Ptemp,Pprod,L,norm_A,normj,g,gmax;
   n = rows(A);
   m = cols(A);
   L = zeros(n,n);
   deltaprev=0;
   gamm = maxc(abs(diag(A))); 
   tau = __macheps^(1/3);
   if  minc(eig(A)') > 0;
      /* print("not pivoting"); */
      tau = -1000000;
   endif;
   if m ne n;
      print("sechol: input matrix must be square");
      retp(A);
   endif;

   norm_A = maxc(sumc(abs(A)));
   gamm = maxc(abs(diag(A))); 
   delta = maxc(maxc(__macheps*norm_A~__macheps));
   Pprod = eye(n);
  
   if n > 2; 
      for k (1,(n-2),1);
         trap 1,1; 
         if (minc((diag(A[(k+1):n,(k+1):n])' - A[k,(k+1):n]^2/A[k,k])') < tau*gamm
		and minc(eig(A[(k+1):n,(k+1):n])) < 0) or (A[1,1] < 0); 
            dmax = maxindc(diag(A[k:n,k:n]));
            if (A[(k+dmax-1),(k+dmax-1)] > A[k,k]);
	       /* print("pivot using dmax:"); print(dmax); */
               P = eye(n);
               Ptemp = P[k,.]; P[k,.] = P[(k+dmax-1),.]; P[(k+dmax-1),.] = Ptemp;
               A = P*A*P;
               L = P*L*P;
               Pprod = P*Pprod;
            endif;
            g = zeros(n-(k-1),1);
            for i ((k), (n), 1);  
               if i == 1;
	          sum1 = 0;
               else;
	          sum1 = sumc(abs(A[i,k:(i-1)])');
	       endif;
               if i == n;
	          sum2 = 0;
               else; 
	          sum2 = sumc(abs(A[(i+1):n,i]));
	       endif; 
               g[i-(k-1)] = A[i,i] - sum1 - sum2;
            endfor; 
            gmax = maxindc(g);
            if gmax /= k;
	       /* print("gerschgorin pivot on cycle:"); print(k); */
               P = eye(n);
               Ptemp = P[k,.]; P[k,.] = P[(k+dmax-1),.]; P[(k+dmax-1),.] = Ptemp;
               A = P*A*P;
               L = P*L*P;
               Pprod = P*Pprod;
            endif; 
            normj = sumc(abs(A[(k+1):n,k]));
	    delta = maxc((0~deltaprev~-A[k,k]+(maxc(normj~tau*gamm))')');
            if delta > 0;
               A[k,k] = A[k,k] + delta;
               deltaprev = delta;
            endif;
         endif; 
         A[k,k] = sqrt(A[k,k]);
         L[k,k] = A[k,k]; 
         for i ((k+1), (n), 1); 
            if L[k,k] > __macheps; A[i,k] = A[i,k]/L[k,k]; endif;
   	    if isinf(A[i,k]); A[i,k] = 0; endif;
	    L[i,k] = A[i,k];
            A[i,(k+1):i] = A[i,(k+1):i] - L[i,k]*L[(k+1):i,k]';
            if A[i,i] < 0; A[i,i] = 0; endif;
         endfor;
     endfor;
   endif;
   A[(n-1),n] = A[n,(n-1)];
   eigvals = eig(A[(n-1):n,(n-1):n]);
   dlist = ( (0|deltaprev|-minc(eigvals)+tau*maxc( (1/(1-tau))*(maxc(eigvals)-minc(eigvals))|gamm)) );
   if dlist[1] > dlist[2]; 
      delta = dlist[1];   
   else;
      delta = dlist[2];
   endif;
   if delta < dlist[3];
      delta = dlist[3];
   endif;
   if delta > 0;
      A[(n-1),(n-1)] = A[(n-1),(n-1)] + delta;
      A[n,n] = A[n,n] + delta;
      deltaprev = delta;
   endif;
   A[(n-1),(n-1)] = sqrt(A[(n-1),(n-1)]);
   L[(n-1),(n-1)] = A[(n-1),(n-1)];
   A[n,(n-1)] = A[n,(n-1)]/L[(n-1),(n-1)];
   if isinf(A[n,(n-1)]); A[n,(n-1)] = 0; endif;
   L[n,(n-1)] = A[n,(n-1)];
   A[n,n] = sqrt(A[n,n] - L[n,(n-1)]^2);
   L[n,n] = A[n,n];
   retp(Pprod'*L'*Pprod');
endp;

/* infinity checking function from Gauss manual, page 72. */
proc isinf(x);
    local plus, minus;
    plus = 0v7ff0000000000000;
    minus = 0vfff0000000000000;
    retp(not x /= plus or not x /= minus);
endp;
