// Cudeck, R. & Browne, M. W. (1992). Constructing a 
// covariance matrix that yields a specified minimizer 
// and a specified minimum discrepancy function value. 
// Psychometrika, 57, 357-369



p = 4;  let th = 8 5    7 2 4   48 50 9 12;
delta = 0.5; 
Ftype = "OLS" ; 
Ftype = "ML";



proc model (th);
  local lam, phi, psi, sig;
    lam = (th[1] | th[1] | 0 | 0) ~ ( 0 | 0 | th[2] | th[2]); 
    phi = th[3:4] ~ th[4:5];
    psi = eye(4) .* th[6:9];
    sig = lam * phi * lam' + psi;
  retp(sig);
endp;


proc makederiv(th);
  local sig, dv, np, eps, h;  
    sig = model(th);
    np=1; dv={};
    do while np <= rows(th); 
       h      = th[np];
       eps    = 0.00001 .* h;
       th[np] = h + eps;
       dv     = dv ~ vech( (model(th) - sig) ./ eps );
       th[np] = h;
       np     = np + 1;
    endo;
  retp(dv);
endp;


sig   = model(th);
sigiv = inv(sig);
cls; ""; 
format 5,0; ""; "Number of variables                        =" p; 
format 9,3;     
if Ftype $== "ML"; 
   "Target  ML discrepancy funct value (delta) =" delta;
elseif Ftype $== "OLS";
    "Target OLS discrepancy funct value (delta) =" delta;
endif;
"Parameters"; th';
""; ""; "Sigma for true model" sig;
" -------------------------------------------------------------";


pstar = 0.5 .* p .* (p + 1);
M  = 2.* ones(p,p) - eye(p); 
Dk = eye(pstar) .* vech(M);  

deriv = makederiv(th);

if Ftype $== "ML"; 
   W = sigiv; 
elseif Ftype $== "OLS";
   W = eye(rows(sig));
endif;

B={};  np=1; 
do while np <= rows(th);
  B  = B ~ ( -Dk * vech( W * xpnd(deriv[.,np]) * W )  );
  np = np + 1;
endo;

// Initial random vector
   y = rndu(pstar,1);
   v = olsqr( y, B ); 

// Solve for initial discrepancy, sigma + Etil 
   etilde = y - B * v; 
   Etil   = xpnd(etilde); 

// Initial sigma_star
   sigstar = sig + Etil; 
   format 12,4;
   ""; "Initial Random Matrix (E-tilde)" Etil;
   ""; "Initial Sigma_star (Sigma + E-tilde)" sigstar;
   

   G  = W * Etil;
   ID = eye(rows(G));
   kappa = sqrt (2 .* delta ./ sumc(diag(G*G))); 

   if Ftype $== "OLS";
      ""; "Kappa =" kappa;
      goto skip;
   endif;


format 16,8; ""; "";
dif   = kappa .* sumc(diag(G)) - ln(det(ID + kappa .* G));
"  Iter      M.L. Funct       Difference         Kappa";
"    0 " dif;; (dif-delta);; kappa;

loop = 1;
do while loop <= 15;
   tk      = dif - delta;
   tkprime = sumc(diag(G)) - sumc(diag(inv(ID + kappa .* G) * G));
   kappan  = kappa - tk ./ tkprime;
   dif     = kappan .* sumc(diag(G)) - ln(det(ID + kappan .* G));

   format 5,0; loop;; format 16,8; dif;; tk;; kappan;
   kappa  = kappan;

   if tk .lt 1e-5; goto skip; endif;
   loop   = loop + 1;
endo;

skip:

sigstar = sig + kappa .* Etil; 
format 12,4; ""; ""; "Final Difference Matrix (E = kappa * E-tilde)" (kappa .* Etil);
                 ""; "Sigma_star = Sigma + E" sigstar;

""; "det(sigstar)  =  " det(sigstar);
if Ftype $== "ML";
  Fml = ln(det(sig)) - ln(det(sigstar)) + sumc(diag(sigstar*sigiv)) - p;
  ""; ""; format 10,4; "Fml(Sigma,Sigma_star) =" Fml; 
elseif Ftype $== "OLS";
   D    = sigstar - sig;
   Fols = 0.5 .* sumc(diag( D * D ));
   ""; ""; format 10,4; "Fols(Sigma,Sigma_star) =" Fols; 
endif;
 