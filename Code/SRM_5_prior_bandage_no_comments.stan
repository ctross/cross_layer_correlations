
data{
 int N;                       
 int K;                    
 int P;                      

 int Give[N,N];              
 int Exploit[N,N];            
 int Reduce[N,N];          
 int Selfish[N,N];          
 int Generous[N,N];      

 real bandage_penalty;
}

transformed data{
 int Outcome [N,N,K];

  for(i in 1:N){
      for(j in 1:N){
         Outcome[i,j,1] = Give[i,j];     
         Outcome[i,j,2] = Exploit[i,j];     
         Outcome[i,j,3] = Reduce[i,j];    
         Outcome[i,j,4] = Selfish[i,j];   
         Outcome[i,j,5] = Generous[i,j];  
        }
    }
}


parameters {
 vector[P] B[K];                        

 vector<lower=0>[2*K] FA_SD;            
 cholesky_factor_corr[2*K] FA_L_chol;  
 vector[2*K] FA_raw[N];                

 vector<lower=0>[K] D_SD;              
 cholesky_factor_corr[2*K] D_L_chol;   
 matrix[N,N] D_raw[K];                
}

transformed parameters{
 matrix[2*K,2*K] G_corr; 
 matrix[2*K,2*K] D_corr; 

 G_corr = tcrossprod(FA_L_chol); 
 D_corr = tcrossprod(D_L_chol);  
}


model{
 real FocalFactors;           
 vector[N] TargetFactors;    
 vector[N] DyadFactors;       

 vector[N] F[K];             
 vector[N] A[K];         
 matrix[N,N] D[K];        

 vector[2*K] FA[N];          
 vector[2*K] scrap;        

  for(m in 1:(K-1)){
  for(n in (m+1):K){
  target += normal_lpdf(D_corr[m+K, n+K] | D_corr[m, n],   bandage_penalty);
  target += normal_lpdf(D_corr[m, n+K]   | D_corr[n, m+K], bandage_penalty);
  }}

  for(k in 1:K){
    B[k] ~ normal(0, 2.5);             
   }

  FA_SD ~ exponential(2.5);          
  FA_L_chol ~ lkj_corr_cholesky(2.5);  

  for(i in 1:N){
     FA_raw[i] ~ normal(0,1);          
    }

   for(i in 1:N){
     FA[i] = FA_SD .* (FA_L_chol*FA_raw[i]); 
    }
    
   for(i in 1:N){
      F[1,i] = FA[i,1];
      F[2,i] = FA[i,2]; 
      F[3,i] = FA[i,3]; 
      F[4,i] = FA[i,4]; 
      F[5,i] = FA[i,5]; 

      A[1,i] = FA[i,6];
      A[2,i] = FA[i,7]; 
      A[3,i] = FA[i,8]; 
      A[4,i] = FA[i,9];
      A[5,i] = FA[i,10]; 
    }

   D_SD ~ exponential(2.5);          
   D_L_chol ~ lkj_corr_cholesky(2.5); 

   for(k in 1:K){
     to_vector(D_raw[k]) ~ normal(0,1);
    }


  for(i in 1:(N-1)){
   for(j in (i+1):N){  
      scrap[1] = D_raw[1,i,j];
      scrap[2] = D_raw[2,i,j];
      scrap[3] = D_raw[3,i,j];
      scrap[4] = D_raw[4,i,j];
      scrap[5] = D_raw[5,i,j];

      scrap[6] = D_raw[1,j,i];
      scrap[7] = D_raw[2,j,i];
      scrap[8] = D_raw[3,j,i];
      scrap[9] = D_raw[4,j,i];
      scrap[10] = D_raw[5,j,i];
      
     scrap = append_row(D_SD, D_SD) .* (D_L_chol*scrap);
   
      D[1,i,j] = scrap[1];
      D[2,i,j] = scrap[2];
      D[3,i,j] = scrap[3];
      D[4,i,j] = scrap[4];
      D[5,i,j] = scrap[5];

      D[1,j,i] = scrap[6];
      D[2,j,i] = scrap[7];
      D[3,j,i] = scrap[8]; 
      D[4,j,i] = scrap[9]; 
      D[5,j,i] = scrap[10];              
    }}
    
   for(k in 1:K){
     for(i in 1:N){
          D[k,i,i] = -99999;                                
        }
    }  

 for(i in 1:N){                        
            for(k in 1:K){               
               FocalFactors  = B[k,1] + F[k,i];                      
               TargetFactors = A[k];                   
               DyadFactors   = to_vector(D[k,i]);      

                for(j in 1:N){          
                  if(i != j)        
                   Outcome[i,j,k] ~ bernoulli_logit(FocalFactors + TargetFactors[j] + DyadFactors[j]); 
                 }
                
            }
        
    } 
}

   


