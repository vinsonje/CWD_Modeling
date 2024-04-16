// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;
using namespace RcppParallel;
using namespace arma;

//Inner FOI loop configuration begin
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////


/////////////////////////////////////////////////////
///////Set up wrapper to run with RcppParallel//////
///////////////////////////////////////////////////

//this wrapper makes the function available to RcppParallel sort of
//the function that you want made parallel goes here in void operator
//struct SquareRoot : public Worker
struct FOILoop : public Worker {
  // source matrix
  //const RMatrix<double> input;
  //needed inputs:Imat, Pmat, cent, pdI_1, B_I, W_I, B2, F1
  const RMatrix<double> Imat;
  const RMatrix<double> Pmat;
  const RMatrix<double> cent;
  const RMatrix<double> pdI_1;
  const RMatrix<double> pdP_1;
  const RMatrix<double> B_I;
  const RMatrix<double> W_I;
  const RMatrix<double> B_P;
  const RMatrix<double> W_P;
  const double B1;
  const double B2;
  const double F1;
  const double F2_int;
  const double F2_B;
  const double F1P;
  const double F2P_int;
  const double F2P_B;
  
  // destination matrix
  //RMatrix<double> output;
  RMatrix<double> B_tot;
  
  // initialize with source and destination
  //kind of like calling the function with a header, but with slightly different format
  //SquareRoot(const NumericMatrix input, NumericMatrix output) 
  //  : input(input), output(output) {}
  
  //needed inputs:Imat, Pmat, cent, pdI_1, pdC_1, B_I, W_I, B2, F1
  FOILoop(const NumericMatrix& Imat,
          const NumericMatrix& Pmat,
          const NumericMatrix& cent,
          const NumericMatrix& pdI_1,
          const NumericMatrix& pdP_1,
          const NumericMatrix& B_I,
          const NumericMatrix& W_I,
          const NumericMatrix& B_P,
          const NumericMatrix& W_P,
          const double B1,
          const double B2,
          const double F1,
          const double F2_int,
          const double F2_B,
          const double F1P,
          const double F2P_int,
          const double F2P_B,
          NumericMatrix B_tot) 
    : Imat(Imat), 
      Pmat(Pmat),
      cent(cent), 
      pdI_1(pdI_1),
      pdP_1(pdP_1),
      B_I(B_I),
      W_I(W_I),
      B_P(B_P),
      W_P(W_P),
      B1(B1),
      B2(B2),
      F1(F1),
      F2_int(F2_int),
      F2_B(F2_B),
      F1P(F1P),
      F2P_int(F2P_int),
      F2P_B(F2P_B),
      B_tot(B_tot) {}
  
  //needed inputs: Imat, Pmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
  arma::mat convertImat()
  {
    RMatrix<double> tmp_mat = Imat;
    const arma::mat Imat2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return Imat2;
  }
  
  arma::mat convertPmat()
  {
    RMatrix<double> tmp_mat = Pmat;
    const arma::mat Pmat2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return Pmat2;
  }
  
  arma::mat convertcent()
  {
    RMatrix<double> tmp_mat = cent;
    const arma::mat cent2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return cent2;
  }
  
  arma::mat convertpdI_1()
  {
    RMatrix<double> tmp_mat = pdI_1;
    const arma::mat pdI_1_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return pdI_1_2;
  }
  
  //you need a convertPdC_1 function here. 
  
  arma::mat convertB_I()
  {
    RMatrix<double> tmp_mat = B_I;
    const arma::mat B_I_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return B_I_2;
  }
  
  arma::mat convertW_I()
  {
    RMatrix<double> tmp_mat = W_I;
    const arma::mat W_I_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return W_I_2;
  }
  
  arma::mat convertpdP_1()
  {
    RMatrix<double> tmp_mat = pdP_1;
    const arma::mat pdP_1_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return pdP_1_2;
  }

  arma::mat convertB_P()
  {
    RMatrix<double> tmp_mat = B_P;
    const arma::mat B_P_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return B_P_2;
  }
  
  arma::mat convertW_P()
  {
    RMatrix<double> tmp_mat = W_P;
    const arma::mat W_P_2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return W_P_2;
  }
  

  
  /////////////////////////////////////////////////
  ///////Inner FOI function loop begins here//////
  ///////////////////////////////////////////////
  
  void operator()(std::size_t begin, std::size_t end) {
    // rows we will operate on
    //needed inputs:Imat, Pmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
    arma::mat Imat3 = convertImat();
    arma::mat Pmat3 = convertPmat();
    arma::mat cent3 = convertcent();
    arma::mat pdI_1_3 = convertpdI_1();
    arma::mat B_I_3 = convertB_I();
    arma::mat W_I_3 = convertW_I();
    arma::mat pdP_1_3 = convertpdP_1();
    arma::mat B_P_3 = convertB_P();
    arma::mat W_P_3 = convertW_P();
    
    //needed inputs:Imat, cent, pdI_1, B_I, W_I, B2, F1
    //needed output: arma::mat Btot(cells,2)
    for(std::size_t j = begin; j < end; j++) {
      
      //Rcout << "infected ind. j=" << j << "\n";
      
      //if any infected rows in Imat
      if(Imat3.n_rows > 0){
        for(std::size_t i=0; i < Imat3.n_rows; ++i) {
          
          //Rcout << "infected ind. i=" << i << "\n";
          //get distance between infected cells, and all other cells
          pdI_1_3(i,j)=sqrt(pow((cent3(j,0)-Imat3(i,4)),2)+pow((cent3(j,1)-Imat3(i,5)),2));
          
          //using distance to get probability of contact for cell j
          //B_I_3(j,i)=(exp(F2_int+F2_B*pdI_1_3(i,j)))*B1;
          B_I_3(j,i)=(1/(1+exp(-1*(F2_int+F2_B*pdI_1_3(i,j)))))*B1;
          //1/(1+exp(-(F2_int+F2_B*0.4)))
          
          
          if(pdI_1_3(i,j)==0){
            W_I_3(j,i)=F1;
            B_I_3(j,i)=0;
          } else{
            W_I_3(j,i)=0;
          }
          
          //finished prob of contacts for cell j for every infected individual
          
        }
        
        //B_tot is the sum of all contact rates for each cell, nrow=centroids
        //get sum of contact rates for cell j
        B_tot(j,0)=sum(B_I_3.row(j))+sum(W_I_3.row(j));
        
      } //if num_I closing loop
      
      //same as above for prions
      if(Pmat3.n_rows > 0){
        
        //Rcout << "prions j=" << j << "\n";
        
        for(std::size_t k=0; k < Pmat3.n_rows; ++k) {
          
          //Rcout << "prions i=" << i << "\n";
          
          pdP_1_3(k,j)=sqrt(pow((cent3(j,0)-Pmat3(k,0)),2)+pow((cent3(j,1)-Pmat3(k,1)),2)); //the prions matrix should be a subset of the landscape.prions dataframe
          
          B_P_3(j,k)=(exp(F2P_int+F2P_B*pdP_1_3(k,j)))*B2;
          
          if(pdP_1_3(k,j)==0){
            W_P_3(j,k)=F1P;} else{
              W_P_3(j,k)=0;
            }
        }
      
        B_tot(j,1)=sum(B_P_3.row(j))+sum(W_P_3.row(j));
      } //if num_P > 0 closing bracket
      
      //B_tot would be the export
      
    } //worker for loop
    
  } //void operator closing loop
  
}; //worker closing loop



//So, above, Movement_worker derives from RcppParallel::Worker
//this is required for function objects passed to parallelFor
//Now that the worker is described above, can call the worker we defined

////////////////////////////////////////
///////Call inner FOI loop worker//////
//////////////////////////////////////

// [[Rcpp::export]]
NumericMatrix parallelFOI(
    const NumericMatrix& Imat,
    const NumericMatrix& Pmat,
    const NumericMatrix& cent,
    const NumericMatrix& pdI_1,
    const NumericMatrix& pdP_1,
    const NumericMatrix& B_I,
    const NumericMatrix& W_I,
    const NumericMatrix& B_P,
    const NumericMatrix& W_P,
    const double B1,
    const double B2,
    const double F1,
    const double F2_int,
    const double F2_B, 
    const double F1P,
    const double F2P_int,
    const double F2P_B
){
  
  // allocate the output matrix
  //essentially just defining the size of the output
  NumericMatrix B_tot(cent.nrow(), 2);
  
  
  //output is output matrix defined above
  FOILoop foiloop(Imat,Pmat,cent,pdI_1,pdP_1,B_I,W_I,B_P,W_P,B1,B2,F1,F2_int,F2_B,F1P,F2P_int,F2P_B,B_tot);
  
  
  parallelFor(0,cent.nrow(), foiloop);
  
  // return the output matrix
  return B_tot;
  //}
}

//Inner FOI loop configuration end
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////
///////////////////////////////////////////////

///////////////////////////////////////////
///////Call FOIParallelFull function//////
/////////////////////////////////////////
//this is the function called in R

//[[Rcpp::export]]
//define the main function, FOIParallelFull
arma::mat FOIParallelFullCWD(
    arma::mat pop,
    arma:: mat prions,
    NumericMatrix& cent,
    double cells,
    double B1,
    double B2,
    double F1,
    double F2_int,
    double F2_B,
    double F1P,
    double F2P_int,
    double F2P_B
) {              
  
  //define Pse output
  arma::mat Pse(cells,1,fill::zeros);
  
  //Rcout << Pse;
  
  //find rows with infected pigs
  arma::ivec setmaskI(pop.n_rows);
  for(std::size_t s=0; s < pop.n_rows; ++s) {
    if(pop(s,9)>0) setmaskI[s]=1;
    else setmaskI[s]=0;
  }
  
  //subset pop to get only rows with infected pigs
  arma::mat Imata = pop.rows(find(setmaskI==1));
  
  
  //find rows with prions in them
  arma::ivec setmaskP(pop.n_rows);
  for(std::size_t s=0; s < prions.n_rows; ++s){
    if(prions(s,2)>0) setmaskP[s]=1;
    else setmaskP[s]=0;
  }
  
  //subset pop to get only rows with infected pigs
  arma::mat Pmata = prions.rows(find(setmaskP==1));
  
  //convert class of Imat from arma to NumericMatrix
  //needs to be this way to work in parallel
  Rcpp::NumericMatrix Imat=wrap(Imata);
  Rcpp::NumericMatrix Pmat=wrap(Pmata);
  
  //get Imat/Pmat row numbers
  int num_I = Imat.nrow();
  int num_P = Pmat.nrow();

  //initialize transmission matrices
  Rcpp::NumericMatrix B_I(cells,num_I);
  Rcpp::NumericMatrix B_tot(cells,2);
  Rcpp::NumericMatrix pdI_1(num_I,cells);
  Rcpp::NumericMatrix W_I(cells,num_I);
  Rcpp::NumericMatrix B_P(cells,num_P);
  Rcpp::NumericMatrix pdP_1(num_P,cells);
  Rcpp::NumericMatrix W_P(cells,num_P);
  arma::mat Bsum(cells,1);
  
  //run parallel FOI function (defined above)
  //needed inputs:Imat, Cmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
  B_tot=parallelFOI(Imat,Pmat,cent,pdI_1,pdP_1,B_I,W_I,B_P,W_P,B1,B2,F1,F2_int,F2_B,F1P,F2P_int,F2P_B);
  
  //convert B_tot back to arma::mat
  //is faster this way
  arma::mat B_tota=as<arma::mat>(B_tot);
  
  //combine C/I to get total transmission probabilities
  if(num_P>0&num_I>0){
    Bsum=sum(B_tota,1); //this returns the rowsums of the matrix (why the second element is 1)
  }
  else { if(num_I>0){
    Bsum=B_tota.col(0);
    
  }
  else { if(num_P>0){
    Bsum=B_tota.col(1); 
  }
  }
  }
  
  Pse = 1-exp(-Bsum.col(0));
  
  return Pse;
  
}