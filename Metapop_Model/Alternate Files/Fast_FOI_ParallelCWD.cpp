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
  //needed inputs:Imat, cent, pdI_1, B_I, W_I, B2, F1
  const RMatrix<double> Imat;
  const RMatrix<double> cent;
  const RMatrix<double> pdI_1;;
  const RMatrix<double> B_I;
  const RMatrix<double> W_I;
  const double B1;
  const double B2;
  const double F1;
  const double F2_int;
  const double F2_B;
  
  // destination matrix
  //RMatrix<double> output;
  RMatrix<double> B_tot;
  
  // initialize with source and destination
  //kind of like calling the function with a header, but with slightly different format
  //SquareRoot(const NumericMatrix input, NumericMatrix output) 
  //  : input(input), output(output) {}
  
  //needed inputs:Imat, Cmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
  FOILoop(const NumericMatrix& Imat,
          const NumericMatrix& cent,
          const NumericMatrix& pdI_1,
          const NumericMatrix& B_I,
          const NumericMatrix& W_I,
          const double B1,
          const double B2,
          const double F1,
          const double F2_int,
          const double F2_B,
          NumericMatrix B_tot) 
    : Imat(Imat), 
      cent(cent), 
      pdI_1(pdI_1),
      B_I(B_I),
      W_I(W_I),
      B1(B1),
      B2(B2),
      F1(F1),
      F2_int(F2_int),
      F2_B(F2_B),
      B_tot(B_tot) {}
  
  //needed inputs:Imat, Cmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
  arma::mat convertImat()
  {
    RMatrix<double> tmp_mat = Imat;
    const arma::mat Imat2(tmp_mat.begin(), tmp_mat.nrow(), tmp_mat.ncol(), false);
    return Imat2;
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

  
  /////////////////////////////////////////////////
  ///////Inner FOI function loop begins here//////
  ///////////////////////////////////////////////
  
  void operator()(std::size_t begin, std::size_t end) {
    // rows we will operate on
    //needed inputs:Imat, Cmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
    arma::mat Imat3 = convertImat();
    arma::mat cent3 = convertcent();
    arma::mat pdI_1_3 = convertpdI_1();
    arma::mat B_I_3 = convertB_I();
    arma::mat W_I_3 = convertW_I();
    
    //needed inputs:Imat,  cent, pdI_1, B_I, W_I, B2, F1
    //needed output: arma::mat Btot(cells,2)
    for(std::size_t j = begin; j < end; j++) {
      
      //if any infected rows in Imat
      if(Imat3.n_rows > 0){
        for(std::size_t i=0; i < Imat3.n_rows; ++i) {
          
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
    const NumericMatrix& cent,
    const NumericMatrix& pdI_1,
    const NumericMatrix& B_I,
    const NumericMatrix& W_I,
    const double B1,
    const double B2,
    const double F1,
    const double F2_int,
    const double F2_B
){
  
  // allocate the output matrix
  //essentially just defining the size of the output
  NumericMatrix B_tot(cent.nrow(), 2);
  
  
  //output is output matrix defined above
  FOILoop foiloop(Imat,cent,pdI_1,B_I,W_I,B1,B2,F1,F2_int,F2_B,B_tot);
  
  
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
    NumericMatrix& cent,
    double cells,
    double B1,
    double B2,
    double F1,
    double F2_int,
    double F2_B
) {              
  
  //define Pse output
  arma::mat Pse(cells,1,fill::zeros);
  
  //find rows with infected pigs
  arma::ivec setmaskI(pop.n_rows);
  for(std::size_t s=0; s < pop.n_rows; ++s) {
    if(pop(s,9)>0) setmaskI[s]=1;
    else setmaskI[s]=0;
  }
  
  //subset pop to get only rows with infected pigs
  arma::mat Imata = pop.rows(find(setmaskI==1));
  
  
  //convert class of Imat from arma to NumericMatrix
  //needs to be this way to work in parallel
  Rcpp::NumericMatrix Imat=wrap(Imata);
  
  //get Imat/Cmat row numbers
  int num_I = Imat.nrow();

  //initialize transmission matrices
  Rcpp::NumericMatrix B_I(cells,num_I);
  Rcpp::NumericMatrix B_tot(cells,2);
  Rcpp::NumericMatrix pdI_1(num_I,cells);
  Rcpp::NumericMatrix W_I(cells,num_I);
  arma::mat Bsum(cells,1);
  
  //run parallel FOI function (defined above)
  //needed inputs:Imat, Cmat, cent, pdI_1, pdC_1, B_I, B_C, W_I, W_C, B2, F1
  B_tot=parallelFOI(Imat,cent,pdI_1,B_I,W_I,B1,B2,F1,F2_int,F2_B);
  
  //convert B_tot back to arma::mat
  //is faster this way
  arma::mat B_tota=as<arma::mat>(B_tot);
  
  //combine C/I to get total transmission probabilities
  if(num_I>0){
    Bsum=sum(B_tota,1);
  }
  else { if(num_I>0){
    Bsum=B_tota.col(0);
    
  }
  }
  
  Pse = 1-exp(-Bsum.col(0));
  
  return Pse;
  
}