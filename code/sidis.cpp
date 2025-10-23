/*


    sidis.cpp
    
    NLO PhT-integrated SIDIS numerics for A_UT SSA
    October 2025
    D.S.


*/

/* HOW TO RUN THIS CODE:

After installation of LHAPDF6, one has to tell the g++ compiler where to find the 
LHAPDF6 headers and libraries (this in principle should happen automatically if linked properly, but in my case I was having troubles).
For this reason, in the working folder where sidis.cpp lives, we tell the compiler where to find LHAPDF6 with the following command in the terminal:
    
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/diego/LHAPDF-6.5.5/src/.libs

    To be modified in general to: 
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:MODIFYHEREYOURPATH/LHAPDF-X.Y.Z/src/.libs


Then, in principle, one should be able to download and install the PDF/FF sets directly via a terminal LHAPDF6 interface (also didn't manage to get it running...).
Hence, we created a folder called LHAPDF_tables in the working directory where sidis.cpp lives, and manually copied 
the PDF/FF sets there (easily downloadable from the website, it takes 1 min...).
We then tell LHAPDF6 where to find the PDF/FF tables with the command:

    export LHAPDF_DATA_PATH=/home/diego/sidis/sidis-NLO-twist3/code/LHAPDF_tables

    To be modified in general to: 
    LHAPDF_DATA_PATH=MODIFYHEREYOURPATH/sidis-NLO-twist3/code/LHAPDF_tables

In the working directory where sidis.cpp lives, there is a gsl folder which contains the GNU scientific routines (including our vegas monte carlo integrator that we need here).

Once we have done that (might not be necessary for everyone!), we can compile and run the code with:

    g++ sidis.cpp -o sidis `lhapdf-config --cflags --ldflags` -I/MODIFYHEREYOURPATH/sidis-NLO-twist3/code -lgsl -lgslcblas -lm
 
    ./sidis

*/


// Include libraries
#include <vector>
#include <cmath>
#include <algorithm>
#include <functional>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <numeric>
#include "LHAPDF/LHAPDF.h"
#include "LHAPDF/GridPDF.h"
#include <gsl/gsl_math.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_vegas.h>

// Define a namespace, so we don't have to write LHAPDF::member_function() everytime,
// but just member_function()
using namespace LHAPDF;

// Declare some global variables
double Mpi = 0.1396; // GeV, pion mass
int which_pion; // 1 for pi+, 0 for pi0, -1 for pi-. Modified in the main when needed!
double pi = M_PI; // pi =  3.1415...


// Here we define some helper and auxiliary functions. The main program is at the end.

/**
 * @brief Helper function that displays the results of the vegas integration
 * @param title Little text title to identify the integration
 * @param result Value of integral
 * @param error Estimated error on integral
 */
void display_results (const char *title, double result, double error)
{
  printf ("%s ==================\n", title);
  printf ("result = % .10f\n", result);
  printf ("sigma  = % .10f\n", error);
}



/**
 * @brief Returns the weigthed sum over flavors q qbar of f1(x) D1(z), used for unpolarized q2q channel
 * @param x Argument of f1
 * @param z Argument of D1
 * @param mu Factorization scale 
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF
 */
double weighted_sum_f1D1(double x, double z, double mu, const PDF* f1, const PDF* D1){

    double xf1u, xf1ub, xf1d, xf1db, xf1c, xf1cb, xf1s, xf1sb, xf1b, xf1bb;
    double zD1u, zD1ub, zD1d, zD1db, zD1c, zD1cb, zD1s, zD1sb, zD1b, zD1bb;
    double mu2, resultA, resultB, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!

    xf1u = f1->xfxQ2(2,x,mu2);
    xf1ub = f1->xfxQ2(-2,x,mu2);
    xf1d = f1->xfxQ2(1,x,mu2);
    xf1db = f1->xfxQ2(-1,x,mu2);
    xf1c = f1->xfxQ2(4,x,mu2);
    xf1cb = f1->xfxQ2(-4,x,mu2);
    xf1s = f1->xfxQ2(3,x,mu2);
    xf1sb = f1->xfxQ2(-3,x,mu2);
    xf1b = f1->xfxQ2(5,x,mu2);
    xf1bb = f1->xfxQ2(-5,x,mu2);


    zD1u = D1->xfxQ2(2,z,mu2);
    zD1ub = D1->xfxQ2(-2,z,mu2);
    zD1d = D1->xfxQ2(1,z,mu2);
    zD1db = D1->xfxQ2(-1,z,mu2);
    zD1c = D1->xfxQ2(4,z,mu2);
    zD1cb = D1->xfxQ2(-4,z,mu2);
    zD1s = D1->xfxQ2(3,z,mu2);
    zD1sb = D1->xfxQ2(-3,z,mu2);
    zD1b = D1->xfxQ2(5,z,mu2);
    zD1bb = D1->xfxQ2(-5,z,mu2);

    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (xf1u/x)*(zD1u/z)+(xf1ub/x)*(zD1ub/z)+(xf1c/x)*(zD1c/z)+(xf1cb/x)*(zD1cb/z) );
    resultB = ( pow((1./3.),2) )*( (xf1d/x)*(zD1d/z)+ (xf1db/x)*(zD1db/z)+(xf1s/x)*(zD1s/z)+(xf1sb/x)*(zD1sb/z)+(xf1b/x)*(zD1b/z)+(xf1bb/x)*(zD1bb/z) );
    
    return resultA+resultB;

    /* implement here which_pion switches if we take as input the pi+ alone, like in JAM20 FF
    
    if(which_pion == +1){
        // Return weigthed sum
        resultA = ( pow((2./3.),2) )*( (xf1u/x)*(zD1u/z)+(xf1ub/x)*(zD1ub/z)+(xf1c/x)*(zD1c/z)+(xf1cb/x)*(zD1cb/z) );
        resultB = ( pow((1./3.),2) )*( (xf1d/x)*(zD1d/z)+ (xf1db/x)*(zD1db/z)+(xf1s/x)*(zD1s/z)+(xf1sb/x)*(zD1sb/z)+(xf1b/x)*(zD1b/z)+(xf1bb/x)*(zD1bb/z) );
        
        return resultA+resultB;
    }
    else if(which_pion ==-1){
        // Return weigthed sum
        resultA = ( pow((2./3.),2) )*( (xf1u/x)*(zD1ub/z)+(xf1ub/x)*(zD1u/z)+(xf1c/x)*(zD1cb/z)+(xf1cb/x)*(zD1c/z) );
        resultB = ( pow((1./3.),2) )*( (xf1d/x)*(zD1db/z)+ (xf1db/x)*(zD1d/z)+(xf1s/x)*(zD1sb/z)+(xf1sb/x)*(zD1s/z)+(xf1b/x)*(zD1bb/z)+(xf1bb/x)*(zD1b/z) );
        
        return resultA+resultB;
        
    }
    else{ // pi0 not there yet
        std::cout << "which_pion variable not correctly set! Exiting..." << std::endl;
        exit(1);
    }*/
};


/**
 * @brief Returns the [weigthed sum over flavors q qbar of f1(x)] X D1^g(z), used for unpolarized q2g channel
 * @param x Argument of f1
 * @param z Argument of D1
 * @param mu Factorization scale 
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF 
 */
double weighted_sum_f1_times_D1g(double x, double z, double mu, const PDF* f1, const PDF* D1){

    double xf1u, xf1ub, xf1d, xf1db, xf1c, xf1cb, xf1s, xf1sb, xf1b, xf1bb;
    double zD1g;
    double mu2, resultA, resultB, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!

    xf1u = f1->xfxQ2(2,x,mu2);
    xf1ub = f1->xfxQ2(-2,x,mu2);
    xf1d = f1->xfxQ2(1,x,mu2);
    xf1db = f1->xfxQ2(-1,x,mu2);
    xf1c = f1->xfxQ2(4,x,mu2);
    xf1cb = f1->xfxQ2(-4,x,mu2);
    xf1s = f1->xfxQ2(3,x,mu2);
    xf1sb = f1->xfxQ2(-3,x,mu2);
    xf1b = f1->xfxQ2(5,x,mu2);
    xf1bb = f1->xfxQ2(-5,x,mu2);

    zD1g = D1->xfxQ2(21,z,mu2);
    
    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (xf1u/x)+(xf1ub/x)+(xf1c/x)+(xf1cb/x) );
    resultB = ( pow((1./3.),2) )*( (xf1d/x)+ (xf1db/x)+(xf1s/x)+(xf1sb/x)+(xf1b/x)+(xf1bb/x) );
    
    return (resultA+resultB)*zD1g/z;
};



/**
 * @brief Returns the [weigthed sum over flavors q qbar of D1(z)] X f1^g(z), used for unpolarized g2q channel
 * @param x Argument of f1
 * @param z Argument of D1
 * @param mu Factorization scale 
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF 
 */
double weighted_sum_D1_times_f1g(double x, double z, double mu, const PDF* f1, const PDF* D1){

    double xf1g;
    double zD1u, zD1ub, zD1d, zD1db, zD1c, zD1cb, zD1s, zD1sb, zD1b, zD1bb;
    double mu2, resultA, resultB, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!

    xf1g = f1->xfxQ2(21,x,mu2);

    zD1u = D1->xfxQ2(2,z,mu2);
    zD1ub = D1->xfxQ2(-2,z,mu2);
    zD1d = D1->xfxQ2(1,z,mu2);
    zD1db = D1->xfxQ2(-1,z,mu2);
    zD1c = D1->xfxQ2(4,z,mu2);
    zD1cb = D1->xfxQ2(-4,z,mu2);
    zD1s = D1->xfxQ2(3,z,mu2);
    zD1sb = D1->xfxQ2(-3,z,mu2);
    zD1b = D1->xfxQ2(5,z,mu2);
    zD1bb = D1->xfxQ2(-5,z,mu2);

    
    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (zD1u/z)+(zD1ub/z)+(zD1c/z)+(zD1cb/z) );
    resultB = ( pow((1./3.),2) )*( (zD1d/z)+ (zD1db/z)+(zD1s/z)+(zD1sb/z)+(zD1b/z)+(zD1bb/z) );

    return (resultA+resultB)*xf1g/x;

    /* implement here which_pion switches if we take as input the pi+ alone, like in JAM20 FF
    }
    if(which_pion == -1){
        // Return weigthed sum
        resultA = ( pow((2./3.),2) )*( (zD1ub/z)+(zD1u/z)+(zD1cb/z)+(zD1c/z) );
        resultB = ( pow((1./3.),2) )*( (zD1db/z)+ (zD1d/z)+(zD1sb/z)+(zD1s/z)+(zD1bb/z)+(zD1b/z) );
    
        return (resultA+resultB)*xf1g/x;

    }*/
};



/**
 * @brief Returns the weigthed sum over flavors of h1(x) Ht(z)
 * @param x Argument of h1
 * @param z Argument of Ht
 * @param mu Factorization scale 
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 */
double weighted_sum_h1Ht(double x, double z, double mu, const PDF* h1, const PDF* Ht){

    double xh1u, xh1d, zHtu, zHtd,xh1ub,xh1db,zHtub,zHtdb, mu2, result, resultp, resultm ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!
    //WHAT ABOUT ANTIQURKS? Zero since h^qbar = 0
    xh1u = h1->xfxQ2(2,x,mu2);
    //xh1ub = -h1->xfxQ2(1,-x,mu2);
    xh1d = h1->xfxQ2(1,x,mu2);
    //xh1db = -h1->xfxQ2(2,-x,mu2);
    zHtu = Ht->xfxQ2(2,z,mu2);
    zHtub = Ht->xfxQ2(-2,z,mu2);
    zHtd = Ht->xfxQ2(1,z,mu2);
    zHtdb = Ht->xfxQ2(-1,z,mu2);

    // Return weigthed sum

    
    if(which_pion == +1){
            result = ( pow((2./3.),2)  )*(xh1u/x)*(zHtu/z) + ( pow((1./3.),2) )*(xh1d/x)*(zHtd/z);
            return result;
    }
    else if(which_pion == -1){// Again, is this pi- charge conj correct? I don't understand...
            result = ( pow((2./3.),2)  )*(xh1u/x)*(zHtub/z) + ( pow((1./3.),2) )*(xh1d/x)*(zHtdb/z);
            return result;
    }
    else{ // pi0 not there yet
        std::cout << "which_pion variable not correctly set! Exiting..." << std::endl;
        exit(1);
    }
};

/**
 * @brief Returns the weigthed sum over flavors of h1(x) ImH_FU^qg
 * @param x Argument of h1
 * @param z Argument of ImH_FU^qg
 * @param zeta Argument of ImH_FU^qg
 * @param mu Factorization scale 
 * @param au Model parameter for u quark
 * @param ad Model parameter for d quark
 * @param bu Model parameter for u quark
 * @param bd Model parameter for d quark
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 */
double weighted_sum_h1ImHFUqg(double x, double z, double zeta, double mu,double au,double ad,double bu,double bd, const PDF* h1, const PDF* Ht){
      
    double xh1u, xh1d, zHtu, zHtd,xh1ub,xh1db,zHtub,zHtdb, mu2, result ;
    double ImHFUug, ImHFUdg,ImHFUubg, ImHFUdbg;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!
    //WHAT ABOUT ANTIQURKS?
    xh1u = h1->xfxQ2(2,x,mu2);
    //xh1ub = -h1->xfxQ2(1,-x,mu2);
    xh1d = h1->xfxQ2(1,x,mu2);
    //xh1db = -h1->xfxQ2(2,-x,mu2);
    zHtu = Ht->xfxQ2(2,z,mu2);
    zHtub = Ht->xfxQ2(-2,z,mu2);
    zHtd = Ht->xfxQ2(1,z,mu2);
    zHtdb = Ht->xfxQ2(-1,z,mu2);

    
    // Return weigthed sum
    if(which_pion == +1){
        ImHFUug = (zHtu/z)*(1./(2.*z)) * pow(zeta,au) * pow(1.-zeta,bu) * std::tgamma(1.+au+bu)/std::tgamma(1.+au)/std::tgamma(bu);
        ImHFUdg = (zHtd/z)*(1./(2.*z)) * pow(zeta,ad) * pow(1.-zeta,bd) * std::tgamma(1.+ad+bd)/std::tgamma(1.+ad)/std::tgamma(bd);

        result = ( pow((2./3.),2)  )*(xh1u/x)*ImHFUug + ( pow((1./3.),2) )*(xh1d/x)*ImHFUdg;
    }
    else if(which_pion == -1){// again, is this correct here?
        ImHFUubg = (zHtub/z)*(1./(2.*z)) * pow(zeta,au) * pow(1.-zeta,bu) * std::tgamma(1.+au+bu)/std::tgamma(1.+au)/std::tgamma(bu);
        ImHFUdbg = (zHtdb/z)*(1./(2.*z)) * pow(zeta,ad) * pow(1.-zeta,bd) * std::tgamma(1.+ad+bd)/std::tgamma(1.+ad)/std::tgamma(bd);

        result = ( pow((2./3.),2)  )*(xh1u/x)*ImHFUubg + ( pow((1./3.),2) )*(xh1d/x)*ImHFUdbg;
    
    }
    else{
        //pi0 not implemented yet
        std::cout << "which_pion variable not correctly set! Exiting..." << std::endl;
        exit(1);
    }
    return result;
};

/**
 * @brief Returns the weigthed sum over flavors of h1(x) ImH_FU^qbarq
 * @param x Argument of h1
 * @param z Argument of ImH_FU^qbarq
 * @param zeta Argument of ImH_FU^qbarq
 * @param mu Factorization scale 
 * @param cu Model parameter for u quark
 * @param cd Model parameter for d quark
 * @param Nu Model parameter for u quark
 * @param Nd Model parameter for d quark
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 */
double weighted_sum_h1ImHFUqbarq(double x, double z, double zeta, double mu,double cu,double cd,double Nu,double Nd, const PDF* h1, const PDF* Ht){
      
    double xh1u, xh1d, zHtu, zHtd,xh1ub,xh1db,zHtub,zHtdb, mu2, result ;
    double ImHFUubaru, ImHFUdbard;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!
    //WHAT ABOUT ANTIQURKS?
    xh1u = h1->xfxQ2(2,x,mu2);
    //xh1ub = -h1->xfxQ2(1,-x,mu2);
    xh1d = h1->xfxQ2(1,x,mu2);
    //xh1db = -h1->xfxQ2(2,-x,mu2);
    zHtu = Ht->xfxQ2(2,z,mu2);
    zHtub = Ht->xfxQ2(-2,z,mu2);
    zHtd = Ht->xfxQ2(1,z,mu2);
    zHtdb = Ht->xfxQ2(-1,z,mu2);

    if(which_pion == 1){
        ImHFUubaru = (zHtu/z)*(1./(2.*z)) *Nu* pow(zeta,cu) * pow(1.-zeta,cu) * std::tgamma(2.+2.*cu)/std::tgamma(1.+cu)/std::tgamma(1.+cu);
        ImHFUdbard = (zHtd/z)*(1./(2.*z)) *Nd* pow(zeta,cd) * pow(1.-zeta,cd) * std::tgamma(2.+2.*cu)/std::tgamma(1.+cu)/std::tgamma(1.+cu);
        // Return weigthed sum
        result = ( pow((2./3.),2)  )*(xh1u/x)*ImHFUubaru + ( pow((1./3.),2) )*(xh1d/x)*ImHFUdbard;
    }
    else if(which_pion == -1){
        ImHFUubaru = (zHtub/z)*(1./(2.*z)) *Nu* pow(zeta,cu) * pow(1.-zeta,cu) * std::tgamma(2.+2.*cu)/std::tgamma(1.+cu)/std::tgamma(1.+cu);
        ImHFUdbard = (zHtdb/z)*(1./(2.*z)) *Nd* pow(zeta,cd) * pow(1.-zeta,cd) * std::tgamma(2.+2.*cu)/std::tgamma(1.+cu)/std::tgamma(1.+cu);
        // Return weigthed sum
        result = ( pow((2./3.),2)  )*(xh1u/x)*ImHFUubaru + ( pow((1./3.),2) )*(xh1d/x)*ImHFUdbard;
    }
    else{
        //pi0 not implemented yet
        std::cout << "which_pion variable not correctly set! Exiting..." << std::endl;
        exit(1);
    }
    return result;
};

// This struct is to pass multiple parameters to the gsl function for the monte carlo integration.
// It is essentially a way of passing variables to the vegas function 
// without them being arguments of the function to be integrated.
// Basically it serves the same purpose as a common block in fortran, kind of... 
struct vegas_params_unpol {
        double xB, zh, mu, Q;
        const LHAPDF::PDF* f1;
        const LHAPDF::PDF* D1;
};



/**
 * @brief Returns (auxiliary) unpolarized structure function F_UU,T @ NLO, explicit terms implemented
 * @param  X Integration variables, X[0] -> w, X[1] -> v
 * @param dim Dimensions of integral
 * @param params Parameters not to be integrated, passed through a struct
 */
double AuxF_UUT4vegas(double *X, size_t dim, void *params){

    // Here we grab the parameters from the params struct
    vegas_params_unpol* p = static_cast<vegas_params_unpol*>(params);
    double xB = p->xB;
    double zh = p->zh;
    double mu = p->mu;
    double Q = p->Q;
    const PDF* f1 = p->f1;
    const PDF* D1 = p->D1;

    // And the integration variables
    double w = X[0];
    double v = X[1];

    double result ;

    
    // IMPLEMENT HERE NLO
   
    // Color factors
    double CF = 4./3., TF = 1./2. ;
    //double Q;
    //Q = mu;
    double Q2= Q*Q;

    // Strong coupling
    double alphas;
    // Evaluated at mu^2
    alphas = f1->alphasQ2(mu*mu);

    // Calculate soft parts in advance, for all 3 channels
    double softq2q_del1mwdel1mv, softq2q_del1mw, softq2q_del1mv, softq2q;
    double softg2q_del1mwdel1mv, softg2q_del1mw, softg2q_del1mv, softg2q;
    double softq2g_del1mwdel1mv, softq2g_del1mw, softq2g_del1mv, softq2g;

    softq2q_del1mwdel1mv = (xB)* weighted_sum_f1D1(xB,zh,mu,f1,D1);
    softq2q_del1mw = (xB)* weighted_sum_f1D1(xB,zh/v,mu,f1,D1);
    softq2q_del1mv = (xB/w)*  weighted_sum_f1D1(xB/w,zh,mu,f1,D1);
    softq2q = (xB/w)*  weighted_sum_f1D1(xB/w,zh/v,mu,f1,D1);

    softg2q_del1mv = (xB/w)* weighted_sum_D1_times_f1g(xB/w,zh,mu,f1,D1);
    softg2q = (xB/w)* weighted_sum_D1_times_f1g(xB/w,zh/v,mu,f1,D1);

    softq2g_del1mw = (xB)*  weighted_sum_f1_times_D1g(xB,zh/v,mu,f1,D1);
    softq2g = (xB/w)* weighted_sum_f1_times_D1g(xB/w,zh/v,mu,f1,D1);

    // q2q channel
    double resultq2q,resultg2q,resultq2g;
    resultq2q = 0.;
    resultg2q = 0.;
    resultq2g = 0.;

    
    
    resultq2q += softq2q_del1mwdel1mv*(1. + CF * (alphas/2./pi)*(-8. -3. *std::log(mu*mu/Q2) )  ) /(1.-xB)/(1.-zh);
    resultq2q += +CF*(alphas/2./pi)*(1./v)*softq2q_del1mw*(   
              (std::log(1.-v)/(1.-v))*(1.+v*v) +1. - v + 
              (1./(1.-v))*(1.+v*v)*(+std::log(v)-std::log(mu*mu/Q2)) 
                )/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (std::log(1.-v)/(1.-v))*( -2.))/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-0.5*std::log(1.-zh)*std::log(1.-zh))*( -2. ))/(1.-xB)/(1.-zh) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (1./(1.-v))*(-2.)* (-std::log(mu*mu/Q2)))/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-std::log(1.-zh))*(-2. )*(-std::log(mu*mu/Q2)))/(1.-xB)/(1.-zh) ;

   resultq2q +=  +CF*(alphas/2./pi)*(1./w)*softq2q_del1mv*(   
              (std::log(1.-w)/(1.-w))*(1.+w*w) +1. - w + 
              (1./(1.-w))*(1.+w*w)*(-std::log(w)-std::log(mu*mu/Q2)) 
                )/(1.-zh) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (std::log(1.-w)/(1.-w))*( -2. ))/(1.-zh) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-0.5*std::log(1.-xB)*std::log(1.-xB))*( -2. ))/(1.-zh)/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (1./(1.-w))*(-2. )* (-std::log(mu*mu/Q2)))/(1.-zh) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-std::log(1.-xB))*(-2. )* (-std::log(mu*mu/Q2)))/(1.-zh)/(1.-xB) ;

    resultq2q +=  +CF*(alphas/2./pi)*(1./v/w)*softq2q*(  
            (1./(1.-w)/(1.-v))*(2.*v*v*w*w - 2.*v*v*w - 2.*v*w*w + 4.*v*w + v*v + w*w - 2.*v - 2.*w + 2. )) 
            -CF*(alphas/2./pi)*(1./v)*softq2q_del1mw*((1./(1.-w)/(1.-v))*(1. + v*v)) 
            -CF*(alphas/2./pi)*(1./w)*softq2q_del1mv*((1./(1.-w)/(1.-v))*(1. + w*w)) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*((1./(1.-w)/(1.-v))*(2.)) 
            +CF*(alphas/2./pi)*std::log(1. - zh)/(1.-zh)*(1./(1.-w))*
              (softq2q_del1mv*(1. + w*w)/w - softq2q_del1mwdel1mv*(2.)) 
            +CF*(alphas/2./pi)*std::log(1. - xB)/(1.-xB)*(1./(1.-v))*
              (softq2q_del1mw*(1. + v*v)/v - softq2q_del1mwdel1mv*(2.)) 
            +CF*(alphas/2./pi)*std::log(1.-xB)*std::log(1.-zh)*(2.)/(1.-zh)/(1.-xB);
    // g2q channel
    resultg2q += +TF*(alphas/2./pi)*(1./w)*softg2q_del1mv*( (w*w + (1. - w)*(1. - w) )*(std::log((1. - w)/w)-std::log(mu*mu/Q2) ) + 2.*w*(1. - w) )/(1.-zh);
    resultg2q += +TF*(alphas/2./pi)*(1./w/v)*softg2q*(  (w*w + (1. - w)*(1. - w))*(v*v + (1. - v)*(1. - v)))/v/(1-v)
                 -TF*(alphas/2./pi)*(1./w)*softg2q_del1mv*(  (w*w + (1. - w)*(1. - w)))/(1-v)
                 -TF*(alphas/2./pi)*(1./w)*softg2q_del1mv*(  (w*w + (1. - w)*(1. - w)))*(-std::log(1.-zh))/(1.-zh);
    // q2g channel
    resultq2g += +CF*(alphas/2./pi)*(1./v)*softq2g_del1mw*(  ((1. + (1. - v)*(1. - v) )/v)*(std::log((1. - v)*v)-std::log(mu*mu/Q2) ) + v )/(1.-xB);
    resultq2g += +CF*(alphas/2./pi)*(1./w/v)*softq2g*(1. + v*v + w*w - 2.*v*w*w - 2.*v*v*w + 2.*v*v*w*w )/v/(1-w)
                 -CF*(alphas/2./pi)*(1./v)*softq2g_del1mw*(1. + v*v + 1. - 2.*v - 2.*v*v + 2.*v*v )/v/(1-w)
                 -CF*(alphas/2./pi)*(1./v)*softq2g_del1mw*(1. + v*v + 1. - 2.*v - 2.*v*v + 2.*v*v )/v*(-std::log(1.-xB))/(1.-xB);
    
    return resultq2q + resultg2q + resultq2g;
    


};


/**
 * @brief Returns (auxiliary) unpolarized structure function F_UU,L @ NLO, explicit terms implemented
 * @param  X Integration variables, X[0] -> w, X[1] -> v
 * @param dim Dimensions of integral
 * @param params Parameters not to be integrated, passed thrugh a struct
 */
double AuxF_UUL4vegas(double *X, size_t dim, void *params){

    // Here we grab the parameters from the params struct
    vegas_params_unpol* p = static_cast<vegas_params_unpol*>(params);
    double xB = p->xB;
    double zh = p->zh;
    double mu = p->mu;
    double Q = p->Q;
    const PDF* f1 = p->f1;
    const PDF* D1 = p->D1;

    // And the integration variables
    double w = X[0];
    double v = X[1];

    double result ;

    
    // IMPLEMENT HERE NLO
   
    // Color factors
    double CF = 4./3., TF = 1./2. ;
    //double Q;
    //Q = mu;
    double Q2= Q*Q;

    // Strong coupling
    double alphas;
    // Evaluated at mu^2
    alphas = f1->alphasQ2(mu*mu);

    // Calculate soft parts in advance, for all 3 channels
    double  softq2q;
    double  softg2q;
    double  softq2g;

    softq2q = weighted_sum_f1D1(xB/w,zh/v,mu,f1,D1);

    softg2q = weighted_sum_D1_times_f1g(xB/w,zh/v,mu,f1,D1);

    softq2g = weighted_sum_f1_times_D1g(xB/w,zh/v,mu,f1,D1);


    result = 0.;

    result += +CF*(alphas/2./pi)*softq2q*(4.*w*v)/v/w ;
    result += +TF*(alphas/2./pi)*softg2q*(8.*w*(1. - w))/v/w ;
    result += +CF*(alphas/2./pi)*softq2g*(4.*w*(1. - v))/v/w ;

    return result;


};

// This struct is to pass multiple parameters to the gsl function for the monte carlo integration.
// It is essentially a way of passing variables to the vegas function 
// without them being arguments of the function to be integrated.
// Basically it serves the same purpose as a common block in fortran, kind of... 
struct vegas_params_pol {
        double xB, zh, mu, Q;
        const LHAPDF::PDF* h1;
        const LHAPDF::PDF* Ht;
};

/**
 * @brief Returns (auxiliary) polarized structure function F_UT @ NLO, explicit terms implemented
 * @param  X Integration variables, X[0] -> w, X[1] -> v
 * @param dim Dimensions of integral
 * @param params Parameters not to be integrated, passed thrugh a struct
 */
double AuxF_UT4vegas(double *X, size_t dim, void *params){

    // Here we grab the parameters from the params struct
    vegas_params_pol* p = static_cast<vegas_params_pol*>(params);
    double xB = p->xB;
    double zh = p->zh;
    double mu = p->mu;
    double Q = p->Q;
    const PDF* h1 = p->h1;
    const PDF* Ht = p->Ht;

    // And the integration variables
    double w = X[0];
    double v = X[1];
    double zeta = X[2];
    double result, resultqbarq ;

    
    // IMPLEMENT HERE NLO
   
    // Color factors
    double CF = 4./3., TF = 1./2., Nc = 3. ;
    //double Q;
    //Q = sqrt(Sep*xB*y);
    double Q2= Q*Q;

    // Strong coupling
    double alphas;
    // Evaluated at mu^2
    alphas = h1->alphasQ2(mu*mu);

    double asd2pi= alphas/2./pi;

    // Calculate soft parts in advance
    double softqg_del1mwdel1mv, softqg_del1mw, softqg_del1mv, softqg;
    double softqbarq_del1mwdel1mv, softqbarq_del1mw, softqbarq_del1mv, softqbarq;

    // Model parameters
  
    double  au = 2.1;
    double  ad = 2.1;
    double  bu = 2.5;
    double  bd = 2.7;
    double  cu = 2.4;
    double  cd = 2.3;
    double  Nu = 1.;
    double  Nd = 1.;

   
    // Soft parts: first factor is from strcture function

    // qg channel
    softqg_del1mwdel1mv = (-xB*2*Mpi/Q/zh)*weighted_sum_h1ImHFUqg(xB,zh,zeta,mu,au,ad,bu,bd,h1,Ht)/(1.-zeta);
    softqg_del1mw = (-xB*2*Mpi/Q/(zh/v))*weighted_sum_h1ImHFUqg(xB,zh/v,zeta,mu,au,ad,bu,bd,h1,Ht)/(1.-zeta);
    softqg_del1mv = (-(xB/w)*2*Mpi/Q/zh)*weighted_sum_h1ImHFUqg(xB/w,zh,zeta,mu,au,ad,bu,bd,h1,Ht)/(1.-zeta);
    softqg = (-(xB/w)*2*Mpi/Q/(zh/v))*weighted_sum_h1ImHFUqg(xB/w,zh/v,zeta,mu,au,ad,bu,bd,h1,Ht)/(1.-zeta);

    // qbarq channel
    softqbarq_del1mwdel1mv = (-xB*2*Mpi/Q/zh)*weighted_sum_h1ImHFUqbarq(xB,zh,zeta,mu,cu,cd,Nu,Nd,h1,Ht);
    softqbarq_del1mw = (-xB*2*Mpi/Q/(zh/v))*weighted_sum_h1ImHFUqbarq(xB,zh/v,zeta,mu,cu,cd,Nu,Nd,h1,Ht);
    softqbarq_del1mv = (-(xB/w)*2*Mpi/Q/zh)*weighted_sum_h1ImHFUqbarq(xB/w,zh,zeta,mu,cu,cd,Nu,Nd,h1,Ht);
    softqbarq = (-(xB/w)*2*Mpi/Q/(zh/v))*weighted_sum_h1ImHFUqbarq(xB/w,zh/v,zeta,mu,cu,cd,Nu,Nd,h1,Ht);

    
    result = 0. ;
    
    // LO
    result += 2*zh *softqg_del1mwdel1mv/(1.-xB)/(1.-zh) ;

    
    // NLO

    // qg channel
    // Nc part
    result +=asd2pi* Nc/(1.-zh)/(1.-xB) /4 *( -2. * std::log(1. - zeta)/zeta - std::log(zeta)*(-4. + 2. * std::log(Q2/mu/mu) +std::log(zeta)  )/(-1.+zeta)    )*softqg_del1mwdel1mv;

    result += asd2pi* Nc/(1.-xB) /(2.*(1.-zeta)*zeta*(1.-v*zeta) )*(    (1.+zeta+v*(-2. +zeta - zeta*zeta))*( std::log((1.-v)*v)+std::log(Q2/mu/mu) )
                 - v*zeta - v*zeta*zeta  + 2.*v*v*zeta*zeta   )*softqg_del1mw/v;
    
    result += asd2pi* Nc /(1.-w) /( 2.*(1.-zeta)*zeta*(w*(-1+zeta) + (-1+v)*zeta )) *w*( (-1.+2.*v)*w*w*(1.-zeta)*(1.-zeta) - 3.*(-1.+v)*w*(-1.+zeta)*zeta + 2.*(-1.+v)*zeta*zeta  )*softqg/v/w;
    result -=asd2pi*  Nc /(1.-w) /( 2.*(1.-zeta)*zeta*((-1+zeta) + (-1+v)*zeta )) *( (-1.+2.*v)*(1.-zeta)*(1.-zeta) - 3.*(-1.+v)*(-1.+zeta)*zeta + 2.*(-1.+v)*zeta*zeta  )*softqg_del1mw/v;
    result -=asd2pi*  Nc *(-std::log(1. - xB)/(1.-xB))/( 2.*(1.-zeta)*zeta*((-1+zeta) + (-1+v)*zeta )) *( (-1.+2.*v)*(1.-zeta)*(1.-zeta) - 3.*(-1.+v)*(-1.+zeta)*zeta + 2.*(-1.+v)*zeta*zeta  )*softqg_del1mw/v;

    // CF part
    result += asd2pi* CF/(1.-zh)/(1.-xB) /(2.*(-1.+zeta)) *( 15. - 15.*zeta - 6.*std::log(zeta) + std::log(zeta)*std::log(zeta) + 2.*std::log(Q2/mu/mu)*(-2. + 2.*zeta + std::log(zeta)) )*softqg_del1mwdel1mv;

    result += asd2pi* CF/(1.-zh)/(1. - w) *2.*w*(-1. + std::log(Q2/mu/mu)  - std::log(w) )*softqg_del1mv/w;
    result -= asd2pi* CF/(1.-zh)/(1. - w) *2.*(-1. + std::log(Q2/mu/mu) )*softqg_del1mwdel1mv;
    result -= asd2pi* CF/(1.-zh)*(-std::log(1.-xB)/(1.-xB)) *2.*(-1. + std::log(Q2/mu/mu) )*softqg_del1mwdel1mv;
    result += asd2pi* CF/(1.-zh)*(std::log(1.-w)/(1. - w) )*2.*w*softqg_del1mv/w;
    result -= asd2pi* CF/(1.-zh)*(std::log(1.-w)/(1. - w) )*2.*softqg_del1mwdel1mv;
    result -= asd2pi* CF/(1.-zh)*(-0.5*std::log(1.-xB)*std::log(1.-xB)/(1.-xB) )*2.*softqg_del1mwdel1mv;

    result += asd2pi* CF/(1.-xB)/(1.-v)/zeta * ( (-1. -v*(-3.+zeta) +2.*v*v*(-1.+zeta) + zeta )*(std::log(Q2/mu/mu) +std::log(v) )  + 1. - 3.*v + 2.*v*v - 2.*v*v*zeta  )*softqg_del1mw/v;
    result -= asd2pi* CF/(1.-xB)/(1.-v)/zeta * ( (-1.-(-3.+zeta) +2.*(-1.+zeta) + zeta )*(std::log(Q2/mu/mu) )  + 1. - 3. + 2. - 2.*zeta  )*softqg_del1mwdel1mv;
    result -= asd2pi* CF/(1.-xB)*(-std::log(1.-zh)/(1.-zh))/zeta * ( (-1.-(-3.+zeta) +2.*(-1.+zeta) + zeta )*(std::log(Q2/mu/mu) )  + 1. - 3. + 2. - 2.*zeta  )*softqg_del1mwdel1mv;
    result += asd2pi* CF/(1.-xB)*(std::log(1.-v) /(1.-v))/zeta * ( -1. -v*(-3.+zeta) +2.*v*v*(-1.+zeta) + zeta  )*softqg_del1mw/v;
    result -= asd2pi* CF/(1.-xB)*(std::log(1.-v) /(1.-v))/zeta * ( -1. -(-3.+zeta) +2.*(-1.+zeta) + zeta  )*softqg_del1mwdel1mv;
    result -= asd2pi* CF/(1.-xB)*(-0.5*std::log(1.-zh)*std::log(1.-zh)/(1.-zh))/zeta * ( -1. -(-3.+zeta) +2.*(-1.+zeta) + zeta  )*softqg_del1mwdel1mv;

    result += -asd2pi* CF/(1.-w)/(1.-v)/zeta/(w*(-1.+zeta) +(-1.+v)*zeta ) *w* ( (1.-3.*v +2.*v*v)*w*w*(-1.+zeta) + (1.-v)*(1.-v)*zeta*zeta + 
                        w*zeta*( -3.*v*v -2.*v*v*v*(-1.+zeta) - zeta + v*(3. + zeta) )     )*softqg/w/v;
    result -= asd2pi* CF/(1.-w)/(1.-v) * 2.*softqg_del1mv;
    result -= -asd2pi* CF/(1.-w)/(1.-v)/zeta/((-1.+zeta) +(-1.+v)*zeta ) * ( (1.-3.*v +2.*v*v)*(-1.+zeta) + (1.-v)*(1.-v)*zeta*zeta + 
                        zeta*( -3.*v*v -2.*v*v*v*(-1.+zeta) - zeta + v*(3. + zeta) )    )*softqg_del1mw/v;
    result += asd2pi* CF/(1.-w)/(1.-v) * 2.* softqg_del1mwdel1mv;

    result += asd2pi* std::log(1.-zh)/(1.-zh)/(1.-w) *( 2.*CF*softqg_del1mv -  2.*CF*softqg_del1mwdel1mv);
    result += asd2pi* std::log(1.-xB)/(1.-xB)/(1.-v) *(  -CF/zeta/((-1.+zeta) +(-1.+v)*zeta ) * ( (1.-3.*v +2.*v*v)*(-1.+zeta) + (1.-v)*(1.-v)*zeta*zeta + 
                        zeta*( -3.*v*v -2.*v*v*v*(-1.+zeta) - zeta + v*(3. + zeta) )    )*softqg_del1mw/v   -  2.*CF*softqg_del1mwdel1mv    );
    result += asd2pi* 2.*CF*softqg_del1mwdel1mv*std::log(1.-xB)*std::log(1.-zh)/(1.-xB)/(1.-zh);

    

    // qbarq channel
    result += asd2pi*(CF - Nc/2.)/(1. + v*(-1.+zeta))/zeta * (v*zeta + (1.-3.*v+2.*v*v)*(std::log(v*(1.-v))  + std::log(Q2/mu/mu)  )  )*softqbarq_del1mw/v/(1.-xB);

    result += (1./(1. - w))*asd2pi*(CF - Nc/2.)/(v*(-1. + zeta)*zeta*(w*(-1. + zeta) + (-1. + v)*zeta )*(1. + v*(-1.+zeta) + zeta*(-1.+w)  )   ) * 
                (1. - 3.*v + 2.*v*v )*w*w*(v*v*(-1.+zeta)*zeta + (-1.+w)*zeta*(1. + (-1. + w)*zeta) + v*(-1.+zeta)*(-2.*zeta + w*(-1. + 2.*zeta))  )*softqbarq/w/v;
    result -= (1./(1. - w))*asd2pi*(CF - Nc/2.)/(v*(-1. + zeta)*zeta*((-1. + zeta) + (-1. + v)*zeta )*(1. + v*(-1.+zeta)  )   ) * 
                (1. - 3.*v + 2.*v*v )*(v*v*(-1.+zeta)*zeta + v*(-1.+zeta)*(-2.*zeta + (-1. + 2.*zeta))  )*softqbarq_del1mw/v;
    result -= (-std::log(1.-xB)/(1.-xB))*asd2pi*(CF - Nc/2.)/(v*(-1. + zeta)*zeta*((-1. + zeta) + (-1. + v)*zeta )*(1. + v*(-1.+zeta)  )   ) * 
                (1. - 3.*v + 2.*v*v )*(v*v*(-1.+zeta)*zeta + v*(-1.+zeta)*(-2.*zeta + (-1. + 2.*zeta))  )*softqbarq_del1mw/v;
                 



    return result;
    

    


};


/**
 * @brief Returns the unpolarized structure function F_UU,T @ given alphasS order of accuracy.
 * The w and v integrals are performed here
 * @param xB Scaling variable
 * @param zh Scaling variable
 * @param mu Factorization scale 
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF
 * @param accuracy = 0 LO, = 1 LO+NLO
 */
double F_UUT(double xB, double zh, double mu, double Q, const PDF* f1, const PDF* D1, int accuracy){

    double result ;

    if(accuracy == 0){
        // Return structure function @ LO [Bacchetta07]
        result =  xB*weighted_sum_f1D1(xB,zh,mu,f1,D1);
        return result;
    }
    
    else if(accuracy == 1){
        // Set up the gsl monte carlo integration for the 2D integral in w and v

        
        // Define the struct and fill it with the parameters to be passed to the gsl monte carlo function
        vegas_params_unpol myparams;
        myparams.xB = xB;
        myparams.zh = zh;
        myparams.mu = mu;
        myparams.Q = Q;
        myparams.f1 = f1;
        myparams.D1 = D1;

        // Declare result and sigma of the integration
        double res, err;

        
        // Dimensions
        size_t dim = 2;
        // Boundaries of the integration
        double xl[2] = {xB, zh};
        double xu[2] = {1, 1};
        

        // Function to integrate. Modify calls variable if needed!
        gsl_monte_function G = {&AuxF_UUT4vegas,dim,&myparams};

        size_t calls = 10000;

        // GSL MONTE CARLO INTEGRATION. COPIED FROM OFFICIAL GNU WEBSITE
        const gsl_rng_type *T;
        gsl_rng *r;

        gsl_rng_env_setup ();

        T = gsl_rng_default;
        r = gsl_rng_alloc (T);
        gsl_monte_vegas_state *s = gsl_monte_vegas_alloc (dim);

        gsl_monte_vegas_integrate (&G, xl, xu, dim, calls, r, s,
                                &res, &err);
        display_results ("vegas warm-up", res, err);

        printf ("converging...\n");

        do
        {
            gsl_monte_vegas_integrate (&G, xl, xu, dim, calls, r, s,
                                    &res, &err);
            printf ("result = % .10f sigma = % .10f "
                    "chisq/dof = %.1f\n", res, err, gsl_monte_vegas_chisq (s));
        }
        while (fabs (gsl_monte_vegas_chisq (s) - 1.0) > 0.5);

        display_results ("vegas final", res, err);

        gsl_monte_vegas_free (s);

        
        return res;
        } 

        else{
            std::cout << "ERROR: accuracy parameter not recognized, returning 0" << std::endl;
            return 0.;
        }

    
};

/**
     * @brief Returns the unpolarized structure function F_UU,L @ NLO accuracy (only present at NLO! At LO, this function returns 0.0 ).
     * The w and v integrals are performed here
     * @param xB Scaling variable
     * @param zh Scaling variable
     * @param mu Factorization scale
     * @param Q Virtuality of exchanged photon
     * @param f1 Pointer to f1 PDF
     * @param D1 Pointer to D1 FF
     * @param accuracy = 0 LO, = 1 LO+NLO
     */
double F_UUL(double xB, double zh, double mu, double Q, const PDF* f1, const PDF* D1, int accuracy){

    double result ;

    if(accuracy == 1){
        // Set up the gsl monte carlo integration for the 2D integral in w and v

    
    // Define the struct and fill it with the parameters to be passed to the gsl vegas monte carlo function
    vegas_params_unpol myparams;
    myparams.xB = xB;
    myparams.zh = zh;
    myparams.mu = mu;
    myparams.Q = Q;
    myparams.f1 = f1;
    myparams.D1 = D1;

    // Declare result and sigma of the integration
    double res, err;

    
    // Dimensions
    size_t dim = 2;
    // Boundaries of the integration
    double xl[2] = {xB, zh};
    double xu[2] = {1, 1};
    
    
    // Function to integrate. Modify calls variable if needed!
    gsl_monte_function G = {&AuxF_UUL4vegas,dim,&myparams};

    size_t calls = 10000;

    // GSL MONTE CARLO INTEGRATION. COPIED FROM OFFICIAL GNU WEBSITE
    const gsl_rng_type *T;
    gsl_rng *r;

    gsl_rng_env_setup ();

    T = gsl_rng_default;
    r = gsl_rng_alloc (T);
    gsl_monte_vegas_state *s = gsl_monte_vegas_alloc (dim);

    gsl_monte_vegas_integrate (&G, xl, xu, dim, calls, r, s,
                            &res, &err);
    display_results ("vegas warm-up", res, err);

    printf ("converging...\n");

    do
    {
        gsl_monte_vegas_integrate (&G, xl, xu, dim, calls*3, r, s,
                                &res, &err);
        printf ("result = % .10f sigma = % .10f "
                "chisq/dof = %.1f\n", res, err, gsl_monte_vegas_chisq (s));
    }
    while (fabs (gsl_monte_vegas_chisq (s) - 1.0) > 0.5);

    display_results ("vegas final", res, err);

    gsl_monte_vegas_free (s);

    
    return res;

    }
    else{
        return 0.0;
    }
 

    

    
};



/**
 * @brief Returns the structure function F_UT^sin(phi_S) @ given alphasS order of accuracy.
 * The w and v (and zeta) integrals are performed here
 * @param xB Scaling variable
 * @param zh Scaling variable
 * @param mu Factorization scale 
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 * @param accuracy = 0 LO, = 1 LO+NLO
 */
double F_UT(double xB, double zh, double mu,double Q, const PDF* h1, const PDF* Ht, int accuracy){

    double result;

    if(accuracy == 0){
        // Return structure function @ LO [Bacchetta07]
        result = - xB*2*Mpi/Q *weighted_sum_h1Ht(xB,zh,mu,h1,Ht)/zh;
        return result;
    }
    // IMPLEMENT HERE NLO
    else {

        // Test for qgq function
        // Set up the gsl monte carlo integration for the 3D integral in w and v and zeta

    
        // Define the struct and fill it with the parameters to be passed to the gsl vegas monte carlo function
        vegas_params_pol myparams;
        myparams.xB = xB;
        myparams.zh = zh;
        myparams.mu = mu;
        myparams.Q = Q;
        myparams.h1 = h1;
        myparams.Ht = Ht;

        // Declare result and sigma of the integration
        double res, err;

        
        // Dimensions
        size_t dim = 3;
        // Boundaries of the integration
        double xl[3] = {xB, zh, 0.};
        double xu[3] = {1, 1, 1};

        
        // Function to integrate. Modify calls variable if needed!
        gsl_monte_function G = {&AuxF_UT4vegas,dim,&myparams};

        size_t calls = 10000;

        // GSL MONTE CARLO INTEGRATION
        const gsl_rng_type *T;
        gsl_rng *r;

        gsl_rng_env_setup ();

        T = gsl_rng_default;
        r = gsl_rng_alloc (T);
        gsl_monte_vegas_state *s = gsl_monte_vegas_alloc (dim);

        gsl_monte_vegas_integrate (&G, xl, xu, dim, calls, r, s,
                                &res, &err);
        display_results ("vegas warm-up", res, err);

        printf ("converging...\n");

        do
        {
            gsl_monte_vegas_integrate (&G, xl, xu, dim, calls*3, r, s,
                                    &res, &err);
            printf ("result = % .10f sigma = % .10f "
                    "chisq/dof = %.1f\n", res, err, gsl_monte_vegas_chisq (s));
        }
        while (fabs (gsl_monte_vegas_chisq (s) - 1.0) > 0.5);

        display_results ("vegas final", res, err);

        gsl_monte_vegas_free (s);

        return res;
    }
    
};




/**
 * @brief Function that returns the xB and zh vectors filed with the kinematical points we want to evaluate further on in the analysis
 * @param xmin Minimum x or z value
 * @param xmax Maximum x or z value
 * @param Nx Number of points for x variable
 */
std::vector<double> fill_xz_vector(double min, double max,double N){

    std::vector<double> v(N);
    double delta;

    delta = (max - min)/N;


    // Fill from 0 to N
    std::iota( std::begin(v), std::end(v), min); 
    
    for(int i = 1; i<N; i++){
        v[i] = v[i-1] + delta;
        //std::cout << v[i] << std::endl;
    }
    return v;

}

/**
 * @brief Function that returns the A_UT asymmetry as ratio of structure functions
 * @param xB Scaling variable
 * @param y Scaling variable
 * @param zh Scaling variable
 * @param mu Factorization scale
 * @param Q Virtuality of photon
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 * @param accuracyFUT alpha_s accuracy for numerator: = 0 LO, = 1 LO+NLO
 * @param accuracyFUU alpha_s accuracy for denominator: = 0 LO, = 1 LO+NLO
 */
double A_UT(double xB,double y, double zh, double mu,double Q, const PDF* f1, const PDF* D1, const PDF* h1, const PDF* Ht, int accuracyFUT, int accuracyFUU){

    double eps;


    // The ratio ε of longitudinal and transverse photon flux
    eps = (1. - y)/(1. - y + y*y/2.);

    return F_UT(xB,zh,mu,Q,h1,Ht,accuracyFUT)/(F_UUT(xB,zh,mu,Q,f1,D1,accuracyFUU)  + eps*F_UUL(xB,zh,mu,Q,f1,D1,accuracyFUU) );

};

/**
 * @brief Writes down to file the asymmetry A_UT^sin(phi_S) as a function of x and z. 
 * The data is stored in a table with coordinates x (row) and z (columns)
 * If either xB or zh are fixed to a single value, the function writes down A_UT as a function of the other variable only.
 * @param x_sample Vector of x values in which A_UT is meant to be evaluated
 * @param z_sample Vector of z values in which A_UT is meant to be evaluated
 * @param Q 
 * @param f1 Pointer to f1 PDF
 * @param D1 Pointer to D1 FF
 * @param h1 Pointer to h1 PDF
 * @param Ht Pointer to Ht FF
 * @param accuracyFUT = 0 LO, = 1 NLO, = 2 LO+NLO
 * @param accuracyFUU = 0 LO, = 1 NLO, = 2 LO+NLO
 */
void write_A_UT_to_file(std::vector<double> x_sample, double y, std::vector<double> z_sample,double mu, double Q, const PDF* f1, const PDF* D1,const PDF* h1, const PDF* Ht, int accuracyFUT, int accuracyFUU, std::string fname){

    // Declare ofstream object (file in which we are going to store our data points)
    std::ofstream ofile, ofilex, ofilez;
    

    // Save A_UT(x,z)
    // Open it
    ofile.open(fname);

    if(x_sample.size() == 1 || z_sample.size() == 1 ){
        for (double x : x_sample) {
            for (double z : z_sample){
                if(x_sample.size() == 1){
                    // Write to file
                    ofile << z << " " << A_UT(x, y, z, mu, Q, f1, D1, h1, Ht, accuracyFUT, accuracyFUU)  << std::endl;
                }
                if(z_sample.size() == 1){
                    // Write to file
                    ofile << x << " " << A_UT(x, y, z, mu, Q, f1, D1, h1, Ht, accuracyFUT, accuracyFUU)  << std::endl;
                }
            }
        }
        // Close the file
        ofile.close();
    }
    else{
        for (double x : x_sample) {
            for (double z : z_sample){
                // Write to file
                ofile << A_UT(x, y, z, mu, Q, f1, D1, h1, Ht, accuracyFUT, accuracyFUU) << " ";
            }
        // End the line
        ofile << std::endl;
        }

        // Save x vector
        ofilex.open("xB.txt");
        for (double x : x_sample) {
        // Write to file
        ofilex << x << std::endl;
        }
        // Close the file
        ofilex.close();

        // Save z vector
        ofilez.open("zh.txt");
        for (double z : z_sample) {
            // Write to file
            ofilez << z << std::endl;
        }
        // Close the file
        ofilez.close();
        // Close the file
        ofile.close();
    }
    
    


};

// THIS IS THE MAIN PROGRAM
int main(){

    // Declare the PDFs/FFs that we are going to use as LHAPDF::PDF objects

    // Import f1 PDF 
    // Do they use JAM20-SIDIS_PDF_proton_nlo? For both orders?
    const PDF* f1 = LHAPDF::mkPDF("JAM20-SIDIS_PDF_proton_nlo", 0); // 0 is the member number
    const PDF* f1nlo = LHAPDF::mkPDF("JAM20-SIDIS_PDF_proton_nlo", 0); // 0 is the member number

    // Import h1 PDF
    const PDF* h1 = LHAPDF::mkPDF("JAM22-transversity_proton_lo", 0); // 0 is the member number
    
    // Import D1 FF (pi+ ad pi-)
    // Here I should use JAM20-SIDIS_FF_pion_nlo but i don't 100% undesrtand what they mean by charge conjugation.
    // I am sure it is an easy 2min fix once I know that. Same doubt propagates to Ht for pi- channel...
    const PDF* D1p = LHAPDF::mkPDF("NNFF10_PIp_lo", 0); // 0 is the member number
    const PDF* D1pnlo = LHAPDF::mkPDF("NNFF10_PIp_nlo", 0); // 0 is the member number
    const PDF* D1m = LHAPDF::mkPDF("NNFF10_PIm_lo", 0); // 0 is the member number
    const PDF* D1mnlo = LHAPDF::mkPDF("NNFF10_PIm_nlo", 0); // 0 is the member number

    // Import H tilde FF (pi+) (later on changed to pi- via C conjugation(again, same doubt) )
    const PDF* Ht = LHAPDF::mkPDF("JAM22-Htilde_pion_lo", 0); // 0 is the member number


    // Declare useful variables
    double avgQ2, avgzh,avgxB, avgy;
    std::vector<double> x_sample, z_sample;
    
    
    
    ////////////////////////////////////////////////////
    // HERMES 9066. pi+, z dependence
    // Average values obtained from 9066 data set
    avgQ2 = 2.415;
    avgxB = 0.1017;
    avgy = 0.4976; 
   
    // Here we fill the vectors with the kinematical points we want to evaluate
    x_sample.push_back(avgxB);
    z_sample.push_back(0.1);
    z_sample.push_back(0.15);
    z_sample.push_back(0.2);
    z_sample.push_back(0.25);
    z_sample.push_back(0.3);
    z_sample.push_back(0.35);
    z_sample.push_back(0.4);
    z_sample.push_back(0.45);
    z_sample.push_back(0.5);
    z_sample.push_back(0.55);
    z_sample.push_back(0.6);
    z_sample.push_back(0.65);
    z_sample.push_back(0.7);
    z_sample.push_back(0.75);
    z_sample.push_back(0.8);
    z_sample.push_back(0.85);
    z_sample.push_back(0.9);

    which_pion = +1; // It is a pi+

    // Write to file LO and NLO
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1,D1p,h1,Ht,0,0,"out/AUTz_LO_pp.txt");
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1nlo,D1pnlo,h1,Ht,1,1,"out/AUTz_NLO_pp.txt");

    //Reset the vectors (otherwise pushback would just add more points to the previous ones, at then end of the already existing vector)
    x_sample.clear();
    z_sample.clear();

    ////////////////////////////////////////////////////
    // HERMES 9055. pi+, x dependence
    // Average values obtained from 9055 data set
    avgQ2 = 2.79;
    avgzh = 0.3664;
    avgy = 0.519; 

   // Here we fill the vectors with the kinematical points we want to evaluate
    z_sample.push_back(avgzh);

    x_sample.push_back(0.05);
    x_sample.push_back(0.1);
    x_sample.push_back(0.15);
    x_sample.push_back(0.2);
    x_sample.push_back(0.25);
    x_sample.push_back(0.3);
    x_sample.push_back(0.35);
    x_sample.push_back(0.4);
    x_sample.push_back(0.45);
    x_sample.push_back(0.5);

    which_pion = +1; // It is a pi+

    // Write to file LO and NLO
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1,D1p,h1,Ht,0,0,"out/AUTx_LO_pp.txt");
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1nlo,D1pnlo,h1,Ht,1,1,"out/AUTx_NLO_pp.txt");

    //Reset the vectors (otherwise pushback would just add more points to the previous ones, at then end of the already existing vector)
    x_sample.clear();
    z_sample.clear();
    
    ////////////////////////////////////////////////////
    // HERMES 10032. pi-, z dependence
    // Average values obtained from 10032 data set
    avgQ2 = 2.285;
    avgxB = 0.0965;
    avgy = 0.4983; 

    // Here we fill the vectors with the kinematical points we want to evaluate
    x_sample.push_back(avgxB);
    z_sample.push_back(0.1);
    z_sample.push_back(0.15);
    z_sample.push_back(0.2);
    z_sample.push_back(0.25);
    z_sample.push_back(0.3);
    z_sample.push_back(0.35);
    z_sample.push_back(0.4);
    z_sample.push_back(0.45);
    z_sample.push_back(0.5);
    z_sample.push_back(0.55);
    z_sample.push_back(0.6);
    z_sample.push_back(0.65);
    z_sample.push_back(0.7);
    z_sample.push_back(0.75);
    z_sample.push_back(0.8);
    z_sample.push_back(0.85);
    z_sample.push_back(0.9);

    which_pion = -1; // It is a pi-

    // Write to file LO and NLO
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1,D1m,h1,Ht,0,0,"out/AUTz_LO_pm.txt");
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1nlo,D1mnlo,h1,Ht,1,1,"out/AUTz_NLO_pm.txt");

    //Reset the vectors (otherwise pushback would just add more points to the previous ones, at then end of the already existing vector)
    x_sample.clear();
    z_sample.clear();
    
    ////////////////////////////////////////////////////
    // HERMES 10021. pi-, x dependence
    // Average values obtained from 10021 data set
    avgQ2 = 2.774;
    avgzh= 0.358;
    avgy = 0.51857; 

    // Here we fill the vectors with the kinematical points we want to evaluate
    z_sample.push_back(avgzh);

    x_sample.push_back(0.05);
    x_sample.push_back(0.1);
    x_sample.push_back(0.15);
    x_sample.push_back(0.2);
    x_sample.push_back(0.25);
    x_sample.push_back(0.3);
    x_sample.push_back(0.35);
    x_sample.push_back(0.4);
    x_sample.push_back(0.45);
    x_sample.push_back(0.5);

    which_pion = -1; // It is a pi-

    // Write to file LO and NLO
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1,D1m,h1,Ht,0,0,"out/AUTx_LO_pm.txt");
    write_A_UT_to_file(x_sample, avgy, z_sample, sqrt(avgQ2), sqrt(avgQ2), f1nlo,D1mnlo,h1,Ht,1,1,"out/AUTx_NLO_pm.txt");

    //Reset the vectors (otherwise pushback would just add more points to the previous ones, at then end of the already existing vector)
    x_sample.clear();
    z_sample.clear();
    

    


    return 0;
    
};

