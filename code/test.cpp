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

// g++ test.cpp -o test `lhapdf-config --cflags --ldflags`-I/home/diego/sidis/sidis-NLO-twist3/code
// export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/diego/LHAPDF-6.5.5/src/.libs
// export LHAPDF_DATA_PATH=/home/diego/sidis/sidis-NLO-twist3/code/LHAPDF_tables
// ./test

using namespace LHAPDF;

// Global variables
double Mpi = 0.1396; // GeV, pion mass
double sqrtS = 100; // Gev, center-of-mass energy S = Q^2 xB y
double pi = M_PI;


// TEST FUNCTION
double CUU(double *X, size_t dim, void *params)
{
    // (void)(dim); /* avoid unused parameter warnings */
  //(void)(params);
  double w,v;

  w = X[0];
  v = X[1];

  return (w*w +1.)/(v+2.);
}


void display_results (const char *title, double result, double error)
{
  printf ("%s ==================\n", title);
  printf ("result = % .6f\n", result);
  printf ("sigma  = % .6f\n", error);
}




/**
     * @brief Returns the weigthed sum over flavors q qbar of f1(x) D1(z), unpolarized q2q channel
     * @param x Argument of f1
     * @param z Argument of D1
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param f1 Pointer to f1 PDF
     * @param D1 Pointer to D1 FF
     */
double weighted_sum_f1D1(double x, double z, double mu, const PDF* f1, const PDF* D1){

    double xf1u, xf1ub, xf1d, xf1db, xf1c, xf1cb, xf1s, xf1sb, xf1b, xf1bb;
    double zD1u, zD1ub, zD1d, zD1db, zD1c, zD1cb, zD1s, zD1sb, zD1b, zD1bb;
    double mu2, resultA, resultB, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!

    xf1u = f1->xfxQ2(1,x,mu2);
    xf1ub = f1->xfxQ2(-1,x,mu2);
    xf1d = f1->xfxQ2(2,x,mu2);
    xf1db = f1->xfxQ2(-2,x,mu2);
    xf1c = f1->xfxQ2(3,x,mu2);
    xf1cb = f1->xfxQ2(-3,x,mu2);
    xf1s = f1->xfxQ2(4,x,mu2);
    xf1sb = f1->xfxQ2(-4,x,mu2);
    xf1b = f1->xfxQ2(5,x,mu2);
    xf1bb = f1->xfxQ2(-5,x,mu2);


    zD1u = D1->xfxQ2(1,z,mu2);
    zD1ub = D1->xfxQ2(-1,z,mu2);
    zD1d = D1->xfxQ2(2,z,mu2);
    zD1db = D1->xfxQ2(-2,z,mu2);
    zD1c = D1->xfxQ2(3,z,mu2);
    zD1cb = D1->xfxQ2(-3,z,mu2);
    zD1s = D1->xfxQ2(4,z,mu2);
    zD1sb = D1->xfxQ2(-4,z,mu2);
    zD1b = D1->xfxQ2(5,z,mu2);
    zD1bb = D1->xfxQ2(-5,z,mu2);

    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (xf1u/x)*(zD1u/z)+(xf1ub/x)*(zD1ub/z)+(xf1c/x)*(zD1c/z)+(xf1cb/x)*(zD1cb/z) );
    resultB = ( pow((1./3.),2) )*( (xf1d/x)*(zD1d/z)+ (xf1db/x)*(zD1db/z)+(xf1s/x)*(zD1s/z)+(xf1sb/x)*(zD1sb/z)+(xf1b/x)*(zD1b/z)+(xf1bb/x)*(zD1bb/z) );
    
    return resultA+resultB;
};


/**
     * @brief Returns the [weigthed sum over flavors q qbar of f1(x)] X D1^g(z), unpolarized q2g channel
     * @param x Argument of f1
     * @param z Argument of D1
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param f1 Pointer to f1 PDF
     * @param D1 Pointer to D1 FF 
     */
double weighted_sum_f1_times_D1g(double x, double z, double mu, const PDF* f1, const PDF* D1){

    double xf1u, xf1ub, xf1d, xf1db, xf1c, xf1cb, xf1s, xf1sb, xf1b, xf1bb;
    double zD1g;
    double mu2, resultA, resultB, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!

    xf1u = f1->xfxQ2(1,x,mu2);
    xf1ub = f1->xfxQ2(-1,x,mu2);
    xf1d = f1->xfxQ2(2,x,mu2);
    xf1db = f1->xfxQ2(-2,x,mu2);
    xf1c = f1->xfxQ2(3,x,mu2);
    xf1cb = f1->xfxQ2(-3,x,mu2);
    xf1s = f1->xfxQ2(4,x,mu2);
    xf1sb = f1->xfxQ2(-4,x,mu2);
    xf1b = f1->xfxQ2(5,x,mu2);
    xf1bb = f1->xfxQ2(-5,x,mu2);

    zD1g = D1->xfxQ2(21,x,mu2);

    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (xf1u/x)+(xf1ub/x)+(xf1c/x)+(xf1cb/x) );
    resultB = ( pow((1./3.),2) )*( (xf1d/x)+ (xf1db/x)+(xf1s/x)+(xf1sb/x)+(xf1b/x)+(xf1bb/x) );
    
    return (resultA+resultB)*zD1g/z;
};



/**
     * @brief Returns the [weigthed sum over flavors q qbar of D1(z)] X f1^g(z), unpolarized g2q channel
     * @param x Argument of f1
     * @param z Argument of D1
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
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

    zD1u = D1->xfxQ2(1,z,mu2);
    zD1ub = D1->xfxQ2(-1,z,mu2);
    zD1d = D1->xfxQ2(2,z,mu2);
    zD1db = D1->xfxQ2(-2,z,mu2);
    zD1c = D1->xfxQ2(3,z,mu2);
    zD1cb = D1->xfxQ2(-3,z,mu2);
    zD1s = D1->xfxQ2(4,z,mu2);
    zD1sb = D1->xfxQ2(-4,z,mu2);
    zD1b = D1->xfxQ2(5,z,mu2);
    zD1bb = D1->xfxQ2(-5,z,mu2);

    // Return weigthed sum
    resultA = ( pow((2./3.),2) )*( (zD1u/z)+(zD1ub/z)+(zD1c/z)+(zD1cb/z) );
    resultB = ( pow((1./3.),2) )*( (zD1d/z)+ (zD1db/z)+(zD1s/z)+(zD1sb/z)+(zD1b/z)+(zD1bb/z) );
    
    return (resultA+resultB)*xf1g/x;
};


/**
     * @brief Returns the unpolarized structure function F_UU,T @ given alphasS order of accuracy
     * @param xB Scaling variable
     * @param zh Scaling variable
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param Q Virtuality of exchanged photon
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
    // IMPLEMENT HERE NLO
    else{


        return 0.;
    } 

    
};

/**
     * @brief Returns (auxiliary) unpolarized structure function F_UU,T @ NLO, explicit terms implemented
     * @param w Integration variable x = xB/w
     * @param v Integration variable z = xh/v
     * @param xB Scaling variable
     * @param zh Scaling variable
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param Q Virtuality of exchanged photon
     * @param f1 Pointer to f1 PDF
     * @param D1 Pointer to D1 FF
     * @param accuracy = 0 LO, = 1 LO+NLO
     */
double AuxF_UUT(double w, double v, double xB, double zh, double mu, double Q, const PDF* f1, const PDF* D1){

    double result ;

    
    // IMPLEMENT HERE NLO
   
    // Color factors
    double CF = 4./3., TF = 1./2., Q2= Q*Q;

    // Strong coupling
    double alphas;
    // Evaluated at mu^2
    alphas = f1->alphasQ2(mu*mu);

    // Calculate soft parts in advance, for all 3 channels
    double softq2q_del1mwdel1mv, softq2q_del1mw, softq2q_del1mv, softq2q;
    double softg2q_del1mwdel1mv, softg2q_del1mw, softg2q_del1mv, softg2q;
    double softq2g_del1mwdel1mv, softq2g_del1mw, softq2g_del1mv, softq2g;

    softq2q_del1mwdel1mv = weighted_sum_f1D1(xB,zh,mu,f1,D1);
    softq2q_del1mw = weighted_sum_f1D1(xB,zh/v,mu,f1,D1);
    softq2q_del1mv = weighted_sum_f1D1(xB/w,zh,mu,f1,D1);
    softq2q = weighted_sum_f1D1(xB/w,zh/v,mu,f1,D1);

    softg2q_del1mwdel1mv = weighted_sum_D1_times_f1g(xB,zh,mu,f1,D1);
    softg2q_del1mw = weighted_sum_D1_times_f1g(xB,zh/v,mu,f1,D1);
    softg2q_del1mv = weighted_sum_D1_times_f1g(xB/w,zh,mu,f1,D1);
    softg2q = weighted_sum_D1_times_f1g(xB/w,zh/v,mu,f1,D1);

    softq2g_del1mwdel1mv = weighted_sum_f1_times_D1g(xB,zh,mu,f1,D1);
    softq2g_del1mw = weighted_sum_f1_times_D1g(xB,zh/v,mu,f1,D1);
    softq2g_del1mv = weighted_sum_f1_times_D1g(xB/w,zh,mu,f1,D1);
    softq2g = weighted_sum_f1_times_D1g(xB/w,zh/v,mu,f1,D1);

    // q2q channel
    double resultq2q;

    resultq2q = softq2q_del1mwdel1mv*(1. + CF * alphas/2./pi)*(-8. -3. *std::log(mu*mu/Q2) ) /(1.-xB)/(1.-zh);
    resultq2q += +CF*(alphas/2./pi)*(   
              (std::log(1.-v)/(1.-v))*(1.+v*v)*((1./v)*softq2q_del1mw -2.*softq2q_del1mwdel1mv ) + 1.- v + 
              (1./(1.-v))*((1.+v*v)*(std::log(v)-std::log(mu*mu/Q2))*(1./v)*softq2q_del1mw  - 2.*(-std::log(mu*mu/Q2))*softq2q_del1mwdel1mv  ) 
                )/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (std::log(1.-v)/(1.-v))*( -2.))/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-0.5*std::log(1.-zh)*std::log(1.-zh))*( -2. ))/(1.-xB)/(1.-zh) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (1./(1.-v))*(-2. ))/(1.-xB) 
            +CF*(alphas/2./pi)*softq2q_del1mwdel1mv*(  (-std::log(1.-zh))*(-2. ))/(1.-xB)/(1.-zh) ;

    return 0.;

    
};



/**
     * @brief Returns the weigthed sum over flavors of h1(x) Ht(z)
     * @param x Argument of h1
     * @param z Argument of Ht
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param h1 Pointer to h1 PDF
     * @param Ht Pointer to Ht FF
     */
double weighted_sum_h1Ht(double x, double z, double mu, const PDF* h1, const PDF* Ht){

    double xh1u, xh1d, zHtu, zHtd,xh1ub,xh1db,zHtub,zHtdb, mu2, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu. Here I am considering pi+ !!
    //WHAT ABOUT ANTIQURKS?
    xh1u = h1->xfxQ2(1,x,mu2);
    //xh1ub = -h1->xfxQ2(1,-x,mu2);
    xh1d = h1->xfxQ2(2,x,mu2);
    //xh1db = -h1->xfxQ2(2,-x,mu2);
    zHtu = Ht->xfxQ2(1,z,mu2);
    //zHtub = Ht->xfxQ2(-1,z,mu2);
    zHtd = Ht->xfxQ2(2,z,mu2);
    //zHtdb = Ht->xfxQ2(-2,z,mu2);

    // Return weigthed sum
    result = ( pow((2./3.),2)  )*(xh1u/x)*(zHtu/z) + ( pow((1./3.),2) )*(xh1d/x)*(zHtd/z);

    return result;
};

/**
     * @brief Returns the structure function F_UT^sin(phi_S) @ given alphasS order of accuracy
     * @param x Argument of h1
     * @param z Argument of Ht
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param Q Virtuality of exchanged photon
     * @param h1 Pointer to h1 PDF
     * @param Ht Pointer to Ht FF
     * @param accuracy = 0 LO, = 1 NLO, = 2 LO+NLO
     */
double F_UT(double x, double z, double mu, double Q, const PDF* h1, const PDF* Ht, int accuracy){

    double result ;

    if(accuracy == 0){
        // Return structure function @ LO [Bacchetta07]
        result = - x*2*Mpi/Q *weighted_sum_h1Ht(x,z,mu,h1,Ht)/z;
        return result;
    }
    // IMPLEMENT HERE NLO
    else 
        return 0.;
    
};

/**
     * @brief Writes down to file the structure function F_UT^sin(phi_S) as a function of x and z. 
     * The data is stored in a table with coordinates x (row) and z (columns)
     * @param x_sample Vector of x values in which F_UT is meant to be evaluated
     * @param z_sample Vector of z values in which F_UT is meant to be evaluated
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param Q Virtuality of exchanged photon
     * @param h1 Pointer to h1 PDF
     * @param Ht Pointer to Ht FF
     * @param accuracy = 0 LO, = 1 NLO, = 2 LO+NLO
     */
void write_F_UT_to_file(std::vector<double> x_sample, std::vector<double> z_sample, double mu, double Q, const PDF* h1, const PDF* Ht, int accuracy, std::string fname){

    // Declare ofstream object (file in which we are going to store our data points)
    std::ofstream ofile, ofilex, ofilez;
    

    // Save F_UT(x,z)
    // Open it
    ofile.open(fname);

    for (double x : x_sample) {
        for (double z : z_sample){
            // Write to file
            ofile << F_UT(x, z, mu, Q, h1, Ht, accuracy) << " ";
        }
        // End the line
        ofile << std::endl;
    }
    // Close the file
    ofilex.close();

    // Save x vector
    ofilex.open("xB.txt");
    for (double x : x_sample) {
        // Write to file
        ofilex << x << std::endl;
    }

    // Save z vector
    ofilez.open("zh.txt");
    for (double z : z_sample) {
        // Write to file
        ofilez << z << std::endl;
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

int main(){
    //std::cout <<"Hello"  << std::endl;

    double min = 0.1;
    double max = 0.9;
    double n_points = 30;
    

    // Import f1 PDF
    const PDF* f1 = LHAPDF::mkPDF("MSTW2008lo68cl", 0); // 0 is the member number

    // Import h1 PDF
    const PDF* h1 = LHAPDF::mkPDF("JAM22-transversity_proton_lo", 0); // 0 is the member number

    // Import D1 FF (pi+)
    const PDF* D1 = LHAPDF::mkPDF("NNFF10_PIp_lo", 0); // 0 is the member number

    // Import H tilde FF (pi+)
    const PDF* Ht = LHAPDF::mkPDF("JAM22-Htilde_pion_lo", 0); // 0 is the member number

    






    double res, err;
    double xl[3] = {0, 0};
    double xu[3] = {1, 1};

    const gsl_rng_type *T;
    gsl_rng *r;

    gsl_monte_function G = {&CUU,2,0};

    size_t calls = 10000;

    gsl_rng_env_setup ();

    T = gsl_rng_default;
    r = gsl_rng_alloc (T);
    gsl_monte_vegas_state *s = gsl_monte_vegas_alloc (2);

    gsl_monte_vegas_integrate (&G, xl, xu, 2, calls, r, s,
                               &res, &err);
    display_results ("vegas warm-up", res, err);

    printf ("converging...\n");

    do
      {
        gsl_monte_vegas_integrate (&G, xl, xu, 2, calls, r, s,
                                   &res, &err);
        printf ("result = % .6f sigma = % .6f "
                "chisq/dof = %.1f\n", res, err, gsl_monte_vegas_chisq (s));
      }
    while (fabs (gsl_monte_vegas_chisq (s) - 1.0) > 0.1);

    display_results ("vegas final", res, err);

    gsl_monte_vegas_free (s);

    return 0;
    
};

