#include <vector>
#include <cmath>
#include <algorithm>
#include <functional>
#include <stdio.h>
#include <iostream>
#include "LHAPDF/LHAPDF.h"
#include "LHAPDF/GridPDF.h"

// g++ test.cpp -o test `lhapdf-config --cflags --ldflags`
// export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/diego/LHAPDF-6.5.5/src/.libs FUNZIONA
// export LHAPDF_DATA_PATH=/home/diego/sidis/sidis-NLO-twist3/code/LHAPDF_tables
// ./test
using namespace LHAPDF;




/**
     * @brief Returns the weigthed sum over flavors of h1(x) Ht(z)
     * @param x Argument of h1
     * @param z Argument of Ht
     * @param mu Factorization scale (effectively ignored for Ht since evolution is not implemented)
     * @param h1 Pointer to h1 PDF
     * @param Ht Pointer to Ht FF
     */
double weighted_sum_h1Ht(double x, double z, double mu, const PDF* h1, const PDF* Ht){

    double xh1u, xh1d, zHtu, zHtd, mu2, result ;

    mu2 = mu*mu;

    // Evaluate PDF & FF at x, z and mu
    xh1u = h1->xfxQ2(1,x,mu2);
    xh1d = h1->xfxQ2(2,x,mu2);
    zHtu = Ht->xfxQ2(1,z,mu2);
    zHtd = Ht->xfxQ2(2,z,mu2);

    // Return weigthed sum
    result = ( pow((2./3.),2)  )*(xh1u/x)*(zHtu/z) + ( pow((1./3.),2) )*(xh1d/x)*(zHtd/z);

    return result;
};

int main(){
    //std::cout <<"Hello"  << std::endl;

    // Import H tilde FF
    const PDF* Ht = LHAPDF::mkPDF("JAM22-Htilde_pion_lo", 0); // 0 is the member number

    // Import h1 PDF
    const PDF* h1 = LHAPDF::mkPDF("JAM22-transversity_proton_lo", 0); // 0 is the member number

    

    return 0;
    
};


