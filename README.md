
# sidis-NLO-twist3 - Transverse Spin Analysis of SIDIS at NLO

### How to Run

Clone this repo with

```bash
git clone https://github.com/diegoscantam/sidis-NLO-twist3
```
or manually download it by clicking the green button "Code".

After installation of LHAPDF6, one has to tell the `g++` compiler where to find the 
LHAPDF6 headers and libraries (this in principle should happen automatically if linked properly, but in my case I was having troubles).

To solve this, go to the `code` folder and tell the compiler where to find LHAPDF6 with the following command in the terminal:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/MODIFYHEREYOURPATHTOLHAPDFFOLDER/LHAPDF-X.Y.Z/src/.libs
```

Then, in principle, one should be able to download and install the PDF/FF sets directly via a terminal LHAPDF6 interface (also didn't manage to get it running...). Hence, we create a folder called `LHAPDF_tables` in the working directory where `sidis.cpp` lives, and manually copied 
the PDF/FF sets there (easily downloadable from the [website](https://www.lhapdf.org/pdfsets.html), it takes 1 min...).
Included in this repository, inside `LHAPDF_tables`, there are some sets useful to our analysis. We then tell LHAPDF6 where to find the PDF/FF tables with:
```bash
export LHAPDF_DATA_PATH=/MODIFYHEREYOURPATHTOTHISCLONEDREPO/sidis-NLO-twist3/code/LHAPDF_tables
```

In the working directory where `sidis.cpp` lives, there is a `gsl` folder which contains the GNU scientific routines (including our VEGAS monte carlo integrator that we need here). Nothing needs to be done here, besides adding the appropriate flags to the compilation.

Once we have done that (might not be necessary for everyone!), we can compile with
```bash
g++ sidis.cpp -o sidis `lhapdf-config --cflags --ldflags` -I/MODIFYHEREYOURPATH/sidis-NLO-twist3/code -lgsl -lgslcblas -lm
 ```
You might need to add the  `-std=gnu++11` flag if you are a Mac user. Then, we run the produced executable with
```bash
./sidis
```

For plotting the results, the file `plotC.ipynb` is simple Jupyter Notebook running python.
