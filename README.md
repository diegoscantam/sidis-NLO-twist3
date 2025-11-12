
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

Then, in principle, one should be able to download and install the PDF/FF sets directly via a terminal LHAPDF6 interface (also didn't manage to get it running...). Hence, we created a folder called `LHAPDF_tables` in the working directory `code`, and manually copied 
the PDF/FF sets there (easily downloadable from the [website](https://www.lhapdf.org/pdfsets.html), it takes 1 min...).
Included in this repository, inside `LHAPDF_tables`, there are some sets useful to our analysis. We then tell LHAPDF6 where to find the PDF/FF tables with:
```bash
export LHAPDF_DATA_PATH=/MODIFYHEREYOURPATHTOTHISCLONEDREPO/sidis-NLO-twist3/code/LHAPDF_tables
```

In the directory `code` there is a `gsl` folder which contains the GNU scientific routines (including our VEGAS monte carlo integrator that we need here). To install the library (more details in `gsl-2.8/INSTALL` file), go in the `gsl-2.8` folder and run the command
```bash
./configure && make && make install
```

Once we have done that, we can compile with
```bash
g++ sidis.cpp -o sidis `lhapdf-config --cflags --ldflags` -lgsl -lgslcblas -lm
 ```
You might need to add the  `-std=gnu++11` flag if you are a Mac user. 

Then, we run the produced executable with, for example, 
```bash
./sidis 1
```
where `1` here is just an example of any positive integer index. This is just a book-keeping index to identify the specific run. The NLO results will be stored in the folder `out/run1`. Different runs usually differ by the model parameters used for the $qg$ and $\bar{q}q$ twist-3 fragmentation correlators. (these are also stored to file inside the `out/run1` folder inside `params.txt`.) It is also possible to execute multiple NLO runs at once by running the bash scripts such as
```bash
./20runs.sh
```
The runs 1, 2 and 3 are fixed models (3 specific scenarios). Any index greater than 3 will produce a random scenario, with parameters sampled from uniform distributions.

For plotting the results (LO + NLO scenarios `run1` + `run2` + `run3`), run the script `python3 plot.py`.
