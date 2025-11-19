import math
import numpy as np
import statistics
import random
pi = math.pi
import pandas as pd
import matplotlib as mpl
import scipy
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
plt.ion()
#from matplotlib.legend_handler import HandlerTuple
#from matplotlib.legend_handler import HandlerLine2D
#from matplotlib.legend_handler import LandlerTuple
from matplotlib.colors import LogNorm
from scipy.optimize import curve_fit
from scipy import signal
import re

mpl.rcdefaults()
mpl.style.use('default')

# Comment tLis to not Lave latex plotsx

mpl.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'font.family': 'serif',
    'text.usetex': True,
    'pgf.rcfonts': False,
})

# pi+ z dependence HERMES 9066
df9066 = pd.read_csv('HERMESdata/9066.csv',sep=',', decimal = '.')

NN = 6
Q2_z_pp = df9066['Q2'][0:NN].mean()
x_z_pp = df9066['x'][0:NN].mean()
y_z_pp = df9066['y'][0:NN].mean()
meas_zp_z = df9066['z']
meas_zp_value = df9066['value']
meas_zp_dvalue = np.sqrt( df9066['stat_err_u']**2+df9066['sys_err_u']**2 )

# pi+ x dependence HERMES 9055
df9055 = pd.read_csv('HERMESdata/9055.csv',sep=',', decimal = '.')

cut = 1
Q2_x_pp = df9055['Q2'][cut:].mean()
z_x_pp = df9055['z'][cut:].mean()
y_x_pp = df9055['y'][cut:].mean()
meas_xp_x = df9055['x']
meas_xp_value = df9055['value']
meas_xp_dvalue = np.sqrt(df9055['stat_err_u']**2+df9055['sys_err_u']**2 )

# pi- z dependence HERMES 10032
df10032 = pd.read_csv('HERMESdata/10032.csv',sep=',', decimal = '.')

NN = 6
Q2_z_pm = df10032['Q2'][0:NN].mean()
x_z_pm = df10032['x'][0:NN].mean()
y_z_pm = df10032['y'][0:NN].mean()
meas_zm_z = df10032['z']
meas_zm_value = df10032['value']
meas_zm_dvalue = np.sqrt(df10032['stat_err_u']**2 +df10032['sys_err_u']**2  )

# pi- x dependence HERMES 10021
df10021 = pd.read_csv('HERMESdata/10021.csv',sep=',', decimal = '.')
cut = 1
Q2_x_pm = df10021['Q2'][cut:].mean()
z_x_pm = df10021['z'][cut:].mean()
y_x_pm = df10021['y'][cut:].mean()
meas_xm_x = df10021['x']
meas_xm_value = df10021['value']
meas_xm_dvalue = np.sqrt( df10021['stat_err_u']**2+df10021['sys_err_u']**2  )



############################### LO #############################
dfxp = pd.read_csv('out/AUTx_LO_pp.txt',header=None,sep=' ', decimal = '.')
dfxm = pd.read_csv('out/AUTx_LO_pm.txt',header=None,sep=' ', decimal = '.')
dfzp = pd.read_csv('out/AUTz_LO_pp.txt',header=None,sep=' ', decimal = '.')
dfzm = pd.read_csv('out/AUTz_LO_pm.txt',header=None,sep=' ', decimal = '.')

mycmap=["#003a7d","#008dff","#ff73b6","#c701ff","#4ecb8d","#ff9d3a","#f9e858","#d83034"]


fs = 13
w = 10
h = 3



fig, axs = plt.subplots(1,2, figsize= (w,h), sharey = False)

#tl1 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$z_h=' + str(zh[idx_zh]) + '$'
#tl2 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$x_B=' + str(xB[idx_xB]) + '$'


axs[0].plot(dfxp[0], dfxp[4], color=mycmap[7],label='$\\pi^+$, LO')
axs[0].errorbar(meas_xp_x,meas_xp_value,meas_xp_dvalue, linestyle='none',fmt='d',mfc="k",mec='k',ecolor='k',ms=6, capsize=3, label='HERMES data $\\pi^+$')
axs[0].errorbar(meas_xp_x[0],meas_xp_value[0],meas_xp_dvalue[0], linestyle='none',fmt='d',mfc='lightgrey',mec='k',ecolor='k',ms=6, capsize=3)

axs[0].plot(dfxm[0], dfxm[4], color=mycmap[0],label='$\\pi^-$, LO')
axs[0].errorbar(meas_xm_x,meas_xm_value,meas_xm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=5, capsize=3,label='HERMES data $\\pi^-$')
axs[0].errorbar(meas_xm_x[0],meas_xm_value[0],meas_xm_dvalue[0], linestyle='none',fmt='o',mfc='lightgrey',mec='k',ecolor='k',ms=5, capsize=3)

axs[0].set_xlabel('$x_B$', fontsize=14)
axs[0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=14)
axs[0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=14)

axs[0].tick_params(axis='both', which='minor', labelsize=12)       
axs[0].tick_params(axis='both', which='minor', labelsize=12)  

axs[1].plot(dfzp[2], dfzp[4], color=mycmap[7],label='$\\pi^+$, LO')
axs[1].errorbar(meas_zp_z,meas_zp_value,meas_zp_dvalue, linestyle='none',fmt='d',mfc='k',mec='k',ecolor='k',ms=6, capsize=3)
axs[1].errorbar(meas_zp_z[7:10],meas_zp_value[7:10],meas_zp_dvalue[7:10], linestyle='none',fmt='d',mfc='lightgrey',mec='k',ecolor='k',ms=6, capsize=3)



axs[1].plot(dfzm[2], dfzm[4], color=mycmap[0],label='$\\pi^-$, LO')
axs[1].errorbar(meas_zm_z,meas_zm_value,meas_zm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=5, capsize=3)
axs[1].errorbar(meas_zm_z[7:10],meas_zm_value[7:10],meas_zm_dvalue[7:10], linestyle='none',fmt='o',mfc='lightgrey',mec='k',ecolor='k',ms=5, capsize=3)


#axs[1].vlines(0.7, -0.2,+0.2, color='grey', linestyle='dashed', alpha=0.5)
#axs[1].vlines(0.2, -0.2,+0.2, color='grey', linestyle='dashed', alpha=0.5)

axs[0].set_xlim(0.01 ,0.3)
axs[0].set_ylim(-0.045,0.04)
axs[1].set_xlim(0.15 ,.99)
axs[1].set_ylim(-0.1,0.15)
axs[1].set_xlabel('$z_h$', fontsize=14)

axs[0].set_xlabel('$x_B$', fontsize=14)
#axs[1].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=14)


#axs[0].tick_params(axis='both', which='minor', labelsize=fs)       
#axs[1].tick_params(axis='both', which='minor', labelsize=fs)  
axs[0].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=-100)
axs[1].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=-100)

#axs[0].grid(True)

axs[0].tick_params(axis='both', which='major', labelsize=fs) 
axs[1].tick_params(axis='both', which='major', labelsize=fs)  
#axs[1].tick_params(axis='both', which='minor', labelsize=12)  

#axs[0].text(0.05,0.008,tl1)
#axs[1].text(0.05,0.008,tl2)

axs[0].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=2)
#axs[1,0].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)
#axs[1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs, ncol=1)
#axs[1,1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)

plt.savefig('./out/A_UT_LO.pdf', dpi=300, bbox_inches='tight')


########################################### NLO ##################################################

ids = [1,2,3]

dfxpNLO= list()
dfxmNLO= list()
dfzpNLO= list()
dfzmNLO= list()





#mycmap=["#003a7d","#008dff","#8acbff","#c701ff","#4ecb8d","#ff9d3a","#e08e8f","#d83034"]

mycmap=["#8cc5e3","#1a80bb","#d8a6a6","#a00000","#9fc8c8","#298c8c"]

for k in ids:

    fnamexp = 'out/run'+str(k)+'/AUTx_NLO_pp.txt'
    fnamexm = 'out/run'+str(k)+'/AUTx_NLO_pm.txt'
    fnamezp = 'out/run'+str(k)+'/AUTz_NLO_pp.txt'
    fnamezm = 'out/run'+str(k)+'/AUTz_NLO_pm.txt'

    dfxpNLO.append(pd.read_csv(fnamexp,header=None,sep=' ', decimal = '.',engine='python',skiprows = 1))
    dfxmNLO.append(pd.read_csv(fnamexm,header=None,sep=' ', decimal = '.',engine='python',skiprows = 1))
    dfzpNLO.append(pd.read_csv(fnamezp,header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3))
    dfzmNLO.append(pd.read_csv(fnamezm,header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3))


    


dfxp = pd.read_csv('out/AUTx_LO_pp.txt',header=None,sep=' ', decimal = '.',engine='python',skiprows = 1)
dfxm = pd.read_csv('out/AUTx_LO_pm.txt',header=None,sep=' ', decimal = '.',engine='python',skiprows = 1)
dfzp = pd.read_csv('out/AUTz_LO_pp.txt',header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3)
dfzm = pd.read_csv('out/AUTz_LO_pm.txt',header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3)



fs = 13
w = 10
h = 5


fig, axs = plt.subplots(2,2, figsize= (w,h),sharey='row',sharex='col')

#tl1 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$z_h=' + str(zh[idx_zh]) + '$'
#tl2 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$x_B=' + str(xB[idx_xB]) + '$'


#fig.suptitle("PRELIMINARY",color='r', fontsize=fs+5, bbox=dict(facecolor='w', edgecolor='k'))
#fig.subplots_adjust(top=.9) 

# xp

l1 =axs[0,0].plot(dfxp[0], dfxp[4], color='k',linestyle='dashed',zorder=90)
l2=axs[0,0].plot(dfxpNLO[0][0], dfxpNLO[0][4], color =mycmap[1],linestyle='solid',linewidth=2,alpha=1,zorder=100)
l2shade=axs[0,0].fill_between(dfxpNLO[0][0], dfxpNLO[0][4],dfxpNLO[0][5],color =mycmap[0],alpha=1)
axs[0,0].fill_between(dfxpNLO[0][0], dfxpNLO[0][4],dfxpNLO[0][6],color =mycmap[0],alpha=1)

l3=axs[0,0].plot(dfxpNLO[1][0], dfxpNLO[1][4], color =mycmap[3],linestyle='solid',linewidth=2,alpha=1)
l3shade=axs[0,0].fill_between(dfxpNLO[1][0], dfxpNLO[1][4],dfxpNLO[1][5],color =mycmap[2],alpha=1)
axs[0,0].fill_between(dfxpNLO[1][0], dfxpNLO[1][4],dfxpNLO[1][6],color =mycmap[2],alpha=1)


l4=axs[0,0].plot(dfxpNLO[2][0], dfxpNLO[2][4], color =mycmap[5],linestyle='solid',linewidth=2,alpha=1)
l4shade=axs[0,0].fill_between(dfxpNLO[2][0], dfxpNLO[2][4],dfxpNLO[2][5],color =mycmap[4],alpha=1)
axs[0,0].fill_between(dfxpNLO[2][0], dfxpNLO[2][4],dfxpNLO[2][6],color =mycmap[4],alpha=1)

l5=axs[0,0].errorbar(meas_xp_x,meas_xp_value,meas_xp_dvalue, linestyle='none',fmt='d',mfc='w',mec='k',ecolor='k',ms=7, capsize=3, zorder=101)

# xm
axs[1,0].plot(dfxmNLO[0][0], dfxmNLO[0][4], color =mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[1,0].fill_between(dfxmNLO[0][0], dfxmNLO[0][4],dfxmNLO[0][5],color =mycmap[0],alpha=1)
axs[1,0].fill_between(dfxmNLO[0][0], dfxmNLO[0][4],dfxmNLO[0][6],color =mycmap[0],alpha=1)

axs[1,0].plot(dfxmNLO[1][0], dfxmNLO[1][4], color =mycmap[3] ,linestyle='solid',linewidth=2,alpha=1)
axs[1,0].fill_between(dfxmNLO[1][0], dfxmNLO[1][4],dfxmNLO[1][5],color =mycmap[2],alpha=1)
axs[1,0].fill_between(dfxmNLO[1][0], dfxmNLO[1][4],dfxmNLO[1][6],color =mycmap[2],alpha=1)

axs[1,0].plot(dfxmNLO[2][0], dfxmNLO[2][4], color =mycmap[5] ,linestyle='solid',linewidth=2,alpha=1)
axs[1,0].fill_between(dfxmNLO[2][0], dfxmNLO[2][4],dfxmNLO[2][5],color =mycmap[4],alpha=1)
axs[1,0].fill_between(dfxmNLO[2][0], dfxmNLO[2][4],dfxmNLO[2][6],color =mycmap[4],alpha=1)

axs[1,0].plot(dfxm[0], dfxm[4], color='k',linestyle='dashed')
axs[1,0].errorbar(meas_xm_x,meas_xm_value,meas_xm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=6, capsize=3,zorder=101,label='HERMES $\\pi^-$')

#axs[0,0].set_xlabel('$x_B$', fontsize=fs)
axs[0,0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=fs+2)
axs[1,0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=fs+2)

# zp
axs[0,1].plot(dfzp[2], dfzp[4], color='k',linestyle='dashed',zorder=90)
axs[0,1].plot(dfzpNLO[0][2], dfzpNLO[0][4], color = mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[0,1].fill_between(dfzpNLO[0][2], dfzpNLO[0][4],dfzpNLO[0][5],color =mycmap[0],alpha=1)
axs[0,1].fill_between(dfzpNLO[0][2], dfzpNLO[0][4],dfzpNLO[0][6],color =mycmap[0],alpha=1)

axs[0,1].plot(dfzpNLO[1][2], dfzpNLO[1][4], color =mycmap[3] ,linestyle='solid',linewidth=2,alpha=1)
axs[0,1].fill_between(dfzpNLO[1][2], dfzpNLO[1][4],dfzpNLO[1][5],color =mycmap[2],alpha=1)
axs[0,1].fill_between(dfzpNLO[1][2], dfzpNLO[1][4],dfzpNLO[1][6],color =mycmap[2],alpha=1)

axs[0,1].plot(dfzpNLO[2][2], dfzpNLO[2][4], color =mycmap[5] ,linestyle='solid',linewidth=2,alpha=1)
axs[0,1].fill_between(dfzpNLO[2][2], dfzpNLO[2][4],dfzpNLO[2][5],color =mycmap[4],alpha=1)
axs[0,1].fill_between(dfzpNLO[2][2], dfzpNLO[2][4],dfzpNLO[2][6],color =mycmap[4],alpha=1)


axs[0,1].errorbar(meas_zp_z,meas_zp_value,meas_zp_dvalue, linestyle='none',fmt='d',mfc='w',mec='k',ecolor='k',ms=7, capsize=3,zorder=101)

# zm
axs[1,1].plot(dfzmNLO[0][2], dfzmNLO[0][4], color = mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[1,1].fill_between(dfzmNLO[0][2], dfzmNLO[0][4],dfzmNLO[0][5],color =mycmap[0],alpha=1)
axs[1,1].fill_between(dfzmNLO[0][2], dfzmNLO[0][4],dfzmNLO[0][6],color =mycmap[0],alpha=1)

axs[1,1].plot(dfzmNLO[1][2], dfzmNLO[1][4], color =mycmap[3],linestyle='solid',linewidth=2,alpha=1)
axs[1,1].fill_between(dfzmNLO[1][2], dfzmNLO[1][4],dfzmNLO[1][5],color =mycmap[2],alpha=1)
axs[1,1].fill_between(dfzmNLO[1][2], dfzmNLO[1][4],dfzmNLO[1][6],color =mycmap[2],alpha=1)

axs[1,1].plot(dfzmNLO[2][2], dfzmNLO[2][4], color =mycmap[5],linestyle='solid',linewidth=2,alpha=1)
axs[1,1].fill_between(dfzmNLO[2][2], dfzmNLO[2][4],dfzmNLO[2][5],color =mycmap[4],alpha=1)
axs[1,1].fill_between(dfzmNLO[2][2], dfzmNLO[2][4],dfzmNLO[2][6],color =mycmap[4],alpha=1)

axs[1,1].plot(dfzm[2], dfzm[4], color='k',linestyle='dashed')
axs[1,1].errorbar(meas_zm_z,meas_zm_value,meas_zm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=6, capsize=3,zorder=101)


axs[0,0].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[0,1].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[1,0].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[1,1].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)

axs[0,0].set_xlim(0.04 ,0.27)
axs[0,0].set_ylim(-0.02,0.04)
axs[1,0].set_xlim(0.04 ,0.27)
axs[1,0].set_ylim(-0.08,0.05)
axs[0,1].set_xlim(0.2 ,.7)
axs[0,1].set_ylim(-0.02,0.04)
axs[1,1].set_xlim(0.2 ,.7)
axs[1,1].set_ylim(-0.08,0.05)

axs[0,0].tick_params(axis='y', which='major', labelsize=fs) 
#axs[0,0].tick_params(axis='x', which='major', labelsize=0)
axs[1,0].tick_params(axis='both', which='major', labelsize=fs)  

#axs[0,1].tick_params(axis='both', which='major', labelsize=0)       
axs[1,1].tick_params(axis='both', which='major', labelsize=fs)  

axs[1,1].set_xlabel('$z_h$', fontsize=fs)

axs[1,0].set_xlabel('$x_B$', fontsize=fs)

#handles=[l1,(l2,l2shade),(l3,l3shade),(l4,l4shade),l5]
#labels = ['LO','NLO $S_1$','NLO $S_2$','NLO $S_3$','HERMES $\\pi^+$']

shadel2 = axs[0,0].fill(np.nan, np.nan, color=l2shade.get_facecolor(), linewidth=0.)
shadel3 = axs[0,0].fill(np.nan, np.nan, color=l3shade.get_facecolor(), linewidth=0.)
shadel4 = axs[0,0].fill(np.nan, np.nan, color=l4shade.get_facecolor(), linewidth=0.)

handles=[l1[0],(shadel2[0],l2[0]),(shadel3[0],l3[0]),(shadel4[0],l4[0]),l5]
lbls = ['LO','NLO $S_1$','NLO $S_2$','NLO $S_3$','HERMES $\\pi^+$']

axs[0,0].legend(handles,lbls,fancybox= True, framealpha=0., loc='upper center', fontsize = fs-2, ncol=3,columnspacing=.8)
axs[1,0].legend(fancybox= True, framealpha=0., loc='lower left', fontsize = fs-2, ncol=1)

axs[0,1].text(0.22,0.03,'$\\mu \\in [Q/2,2 Q]$',fontsize=fs-2)
#axs[0,1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)
#axs[1,1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)

plt.tight_layout()
plt.savefig('./out/HERMES_A_UT_NLO.pdf', dpi=300, bbox_inches='tight')


########################################### NLO EIC ##################################################
ids = [1,2,3]

dfxpNLO= list()
dfxmNLO= list()
dfzpNLO= list()
dfzmNLO= list()





#mycmap=["#003a7d","#008dff","#8acbff","#c701ff","#4ecb8d","#ff9d3a","#e08e8f","#d83034"]

mycmap=["#8cc5e3","#1a80bb","#d8a6a6","#a00000","#9fc8c8","#298c8c"]

for k in ids:

    fnamexp = 'out/run'+str(k)+'/AUTx_NLO_pp_EIC.txt'
    fnamexm = 'out/run'+str(k)+'/AUTx_NLO_pm_EIC.txt'
    fnamezp = 'out/run'+str(k)+'/AUTz_NLO_pp_EIC.txt'
    fnamezm = 'out/run'+str(k)+'/AUTz_NLO_pm_EIC.txt'

    dfxpNLO.append(pd.read_csv(fnamexp,header=None,sep=' ', decimal = '.',engine='python',skiprows = 1))
    dfxmNLO.append(pd.read_csv(fnamexm,header=None,sep=' ', decimal = '.',engine='python',skiprows = 1))
    dfzpNLO.append(pd.read_csv(fnamezp,header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3))
    dfzmNLO.append(pd.read_csv(fnamezm,header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3))


    


dfxp = pd.read_csv('out/AUTx_LO_pp_EIC.txt',header=None,sep=' ', decimal = '.',engine='python',skiprows = 1)
dfxm = pd.read_csv('out/AUTx_LO_pm_EIC.txt',header=None,sep=' ', decimal = '.',engine='python',skiprows = 1)
dfzp = pd.read_csv('out/AUTz_LO_pp_EIC.txt',header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3)
dfzm = pd.read_csv('out/AUTz_LO_pm_EIC.txt',header=None,sep=' ', decimal = '.',engine='python',skipfooter = 3)



fs = 13
w = 10
h = 5


fig, axs = plt.subplots(2,2, figsize= (w,h),sharey='row',sharex='col')

#tl1 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$z_h=' + str(zh[idx_zh]) + '$'
#tl2 = '$s='+str(s) +'\\,\\rm{GeV}^2$\n'+'$y=' + str(y)+'$\n$x_B=' + str(xB[idx_xB]) + '$'


#fig.suptitle("PRELIMINARY",color='r', fontsize=fs+5, bbox=dict(facecolor='w', edgecolor='k'))
#fig.subplots_adjust(top=.9) 

# xp

l1 =axs[0,0].plot(dfxp[0], dfxp[4], color='k',linestyle='dashed',   linewidth=2,zorder=90)
l2=axs[0,0].plot(dfxpNLO[0][0], dfxpNLO[0][4], color =mycmap[1],linestyle='solid',linewidth=2,alpha=1,zorder=100)
l2shade=axs[0,0].fill_between(dfxpNLO[0][0], dfxpNLO[0][4],dfxpNLO[0][5],color =mycmap[0],alpha=1)
axs[0,0].fill_between(dfxpNLO[0][0], dfxpNLO[0][4],dfxpNLO[0][6],color =mycmap[0],alpha=1)

l3=axs[0,0].plot(dfxpNLO[1][0], dfxpNLO[1][4], color =mycmap[3],linestyle='solid',linewidth=2,alpha=1)
l3shade=axs[0,0].fill_between(dfxpNLO[1][0], dfxpNLO[1][4],dfxpNLO[1][5],color =mycmap[2],alpha=1)
axs[0,0].fill_between(dfxpNLO[1][0], dfxpNLO[1][4],dfxpNLO[1][6],color =mycmap[2],alpha=1)


l4=axs[0,0].plot(dfxpNLO[2][0], dfxpNLO[2][4], color =mycmap[5],linestyle='solid',linewidth=2,alpha=1)
l4shade=axs[0,0].fill_between(dfxpNLO[2][0], dfxpNLO[2][4],dfxpNLO[2][5],color =mycmap[4],alpha=1)
axs[0,0].fill_between(dfxpNLO[2][0], dfxpNLO[2][4],dfxpNLO[2][6],color =mycmap[4],alpha=1)

#l5=axs[0,0].errorbar(meas_xp_x,meas_xp_value,meas_xp_dvalue, linestyle='none',fmt='d',mfc='w',mec='k',ecolor='k',ms=7, capsize=3, zorder=101)

# xm
axs[1,0].plot(dfxmNLO[0][0], dfxmNLO[0][4], color =mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[1,0].fill_between(dfxmNLO[0][0], dfxmNLO[0][4],dfxmNLO[0][5],color =mycmap[0],alpha=1)
axs[1,0].fill_between(dfxmNLO[0][0], dfxmNLO[0][4],dfxmNLO[0][6],color =mycmap[0],alpha=1)

axs[1,0].plot(dfxmNLO[1][0], dfxmNLO[1][4], color =mycmap[3] ,linestyle='solid',linewidth=2,alpha=1)
axs[1,0].fill_between(dfxmNLO[1][0], dfxmNLO[1][4],dfxmNLO[1][5],color =mycmap[2],alpha=1)
axs[1,0].fill_between(dfxmNLO[1][0], dfxmNLO[1][4],dfxmNLO[1][6],color =mycmap[2],alpha=1)

axs[1,0].plot(dfxmNLO[2][0], dfxmNLO[2][4], color =mycmap[5] ,linestyle='solid',linewidth=2,alpha=1)
axs[1,0].fill_between(dfxmNLO[2][0], dfxmNLO[2][4],dfxmNLO[2][5],color =mycmap[4],alpha=1)
axs[1,0].fill_between(dfxmNLO[2][0], dfxmNLO[2][4],dfxmNLO[2][6],color =mycmap[4],alpha=1)

axs[1,0].plot(dfxm[0], dfxm[4], color='k',linestyle='dashed',linewidth=2)
#axs[1,0].errorbar(meas_xm_x,meas_xm_value,meas_xm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=6, capsize=3,zorder=101,label='HERMES $\\pi^-$')

#axs[0,0].set_xlabel('$x_B$', fontsize=fs)
axs[0,0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=fs+2)
axs[1,0].set_ylabel('$$A_{UT}^{\\sin \\phi_S }$$', fontsize=fs+2)

# zp
axs[0,1].plot(dfzp[2], dfzp[4], color='k',linestyle='dashed',zorder=90,linewidth=2)
axs[0,1].plot(dfzpNLO[0][2], dfzpNLO[0][4], color = mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[0,1].fill_between(dfzpNLO[0][2], dfzpNLO[0][4],dfzpNLO[0][5],color =mycmap[0],alpha=1)
axs[0,1].fill_between(dfzpNLO[0][2], dfzpNLO[0][4],dfzpNLO[0][6],color =mycmap[0],alpha=1)

axs[0,1].plot(dfzpNLO[1][2], dfzpNLO[1][4], color =mycmap[3] ,linestyle='solid',linewidth=2,alpha=1)
axs[0,1].fill_between(dfzpNLO[1][2], dfzpNLO[1][4],dfzpNLO[1][5],color =mycmap[2],alpha=1)
axs[0,1].fill_between(dfzpNLO[1][2], dfzpNLO[1][4],dfzpNLO[1][6],color =mycmap[2],alpha=1)

axs[0,1].plot(dfzpNLO[2][2], dfzpNLO[2][4], color =mycmap[5] ,linestyle='solid',linewidth=2,alpha=1)
axs[0,1].fill_between(dfzpNLO[2][2], dfzpNLO[2][4],dfzpNLO[2][5],color =mycmap[4],alpha=1)
axs[0,1].fill_between(dfzpNLO[2][2], dfzpNLO[2][4],dfzpNLO[2][6],color =mycmap[4],alpha=1)


#axs[0,1].errorbar(meas_zp_z,meas_zp_value,meas_zp_dvalue, linestyle='none',fmt='d',mfc='w',mec='k',ecolor='k',ms=7, capsize=3,zorder=101)

# zm
axs[1,1].plot(dfzmNLO[0][2], dfzmNLO[0][4], color = mycmap[1] ,linestyle='solid',linewidth=2,alpha=1,zorder=100)
axs[1,1].fill_between(dfzmNLO[0][2], dfzmNLO[0][4],dfzmNLO[0][5],color =mycmap[0],alpha=1)
axs[1,1].fill_between(dfzmNLO[0][2], dfzmNLO[0][4],dfzmNLO[0][6],color =mycmap[0],alpha=1)

axs[1,1].plot(dfzmNLO[1][2], dfzmNLO[1][4], color =mycmap[3],linestyle='solid',linewidth=2,alpha=1)
axs[1,1].fill_between(dfzmNLO[1][2], dfzmNLO[1][4],dfzmNLO[1][5],color =mycmap[2],alpha=1)
axs[1,1].fill_between(dfzmNLO[1][2], dfzmNLO[1][4],dfzmNLO[1][6],color =mycmap[2],alpha=1)

axs[1,1].plot(dfzmNLO[2][2], dfzmNLO[2][4], color =mycmap[5],linestyle='solid',linewidth=2,alpha=1)
axs[1,1].fill_between(dfzmNLO[2][2], dfzmNLO[2][4],dfzmNLO[2][5],color =mycmap[4],alpha=1)
axs[1,1].fill_between(dfzmNLO[2][2], dfzmNLO[2][4],dfzmNLO[2][6],color =mycmap[4],alpha=1)

axs[1,1].plot(dfzm[2], dfzm[4], color='k',linestyle='dashed',linewidth=2)
#axs[1,1].errorbar(meas_zm_z,meas_zm_value,meas_zm_dvalue, linestyle='none',fmt='o',mfc='w',mec='k',ecolor='k',ms=6, capsize=3,zorder=101)


axs[0,0].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[0,1].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[1,0].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)
axs[1,1].hlines(0.0, 0,1, color='grey',alpha=0.5,linestyle='dotted', zorder=+100)

axs[0,0].set_xlim(0.04 ,0.27)
axs[0,0].set_ylim(-0.001,0.002)
axs[1,0].set_xlim(0.04 ,0.27)
axs[1,0].set_ylim(-0.003,0.001)
axs[0,1].set_xlim(0.2 ,.7)
#axs[0,1].set_ylim(-0.02,0.04)
axs[1,1].set_xlim(0.2 ,.7)
#axs[1,1].set_ylim(-0.08,0.05)

axs[0,0].tick_params(axis='y', which='major', labelsize=fs) 
#axs[0,0].tick_params(axis='x', which='major', labelsize=0)
axs[1,0].tick_params(axis='both', which='major', labelsize=fs)  

#axs[0,1].tick_params(axis='both', which='major', labelsize=0)       
axs[1,1].tick_params(axis='both', which='major', labelsize=fs)  

axs[1,1].set_xlabel('$z_h$', fontsize=fs)

axs[1,0].set_xlabel('$x_B$', fontsize=fs)

#handles=[l1,(l2,l2shade),(l3,l3shade),(l4,l4shade),l5]
#labels = ['LO','NLO $S_1$','NLO $S_2$','NLO $S_3$','HERMES $\\pi^+$']

shadel2 = axs[0,0].fill(np.nan, np.nan, color=l2shade.get_facecolor(), linewidth=0.)
shadel3 = axs[0,0].fill(np.nan, np.nan, color=l3shade.get_facecolor(), linewidth=0.)
shadel4 = axs[0,0].fill(np.nan, np.nan, color=l4shade.get_facecolor(), linewidth=0.)

handles=[l1[0],(shadel2[0],l2[0]),(shadel3[0],l3[0]),(shadel4[0],l4[0])]
lbls = ['LO','NLO $S_1$','NLO $S_2$','NLO $S_3$']

axs[0,0].legend(handles,lbls,fancybox= True, framealpha=0., loc='upper center', fontsize = fs-2, ncol=2,columnspacing=.8)
#axs[1,0].legend(fancybox= True, framealpha=0., loc='lower left', fontsize = fs-2, ncol=1)

axs[0,1].text(0.22,0.0015,'$\\mu \\in [Q/2,2 Q]$',fontsize=fs-2)
axs[0,1].text(0.22,-0.0008,'EIC $\\sqrt{s}=100\\,\\rm{GeV}$ $\\pi^+$',fontsize=fs-2)
axs[0,0].text(0.05,-0.0008,'EIC $\\sqrt{s}=100\\,\\rm{GeV}$ $\\pi^+$',fontsize=fs-2)
axs[1,1].text(0.22,-0.0028,'EIC $\\sqrt{s}=100\\,\\rm{GeV}$ $\\pi^-$',fontsize=fs-2)
axs[1,0].text(0.05,-0.0028,'EIC $\\sqrt{s}=100\\,\\rm{GeV}$ $\\pi^-$',fontsize=fs-2)

#axs[0,1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)
#axs[1,1].legend(fancybox= True, framealpha=0., loc='best', fontsize = fs-3, ncol=1)

plt.tight_layout()
plt.savefig('./out/EIC_A_UT_NLO.pdf', dpi=300, bbox_inches='tight')
plt.show()