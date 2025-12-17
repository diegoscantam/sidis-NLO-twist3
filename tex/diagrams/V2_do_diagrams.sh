#!/bin/bash
latex V2_W_q2q_n0.tex
dvips V2_W_q2q_n0.dvi -o V2_W_q2q_n0.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2q_n0.pdf V2_W_q2q_n0.ps

latex V2_W_q2g_n0.tex
dvips V2_W_q2g_n0.dvi -o V2_W_q2g_n0.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2g_n0.pdf V2_W_q2g_n0.ps

latex V2_W_g2q_n0.tex
dvips V2_W_g2q_n0.dvi -o V2_W_g2q_n0.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_g2q_n0.pdf V2_W_g2q_n0.ps

latex V2_W_q2q_n1.tex
dvips V2_W_q2q_n1.dvi -o V2_W_q2q_n1.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2q_n1.pdf V2_W_q2q_n1.ps

latex V2_W_q2g_n1.tex
dvips V2_W_q2g_n1.dvi -o V2_W_q2g_n1.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2g_n1.pdf V2_W_q2g_n1.ps

latex V2_W_g2q_n1.tex
dvips V2_W_g2q_n1.dvi -o V2_W_g2q_n1.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_g2q_n1.pdf V2_W_g2q_n1.ps

latex V2_W_q2qg_n0.tex
dvips V2_W_q2qg_n0.dvi -o V2_W_q2qg_n0.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2qg_n0.pdf V2_W_q2qg_n0.ps

latex V2_W_q2qg_n1.tex
dvips V2_W_q2qg_n1.dvi -o V2_W_q2qg_n1.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2qg_n1.pdf V2_W_q2qg_n1.ps

latex V2_W_q2qqbar_n1.tex
dvips V2_W_q2qqbar_n1.dvi -o V2_W_q2qqbar_n1.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/V2_W_q2qqbar_n1.pdf V2_W_q2qqbar_n1.ps