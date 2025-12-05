#!/bin/bash
latex phi.tex
dvips phi.dvi -o phi.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/phi.pdf phi.ps

latex delta_i.tex
dvips delta_i.dvi -o delta_i.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/delta_i.pdf delta_i.ps

latex delta_k.tex
dvips delta_k.dvi -o delta_k.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/delta_k.pdf delta_k.ps

latex delta_d.tex
dvips delta_d.dvi -o delta_d.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/delta_d.pdf delta_d.ps

latex SIDIS.tex
dvips SIDIS.dvi -o SIDIS.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/SIDIS.pdf SIDIS.ps

latex qgq_corr.tex
dvips qgq_corr.dvi -o qgq_corr.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/qgq_corr.pdf qgq_corr.ps

latex RealNLOTw3.tex
dvips RealNLOTw3.dvi -o RealNLOTw3.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/RealNLOTw3.pdf RealNLOTw3.ps

latex VirtNLOTw3.tex
dvips VirtNLOTw3.dvi -o VirtNLOTw3.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/VirtNLOTw3.pdf VirtNLOTw3.ps

latex VirtNLOTw3_reduced.tex
dvips VirtNLOTw3_reduced.dvi -o VirtNLOTw3_reduced.ps
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dALLOWPSTRANSPARENCY -dCompatibilityLevel=1.4 -sOutputFile=../fig/VirtNLOTw3_reduced.pdf VirtNLOTw3_reduced.ps