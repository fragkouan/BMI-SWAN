BMI_LIB = /home/s2006658/myenv/local/lib
BMI_INC = /home/s2006658/myenv/local/include
FC = mpif90
CC = gcc 
CFLAGS = -c -fPIC -g -O0 -I. -I${BMI_INC}
# LDFLAGS = -L${BMI_LIB} - Wl, -rpath=${BMI_LIB} -lbmif -lbmiswan -llapack -lblas -lgfortran
LDFLAGS = -lbmif -lbmiswan -L${BMI_LIB} #-llapack -lblas -lgfortran

cython:
	${FC} ${CFLAGS} bmi_interoperability.f90 ${LDFLAGS}
