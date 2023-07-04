BMI_LIB = /mnt/sentinel/anastasia/myenv/local/lib
BMI_INC = /mnt/sentinel/anastasia/myenv/local/include
FC = mpif90
CC = gcc 
CFLAGS = -c -fPIC -g -O0 -I. -I${BMI_INC}
# LDFLAGS = -L${BMI_LIB} - Wl, -rpath=${BMI_LIB} -lbmif -lbmiswan -llapack -lblas -lgfortran
LDFLAGS = -lbmif -lbmiswan -L${BMI_LIB} #-llapack -lblas -lgfortran

cython:
	${FC} ${CFLAGS} bmi_interoperability.f90 ${LDFLAGS}
