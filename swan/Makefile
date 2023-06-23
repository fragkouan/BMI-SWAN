include macros.inc

FLAGS_BMI = -c -fPIC -g -O0 -fbounds-check -fcheck=all
TARGET = libswanmodel.so

SWAN_OBJS = \
swmod1.$(EXTO) \
swmod2.$(EXTO) \
SwanSpectPart.$(EXTO) \
m_constants.$(EXTO) \
m_fileio.$(EXTO) \
serv_xnl4v5.$(EXTO) \
mod_xnl4v5.$(EXTO) \
SwanGriddata.$(EXTO) \
SwanGridobjects.$(EXTO) \
SwanCompdata.$(EXTO) \
SdsBabanin.$(EXTO) \
$(NCF_OBJS) \
swan2coh.$(EXTO) \
SwanBmiComm.$(EXTO) \
swanmain.$(EXTO) \
swanpre1.$(EXTO) \
swanpre2.$(EXTO) \
swancom1.$(EXTO) \
swancom2.$(EXTO) \
swancom3.$(EXTO) \
swancom4.$(EXTO) \
swancom5.$(EXTO) \
swanout1.$(EXTO) \
swanout2.$(EXTO) \
swanser.$(EXTO) \
swanparll.$(EXTO) \
SwanReadGrid.$(EXTO) \
SwanReadADCGrid.$(EXTO) \
SwanReadTriangleGrid.$(EXTO) \
SwanReadEasymeshGrid.$(EXTO) \
SwanInitCompGrid.$(EXTO) \
SwanCheckGrid.$(EXTO) \
SwanCreateEdges.$(EXTO) \
SwanGridTopology.$(EXTO) \
SwanGridVert.$(EXTO) \
SwanGridCell.$(EXTO) \
SwanGridFace.$(EXTO) \
SwanPrintGridInfo.$(EXTO) \
SwanFindPoint.$(EXTO) \
SwanPointinMesh.$(EXTO) \
SwanBpntlist.$(EXTO) \
SwanPrepComp.$(EXTO) \
SwanVertlist.$(EXTO) \
SwanCompUnstruc.$(EXTO) \
SwanDispParm.$(EXTO) \
SwanPropvelX.$(EXTO) \
SwanSweepSel.$(EXTO) \
SwanPropvelS.$(EXTO) \
SwanTranspAc.$(EXTO) \
SwanTranspX.$(EXTO) \
SwanDiffPar.$(EXTO) \
SwanGSECorr.$(EXTO) \
SwanGradDepthorK.$(EXTO) \
SwanGradVel.$(EXTO) \
SwanInterpolatePoint.$(EXTO) \
SwanInterpolateAc.$(EXTO) \
SwanInterpolateOutput.$(EXTO) \
SwanConvAccur.$(EXTO) \
SwanConvStopc.$(EXTO) \
SwanThreadBounds.$(EXTO) \
SwanFindObstacles.$(EXTO) \
SwanCrossObstacle.$(EXTO) \
SwanComputeForce.$(EXTO) \
SwanIntgratSpc.$(EXTO) \
SwanBndStruc.$(EXTO) \
SwanReadfort18.$(EXTO) \
SwanPunCollect.$(EXTO) \
SwanSumOverNodes.$(EXTO) \
SwanMinOverNodes.$(EXTO) \
SwanMaxOverNodes.$(EXTO) \
ocpids.$(EXTO) \
ocpcre.$(EXTO) \
ocpmix.$(EXTO)

.PHONY: help clean clobber

# Final build step
compile: $(SWAN_OBJS)
	$(F90_MPI) $(SWAN_OBJS) -shared -o $(TARGET)

files:
	perl switch.pl $(swch) -mpi *.ftn *.ftn90

config:
	@perl platform.pl

clean:
	$(RM) *.$(EXTO) *.mod

clobber:
	$(RM) *.$(EXTO) *.mod *.so *.f *.for *.f90 $(SWAN_EXE) $(HCAT_EXE) $(UNHCAT_EXE) $(SWAN_LIB)

mpi:clobber files compile

# Build step for f
%.o: %.f
	$(F90_MPI) $< $(FLAGS_OPT) $(FLAGS_MSC) $(FLAGS_MPI) $(FLAGS_DYN) \
	$(FLAGS_BMI) -o $@

%.o: %.f90
	$(F90_MPI) $< $(FLAGS_OPT) $(FLAGS90_MSC) $(FLAGS_MPI) $(FLAGS_BMI) -o $@
