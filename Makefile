prefix		= /sw/data/tools/dship2nc/SLES_11_SP2/

# POSIX shell.  On some platforms it is not /bin/sh.
SHELL		= /usr/bin/sh

#the netcdf library
NETCDF_ROOT=/sw/data/netcdf/SLES_11_SP2/4.3.3.1-gnu46
NETCDF_LIB=${NETCDF_ROOT}/lib
NETCDF_INC=${NETCDF_ROOT}/include
HDF_LIB=/sw/data/hdf5/SLES_11_SP2/1.8.14-gnu46/lib
SZIP_LIB=/sw/data/szip/SLES_11_SP2/2.1-gcc46/lib
LIBS=-lnetcdf -lnetcdff -Wl,-rpath,${NETCDF_LIB} -lm
STATIC_LIBS=-Wl,-Bstatic -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lsz -Wl,-Bdynamic -lz -lrt -lm -lcurl -ldl

# The compiler 
FC = gfortran-4.6
CFLAGS=-O2 -I$(NETCDF_INC) -I/usr/include
LDFLAGS= -L$(NETCDF_LIB) -L$(HDF_LIB) -L$(SZIP_LIB) $(LIBS) 
STATIC_LDFLAGS= -L$(NETCDF_LIB) -L$(HDF_LIB) -L$(SZIP_LIB) $(STATIC_LIBS) 
OBJ = dshipmod.o dship2nc.o

bindir		= ${prefix}/bin

%o:     %f90
	$(FC) -c -o $@ $< $(CFLAGS)
        
dship2nc_dynamic:        $(OBJ)      
	$(FC) -o $@ $^ $(CFLAGS) $(LDFLAGS); mv $@ dship2nc

dship2nc_static:        $(OBJ)      
	$(FC) -o $@ $^ $(CFLAGS) $(STATIC_LDFLAGS); mv $@ dship2nc

.PHONY: clean

clean:
	rm -f *.o *mod *~ core dship2nc 

install:
	$(MAKE); mkdir $(bindir); cp dship2nc $(bindir) 
