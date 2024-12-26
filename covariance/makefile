ROOT_CFALGS := `root-config --cflags`
ROOT_INCS := `root-config --incdir`
ROOT_LIBS := `root-config --libs`

PROG := covariance

compile: *.o
	g++ $^ -o ${PROG} ${ROOT_LIBS}

%.o: %.cpp
	g++ -c $^ ${ROOT_CFALGS} -I${ROOT_INCS}

.PHONY: clean

clean:
	-rm *.o
	-rm ${PROG}
