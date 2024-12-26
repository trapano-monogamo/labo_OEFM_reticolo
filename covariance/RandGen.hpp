#pragma once

#include "BaseFunction.hpp"

class RandGen {
private:
	unsigned int seed;
	unsigned int a,c,m;
	unsigned int n;

public:
	RandGen() = delete;
	RandGen(unsigned int seed);
	~RandGen() = default;

	void SetA(unsigned int);
	void SetC(unsigned int);
	void SetM(unsigned int);

	double Rand();                                // uniform distribution on [0,1)
	double Unif(double xmin, double xmax);        // rand scaled on [xmin,xmax)
	double Exp(double lambda);                    // exponential distribution with parameter k
	double Gauss(double mean, double sigma);      // gaussian centered at mean of width sigma
	double AR( BaseFunction& f
			 , double a
			 , double b
			 , double max);                       // accept-reject method on f
};
