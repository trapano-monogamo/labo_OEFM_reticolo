#include "RandGen.hpp"
#include <cmath>

RandGen::RandGen(unsigned int seed)
	: seed(seed)
	, a(1664525) , c(1013904223) , m(0b1<<31)
	, n(seed)
{ }

void RandGen::SetA(unsigned int a) { this->a = a; }
void RandGen::SetC(unsigned int c) { this->c = c; }
void RandGen::SetM(unsigned int m) { this->m = m; }

double RandGen::Rand() {
	n = (a*n + c) % m;
	return (double)n / (double)m;
}
double RandGen::Unif(double xmin, double xmax) {
	return xmin + (xmax - xmin) * Rand();
}
double RandGen::Exp(double lambda) {
	return (-1/lambda) * log(1-Rand());
}
double RandGen::Gauss(double mean, double sigma) {
	double s = Rand();
	double t = Rand();
	return mean + sigma * sqrt(-2*log(1-s)) * cos(2*M_PI*t);
}
double RandGen::AR(BaseFunction& f, double a, double b, double max) {
	double x,y;
	do {
		x = Unif(a,b);
		y = max * Rand();
	} while (y > f.eval(x));
	return x;
}
