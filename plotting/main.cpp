#include <iostream>
#include <fstream>
#include <vector>

#include "TApplication.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TLegend.h"
#include "TAxis.h"
#include "TMath.h"

using namespace std;

struct Measure
{
	double x, ex, y, ey;

	friend istream &operator>>(istream &in, Measure &m)
	{
		in >> m.x >> m.y >> m.ex >> m.ey;
		return in;
	}
	friend ostream &operator<<(ostream &out, Measure &m)
	{
		out << "{" << m.x << " +- " << m.ex
			<< ", " << m.y << " +- " << m.ey << "}";
		return out;
	}
};

vector<Measure> read_file(const char *filename, double& expected)
{
	vector<Measure> res;
	ifstream in(filename);

	if (!in.good())
	{
		cerr << "Could not open file '" << filename << "'" << endl;
		throw;
	}

	in >> expected;


	Measure m;
	while (in >> m)
	{
		res.push_back(m);
	}

	return res;
}

int main(int argc, char **argv)
{
	if (argc < 3)
	{
		cerr << "Not enoug arguments. Usage: ./plot <lin_reg_data>.csv <graph title>" << endl;
		return -1;
	}

	double exp_value;

	char *filename = argv[1];
	vector<Measure> data = read_file(filename, exp_value);
	double test_slope = (data[data.size() - 1].y - data[0].y) / (data[data.size() - 1].x - data[0].x);

	cout << "errori:" << endl;
	for (auto &m : data)
	{
		m.ey = sqrt(pow(m.ey, 2) + pow(m.ex * test_slope, 2));
		cout << m.ey << endl;
	}

	TApplication app("linear regression", 0, 0);
	TGraphErrors graph;

	for (int i = 0; i < data.size(); i++)
	{
		graph.SetPoint(i, data[i].x, data[i].y);
		graph.SetPointError(i, data[i].ex, data[i].ey);
	}

	TF1 line_with_intercept("lin_reg", "[0] * x + [1]", data[0].x, data[data.size() - 1].x);
	TF1 line_without_intercept("lin_reg", "[0] * x", data[0].x, data[data.size() - 1].x);
	graph.Fit(&line_without_intercept);
	graph.Fit(&line_with_intercept /*, "+"*/); // uncomment second argument to keep previous fit

	TCanvas canvas("linear regression","linear regression", 1500,1200);
	canvas.SetGrid();

	graph.SetMarkerStyle(20);
	graph.SetMarkerSize(1.5);

	graph.Draw("APsame");
	graph.SetTitle(argv[2]);
	graph.GetXaxis()->SetTitle("1/sin(#theta)");
	graph.GetXaxis()->CenterTitle(true);
	graph.GetYaxis()->SetTitle("d/m [#AA]");
	graph.GetYaxis()->CenterTitle(true);
	gPad->SetLeftMargin(0.15);

	TLegend legend(0.15, 0.7, 0.3, 0.85);
	legend.AddEntry(&graph, "data", "LE");
	// legend.AddEntry(&line_without_intercept, "fit: mx", "L");
	legend.AddEntry(&line_with_intercept, "fit: mx + q", "L");
	legend.Draw();

	double lambda = line_with_intercept.GetParameter(0);
	double lambda_err = line_with_intercept.GetParError(0);
	double intercept = line_with_intercept.GetParameter(1);
	double intercept_err = line_with_intercept.GetParError(1);

	double noq_lambda = line_without_intercept.GetParameter(0);
	double noq_lambda_err = line_without_intercept.GetParError(0);

	double t = abs(lambda - exp_value) / lambda_err;

	cout << endl << endl
		 << "Results with intercept:" << endl
		 << "lambda  = " << lambda << " +- " << lambda_err << " C/Kg" << endl
		 << "p(t)    = " << 2 * TMath::StudentI(-t, graph.GetN() - 2) << endl
		 << "p(chi2) = " << line_with_intercept.GetProb() << endl << endl
		 << "Results without intercept:" << endl
		 << "lambda  = " << noq_lambda << " +- " << noq_lambda_err << " C/Kg" << endl
		 << "p(t)    = " << 2 * TMath::StudentI(-t, graph.GetN() - 1) << endl
		 << "p(chi2) = " << line_without_intercept.GetProb() << endl << endl;

	app.Run();
}
