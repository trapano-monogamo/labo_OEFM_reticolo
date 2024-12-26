#include <iostream>

#include "TApplication.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TAxis.h"
#include "TGaxis.h"

#include "Lattice.hpp"

using namespace std;

int main() {
	Lattice experiment(1, 1 / (360.0 * 60.0), 2.2785273, 0.56141424);

	unsigned int n_sims = 10000;

	TApplication app("Prism experiment simulation", 0,0);
	TCanvas c("Distributions", "Distributions", 100,100, 1000,1000);
	c.Divide(2,2);

	TCanvas d("cov", "cov", 1100,100, 1100,1100);

	TH1F th0("th0", "th0", 70, 1.,0.);
	TH1F th1("th1", "th1", 70, 1.,0.);
	TH1F delta("delta", "delta", 70, 1.,0.);
	th0.StatOverflows(kTRUE);
	th1.StatOverflows(kTRUE);
	delta.StatOverflows(kTRUE);

	TH2F cov("(#theta_{0},#theta_{c})", "(#theta_{0},#theta_{c})", 70,1.,0., 70,1.,0.);
	cov.StatOverflows(kTRUE);

	for (unsigned int i=0; i<n_sims; i++) {
		experiment.simulate();
		experiment.analyze();

		th0.Fill( experiment.get_measured_th0() );
		th1.Fill( experiment.get_measured_th1() );
		delta.Fill( experiment.get_measured_delta() );

		cov.Fill( experiment.get_measured_th0(), experiment.get_measured_delta() );
	}

	c.cd(1);  gPad->SetGrid(); th0.Draw();
	c.cd(2);  gPad->SetGrid(); th1.Draw();
	c.cd(3);  gPad->SetGrid(); delta.Draw();
	cout << endl << "th0 = "   << th0.GetMean()   << " +- " << th0.GetMeanError()   << endl
		         << "th1 = "   << th1.GetMean()   << " +- " << th1.GetMeanError()   << endl
				 << "delta = " << delta.GetMean() << " +- " << delta.GetMeanError() << endl;

	c.cd(4);  gPad->SetGrid(); cov.Draw();
	cout << endl << "cov = " << cov.GetCorrelationFactor() << endl
				 << "cov  = " << cov.GetCovariance() << endl;

	d.cd();
	gPad->SetGrid();
	gPad->SetLeftMargin(0.2);

	cov.GetXaxis()->SetTitle("#Delta#theta [rad]");
	cov.GetXaxis()->CenterTitle(true);
	cov.GetYaxis()->SetTitle("#theta_{c} [rad]");
	cov.GetYaxis()->CenterTitle(true);

	cov.Draw();

	// fix axis values spacing by limiting number of decimals allowed

	// double xmin = cov.GetXaxis()->GetXmin();
	// double xmax = cov.GetXaxis()->GetXmax();
	// double ymin = cov.GetYaxis()->GetXmin();
	// double ymax = cov.GetYaxis()->GetXmax();

	// cov.GetXaxis()->Set(10, xmin, xmax);
	// cov.GetYaxis()->Set(10, ymin, ymax);

	// TGaxis axisX = TGaxis(xmin,ymin, xmax,ymin, xmin,xmax, 10, "-");
	// axisX.SetMaxDigits(2);
	// axisX.SetDecimals(2);
	// axisX.Draw();

	// cov.GetXaxis()->SetDecimals(kFALSE);
	// cov.GetXaxis()->SetMaxDigits(2);
	// cov.GetYaxis()->SetDecimals(kFALSE);
	// cov.GetYaxis()->SetMaxDigits(2);

	app.Run();

	return 0;
}
