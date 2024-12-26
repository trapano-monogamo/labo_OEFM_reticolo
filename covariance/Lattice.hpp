#pragma once

#include "RandGen.hpp"
#include "Experiment.hpp"

class Lattice : public Experiment {
private:
	// simulation parameters
	RandGen m_gen;
	double m_sigma_theta;

	// simulation input-output
	double m_th0_input, m_th0_measured;
	double m_th1_input, m_th1_measured;
	double m_delta_input, m_delta_measured;

public:
	Lattice(unsigned int seed, double th_error, double th0_input, double delta_input);
	~Lattice() = default;

	void simulate() override;
	void analyze() override;

	// setters
	void set_sigma_theta(double sigma_theta);

	void set_input_th0(double th0_input);
	void set_input_th1(double th0_input);
	void set_input_delta(double delta_input);

	// getters
	double get_sigma_theta() const;

	double get_measured_th0() const;
	double get_measured_th1() const;
	double get_measured_delta() const;

	double get_input_th0() const;
	double get_input_th1() const;
	double get_input_delta() const;
};
