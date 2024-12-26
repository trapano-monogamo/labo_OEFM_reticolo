#include <cmath>

#include "Lattice.hpp"

Lattice::Lattice(
		unsigned int seed,
		double th_err,
		double th0_input,
		double delta_input
) : m_gen(seed)
  , m_sigma_theta(th_err)
  , m_th0_input(th0_input)
  , m_delta_input(delta_input)
{
	m_th1_input = m_delta_input + m_th0_input;
}

void Lattice::simulate() {
	m_th0_measured = m_gen.Gauss(m_th0_input, m_sigma_theta);
	m_th1_measured = m_gen.Gauss(m_th1_input, m_sigma_theta);
}

void Lattice::analyze() {
	m_delta_measured = m_th1_measured - m_th0_measured;
}

/* Setters */

void Lattice::set_sigma_theta(double sigma_theta) { m_sigma_theta = sigma_theta; }

void Lattice::set_input_th0(double th0_input) { m_th0_input = th0_input; }
void Lattice::set_input_th1(double th1_input) { m_th1_input = th1_input; }
void Lattice::set_input_delta(double delta_input) { m_delta_input = delta_input; }

/* Getters */

double Lattice::get_sigma_theta() const { return m_sigma_theta; }

double Lattice::get_input_th0() const { return m_th0_input; }
double Lattice::get_input_th1() const { return m_th1_input; }
double Lattice::get_input_delta() const { return m_delta_input; }

double Lattice::get_measured_th0() const { return m_th0_measured; }
double Lattice::get_measured_th1() const { return m_th1_measured; }
double Lattice::get_measured_delta() const { return m_delta_measured; }
