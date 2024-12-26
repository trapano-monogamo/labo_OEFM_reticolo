#pragma once

class Experiment {
public:
	Experiment() = default;
	~Experiment() = default;

	virtual void simulate() = 0;
	virtual void analyze() = 0;
};
