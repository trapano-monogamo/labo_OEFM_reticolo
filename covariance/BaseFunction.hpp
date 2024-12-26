#pragma once

class BaseFunction {
public:
	BaseFunction() = default;
	virtual ~BaseFunction() = default;

	virtual double eval(double) const = 0;
};
