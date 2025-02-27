import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def exponential_function(x, c):
    """
    Exponential function of the form f(x) = c * 7^x
    
    Args:
        x: input value
        c: coefficient to be determined
    """
    return c * (7 ** x)

# The points for interpolation
interpolation_points = [(30, 0.65), (70, 0.76), (100, 0.87), (500, 1.12), (1000, 1.64)]

# The extra point to add to the graph (but not to use for curve fitting)
extra_point = (2000, 2.65)

# Extract x and y coordinates
x_data = np.array([point[0] for point in interpolation_points])
y_data = np.array([point[1] for point in interpolation_points])

# Since the exponential growth with base 7 would be extremely large for these x values,
# we'll use a modified function where x is scaled appropriately
def modified_exponential(x, c):
    """Modified exponential function to handle large x values"""
    return c * (7 ** (x/1000))

# Fit the modified function to the data
params, covariance = curve_fit(modified_exponential, x_data, y_data)
c_value = params[0]

print(f"Fitted parameter: c = {c_value:.10e}")

# Define the function with the fitted parameter
def fitted_function(x):
    return c_value * (7 ** (x/1000))

# Create points for a smooth curve
x_curve = np.linspace(0, 2100, 1000)
y_curve = fitted_function(x_curve)

# Create the plot
plt.figure(figsize=(10, 6))
plt.plot(x_curve, y_curve, 'b-', label=f'f(x) = {c_value:.6f} × 7^(x/1000)')
plt.plot([p[0] for p in interpolation_points], [p[1] for p in interpolation_points], 'ro', markersize=8, label='Interpolation Points')

# Add the extra point with a different color/style
plt.plot(extra_point[0], extra_point[1], 'g*', markersize=10, label='Extra Point (2000, 2.65)')

# Add labels and title
plt.xlabel('x')
plt.ylabel('y')
plt.title(f'f(x) = {c_value:.6f} × 7^(x/1000)')
plt.grid(True)
plt.legend()

# Add annotations for each interpolation point
for x, y in interpolation_points:
    plt.annotate(f'({x}, {y})', 
                 xy=(x, y), 
                 xytext=(10, 10),
                 textcoords='offset points')

# Add annotation for the extra point
plt.annotate(f'({extra_point[0]}, {extra_point[1]})', 
             xy=extra_point, 
             xytext=(10, -15),
             textcoords='offset points')

# Save the plot
plt.savefig('exponential_interpolation.png', dpi=300, bbox_inches='tight')

# Verify the interpolation by calculating f(x) for each point
print("\nVerification:")
for x, y in interpolation_points:
    y_calc = fitted_function(x)
    print(f"f({x}) = {y_calc:.10f}, actual y = {y}")

# Calculate the predicted value at the extra point and compare
x_extra = extra_point[0]
y_extra_actual = extra_point[1]
y_extra_predicted = fitted_function(x_extra)
print(f"\nAt x = {x_extra}:")
print(f"Predicted y = {y_extra_predicted:.10f}")
print(f"Actual y = {y_extra_actual}")
print(f"Difference = {y_extra_actual - y_extra_predicted:.10f}")
