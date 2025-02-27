import numpy as np
import matplotlib.pyplot as plt

def cubic_interpolation(points):
    """
    Perform cubic polynomial interpolation using the given points.
    
    Args:
        points: List of (x, y) coordinate pairs
        
    Returns:
        Coefficients of the cubic polynomial (a, b, c, d) where
        p(x) = a*x^3 + b*x^2 + c*x + d
    """
    # Extract x and y coordinates
    x = np.array([point[0] for point in points])
    y = np.array([point[1] for point in points])
    
    # Build the Vandermonde matrix
    A = np.vstack([x**3, x**2, x, np.ones(len(x))]).T
    
    # Solve the linear system for the coefficients
    coefficients = np.linalg.solve(A, y)
    
    return coefficients

# The four points for interpolation
interpolation_points = [(100, 1.5), (500, 11.6), (1000, 64.2), (1500, 222)]

# The extra point to add to the graph (but not to use for interpolation)
extra_point = (2000, 554)

# Calculate the coefficients using only the interpolation points
a, b, c, d = cubic_interpolation(interpolation_points)

print(f"Cubic polynomial coefficients:")
print(f"a (coefficient of x^3) = {a:.10e}")
print(f"b (coefficient of x^2) = {b:.10e}")
print(f"c (coefficient of x) = {c:.10e}")
print(f"d (constant term) = {d:.10e}")

# Create a function for the polynomial
def p(x):
    return a * x**3 + b * x**2 + c * x + d

# Create points for a smooth curve
x_curve = np.linspace(0, 2100, 1000)  # Extended range to include the extra point
y_curve = p(x_curve)

# Extract x and y coordinates for plotting
x_points = [point[0] for point in interpolation_points]
y_points = [point[1] for point in interpolation_points]

# Create the plot
plt.figure(figsize=(10, 6))
plt.plot(x_curve, y_curve, 'b-', label='Cubic Interpolation')
plt.plot(x_points, y_points, 'ro', markersize=8, label='Interpolation Points')

# Add the extra point with a different color/style
plt.plot(extra_point[0], extra_point[1], 'g*', markersize=10, label='Extra Point (2000, 554)')

# Add labels and title
plt.xlabel('x')
plt.ylabel('y')
plt.title('p(x) = 8.6e-08 * x^3 + -5.0e-05 * x^2 + 2.8e-02 * x + -9.3e-01')
plt.grid(True)
plt.legend()

# Add annotations for each interpolation point
for i, (x, y) in enumerate(interpolation_points):
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
plt.savefig('cubic_interpolation_with_extra_point.png', dpi=300, bbox_inches='tight')
plt.show()

# Verify the interpolation by calculating p(x) for each point
print("\nVerification:")
for x, y in interpolation_points:
    y_calc = p(x)
    print(f"p({x}) = {y_calc:.10f}, actual y = {y}")

# Calculate the predicted value at x=2000 and compare with the extra point
x_extra = extra_point[0]
y_extra_actual = extra_point[1]
y_extra_predicted = p(x_extra)
print(f"\nAt x = {x_extra}:")
print(f"Predicted y = {y_extra_predicted:.10f}")
print(f"Actual y = {y_extra_actual}")
print(f"Difference = {y_extra_actual - y_extra_predicted:.10f}")

# Print the polynomial expression
print("\nCubic polynomial function:")
print(f"p(x) = {a:.10e} * x^3 + {b:.10e} * x^2 + {c:.10e} * x + {d:.10e}")