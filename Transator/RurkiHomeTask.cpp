// RurkiHomeTask.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <functional>
#include <fstream>

using namespace std;

double a = 0, b = 2;
double h;
int n;

double phi_function(int i, double x) {
    if (x < a + (i - 1) * h)
        return 0.0;
    else if (x <= a + i * h)
        return x - (a + (i - 1) * h);
    else if (x < a + (i + 1) * h && i != n-1)
        return -x + (a + (i + 1) * h);
    else
        return 0;
}

double phi_function_prim(int i, double x) {
    if (x < a + (i - 1) * h)
        return 0.0;
    else if (x <= a + i * h)
        return 1.0;
    else if (x < a + (i + 1) * h && i != n-1)
        return -1.0;
    else
        return 0;
}

double L_function(int j) {
    double sum = 0;
    double middle = a + j * h;
    if (j == n - 1)
        sum -= phi_function(j, middle) * cos(middle);
    if (j != 0)
        sum = sum + sin(middle) - sin(middle - h);
    if (j != n - 1)
        sum = sum + sin(middle + h) - sin(middle);
    return sum;
}

double GauseIntegral(function<double(int, int, double)> func, int i, int j, double a, double b) {
    double w1 = 1.0, w2 = 1.0;
    double x1 = 1.0 / sqrt(3), x2 = -1.0 / sqrt(3);
    double ksi1 = (b + a) / 2.0 + (((b - a) / 2.0) * x1);
    double ksi2 = (b + a) / 2.0 + (((b - a) / 2.0) * x2);
    return ((b - a) / 2.0) * (func(i, j, ksi1) + func(i, j, ksi2));
}

double B_Function(int i, int j, double a, double b) {
    double sum = GauseIntegral([](int i, int j, double x) -> double {
                    return phi_function_prim(i, x)* phi_function_prim(j, x); 
                    }, i, j, a, b);
    sum -= GauseIntegral([](int i, int j, double x) -> double {
        return phi_function(i, x)* phi_function(j, x);
        }, i, j, a, b);
    sum -= phi_function(i, 2.0) * phi_function(j, 2.0);
    return sum;
}



int main()
{
    ofstream out("plot.csv");
    cin >> n;
    if (n < 2)
        return 1;
    h = (b - a) / (n - 1);
    double *l = new double[n];
    double* c = new double[n];
    double* r = new double[n];
    double* L = new double[n];
    for (int i = 1; i < n; i++) {
        L[i] = L_function(i) * h;
    }
    c[0] = 1;
    l[1] = 0;
    r[1] = B_Function(1, 1, a, a + h * 2.0);
    r[n - 1] = 0;
    //c[1] = B_Function(1, 2)
    for (int i = 1; i < n - 1; i++) {
        c[i] = B_Function(i, i, a + h * (i - 1), a + h * (i + 1));
        r[i] = B_Function(i, i + 1, a + h * i, a + h * (i+1));
        if (i != 1)
            l[i] = B_Function(i-1, i, a + h * (i - 1), a + h * i);

    }
    c[n-1] = B_Function(n-1, n-1, b - h, b);
    l[n - 1] = B_Function(n-2, n-1, b - h, b);

    cout << "l[]: ";
    for (int i = 0; i < n; i++) {
        cout << l[i] << " ";
    }
    cout << "\nc[]: ";
    for (int i = 0; i < n; i++) {
        cout << c[i] << " ";
    }
    cout << "\nr[]: ";
    for (int i = 0; i < n; i++) {
        cout << r[i] << " ";
    }
    cout << "\nL[]: ";
    for (int i = 0; i < n; i++) {
        cout << L[i] << " ";
    }

    //Solve linear system of equations
    r[1] /= c[1];
    L[1] /= c[1];
    c[1] = 1.0;
    for (int i = 1; i < n - 1; i++) {
        c[i + 1] -= r[i] * l[i + 1];
        L[i + 1] -= L[i] * l[i + 1];
        l[i + 1] = 0;
        r[i + 1] /= c[i + 1];
        L[i + 1] /= c[i + 1];
        c[i + 1] = 1.0;
    }
    for (int i = n - 1; i != 1; i--) {
        L[i - 1] -= L[i] * r[i - 1];
        r[i - 1] = 0;
    }
    // End of block

    

    cout << "\nPoints for plot:\n";
    double h2 = (b - a) / 100.0;
    for (double x = a; x < b; x += h2) {
        double y = 0;
        for (int j = 1; j < n; j++) {
            y += L[j] * phi_function(j, x);
        }
        cout << "(" << x << "," << y << ")\n";
        out << x << "," << y << "\n";
    }
    out.close();
}
