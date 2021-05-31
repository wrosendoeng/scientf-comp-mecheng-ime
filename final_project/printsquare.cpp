#include <iostream>

using namespace std;

void print_square(double x){
    cout << "the square of " << x << " is " << x*x << "\n";
}

int main(){
    print_square(4.0);
}