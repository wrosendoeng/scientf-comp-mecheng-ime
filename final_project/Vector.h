// Vector.h

class Vector
{
    public:
        Vector(int a) :elem{new double[s]}, sz{s} {} // construct a Vector
        double& operator[](int i){return elem[i];}   // element access: subscripting
        int size(){return sz;} 
    private:
        int sz;         // number of elements
        double* elem;   // pointer to elements
};