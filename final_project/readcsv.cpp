#include <iostream>
#include <error.h>
#include <fstream>
#include <vector>

using namespace std;

struct coefs
{
    /* data */
    double Mach;
    double Cx0;
    double Cx2;
    double Cd2;
    double Cx4;
    double Cla;
    double CNa;
    double CNa3;
    double Cmag_f;
    double Cma;
    double Cma3;
    double Cma5;
    double Cmq;
    double Cmq2;
    double Cspin;
    double Cld;
    double Cxf;
    double Cxb;
    double CPN;
};

void read_record(string inname)
{
    // File pointer
    cin >> inname;
    ifstream fin{name,ios_base::in};
    cout << "nome: ";
    cin >> outname;
    ifstream ist{inname};
    ofstream ost{outname};

    vector <coefs> prodas;
    double Mach;
    double Cx0;
    double Cx2;
    double Cd2;
    double Cx4;
    double Cla;
    double CNa;
    double CNa3;
    double Cmag_f;
    double Cma;
    double Cma3;
    double Cma5;
    double Cmq;
    double Cmq2;
    double Cspin;
    double Cld;
    double Cxf;
    double Cxb;
    double CPN;

    while(fin>>Mach>>Cx0>>Cx2>>Cd2>>Cx4>>Cla>>CNa>>CNa3>>Cmag_f>>
    Cma>>Cma3>>Cma5>>Cmq>>Cmq2>>Cspin>>Cld>>Cxf>>Cxb>>CPN)
    {
        prodas.push_back(coefs{Mach,Cx0,Cx2,Cd2,Cx4,Cla,CNa,CNa3,Cmag_f,Cma,Cma3,
                            Cma5,Cmq,Cmq2,Cspin,Cld,Cxf,Cxb,CPN});
    }

    for(int i = 0; i < prodas.size(); i++){
        std::cout << prodas.at(i) << endl;
    }
};

int main(){
    read_record("prodas.csv");
}