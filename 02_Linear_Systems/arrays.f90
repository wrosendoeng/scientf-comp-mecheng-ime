<<<<<<< HEAD
program arrays

    implicit NONE
    real(8), dimension(2,3) :: matriz
    real(8) :: matriz2(2,3) 
    real(8), allocatable :: matrizalocavel(:,:)

    matriz = 0.0_8
    matriz2 = 1.0_8
    allocate(matrizalocavel(2,3))
    matrizalocavel = 2.0_8

    matrizalocavel = soma(2,3,matriz,matriz2)
    print *, matrizalocavel
    deallocate(matrizalocavel) !recomendado usar após o commando "allocate" para evitar vazamento de memória

    STOP
contains

    function soma(arg_nlin, arg_ncol,arg_Mat1, arg_Mat2) result(matriz_soma)
        
        integer(4) :: arg_nlin, arg_ncol
        real(8) :: arg_Mat1(:,:), arg_Mat2(:,:)
        real(8), allocatable :: matriz_soma(:,:)

        matriz_soma = arg_Mat1 + arg_Mat2
    end function

=======
program arrays

    implicit NONE
    real(8), dimension(2,3) :: matriz
    real(8) :: matriz2(2,3) 
    real(8), allocatable :: matrizalocavel(:,:)

    matriz = 0.0_8
    matriz2 = 1.0_8
    allocate(matrizalocavel(2,3))
    matrizalocavel = 2.0_8

    matrizalocavel = soma(2,3,matriz,matriz2)
    print *, matrizalocavel
    deallocate(matrizalocavel) !recomendado usar após o commando "allocate" para evitar vazamento de memória

    STOP
contains

    function soma(arg_nlin, arg_ncol,arg_Mat1, arg_Mat2) result(matriz_soma)
        
        integer(4) :: arg_nlin, arg_ncol
        real(8) :: arg_Mat1(:,:), arg_Mat2(:,:)
        real(8), allocatable :: matriz_soma(:,:)

        matriz_soma = arg_Mat1 + arg_Mat2
    end function

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
end program arrays