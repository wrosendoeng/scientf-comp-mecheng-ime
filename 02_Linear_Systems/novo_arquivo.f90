<<<<<<< HEAD
program novo

    implicit none
    integer(4) :: i,j, unit1, unit2
    real(8), parameter :: pi = 3.1415927
    real(8) :: alpha
    real(8), dimension(17) :: v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,b
    real(8), dimension(17,17) :: a

    open(newunit=unit1,status='replace',file='matrizA.txt',action='write',form='formatted',access='sequential')
    open(newunit=unit2,status='replace',file='matrizB.txt',action='write',form='formatted',access='sequential')
    
    alpha = sqrt(2.0)/2.0_8

    v1 = (/REAL(-alpha),0.0e0, 0.0e0,1.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v2 = (/REAL(-alpha),0.0e0,-1.0e0,0.0e0,REAL(-alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v3 = (/0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v4 = (/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v5 = (/0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v6 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/) 
    v7 = (/0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0,0.0e0,0.0e0,REAL(alpha),1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v8 = (/0.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0,1.0e0,0.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v9 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,REAL(-alpha),0.0e0,0.0e0,1.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0/)
    v10 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),0.0e0,-1.0e0,0.0e0,REAL(-alpha),0.0e0,0.0e0,0.0e0,0.0e0/)
    v11 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0/)
    v12 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v13 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0/)
    v14 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,REAL(-alpha),0.0e0/)
    v15 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0,0.0e0,0.0e0,1.0e0/)
    v16 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0,1.0e0,0.0e0,0.0e0/)
    v17 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0/)

    a = reshape((/v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17/),shape(a),order=(/2,1/))
    b = (/0.0e0,0.0e0,0.0e0,1.0e1,0.0e0,0.0e0,0.0e0,1.5e1,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,1.0e1,0.0e0/)
    
    do i = 1, 17
        write(unit1,*) (a(i,j),j=1,17)
        write(unit2,*) b(i)
    end do

end program novo
=======
program novo

    implicit none
    integer(4) :: i,j, unit1, unit2
    real(8), parameter :: pi = 3.1415927
    real(8) :: alpha
    real(8), dimension(17) :: v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,b
    real(8), dimension(17,17) :: a

    open(newunit=unit1,status='replace',file='matrizA.txt',action='write',form='formatted',access='sequential')
    open(newunit=unit2,status='replace',file='matrizB.txt',action='write',form='formatted',access='sequential')
    
    alpha = sqrt(2.0)/2.0_8

    v1 = (/REAL(-alpha),0.0e0, 0.0e0,1.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v2 = (/REAL(-alpha),0.0e0,-1.0e0,0.0e0,REAL(-alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v3 = (/0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v4 = (/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v5 = (/0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v6 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/) 
    v7 = (/0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0,0.0e0,0.0e0,REAL(alpha),1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v8 = (/0.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0,1.0e0,0.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v9 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,REAL(-alpha),0.0e0,0.0e0,1.0e0,REAL(alpha),0.0e0,0.0e0,0.0e0,0.0e0/)
    v10 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),0.0e0,-1.0e0,0.0e0,REAL(-alpha),0.0e0,0.0e0,0.0e0,0.0e0/)
    v11 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0/)
    v12 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0/)
    v13 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0/)
    v14 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,-1.0e0,REAL(-alpha),0.0e0/)
    v15 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0,0.0e0,0.0e0,1.0e0/)
    v16 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(alpha),0.0e0,1.0e0,0.0e0,0.0e0/)
    v17 = (/0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,REAL(-alpha),-1.0e0/)

    a = reshape((/v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17/),shape(a),order=(/2,1/))
    b = (/0.0e0,0.0e0,0.0e0,1.0e1,0.0e0,0.0e0,0.0e0,1.5e1,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,1.0e1,0.0e0/)
    
    do i = 1, 17
        write(unit1,*) (a(i,j),j=1,17)
        write(unit2,*) b(i)
    end do

end program novo
>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
