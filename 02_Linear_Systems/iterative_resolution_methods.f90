!GAUSS-ELIMINATION MATRIX METHOD
!Transform linear system into an equivalent linear system with superior triangular shape

MODULE iterative_resolution_methods

    implicit NONE 
    
    integer(4) :: idx, jdx, kdx, numrows, rowpivot

        
    CONTAINS

    subroutine gauss_seidel(mainmatrixA,vectorB,numrows,aunit,bunit) !(mainmatrixA,vectorB,numrows)
        
        REAL(8) :: auxvector(numrows), mainmatrixA(numrows,numrows), matrixc(numrows,numrows), &
        & vectorB(numrows), vectorg(numrows), vectorX(numrows), error = 1.0e-3

            integer::i,j ,iter ,k
            real::old,sum,ea,es=0.00001,s
            read(*,*)((a(i,j),j=1,n),i=1,n
            read(*,*)(c(i),i=1,n)
            read(*,*)(x(i),i=1,n)
            checking diagonal dominant
            do i=1,n ;
            do j=1,n
            if(i.ne.j)s=s+abs(a(i,j
            enddo
            if(abs(a(i,i)).lt.s)then
            write(*,*) "these equtions are not diagonal
            stop ; endif
            do i=1,n
            ϰ
            x1=0.99919 , x2=3.0001, and x
            Gauss-Seidel method
            dummy
            )
            s=0
            ))
            -dominance
            ; enddo
            3=4.0001
            ϱ
            dummy=a(i,i)
            do j=1,n
            a(i,j)=a(i,j)/dummy
            enddo ; c(i)=c(i)/dummy
            enddo
            iter=0 ; k=0
            do while (iter<10.and. k==0)
            iter=iter+1
            k=1
            do i=1,n
            old=x(i)
            sum=c(i)
            do j=1,n
            if(i.ne.j)then
            sum=sum-a(i,j)*x(j)
            endif
            enddo
            x(i)=sum
            print*,x(i)
            if(x(i).ne.0 .and. k==1) ea=(abs(x(i)-old)/x(i))*100
            if(ea.gt.es) k=0
            enddo
            print*, BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
            enddo
            end
        
        ! H1_inv = H1^(-1)
        ! C = -H1_inv*R1

        ! g = (I+L1)^(-1)*D^(-1)*b
        ! g = H1_inv*D_inv*b
        
        ! Solving x(k+1) = Cx(k) + g
        
        
        ! First estimative of vector X 
        vectorX(1:numrows) = 0.0

        do kdx = 1, numrows
            do idx = 1, numrows
                do jdx = 1, numrows
                    if (idx /= jdx) then
                        vectorX(numrows) = (vectorB(idx)-mainmatrixA(idx,jdx))/mainmatrixA(idx,idx)
                    end if
                end do
            end do
        end do

    end subroutine gauss_seidel

    subroutine gauss_jacobi(mainmatrixA,vectorB,numrows) ! Gauss-Jacobi Method
                
        ! Line Criteria
        do idx = 1, numrows
            do jdx = 1, numrows
                if (idx /= jdx) then
                    alpha(idx) = abs(alpha(idx)+mainmatrixA(idx,jdx))/mainmatrixA(idx,idx)
                else
                    continue
                end if
            end do
        end do 

        ! Construction of Matrix C
        do idx = 1, numrows
            do jdx = 1, numrows
                if (idx == jdx) then
                    matrixc(idx,jdx) = 0
                else
                    matrixc(idx,jdx) = -mainmatrixA(idx,jdx)/mainmatrixA(idx,idx)
                end if
            end do
            vectorg(idx) = vectorB(idx)/mainmatrixA(idx,idx)
        end do 

        ! Solving x(k+1) = Cx(k) + g
        ! First estimative of vector X 
        vectorX(1:numrows) = 0.0

        do idx = 1, numrows
            auxvector = vectorX
            do jdx = 1, numrows
                vectorX(idx) = matrixc(idx,jdx)*vectorX(idx) + vectorg(idx)
            end do
            vecdiff(idx) = abs(vectorX(idx) - auxvector(idx))
            diff = maxval(vecdiff)/maxval(vectorX)
            if (diff .lt. error) then
                return
            end if
        end do

    end subroutine gauss_jacobi

END MODULE iterative_resolution_methods