program pvi_problem_rk4
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, charlength => PARM_SCL
    implicit none

    ! Declaring the integer variables
    integer(intlength) :: t_points, unidadeleitura, unidadeescrita, rc1, rc2
    ! Declarando variaveis da linha de comando
    character(charlength) :: input_name, output_name
    ! Declaring real variables
    real(wp) :: mass, damp_constant, spring_constant, &
    time, time_step, initial_time, final_time
    ! Declaring dynamical arrays
    real(wp), allocatable :: system_time(:)
    ! Declaring the arrays
    real(wp) :: x(3), v(3), solver(6) 
    
    call get_command_argument(1,input_name)  ! Entrada
    call get_command_argument(2,output_name) ! Saida

    open(newunit=unidadeleitura, & ! Abrir o arquivo csv pela linha de comando
    file=trim(input_name), &
    action='read', &
    iostat=rc1 &
    )

    if (rc1 .ne. 0) then     
        print *, "The file has not been read correctly."
        stop
    end if

    open(newunit=unidadeescrita, & ! Escrever neste arquivo chamado na linha de comando
    file = trim(output_name), &
    action = 'write', &
    iostat = rc2 &
    )

    call read_properties

    t_points = 1 + int((final_time-initial_time)/time_step) ! Calculando o numero de pontos para o tempo

    allocate(system_time(t_points))

    ! Condicoes iniciais do sistema
    x = 0.0e0
    v = (/0.0e0,0.0e0,0.0e0/)
    time = initial_time
    ! Preparando um vetor que armazene todos os dados para realizar o RK4
    solver = (/x,v/)

    call rk4(time,solver)

    deallocate(system_time)

    close(unidadeleitura)
    close(unidadeescrita)

    contains

    subroutine read_properties()
        read(unidadeleitura,*)
        read(unidadeleitura,*) mass, damp_constant, spring_constant
        read(unidadeleitura,*)
        read(unidadeleitura,*) time_step, initial_time, final_time
    end subroutine read_properties
    
    function accel_rk4(time,pos_and_vel) result(derivatives)
        
        real(wp) :: m1, m2, m3, b1, b2, b3, k1, k2, k3, time
        real(wp) :: derivatives(6), x_rk4(3), v_rk4(3), pos_and_vel(6)
        
        ! Massa dos corpos:
        m1 = mass; m2 = m1; m3 = m2
        ! Amortecimento b = 0.3
        b1 = damp_constant; b2 = b1; b3 = b2
        ! Constante das molas:
        k1 = spring_constant; k2 = k1; k3 = k2

        ! Separando os termos para a posicao e velocidade
        x_rk4 = pos_and_vel(1:3)
        v_rk4 = pos_and_vel(4:6)
        
        derivatives(1:3) = v
        derivatives(4) = (2*sin(time)-(b1+b2)*v_rk4(1) + b2*v_rk4(2) - (k1+k2)*x_rk4(1) + k2*x_rk4(2))/m1
        derivatives(5) = (b2*v_rk4(1) -(b2+b3)*v_rk4(2) + b3*v_rk4(3) + k2*x_rk4(1) - (k2-k3)*x_rk4(2) + k3*x_rk4(3))/m2
        derivatives(6) = (b2*v_rk4(2) - b3*v_rk4(3) + x_rk4(2)*k3 - k3*x_rk4(3))/m3

    end function

    subroutine rk4(time,pos_and_vel)
        
        integer(intlength) :: iter
        real(wp) :: time
        real(wp), dimension(6) :: const1, const2, const3, const4, pos_and_vel

        do iter = 1,t_points,1
            const1 = accel_rk4(time,pos_and_vel)
            const2 = accel_rk4(time+0.5*time_step,pos_and_vel+0.5*time_step*const1)
            const3 = accel_rk4(time+0.5*time_step,pos_and_vel+0.5*time_step*const2)
            const4 = accel_rk4(time+1.0*time_step,pos_and_vel+1.0*time_step*const3)

            pos_and_vel = pos_and_vel + time_step/6.0e0*(const1+2.0e0*(const2+const3)+const4)
            write(unidadeescrita,*) time, pos_and_vel
            time = time + time_step	
        end do
    
    end subroutine rk4

end program pvi_problem_rk4