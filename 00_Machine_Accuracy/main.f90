main_precision.f90
!gfortran -march=native -mtune=native -O3 -o precision.exe aux_math.f90 main_precision.f90
!./precision.exe reference_value
!calculates the least representative number that can be added to a given number
!Maj Arantes / IME 2019
!Used in class demostration
!This program is modular

program computer_precision

!CALLING EXTERNAL OBJECTS
   use aux_math

!VARIABLES DECLARATION
   implicit none
   real(4)::input_number, output_number
   character:: argument*20

!READING INPUT DATA
   call get_command_argument(1, argument)
   read(argument,*) input_number

!PERFORMING THE CALCULATION
   output_number = aux_math_precision(input_number)

!PRINTING THE RESULT ON SCREEN
   write(*,'(E25.16)') output_number

end program computer_precision

Algorithm_precision.f90
!calculates the least representative number that can be added to a given number
!Maj Arantes / IME 2019
!Used in class demonstration


   real(wp)::arg_input_number, output_number

   output_number = arg_input_number

   do
      output_number = output_number/real(2.0e0,kind=wp)
      if ((arg_input_number+output_number).eq.(arg_input_number)) exit
   end do

   output_number = output_number*real(2.0e0,kind=wp)

aux_math.f90
module aux_math

!GLOBAL PARAMETERS DEFINITIONS
   implicit none
   integer,parameter:: sp = 4, dp = 8
   !sp | Single Precision length (bytes)
   !sp | Double Precision length (bytes) 

!CALCULATES THE COMPUTER LEAST REPRESENTATIVE FOR SUMS TO A GIVEN NUMBER
   interface aux_math_precision
      module procedure precision_single
      module procedure precision_double
   end interface aux_math_precision

!INTERNAL OBJECTS NOT VISIBLE OUTSIDE THE MODULE
   private:: precision_single, precision_double, sp, dp

!INTERNAL SUBROUTINES AND FUNCTIONS
   contains
      function precision_single(arg_input_number) result(output_number)
         integer(4),parameter:: wp = sp
         !wp stands for Working Precision

         !double quotes for files in the same folder, use <...> instead
         include "algorithm_precision.f90"

      end function precision_single

      function precision_double(arg_input_number) result(output_number)
         integer(4),parameter:: wp = dp
         !wp stands for Working Precision

         include "algorithm_precision.f90"

     end function precision_double

end module aux_math

