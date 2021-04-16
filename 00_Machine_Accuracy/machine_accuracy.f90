<<<<<<< HEAD
PROGRAM machine_accuracy
  IMPLICIT NONE

  !INTEGER :: ref_value, acc_obt !EQUALS TO ZERO
  REAL(4) :: ref_value, acc_obt !SHOWS A RESIDUAL VALUE 
  !REAL(8) :: ref_value, acc_obt !SHOWS A RESIDUAL VALUE
  !COMPLEX :: ref_value, acc_obt ### SHOWS A RESIDUAL VALUE
  !LOGICAL :: ref_value, acc_obt ### GIVES AN ERROR
  !CHARACTER :: ref_value, acc_obt ### GIVES AN ERROR

  READ *, ref_value
  acc_obt = ref_value

  DO WHILE(acc_obt + ref_value .NE. ref_value)
    acc_obt = acc_obt/2
  END DO

  PRINT *, 2*acc_obt
END PROGRAM machine_accuracy

!VARIÁVEIS
!____valor_referência, precisão_obtida

!INÍCIO
!____leia valor_referência
!____precisão_obtida <- valor_referência

!____enquanto ( precisão_obtida+ valor_referência é diferente de valor_referência)
!________precisão_obtida = precisão_obtida/2
!____fim

!____imprima na tela 2*precisão_obtida

!FIM
!
=======
PROGRAM machine_accuracy
  IMPLICIT NONE

  !INTEGER :: ref_value, acc_obt !EQUALS TO ZERO
  REAL(4) :: ref_value, acc_obt !SHOWS A RESIDUAL VALUE 
  !REAL(8) :: ref_value, acc_obt !SHOWS A RESIDUAL VALUE
  !COMPLEX :: ref_value, acc_obt ### SHOWS A RESIDUAL VALUE
  !LOGICAL :: ref_value, acc_obt ### GIVES AN ERROR
  !CHARACTER :: ref_value, acc_obt ### GIVES AN ERROR

  READ *, ref_value
  acc_obt = ref_value

  DO WHILE(acc_obt + ref_value .NE. ref_value)
    acc_obt = acc_obt/2
  END DO

  PRINT *, 2*acc_obt
END PROGRAM machine_accuracy

!VARIÁVEIS
!____valor_referência, precisão_obtida

!INÍCIO
!____leia valor_referência
!____precisão_obtida <- valor_referência

!____enquanto ( precisão_obtida+ valor_referência é diferente de valor_referência)
!________precisão_obtida = precisão_obtida/2
!____fim

!____imprima na tela 2*precisão_obtida

!FIM
!
>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
