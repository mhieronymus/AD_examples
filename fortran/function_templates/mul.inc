! function mul_double(op1, op2) result(op3)

!    type(valder_t), INTENT(IN)          :: op1 
!    double precision, INTENT(IN)        :: op2  
!    type valder_t, INTENT(OUT)          :: op3

    op3%val = op1%val * op2
    op3%der = op1%der*op2

! end function mul_double