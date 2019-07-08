! function div_double(op2, op1) result(op3)

!    type(valder_t), INTENT(IN)          :: op1 
!    double precision, INTENT(IN)        :: op2  
!    type valder_t, INTENT(OUT)          :: op3

    op3%val = op2 / op1%val
    op3%der = -op2 / (op1%val*op1%val)*op1%der

! end function div_double