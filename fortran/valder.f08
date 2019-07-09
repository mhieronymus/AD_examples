module valder 

implicit none 

    type valder_t
        double precision                :: val 
        double precision, allocatable  :: der(:)
    end type valder_t

    interface operator(+)
        procedure add_double, add_float, add_int, add_valder, &
                  radd_double, radd_float, radd_int
    end interface operator(+)

    interface operator(*)
        procedure mul_double, mul_float, mul_int, mul_valder, &
                  rmul_double, rmul_float, rmul_int
    end interface operator(*)

    interface operator(-)
        procedure sub_double, sub_float, sub_int, sub_valder, &
                  rsub_double, rsub_float, rsub_int, sub_unary
    end interface operator(-)

    interface operator(/)
        procedure div_double, div_float, div_int, div_valder, &
                  rdiv_double, rdiv_float, rdiv_int
    end interface operator(/)

    interface operator(**)
        procedure pow_double, pow_float, pow_int, pow_valder, &
                  rpow_double, rpow_float, rpow_int
    end interface operator(**)

    interface sqrt 
        procedure sqrt_valder 
    end interface sqrt 

    interface exp 
        procedure exp_valder
    end interface exp 

    interface sin
        procedure sin_valder
    end interface sin

    interface cos
        procedure cos_valder
    end interface cos

    interface tan 
        procedure tan_valder 
    end interface tan

    interface log 
        procedure log_valder
    end interface log

    contains 
!----------------- add --------------------------------------------------------
    pure function add_double(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function add_double

    pure function add_float(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function add_float

    pure function add_int(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function add_int

    pure function radd_double(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function radd_double

    pure function radd_float(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function radd_float

    pure function radd_int(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2 
        type(valder_t)                      :: op3

        include "function_templates/add.inc"

    end function radd_int

    pure function add_valder(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t), INTENT(IN)          :: op2  
        type(valder_t)                      :: op3

        op3%val = op1%val + op2%val
        op3%der = op1%der + op2%der

    end function add_valder
    
!----------------- mul --------------------------------------------------------

    pure function mul_double(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function mul_double

    pure function mul_float(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function mul_float

    pure function mul_int(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function mul_int

    pure function rmul_double(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function rmul_double

    pure function rmul_float(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function rmul_float

    pure function rmul_int(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/mul.inc"

    end function rmul_int

    pure function mul_valder(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t), INTENT(IN)          :: op2  
        type(valder_t)                      :: op3

        op3%val = op1%val * op2%val
        op3%der = op1%val*op2%der + op1%der*op2%val

    end function mul_valder

!----------------- sub --------------------------------------------------------

    pure function sub_double(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/sub.inc"

    end function sub_double

    pure function sub_float(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/sub.inc"

    end function sub_float

    pure function sub_int(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/sub.inc"

    end function sub_int

    pure function rsub_double(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rsub.inc"

    end function rsub_double

    pure function rsub_float(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rsub.inc"

    end function rsub_float

    pure function rsub_int(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rsub.inc"

    end function rsub_int

    pure function sub_valder(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t), INTENT(IN)          :: op2  
        type(valder_t)                      :: op3

        op3%val = op1%val - op2%val
        op3%der = op1%der - op2%der 

    end function sub_valder
    
    ! Unary 
    pure function sub_unary(op1) result(op2)
        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t)                      :: op2
        op2%val = -op1%val 
        op2%der = -op1%der
    end function sub_unary 

!----------------- div --------------------------------------------------------
    pure function div_double(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/div.inc"

    end function div_double

    pure function div_float(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/div.inc"

    end function div_float

    pure function div_int(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/div.inc"

    end function div_int

    pure function rdiv_double(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rdiv.inc"

    end function rdiv_double

    pure function rdiv_float(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rdiv.inc"

    end function rdiv_float

    pure function rdiv_int(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rdiv.inc"

    end function rdiv_int

    pure function div_valder(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t), INTENT(IN)          :: op2  
        type(valder_t)                      :: op3

        op3%val = op1%val / op2%val
        op3%der = op1%der / op2%val - op1%val/(op2%val*op2%val)*op2%der  

    end function div_valder

!----------------- pow --------------------------------------------------------
    pure function pow_double(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/pow.inc"

    end function pow_double

    pure function pow_float(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/pow.inc"

    end function pow_float

    pure function pow_int(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/pow.inc"

    end function pow_int

    pure function rpow_double(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        double precision, INTENT(IN)        :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rpow.inc"

    end function rpow_double

    pure function rpow_float(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        real, INTENT(IN)                    :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rpow.inc"

    end function rpow_float

    pure function rpow_int(op2, op1) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        integer, INTENT(IN)                 :: op2  
        type(valder_t)                      :: op3

        include "function_templates/rpow.inc"

    end function rpow_int

    pure function pow_valder(op1, op2) result(op3)

        type(valder_t), INTENT(IN)          :: op1 
        type(valder_t), INTENT(IN)          :: op2  
        type(valder_t)                      :: op3

        op3%val = op1%val**op2%val
        op3%der = op1%val**op2%val * (op2%der*log(op1%val) + op2%val/op1%val)   

    end function pow_valder

!----------------- var --------------------------------------------------------
    pure function sin_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2 = valder_t(sin(op1%val), cos(op1%val)*op1%der)

    end function sin_valder

    pure function cos_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2 = valder_t(cos(op1%val), -sin(op1%val)*op1%der)

    end function cos_valder

    pure function tan_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2 = valder_t(tan(op1%val), op1%der/(cos(op1%val))**2)

    end function tan_valder

    pure function sqrt_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2%val = sqrt(op1%val)
        op2%der = op1%der/(2*sqrt(op1%val))

    end function sqrt_valder

    pure function exp_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2%val = exp(op1%val)
        op2%der = exp(op1%val)*op1%der        

    end function exp_valder

    pure function log_valder(op1) result(op2)
    
        type(valder_t), INTENT(IN)      :: op1 
        type(valder_t)                  :: op2 
        op2%val = log(op1%val)
        op2%der = op1%der/op1%val

    end function log_valder

end module valder 