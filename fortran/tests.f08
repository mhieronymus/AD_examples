module tests 

    use valder 
    ! use helper_functions 
    implicit none 

    double precision :: pi = 4 * atan (1.0_16)

    abstract interface
        
        function func(z)
            import :: valder_t
            type(valder_t)             :: func
            type(valder_t), intent(in) :: z
        end function func
    end interface

    contains

    elemental function f(x) result(y)
        type(valder_t), intent(in) :: x
        type(valder_t)             :: y
        y = exp(-sqrt(x))*sin(x*log(1.0+x*x))
    end function f 

    function f2(x) result(y)
        type(valder_t), intent(in) :: x
        type(valder_t)             :: y
        y = exp(-sqrt(x))*sin(x*log(1.0+x*x))
    end function f2

    function g1(x, y, z) result(o)
        type(valder_t), intent(in) :: x,y,z
        type(valder_t)             :: o
        o = 3*cos(y*z)-0.5
    end function g1

    function g2(x, y, z) result(o)
        type(valder_t), intent(in) :: x,y,z
        type(valder_t)             :: o
        o = x*x-81*(y+0.1)**2+sin(z)+1.06
    end function g2

    function g3(x, y, z) result(o)
        type(valder_t), intent(in) :: x,y,z
        type(valder_t)             :: o
        o = exp(-x*y)+20*z+(10*pi-3)/3
    end function g3

    function newton(x, f, epsin) result(z)
        type(valder_t), intent(in)             :: x 
        type(valder_t)                         :: y, z
        procedure(func), pointer, intent(in)   :: f
        double precision, intent(in), optional :: epsin 
        double precision                       :: eps, delta

        z = x
        if(.not. present(epsin)) then 
            eps = 0.00001
        else 
            eps = epsin 
        endif
        delta = 1
        do while(abs(delta) > eps)  
            y = f(z)
            delta = y%val/y%der(1)
            z = z - delta 
        enddo

    end function newton 

    subroutine test1()
        implicit none 
        type(valder_t)    :: x,b,c

        x = valder_t(3.0, [1])
        print*, "x"
        print*, x%val, x%der 
        print*, "x+x"
        b = x+x 
        print*, "Expected: 6, 2"
        print*, b%val, b%der 
        print*, "sin(x)"
        b = sin(x)
        print*, "Expected: 0.14112, -0.9899925"
        print*, b%val, b%der 
        print*, "x*x"
        b = x*x
        print*, "Expected: 9, 6"
        print*, b%val, b%der 
        print*, "2+x"
        b = 2 + x
        print*, "Expected: 5, 1"
        print*, b%val, b%der 
        print*, "x+2"
        b = x + 2
        print*, "Expected: 5, 1"
        print*, b%val, b%der 
        print*, "x**2"
        b = x**2
        print*, "Expected: 9, 6"
        print*, b%val, b%der 
        print*, "2**x"
        b = 2**x
        print*, "Expected: 8, 5.54517744"
        print*, b%val, b%der
        b = valder_t(4.0, [1])
        print*, "y"
        print*, b%val, b%der
        print*, "x/y"
        b = x/b
        print*, "Expected: 0.75, 0.0625"
        print*, b%val, b%der 

    end subroutine test1 

    subroutine test2()
        implicit none 
        integer                         :: i
        type(valder_t), dimension(100)  :: y, x
        ! Yeah, list comprehension! But does not work with allocatable like that -.-
        ! type(valder_t)                  :: x(500) = (/(valder_t(i*0.01, [1]), i=1,500)/)

        do i=1,100
            x(i) = valder_t(i*0.05, [1])
        enddo 
        y = f(x)
        print*, "x, y, y'"
        do i=1,100
            print*, x(i)%val, y(i)%val, y(i)%der 
        enddo

    end subroutine test2

    subroutine test3() 
        implicit none 
        
        procedure (func), pointer :: f_ptr => f2
        type(valder_t)            :: x, zero

        x = valder_t(5, [1])
        zero = newton(x, f_ptr)
        print*, "x for f(x)=0 found with Newton method"
        print*, "Expected: 4.88705597"
        print*, zero%val

    end subroutine test3

    subroutine test4()
        ! Test multivariate stuff
        implicit none 



    end subroutine test4 

end module tests