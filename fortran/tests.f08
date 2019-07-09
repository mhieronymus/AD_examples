module tests 

    use valder 
    ! use helper_functions 
    implicit none 

    double precision :: pi = 4 * atan (1.0_16)

    abstract interface
        
        function func(z)
            import                     :: valder_t
            type(valder_t)             :: func
            type(valder_t), intent(in) :: z
        end function func

        function func2(x, y, z)
            import                     :: valder_t
            type(valder_t)             :: func
            type(valder_t), intent(in) :: x,y,z 
        end function func2
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

    function tennis_serve(angle, velocity, height) result(serve)
        type(valder_t), intent(in)  :: angle, velocity, height
        type(valder_t)              :: serve

        serve = (velocity * cos(angle))**2/32 * ( tan(angle) + &
            sqrt(tan(angle)**2 + 64*height/(velocity*cos(angle))**2) )
        serve%der(1) = serve%der(1)*pi/180 

    end function tennis_serve

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

    ! function newton_multi_dim(x, f, epsin)
    !     double precision, intent(in)            :: x(:)
    !     procedure(func2), pointer, intent(in)   :: f
    !     double precision, intent(in), optional  :: epsin 
    !     double precision                        :: eps, delta
    !     integer                                 :: i 

    !     delta = 1 
    !     i = 1 
    !     ! need a solver for linear systems...


    ! end function newton_multi_dim

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
        type(valder_t)            :: a, v, h, o 

        a = valder_t(20*pi/180, [1, 0, 0])
        v = valder_t(44, [0, 1, 0]) 
        h = valder_t(9, [0, 0, 1])
        print*, v%der
        o = tan(v)
        print*, o%val, o%der
        o = tan(a)
        print*, o%val, o%der
        o = tennis_serve(a, v, h)
        print*, "tennis serve with angle ", a%val, " velocity ", v%val, &
            " height ", h%val 
        print*, "Expected: 56.046141, [1.0717, 1.9504, 1.45056]"
        print*, o%val, o%der 

    end subroutine test4 

    subroutine test5()
        ! Test multivariate stuff
        implicit none 
        type(valder_t)  :: x, y, z, o  

        x = valder_t(2, [1, 0])
        y = valder_t(3, [0, 1])
        z = x*y 
        print*, "Expected: 6, [3, 2]"
        print*, z%val, z%der
        z = x * tan(x*y)
        print*, "Expected: -0.58201, [6.2171, 4.3387]"
        print*, z%val, z%der

        x = valder_t(2, [1, 0, 0])
        y = valder_t(3, [0, 1, 0])
        z = valder_t(4, [0, 0, 1])
        o = x * tan(y*z)**2 
        print*, "Expected: 0.80863, [0.4043, -14.287, -10.71539]"
        print*, o%val, o%der 
        o = tan(y*z)/32
        print*, "Expected: -0.01987, [0, 0.17553, 0.1316]"
        print*, o%val, o%der
 
    end subroutine test5 

end module tests