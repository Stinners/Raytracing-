module mod_random 
    implicit none 

contains 

    real(8) function random()
        call random_number(random)
    end function random

    real(8) function random_double(min, max)
        real(8), intent(in) :: min, max 
        real(8) :: temp_rand
        call random_number(temp_rand)
        random_double = min + (max-min) * temp_rand
    end function random_double

    function random_color(lmin, lmax) result(color)
        real(8) :: color(3)
        real(8), intent(in), optional :: lmin, lmax
        real(8) :: amax, amin

        amax = merge(lmin, 0.0_8, present(lmin))
        amax = merge(lmax, 1.0_8, present(lmax))

        color = [random_double(amin, amax), random_double(amin,amax), random_double(amin, amax)]
    end function random_color

end module mod_random 
