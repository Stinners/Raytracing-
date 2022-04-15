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

end module mod_random 
