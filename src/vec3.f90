! This module requires a lot more work in C++ 
! But Fortran gives us most of what we need for free

module mod_Vec3 
    use iso_fortran_env, only: stdout => output_unit
    use mod_random, only: random, random_double
    implicit none 

    public write_color
    public unit_vec

    real(8), parameter :: epsilon = 1e-8_8

contains 

    real(8) function clamp(x, xmin, xmax)
        real(8), intent(in) :: x, xmin, xmax
        if (x < xmin) then
            clamp = xmin
        else if (x > xmax) then
            clamp = xmax
        else
            clamp = x
        end if 
    end function clamp

    subroutine write_color(vec, samples, handle)
        real(8), intent(in) :: vec(3)
        integer, intent(in), optional :: handle
        integer, intent(in) :: samples
        integer :: color(3)
        integer :: sink, i
        real(8) :: color_scale, clamped, scaled_and_gamma

        if (present(handle)) then 
            sink = handle 
        else 
            sink = stdout
        end if 

        color_scale = 1.0 / samples

        do i = 1,3
            scaled_and_gamma = sqrt(vec(i) * color_scale)
            clamped = clamp(scaled_and_gamma, 0.0_8, 0.999_8)
            color(i) = int(256 * clamped)
        end do 

        write (sink, '(I0, 1X, I0, 1X, I0)') color(1), color(2), color(3)
    end subroutine write_color

    pure function unit_vec(vec) result(res)
        real(8), intent(in) :: vec(3)
        real(8) :: res(3)
        res = vec / norm2(vec)
    end function unit_vec

    function random_vec() result(vec)
        real(8) :: vec(3)
        call random_number(vec)
    end function random_vec

    function random_scaled_vec(lmin, lmax) result(vec)
        real(8), intent(in) :: lmin, lmax 
        real(8) :: vec(3)
        vec = [random_double(lmin, lmax), random_double(lmin, lmax), random_double(lmin, lmax)]
    end function random_scaled_vec

    function random_in_unit_sphere() result(vec) 
        real(8) :: vec(3)
        do 
            vec = random_scaled_vec(-1.0_8, 1.0_8)
            if (dot_product(vec, vec) < 1.0) exit
        end do 
    end function random_in_unit_sphere

    function random_in_unit_disc() result(vec)
        real(8) :: vec(3)
        do 
            vec = [random_double(-1.0_8, 1.0_8), random_double(-1.0_8, 1.0_8), 0.0_8]
            if (dot_product(vec, vec) < 1.0) exit
        end do
    end function random_in_unit_disc


    function random_unit_vector() result(vec)
        real(8) :: vec(3)
        vec = unit_vec(random_in_unit_sphere())
    end function random_unit_vector

    logical function near_zero(vec) 
        real(8), intent(in) :: vec(3)
        near_zero = sum(abs(vec)) < (3 * epsilon)
    end function near_zero 

    function reflect(v, n) result(outgoing)
        real(8), intent(in) :: v(3), n(3)
        real(8) :: outgoing(3)
        outgoing = v - 2 * dot_product(v,n)*n
    end function reflect

    pure function cross_product(u, v) 
        real(8), intent(in) :: u(3), v(3)
        real(8) :: cross_product(3)

        cross_product(1) = u(2) * v(3) - u(3) * v(2)
        cross_product(2) = u(3) * v(1) - u(1) * v(3)
        cross_product(3) = u(1) * v(2) - u(2) * v(1)
    end function cross_product

end module mod_Vec3
