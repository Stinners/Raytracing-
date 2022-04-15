! This module requires a lot more work in C++ 
! But Fortran gives us most of what we need for free

module mod_Vec3 
    use iso_fortran_env, only: stdout => output_unit
    implicit none 

    public write_color
    public unit_vec
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
        real(8) :: color_scale, clamped 

        if (present(handle)) then 
            sink = handle 
        else 
            sink = stdout
        end if 

        color_scale = 1.0 / samples

        do i = 1,3
            clamped = clamp(vec(i) * color_scale, 0.0_8, 0.999_8)
            color(i) = int(256 * clamped)
        end do 

        write (sink, '(I0, 1X, I0, 1X, I0)') color(1), color(2), color(3)
    end subroutine write_color

    pure function unit_vec(vec) result(res)
        real(8), intent(in) :: vec(3)
        real(8) :: res(3)
        res = vec / norm2(vec)
    end function unit_vec
end module mod_Vec3
