! This module requires a lot more work in C++ 
! But Fortran gives us most of what we need for free

module mod_Vec3 
    use iso_fortran_env, only: stdout => output_unit
    implicit none 

    public write_color
    public unit_vec

contains 

    subroutine write_color(vec, handle)
        real(8), intent(in) :: vec(3)
        integer, intent(in), optional :: handle
        integer :: color(3)
        integer :: sink, i

        if (present(handle)) then 
            sink = handle 
        else 
            sink = stdout
        end if 

        color = int(255.99 * vec)

        write (sink, '(I0, 1X, I0, 1X, I0)') color(1), color(2), color(3)
    end subroutine write_color

    pure function unit_vec(vec) result(res)
        real(8), intent(in) :: vec(3)
        real(8) :: res(3)
        res = vec / norm2(vec)
    end function unit_vec
end module mod_Vec3
