program ray_trace 
    implicit none 

    integer, parameter :: image_width = 256 
    integer, parameter :: image_height = 256

    call render(image_width, image_height)

contains 

    subroutine render(width, height)
        integer, intent(in) :: width, height
        integer :: i, j
        real(8) :: r, g, b
        integer :: ir, ig, ib

        write (*,'(a, / I0, 1X, I0, / a)') 'P3', width, height, '255'

        do j = height-1, 0, -1
            do i = 0, width-1
            r = real(i, kind=8) / (width-1)
            g = real(j, kind=8) / (height-1)
            b = 0.25

            ir = int(255.99 * r)
            ig = int(255.99 * g)
            ib = int(255.99 * b)

            write (*, '(I0, 1X, I0, 1X, I0)') ir, ig, ib
            end do 
        end do 

    end subroutine render


end program ray_trace 
