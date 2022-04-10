program ray_trace 
    use mod_vec3, only: write_color, unit_vec
    use mod_ray, only: Ray
    implicit none 

    ! Image 
    real(8), parameter :: aspect_ratio = 16.0 / 9.0
    integer, parameter :: image_width = 400;
    integer, parameter :: image_height = int(image_width / aspect_ratio)

    ! Camera 
    real, parameter :: viewport_height = 2.0
    real, parameter :: viewport_width = aspect_ratio * viewport_height
    real, parameter :: focal_length = 1.0

    real, parameter :: origin(3) = [0,0,0]
    real, parameter :: horizontal(3) = [real(viewport_width), 0.0, 0.0]
    real, parameter :: vertical(3) = [0.0, real(viewport_height), 0.0]
    real, parameter :: lower_left_corner(3) = origin - horizontal/2 - vertical/2 - [0.0, 0.0, focal_length]

    call render(image_width, image_height)

contains 

    subroutine render(width, height)
        integer, intent(in) :: width, height
        integer :: i, j
        real(8) :: u, v
        real(8) :: color(3)
        type(Ray) :: r

        ! Write file header
        write (*, '(a, / I0, 1X, I0, / a)') 'P3', width, height, '255'

        ! Looping over all possible rays
        do j = height-1, 0, -1
            do i = 0, width-1
                u = real(i, kind=8) / (width-1)
                v = real(j, kind=8) / (height-1)
                r = Ray(origin = origin, direction = lower_left_corner + u * horizontal + v * vertical - origin)
                color = ray_color(r)
                call write_color(color)
            end do 
        end do

    end subroutine render

    ! This takes a ray and assigns it a color, for the moment 
    ! we are just doing a simple gradient
    pure function ray_color(r) result(res)
        class(Ray), intent(in) :: r
        real(8) :: res(3), unit_direction(3), sphere(3), normal(3)
        real(8) :: t

        sphere = [0.0, 0.0, -1.0]

        t = r % hit_sphere(sphere, 0.5)
        if (t > 0.0) then 
            normal = unit_vec(r % at(t) - [0.0_8, 0.0_8, -1.0_8])
            res = 0.5 * (normal+1)
        else
            unit_direction = unit_vec(r % direction)
            t = 0.5 * (unit_direction(2)  + 1)
            res = (1-t) * [1.0, 1.0, 1.0] + t * [0.5, 0.7, 1.0]
        end if 
    end function 
end program ray_trace 
