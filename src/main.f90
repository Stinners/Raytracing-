program ray_trace 
    use mod_vec3, only: write_color, unit_vec
    use mod_ray, only: Ray
    use mod_sphere, only: sphere_t
    use mod_world, only: World_t
    use mod_hittable, only: hit_record_t
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

    ! Intit World 
    class(World_t), allocatable :: hit_world

    ! Setup world
    hit_world = World_t(spheres = [sphere_t([0.0,   0.0, -1.0], 0.5), &
                                   sphere_t([0.0,-100.5, -1.0], 100.0)])


    ! Do Rendering 
    call render(image_width, image_height, hit_world)

contains 

    subroutine render(width, height, world)
        integer, intent(in) :: width, height
        class(World_t), intent(in) :: world
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
                color = ray_color(r, world)
                call write_color(color)
            end do 
        end do

    end subroutine render

    function ray_color(r, world) result(res)
        class(Ray), intent(in) :: r 
        class(World_t), intent(in) :: world 
        type(hit_record_t) :: hit_record
        real(8) :: unit_direction(3), t(3), res(3)

        if (world % hit(r, 0.0_8, 1e9_8, hit_record)) then 
            res = 0.5 * (hit_record % normal + [1.0,1.0,1.0])
            return 
        end if 

        unit_direction = unit_vec(r % direction)
        t = 0.5 * (unit_direction(2) + 1.0)
        res = (1.0-t) * [1.0, 1.0, 1.0] + t * [0.5, 0.7, 1.0]
    end function ray_color
        
end program ray_trace 
