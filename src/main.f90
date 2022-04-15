program ray_trace 
    use mod_vec3, only: write_color, unit_vec, random_in_unit_sphere
    use mod_ray, only: Ray
    use mod_camera, only: Camera_t, init_camera
    use mod_sphere, only: sphere_t
    use mod_world, only: World_t
    use mod_random, only: random
    use mod_hittable, only: hit_record_t
    implicit none 

    ! Image 
    real(8), parameter :: aspect_ratio = 16.0 / 9.0
    integer, parameter :: image_width = 400;
    integer, parameter :: image_height = int(image_width / aspect_ratio)
    integer, parameter :: samples_per_pixel = 100
    integer, parameter :: max_depth = 50 

    ! Camera 
    type(Camera_t) camera 

    ! Intit World 
    class(World_t), allocatable :: hit_world

    ! Setup world
    hit_world = World_t(spheres = [sphere_t([0.0,   0.0, -1.0], 0.5), &
                                   sphere_t([0.0,-100.5, -1.0], 100.0)])

    camera = init_camera()

    ! Do Rendering 
    call render(image_width, image_height, hit_world)

contains 

    subroutine render(width, height, world)
        integer, intent(in) :: width, height
        class(World_t), intent(in) :: world
        integer :: i, j, k
        real(8) :: u, v
        real(8) :: color(3)
        type(Ray) :: r

        ! Write file header
        write (*, '(a, / I0, 1X, I0, / a)') 'P3', width, height, '255'

        ! Looping over all possible rays
        do j = height-1, 0, -1
            do i = 0, width-1
                color = [0.0, 0.0, 0.0]
                do k = 1, samples_per_pixel
                    u = (real(i, kind=8) + random()) / (width-1)
                    v = (real(j, kind=8) + random()) / (height-1)
                    r = camera % get_ray(u, v)
                    color = color + ray_color(r, world, max_depth)
                end do 
                call write_color(color, samples_per_pixel)
            end do 
        end do

    end subroutine render

    recursive function ray_color(r, world, depth) result(res)
        class(Ray), intent(in) :: r 
        class(World_t), intent(in) :: world 
        integer, intent(in) :: depth
        type(hit_record_t) :: hit_record
        real(8) :: unit_direction(3), t(3), res(3), rand_target(3)
        type(Ray) :: reflected_ray

        if (depth <= 0) then 
            res = [0.0, 0.0, 0.0]

        else if (world % hit(r, 0.0001_8, 1e9_8, hit_record)) then 
            rand_target = hit_record%point + hit_record%normal + random_in_unit_sphere()
            reflected_ray = Ray(hit_record%point, rand_target - hit_record%point)
            res = 0.5 * ray_color(reflected_ray, world, depth-1)

        else 
            unit_direction = unit_vec(r % direction)
            t = 0.5 * (unit_direction(2) + 1.0)
            res = (1.0-t) * [1.0, 1.0, 1.0] + t * [0.5, 0.7, 1.0]
        end if 
    end function ray_color
        
end program ray_trace 
