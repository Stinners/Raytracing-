program ray_trace 
    use mod_vec3, only: write_color, unit_vec, random_in_unit_sphere
    use mod_ray, only: Ray
    use mod_camera, only: Camera_t, init_camera
    use mod_sphere, only: sphere_t
    use mod_world, only: World_t
    use mod_random, only: random, random_color, random_double
    use mod_hittable, only: hit_record_t, material_t
    use mod_metal, only: metal_t
    use mod_lambertian, only: lambertian_t 
    use mod_dielectric, only: dielectric_t 
    use iso_fortran_env, only: error_unit
    implicit none 

! =========================================================================
!
!                        Data Declaration
!
! =========================================================================

    ! Image 
    real(8), parameter :: aspect_ratio = 3.0 / 2.0
    integer, parameter :: image_width = 1200;
    integer, parameter :: image_height = int(image_width / aspect_ratio)
    integer, parameter :: samples_per_pixel = 500
    integer, parameter :: max_depth = 50 

    ! Camera 
    real(8) :: dist_to_focus, aperture
    real(8), dimension(3) :: lookfrom, lookat, vup, dist_to_f
    type(Camera_t) camera 

    ! Init World 
    class(World_t), allocatable :: hit_world

    ! Materials 
    class(material_t), allocatable :: material_ground, material_center
    class(material_t), allocatable :: material_left, material_right

! =========================================================================
!
!                        Variable Initalization
!
! =========================================================================

    ! Setup world
    call randomize_world(hit_world)

    ! Setup Camera 
    lookfrom = [13.0, 2.0, 3.0]
    lookat = [0.0, 0.0, 0.0]
    vup = [0.0, 1.0, 0.0]
    dist_to_focus = 10.0
    aperture = 0.1
    camera = init_camera(lookfrom, lookat, vup, 20.0_8, aspect_ratio, aperture, dist_to_focus)

! =========================================================================
!
!                         Execution
!
! =========================================================================

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

        integer :: total, tenth
        total = height * width 
        tenth = total / 100

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
                call track_progress(total, tenth)
                call write_color(color, samples_per_pixel)
            end do 
        end do
    end subroutine render

    subroutine track_progress(total, tenth)
        integer, intent(in) :: total , tenth
        integer, save :: current = 0, percent = 0
        current = current + 1
        if (mod(current, tenth) == 0) then
            percent = percent + 1
            write(error_unit, "(I0, a)") percent, "% Done"
        end if 
    end subroutine track_progress

    recursive function ray_color(r, world, depth) result(res)
        class(Ray), intent(in) :: r 
        class(World_t), intent(in) :: world 
        integer, intent(in) :: depth
        type(hit_record_t) :: hit_record
        real(8) :: unit_direction(3), t(3), res(3), attenuation(3)
        type(Ray) :: scattered

        if (depth <= 0) then 
            res = [0.0, 0.0, 0.0]

        else if (world % hit(r, 0.0001_8, 1e9_8, hit_record)) then 
            if (hit_record%mat_ptr%scatter (r, hit_record, attenuation, scattered)) then
                res = attenuation * ray_color(scattered, world, depth-1)
            else 
                res = [0.0, 0.0, 0.0]
            end if 

        else 
            unit_direction = unit_vec(r % direction)
            t = 0.5 * (unit_direction(2) + 1.0)
            res = (1.0-t) * [1.0, 1.0, 1.0] + t * [0.5, 0.7, 1.0]
        end if 
    end function ray_color

    subroutine randomize_world(world) 
        type(World_t), intent(inout), allocatable :: world

        real :: choose_mat, fuzz, color(3), center(3)
        integer :: a, b

        allocate(world)
        allocate(world%spheres(23*23+4))

        ! Place big spheres 
        call world%add(sphere_t([0.0, -1000.0, 0.0], 1000.0, lambertian_t([0.5, 0.5, 0.5])))
        call world%add(sphere_t([ 0.0, 1.0, 0.0], 1.0, dielectric_t(1.5)))
        call world%add(sphere_t([-4.0, 1.0, 0.0], 1.0, lambertian_t([0.4, 0.2, 0.1])))
        call world%add(sphere_t([ 4.0, 1.0, 0.0], 1.0, metal_t([0.7, 0.6, 0.5], 0.0)))

        ! Place random spheres
        do a = -11, 11
            do b = -11, 11
                choose_mat = random()
                center = [a + 0.6*random(), 0.2_8, b + 0.6*random()]

                if (norm2(center - [4.0, 0.2, 0.0]) > 0.9) then

                    if (choose_mat < 0.8) then 
                        color = random_color() * random_color()
                        call world%add(sphere_t(center, 0.2, lambertian_t(color)))

                    else if (choose_mat < 0.95) then 
                        fuzz = random_double(0.0_8, 0.5_8)
                        color = random_color(0.5_8, 1.0_8)
                        call world%add(sphere_t(center, 0.2, metal_t(color, fuzz)))

                    else 
                        call world%add(sphere_t(center, 0.2, dielectric_t(1.5)))
                    end if 
                end if 
            end do
        end do 
        
    end subroutine randomize_world
        
end program ray_trace 
