module mod_world
    use mod_ray, only: Ray
    use mod_hittable, only: hit_record_t
    use mod_sphere, only: Sphere_t
    implicit none

    type :: World_t
        class(sphere_t), allocatable :: spheres(:)
    contains 
        procedure :: hit
    end type World_t

contains 

    logical function hit(self, r, tmin, tmax, record)
        class(World_t), intent(in) :: self
        class(Ray), intent(in) :: r 
        real(8), intent(in) :: tmin, tmax
        type(hit_record_t), intent(inout) :: record
        integer :: i
        real(8) :: closest_so_far
        type(hit_record_t) :: temp_record

        hit = .false.
        closest_so_far = tmax 

        ! Loop over spheres 
        do i = 1, size(self % spheres)
            if (self%spheres(i) % hit(r, tmin, closest_so_far, temp_record)) then 
                hit = .true.
                record = temp_record
                closest_so_far = record % t
            end if 
        end do
    end function

end module mod_world 
