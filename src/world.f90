module mod_world
    use mod_ray, only: Ray
    use mod_hittable, only: hit_record_t
    use mod_sphere, only: Sphere_t
    implicit none

    private 
    public World_t

    type :: World_t
        type(sphere_t), allocatable :: spheres(:)
        integer :: n_objects = 0
    contains 
        procedure :: hit
        procedure :: add
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
        do i = 1, (self % n_objects)
            if (self%spheres(i) % hit(r, tmin, closest_so_far, temp_record)) then 
                hit = .true.
                record = temp_record
                closest_so_far = record % t
            end if 
        end do
    end function

    subroutine add(self, new_sphere)
        class(world_t), intent(inout) :: self
        type(sphere_t), intent(in) :: new_sphere 
        integer :: i 

        i = self % n_objects + 1 
        self%spheres(i) = new_sphere
        self%n_objects = i 
    end subroutine

end module mod_world 
